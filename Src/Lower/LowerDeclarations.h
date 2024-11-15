#pragma once
#include "EASTL/tuple.h"

#include "../Syntax/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerDefinitions.h"
#include "LowerContext.h"

struct LowerDeclarations
{
	LowerContext& context;
	eastl::hash_map<eastl::vector<Stmnt*>*, eastl::vector<SpiteIR::PlatformLib>*> linkMap;


	LowerDeclarations(LowerContext& context) : context(context)
	{}

	void BuildDeclarations()
	{
		for (auto& [key, value] : context.globalTable->packageToSymbolTable)
		{
			BuildPackageDeclarations(value);
		}
	}

	SpiteIR::Package* BuildPackageDeclarations(SymbolTable* symbolTable)
	{
		StringView& packageName = symbolTable->package->val;
		Logger::Info("LowerDeclarations:BuildPackageDeclarations Lowering declarations for package: " + packageName);
		if (MapHas(context.packageMap, packageName)) return context.packageMap[packageName];

		SpiteIR::Package* package = context.ir->AddPackage();

		package->file = *symbolTable->package->pos.file;
		package->name = BuildPackageName(symbolTable->package);
		package->parent = context.ir;

		context.packageToSymbolTableMap[package] = symbolTable;
		context.packageMap[packageName] = package;

		for (Stmnt* key : symbolTable->imports)
		{
			SymbolTable* symbolTable = context.globalTable->FindSymbolTable(key->importStmnt.packageName->val);
			SpiteIR::Package* imported = BuildPackageDeclarations(symbolTable);
			package->imports.push_back(imported);
		}

		for (auto& [key, value] : symbolTable->stateMap)
		{
			BuildStateDeclarations(package, value);
		}

		for (auto& [key, value] : symbolTable->functionMap)
		{
			BuildFunctionDeclaration(package, value);
		}

		for (auto& [key, value] : symbolTable->externFunctionMap)
		{
			BuildExternalFunctionDeclarations(package, value);
		}

		while (context.toResolveStateType.size() > 0)
		{
			eastl::tuple<eastl::string, SpiteIR::Type*> val = context.toResolveStateType.back();
			eastl::string& typeName = eastl::get<0>(val);
			SpiteIR::Type* type = eastl::get<1>(val);
			type->stateType.state = FindState(this, typeName, nullptr);
			context.toResolveStateType.pop_back();
		}

		for (auto& [key, state] : package->states)
		{
			BuildStateSize(state, state);
		}

		while (context.toResolveStateSize.size() > 0)
		{
			SpiteIR::Type* val = context.toResolveStateSize.back();
			val->size = val->stateType.state->size;
			context.toResolveStateSize.pop_back();
		}

		for (auto& [key, value] : symbolTable->globalValMap)
		{
			BuildGlobalVariableDeclaration(package, value);
		}

		return package;
	}

	void BuildStateSize(SpiteIR::State* state, SpiteIR::State* outer)
	{
		if (state->size) return;

		size_t size = 0;
		for (SpiteIR::Member* member : state->members)
		{
			member->offset = size;
			size += GetSizeForType(member->value->type, outer);
		}

		state->size = size;
	}

	size_t GetSizeForType(SpiteIR::Type* type, SpiteIR::State* state)
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
			return type->size;
		case SpiteIR::TypeKind::StateType:
			if (type->stateType.state == state)
			{
				Logger::FatalError("LowerDeclarrations:GetSizeForType Unable to get size for state: "
					+ state->name + ", it is self-referencing");
				return 0;
			}
			BuildStateSize(type->stateType.state, state);
			return type->stateType.state->size;
		case SpiteIR::TypeKind::StructureType:
		{
			size_t size = 0;
			for (SpiteIR::Type* innerType : *type->structureType.types)
			{
				size += GetSizeForType(innerType, state);
			}
			return size;
		}
		case SpiteIR::TypeKind::PointerType:
			return type->size;
		case SpiteIR::TypeKind::DynamicArrayType:
			return type->size;
		case SpiteIR::TypeKind::FixedArrayType:
			return type->size;
		case SpiteIR::TypeKind::FunctionType:
			return type->size;
		default:
			break;
		}

		//Error
		return 0;
	}

	void BuildStateDeclarations(SpiteIR::Package* package, StateSymbol& stateSymbol)
	{
		if (stateSymbol.state->state.generics)
		{
			BuildGenericState(package, stateSymbol);
			return;
		}

		BuildState(package, stateSymbol);
	}

	void BuildState(SpiteIR::Package* package, StateSymbol& stateSymbol)
	{
		Stmnt* stateStmnt = stateSymbol.state;
		SpiteIR::State* state = EmplaceState(package, stateSymbol, stateStmnt, BuildStateName(stateStmnt));

		for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
		{
			Stmnt* memberStmnt = stateStmnt->state.members->at(i);
			BuildMemberForState(state, memberStmnt, i);
		}

		context.stateASTMap[state] = { stateStmnt, nullptr };
		BuildMethodsForState(package, stateSymbol, state);
	}

	void BuildGenericState(SpiteIR::Package* package, StateSymbol& stateSymbol)
	{
		Stmnt* stateStmnt = stateSymbol.state;
		auto& generics = stateStmnt->state.generics->generics;
		eastl::vector<Token*>* genericNames = generics.names;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			SpiteIR::State* state = EmplaceState(package, stateSymbol, stateStmnt,
				BuildTemplatedStateName(stateStmnt, templates));

			for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
			{
				Stmnt* memberStmnt = stateStmnt->state.members->at(i);
				BuildMemberForState(state, memberStmnt, i, genericNames, templates);
			}

			context.stateASTMap[state] = { stateStmnt, templates };
			BuildMethodsForState(package, stateSymbol, state, genericNames, templates);
		}
	}

	SpiteIR::State* EmplaceState(SpiteIR::Package* package, StateSymbol& stateSymbol,
		Stmnt* stateStmnt, const eastl::string& name)
	{
		SpiteIR::State* state = context.ir->AllocateState();

		state->parent = package;
		state->name = name;
		state->metadata.flags = (int)*stateStmnt->state.insetFlags->flags;

		package->states[state->name] = state;
		context.stateMap[state->name] = state;
		return state;
	}

	void BuildMemberForState(SpiteIR::State* state, Stmnt* memberStmnt, size_t index,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Member* member = context.ir->AllocateMember();
		member->value = context.ir->AllocateValue();
		member->parent = state;
		member->value->parent = SpiteIR::Parent(member);
		member->value->type = TypeToIRType(context.ir, memberStmnt->definition.type, this, generics, templates);
		member->value->name = memberStmnt->definition.name->val.ToString();
		state->members.push_back(member);
	}

	void BuildMethodsForState(SpiteIR::Package* package, StateSymbol& stateSymbol, SpiteIR::State* state,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		for (Stmnt* con : stateSymbol.constructors)
		{
			BuildConstructorDeclaration(package, state, con, generics, templates);
		}
		BuildDefaultConstructor(package, state, stateSymbol.state, templates);


		for (Stmnt* method : stateSymbol.methods)
		{
			BuildMethodDeclaration(package, state, method, generics, templates);
		}

		for (Stmnt* op : stateSymbol.operators)
		{
			BuildOperatorDeclaration(package, state, op, generics, templates);
		}

		if (stateSymbol.destructor)
		{
			BuildDestructor(package, state, stateSymbol.destructor, templates);
		}
	}

	void BuildMethodThisArgument(SpiteIR::State* state, SpiteIR::Function* method, SpiteIR::IR* ir)
	{
		SpiteIR::Argument* arg = ir->AllocateArgument();
		arg->value = ir->AllocateValue();
		arg->value->parent = SpiteIR::Parent(state);
		arg->value->name = "this";
		arg->parent = method;

		SpiteIR::Type* thisType = ir->AllocateType();
		thisType->kind = SpiteIR::TypeKind::StateType;
		thisType->stateType.state = state;
		if (state->size) thisType->size = state->size;
		else context.toResolveStateSize.push_back(thisType);
		arg->value->type = MakeReferenceType(thisType, context.ir);

		method->arguments.push_back(arg);
	}

	void BuildDefaultConstructor(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* stateStmnt,
		eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* con = context.ir->AllocateFunction();
		con->parent = package;
		con->name = BuildDefaultConstructorName(stateStmnt, templates);
		con->returnType = CreateVoidType(context.ir);

		BuildMethodThisArgument(state, con, context.ir);
		state->constructors.push_back(con);
		context.functionMap[con->name] = con;
		package->functions[con->name] = con;
		context.functionASTMap[con] = { stateStmnt, templates };
	}

	void BuildConstructorDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* conStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* con = context.ir->AllocateFunction();
		con->parent = package;
		con->name = BuildConstructorName(conStmnt, generics, templates);
		con->returnType = CreateVoidType(context.ir);

		// First argument is this argument
		BuildMethodThisArgument(state, con, context.ir);
		for (size_t i = 1; i < conStmnt->constructor.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = conStmnt->constructor.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(con, param, i, generics, templates);
		}

		state->constructors.push_back(con);
		context.functionMap[con->name] = con;
		context.functionASTMap[con] = { conStmnt, templates };
		package->functions[con->name] = con;
	}

	void BuildOperatorDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* opStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* op = context.ir->AllocateFunction();
		op->parent = package;
		op->name = BuildOperatorMethodName(opStmnt, generics, templates);
		op->returnType = TypeToIRType(context.ir, opStmnt->stateOperator.returnType, this,
			generics, templates);

		// First argument is this argument
		BuildMethodThisArgument(state, op, context.ir);
		for (size_t i = 1; i < opStmnt->stateOperator.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = opStmnt->stateOperator.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(op, param, i, generics, templates);
		}

		state->operators.push_back(op);
		context.functionMap[op->name] = op;
		context.functionASTMap[op] = { opStmnt, templates };
		package->functions[op->name] = op;
	}

	void BuildMethodDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* methodStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		if (methodStmnt->method.generics)
		{
			BuildGenericMethod(package, state, methodStmnt, generics, templates);
			return;
		}

		BuildMethod(package, state, methodStmnt, methodStmnt->method.decl,
			BuildMethodName(methodStmnt), generics, templates);
	}

	void BuildGenericMethod(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* methodStmnt,
		eastl::vector<Token*>* stateGenerics = nullptr, eastl::vector<Expr*>* stateTemplates = nullptr)
	{
		auto& generics = methodStmnt->method.generics->generics;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			eastl::vector<Token*> stateAndMethodGenerics = eastl::vector<Token*>();
			if (stateGenerics) for (Token* stateGeneric : *stateGenerics) stateAndMethodGenerics.push_back(stateGeneric);
			for (Token* methodGeneric : *generics.names) stateAndMethodGenerics.push_back(methodGeneric);

			eastl::vector<Expr*> stateAndMethodTemplates = eastl::vector<Expr*>();
			if (stateTemplates) for (Expr* stateTemplate : *stateTemplates) stateAndMethodTemplates.push_back(stateTemplate);
			for (Expr* methodTemplate : *templates) stateAndMethodTemplates.push_back(methodTemplate);

			BuildMethod(package, state, methodStmnt, methodStmnt->method.decl,
				BuildTemplatedMethodName(methodStmnt, templates),
				&stateAndMethodGenerics, &stateAndMethodTemplates);
		}
	}

	void BuildMethod(SpiteIR::Package* package, SpiteIR::State* state,
		Stmnt* methodStmnt, Stmnt* methodDeclStmnt, const eastl::string& name,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* method = context.ir->AllocateFunction();
		method->parent = package;
		method->name = name;
		method->returnType = TypeToIRType(context.ir, methodStmnt->method.returnType, this, generics, templates);

		// First argument is this argument
		BuildMethodThisArgument(state, method, context.ir);
		for (size_t i = 1; i < methodStmnt->method.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = methodStmnt->method.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(method, param, i, generics, templates);
		}

		state->methods.push_back(method);
		context.functionMap[method->name] = method;
		context.functionASTMap[method] = { methodStmnt, templates };
		package->functions[method->name] = method;
	}

	void BuildDestructor(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* destructorStmnt,
		eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* destructor = context.ir->AllocateFunction();
		destructor->parent = package;
		destructor->name = BuildDestructorName(state);
		destructor->returnType = context.ir->AllocateType();
		destructor->returnType->kind = SpiteIR::TypeKind::PrimitiveType;
		destructor->returnType->primitive.kind = SpiteIR::PrimitiveKind::Void;
		destructor->returnType->size = 0;
		destructor->returnType->primitive.isSigned = false;

		// First argument is this argument
		BuildMethodThisArgument(state, destructor, context.ir);

		state->destructor = destructor;
		context.functionMap[destructor->name] = destructor;
		context.functionASTMap[destructor] = { destructorStmnt, templates };
		package->functions[destructor->name] = destructor;
	}

	void BuildFunctionDeclaration(SpiteIR::Package* package, Stmnt* funcStmnt)
	{
		if (funcStmnt->function.generics)
		{
			BuildGenericFunction(package, funcStmnt);
			return;
		}

		BuildFunction(package, funcStmnt, BuildFunctionName(funcStmnt));
	}

	void BuildGenericFunction(SpiteIR::Package* package, Stmnt* funcStmnt)
	{
		auto& generics = funcStmnt->function.generics->generics;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			BuildFunction(package, funcStmnt, BuildTemplatedFunctionName(funcStmnt, templates),
				generics.names, templates);
		}
	}

	void BuildFunction(SpiteIR::Package* package, Stmnt* funcStmnt, const eastl::string& name,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* function = context.ir->AllocateFunction();
		function->parent = package;
		function->name = name;
		function->returnType = TypeToIRType(context.ir, funcStmnt->function.returnType, this, generics, templates);

		for (size_t i = 0; i < funcStmnt->function.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = funcStmnt->function.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(function, param, i, generics, templates);
		}

		context.functionMap[function->name] = function;
		context.functionASTMap[function] = { funcStmnt, templates };
		package->functions[function->name] = function;
	}

	void BuildArgumentForFunction(SpiteIR::Function* function, Stmnt* param, size_t index,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		
		SpiteIR::Type* argType = TypeToIRType(context.ir, param->definition.type, this, generics, templates);
		if (!argType->byValue) argType = MakeReferenceType(argType, context.ir);

		SpiteIR::Argument* arg = context.ir->AllocateArgument();
		arg->value = context.ir->AllocateValue();
		arg->value->parent = SpiteIR::Parent(arg);
		arg->value->type = argType;
		arg->value->name = param->definition.name->val.ToString();
		arg->parent = function;
		function->arguments.push_back(arg);
	}

	void BuildGlobalVariableDeclaration(SpiteIR::Package* package, Stmnt* globalVarStmnt)
	{
		auto& def = globalVarStmnt->definition;
		SpiteIR::GlobalVariable* globalVar = context.ir->AllocateGlobalVariable();
		globalVar->parent = package;
		globalVar->name = BuildGlobalVariableName(globalVarStmnt);
		globalVar->type = TypeToIRType(context.ir, def.type, this);
		globalVar->index = context.ir->globalSize;
		context.ir->IncremementGlobalSize(globalVar->type);
		package->globalVariables[globalVar->name] = globalVar;
		context.globalVarASTMap[globalVar] = globalVarStmnt;
	}

	eastl::vector<SpiteIR::PlatformLib>* GetPlatformLibs(eastl::vector<Stmnt*>* links)
	{
		if (MapHas(linkMap, links)) return linkMap[links];

		eastl::vector<SpiteIR::PlatformLib>* platformLibs = context.ir->AllocateArray<SpiteIR::PlatformLib>();

		for (Stmnt* link : *links)
		{
			platformLibs->push_back({ link->linkDecl.platform->val.ToString(),
				link->linkDecl.lib->val.ToString() });
		}

		linkMap[links] = platformLibs;
		return platformLibs;
	}

	void BuildExternalFunctionDeclarations(SpiteIR::Package* package, Stmnt* externFunc)
	{
		SpiteIR::Function* function = context.ir->AllocateFunction();
		function->parent = package;
		function->name = externFunc->externFunction.callName->val.ToString();
		function->returnType = TypeToIRType(context.ir, externFunc->externFunction.returnType, this);

		for (size_t i = 0; i < externFunc->externFunction.parameters->size(); i++)
		{
			Stmnt* param = externFunc->externFunction.parameters->at(i);
			BuildArgumentForFunction(function, param, i);
		}

		function->metadata.externFunc = context.ir->AllocateExternFunction();
		function->metadata.externFunc->libs = GetPlatformLibs(externFunc->externFunction.links);
		function->metadata.externFunc->externName = externFunc->externFunction.externName->val.ToString();
		function->metadata.externFunc->callName = externFunc->externFunction.callName->val.ToString();
		context.functionMap[function->name] = function;
		context.functionASTMap[function] = { externFunc, nullptr };
		package->functions[function->name] = function;
	}
};