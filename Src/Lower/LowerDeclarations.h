#pragma once
#include "EASTL/tuple.h"

#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerDefinitions.h"
#include "LowerContext.h"

struct LowerDeclarations
{
	LowerContext& context;
	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolve;

	LowerDeclarations(LowerContext& context) : context(context)
	{}

	SpiteIR::Package* BuildDeclarations(SymbolTable* symbolTable)
	{
		SpiteIR::Package* package = context.ir->AddPackage();

		package->file = *symbolTable->package->pos.file;
		package->name = BuildPackageName(symbolTable->package);
		package->parent = context.ir;

		context.packageMap[package->name] = package;

		for (Stmnt* key : symbolTable->imports)
		{
			if (context.packageMap.find(key->importStmnt.packageName->ToString()) == context.packageMap.end())
			{
				SymbolTable* symbolTable = context.globalTable->FindSymbolTable(key->importStmnt.packageName->val);
				SpiteIR::Package* imported = BuildDeclarations(symbolTable);
				package->imports.push_back(imported);
			}
		}

		context.symbolTable = symbolTable;

		for (auto& [key, value] : symbolTable->stateMap)
		{
			BuildStateDeclarations(package, value);
		}

		while (toResolve.size() > 0)
		{
			eastl::tuple<eastl::string, SpiteIR::Type*> val = toResolve.back();
			eastl::string& typeName = eastl::get<0>(val);
			SpiteIR::Type* type = eastl::get<1>(val);
			type->stateType.state = FindState(this, typeName, nullptr);
			toResolve.pop_back();
		}

		for (auto& [key, state] : package->states)
		{
			BuildStateSize(state, state);
		}

		for (auto& [key, value] : symbolTable->functionMap)
		{
			BuildFunctionDeclaration(package, value);
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
		for (auto& [key, member] : state->members)
		{
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
			return type->fixedArray.count * GetSizeForType(type->fixedArray.type, state);
		case SpiteIR::TypeKind::FunctionType:
			return type->size;
		default:
			break;
		}
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
		member->index = index;
		state->members[member->value->name] = member;
	}

	void BuildMethodsForState(SpiteIR::Package* package, StateSymbol& stateSymbol, SpiteIR::State* state,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		for (Stmnt* con : stateSymbol.constructors)
		{
			BuildConstructorDeclaration(package, state, con, generics, templates);
		}

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

	void BuildConstructorDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* conStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* con = context.ir->AllocateFunction();
		con->parent = package;
		con->name = BuildConstructorName(state, conStmnt, generics, templates);
		con->returnType = context.ir->AllocateType();
		con->returnType->kind = SpiteIR::TypeKind::StateType;
		con->returnType->stateType.state = state;

		// First argument is this argument
		BuildMethodThisArgument(state, con, conStmnt);
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
		op->name = BuildOperatorMethodName(state, opStmnt, generics, templates);
		op->returnType = TypeToIRType(context.ir, opStmnt->stateOperator.returnType, this,
			generics, templates);

		// First argument is this argument
		BuildMethodThisArgument(state, op, opStmnt);
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
			BuildMethodName(state, methodStmnt), generics, templates);
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
				BuildTemplatedMethodName(state, methodStmnt, templates),
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
		BuildMethodThisArgument(state, method, methodStmnt);
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

	void BuildMethodThisArgument(SpiteIR::State* state, SpiteIR::Function* method, Stmnt* methodStmnt)
	{
		SpiteIR::Argument* arg = context.ir->AllocateArgument();
		arg->value = context.ir->AllocateValue();
		arg->value->parent = SpiteIR::Parent(state);
		arg->value->name = "this";
		arg->parent = method;
		arg->index = 0;

		SpiteIR::Type* thisType = context.ir->AllocateType();
		thisType->kind = SpiteIR::TypeKind::StateType;
		thisType->stateType.state = state;
		arg->value->type = thisType;

		method->arguments[arg->value->name] = arg;
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
		BuildMethodThisArgument(state, destructor, destructorStmnt);

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
		SpiteIR::Argument* arg = context.ir->AllocateArgument();
		arg->value = context.ir->AllocateValue();
		arg->value->parent = SpiteIR::Parent(arg);
		arg->value->type = TypeToIRType(context.ir, param->definition.type, this, generics, templates);
		arg->value->name = param->definition.name->val.ToString();
		arg->parent = function;
		arg->index = index;
		function->arguments[arg->value->name] = arg;
	}

	void BuildGlobalVariableDeclaration(SpiteIR::Package* package, Stmnt* globalVarStmnt)
	{
		auto& def = globalVarStmnt->definition;
		SpiteIR::Value* globalVar = context.ir->AllocateValue();
		globalVar->parent = SpiteIR::Parent(package);
		globalVar->name = BuildGlobalVariableName(globalVarStmnt);
		globalVar->type = TypeToIRType(context.ir, def.type, this);
		package->globalVariables[globalVar->name] = globalVar;
	}
};