#pragma once
#include "EASTL/tuple.h"

#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerDefinitions.h"

struct LowerDeclarations
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable = nullptr;
	SpiteIR::IR* ir;

	eastl::hash_map<eastl::string, SpiteIR::Package*> packageMap;
	eastl::hash_map<eastl::string, SpiteIR::State*> stateMap;

	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolve;

	LowerDefinitions lowerDef;

	LowerDeclarations(GlobalTable* globalTable, SpiteIR::IR* ir) : lowerDef(globalTable)
	{
		this->globalTable = globalTable;
		this->ir = ir;
	}

	SpiteIR::Package* BuildDeclarations(SymbolTable* symbolTable)
	{
		SpiteIR::Package* package = ir->AddPackage();

		package->file = *symbolTable->package->pos.file;
		package->name = BuildPackageName(symbolTable->package);
		package->parent = ir;

		packageMap[package->name] = package;

		for (Stmnt* key : symbolTable->imports)
		{
			if (packageMap.find(key->importStmnt.packageName->ToString()) == packageMap.end())
			{
				SymbolTable* symbolTable = globalTable->FindSymbolTable(key->importStmnt.packageName->val);
				SpiteIR::Package* imported = BuildDeclarations(symbolTable);
				package->imports.push_back(imported);
			}
		}

		this->symbolTable = symbolTable;
		lowerDef.symbolTable = symbolTable;

		toResolve.clear();
		for (auto& [key, value] : symbolTable->stateMap)
		{
			BuildStateDeclarations(package, value);
		}

		for (auto& val : toResolve)
		{
			eastl::string& typeName = eastl::get<0>(val);
			SpiteIR::Type* type = eastl::get<1>(val);
			type->stateType.state = FindState(this, typeName);
		}

		for (auto& [key, state] : package->states)
		{
			BuildStateSize(state);
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

	void BuildStateSize(SpiteIR::State* state)
	{
		if (state->size) return;

		size_t size = 0;
		for (auto& [key, member] : state->members)
		{
			size += GetSizeForType(member->value.type);
		}

		state->size = size;
	}

	size_t GetSizeForType(SpiteIR::Type* type)
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
			return type->size;
		case SpiteIR::TypeKind::StateType:
			BuildStateSize(type->stateType.state);
			return type->stateType.state->size;
		case SpiteIR::TypeKind::StructureType:
		{
			size_t size = 0;
			for (SpiteIR::Type* innerType : *type->structureType.types)
			{
				size += GetSizeForType(innerType);
			}
			return size;
		}
		case SpiteIR::TypeKind::PointerType:
			return type->size;
		case SpiteIR::TypeKind::DynamicArrayType:
			return type->size;
		case SpiteIR::TypeKind::FixedArrayType:
			return type->fixedArray.count * GetSizeForType(type->fixedArray.type);
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

			BuildMethodsForState(package, stateSymbol, state, genericNames, templates);
		}
	}

	SpiteIR::State* EmplaceState(SpiteIR::Package* package, StateSymbol& stateSymbol,
		Stmnt* stateStmnt, const eastl::string& name)
	{
		SpiteIR::State* state = ir->AllocateState();

		state->parent = package;
		state->pos = stateStmnt->start->pos;
		state->name = name;
		state->metadata.flags = (int)*stateStmnt->state.insetFlags->flags;

		package->states[state->name] = state;
		stateMap[state->name] = state;
		return state;
	}

	void BuildMemberForState(SpiteIR::State* state, Stmnt* memberStmnt, size_t index,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Member* member = ir->AllocateMember();
		member->parent = state;
		member->pos = memberStmnt->start->pos;
		member->value.type = TypeToIRType(ir, memberStmnt->definition.type, state, this, generics, templates);
		member->value.name = memberStmnt->definition.name->val.ToString();
		member->index = index;
		state->members[member->value.name] = member;
		lowerDef.BuildMemberDefinition(state, member, memberStmnt, generics, templates);
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
			BuildDestructor(package, state, stateSymbol.destructor);
		}
	}

	void BuildConstructorDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* conStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* con = ir->AllocateFunction();
		con->parent = package;
		con->pos = conStmnt->start->pos;
		con->name = BuildConstructorName(state, conStmnt, generics, templates);
		con->returnType = ir->AllocateType(con);
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
		package->functions[con->name] = con;
	}

	void BuildOperatorDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* opStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* op = ir->AllocateFunction();
		op->parent = package;
		op->pos = opStmnt->start->pos;
		op->name = BuildOperatorMethodName(state, opStmnt, generics, templates);
		op->returnType = TypeToIRType(ir, opStmnt->stateOperator.returnType, op, this);

		// First argument is this argument
		BuildMethodThisArgument(state, op, opStmnt);
		for (size_t i = 1; i < opStmnt->stateOperator.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = opStmnt->stateOperator.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(op, param, i, generics, templates);
		}

		state->operators.push_back(op);
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
		SpiteIR::Function* method = ir->AllocateFunction();
		method->parent = package;
		method->pos = methodStmnt->start->pos;
		method->name = name;
		method->returnType = TypeToIRType(ir, methodStmnt->method.returnType, method, this, generics, templates);

		// First argument is this argument
		BuildMethodThisArgument(state, method, methodStmnt);
		for (size_t i = 1; i < methodStmnt->method.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = methodStmnt->method.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(method, param, i, generics, templates);
		}

		state->methods.push_back(method);
		package->functions[method->name] = method;
	}

	void BuildMethodThisArgument(SpiteIR::State* state, SpiteIR::Function* method, Stmnt* methodStmnt)
	{
		SpiteIR::Argument* arg = ir->AllocateArgument();
		arg->parent = method;
		arg->pos = methodStmnt->start->pos;
		arg->index = 0;
		arg->value.name = "this";

		SpiteIR::Type* thisType = ir->AllocateType(method);
		thisType->kind = SpiteIR::TypeKind::StateType;
		thisType->stateType.state = state;
		arg->value.type = thisType;

		method->arguments[arg->value.name] = arg;
	}

	void BuildDestructor(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* destructorStmnt)
	{
		SpiteIR::Function* destructor = ir->AllocateFunction();
		destructor->parent = package;
		destructor->pos = destructorStmnt->start->pos;
		destructor->name = BuildDestructorName(state);
		destructor->returnType = ir->AllocateType(destructor);
		destructor->returnType->kind = SpiteIR::TypeKind::PrimitiveType;
		destructor->returnType->primitive.kind = SpiteIR::PrimitiveKind::Void;
		destructor->returnType->size = 0;
		destructor->returnType->primitive.isSigned = false;

		// First argument is this argument
		BuildMethodThisArgument(state, destructor, destructorStmnt);

		state->destructor = destructor;
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
		SpiteIR::Function* function = ir->AllocateFunction();
		function->parent = package;
		function->pos = funcStmnt->start->pos;
		function->name = name;
		function->returnType = TypeToIRType(ir, funcStmnt->function.returnType, function, this, generics, templates);

		for (size_t i = 0; i < funcStmnt->function.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = funcStmnt->function.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(function, param, i, generics, templates);
		}

		package->functions[function->name] = function;
	}

	void BuildArgumentForFunction(SpiteIR::Function* function, Stmnt* param, size_t index,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Argument* arg = ir->AllocateArgument();
		arg->parent = function;
		arg->pos = param->start->pos;
		arg->index = index;
		arg->value.type = TypeToIRType(ir, param->definition.type, arg, this, generics, templates);
		arg->value.name = param->definition.name->val.ToString();
		function->arguments[arg->value.name] = arg;
	}

	void BuildGlobalVariableDeclaration(SpiteIR::Package* package, Stmnt* globalVarStmnt)
	{
		auto& def = globalVarStmnt->definition;
		SpiteIR::Value* globalVar = ir->AllocateValue();
		globalVar->parent = SpiteIR::Parent(package);
		globalVar->pos = globalVarStmnt->start->pos;
		globalVar->name = BuildGlobalVariableName(globalVarStmnt);
		globalVar->type = TypeToIRType(ir, def.type, globalVar, this);
		package->globalVariables[globalVar->name] = globalVar;
	}
};