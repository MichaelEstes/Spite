#pragma once
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"

struct LowerDeclarations
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	SpiteIR::IR* ir;

	eastl::hash_map<eastl::string, SpiteIR::Package*>& packageMap;
	eastl::hash_map<eastl::string, SpiteIR::Package*>& stateMap;

	LowerDeclarations(GlobalTable* globalTable, SpiteIR::IR* ir, 
		eastl::hash_map<eastl::string, SpiteIR::Package*>& packageMap,
		eastl::hash_map<eastl::string, SpiteIR::Package*>& stateMap): packageMap(packageMap), stateMap(stateMap)
	{
		this->globalTable = globalTable;
		this->ir = ir;
		symbolTable = nullptr;
	}

	void BuildDeclarations(SpiteIR::Package* package, SymbolTable* symbolTable)
	{
		this->symbolTable = symbolTable;

		for (auto& [key, value] : symbolTable->stateMap)
		{
			BuildStateDeclarations(package, value);
		}

		for (auto& [key, value] : symbolTable->functionMap)
		{
			BuildFunctionDeclaration(package, value);
		}

		for (auto& [key, value] : symbolTable->globalValMap)
		{
			BuildGlobalVariableDeclaration(package, value);
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
		SpiteIR::State* state = EmplaceState(package, stateStmnt, BuildStateName(stateStmnt));

		for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
		{
			Stmnt* memberStmnt = stateStmnt->state.members->at(i);
			BuildMemberForState(state, memberStmnt, i);
		}

		for (Stmnt* method : stateSymbol.methods)
		{
			BuildMethod(package, state, method);
		}

		for (Stmnt* con : stateSymbol.constructors)
		{

		}

		for (Stmnt* op : stateSymbol.operators)
		{

		}

		if (stateSymbol.destructor)
		{

		}
	}

	void BuildGenericState(SpiteIR::Package* package, StateSymbol& stateSymbol)
	{
		Stmnt* stateStmnt = stateSymbol.state;
		auto& generics = stateStmnt->state.generics->generics;
		eastl::vector<Token*>* genericNames = generics.names;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			SpiteIR::State* state = EmplaceState(package, stateStmnt, BuildTemplatedStateName(stateStmnt, templates));

			for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
			{
				Stmnt* memberStmnt = stateStmnt->state.members->at(i);
				BuildMemberForState(state, memberStmnt,i, genericNames, templates);
			}

			for (Stmnt* method : stateSymbol.methods)
			{
				BuildMethod(package, state, method);
			}

			for (Stmnt* con : stateSymbol.constructors)
			{

			}

			for (Stmnt* op : stateSymbol.operators)
			{

			}

			if (stateSymbol.destructor)
			{

			}
		}
	}

	void BuildMethod(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* methodStmnt)
	{
		if (methodStmnt->method.generics)
		{
			for (eastl::vector<Expr*>* templates : *methodStmnt->method.generics->generics.templatesToExpand)
			{
				BuildGenericMethod(package, state, methodStmnt,
					methodStmnt->method.generics->generics.names, templates);
			}

			return;
		}

		SpiteIR::Function* method = BuildFunction(package, methodStmnt, methodStmnt->method.returnType,
			methodStmnt->method.decl, BuildMethodName(state, methodStmnt));

		state->methods.push_back(method);
		package->functions[method->name] = method;
	}

	void BuildGenericMethod(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* methodStmnt,
		eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
	{
		SpiteIR::Function* method = BuildFunction(package, methodStmnt, methodStmnt->method.returnType,
			methodStmnt->method.decl, BuildTemplatedMethodName(state, methodStmnt, templates),
			generics, templates);

		state->methods.push_back(method);
		package->functions[method->name] = method;
	}

	void BuildMemberForState(SpiteIR::State* state, Stmnt* memberStmnt, size_t index, 
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Member* member = ir->AllocateMember();
		member->parent = state;
		member->pos = memberStmnt->start->pos;
		member->type = TypeToIRType(ir, memberStmnt->definition.type, state, this, generics, templates);
		member->name = memberStmnt->definition.name->val.ToString();
		member->index = index;
		state->members[member->name] = member;
	}

	SpiteIR::State* EmplaceState(SpiteIR::Package* package, Stmnt* stateStmnt, const eastl::string& name)
	{
		SpiteIR::State* state = ir->AllocateState();

		state->parent = package;
		state->pos = stateStmnt->start->pos;
		state->name = name;
		state->metadata.flags = (int)*stateStmnt->state.insetFlags->flags;

		package->states[state->name] = state;
		return state;
	}

	SpiteIR::Function* BuildFunction(SpiteIR::Package* package, Stmnt* funcLike, Type* returnType,
		Stmnt* funcDecl, const eastl::string& name,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* function = ir->AllocateFunction();
		function->parent = package;
		function->pos = funcLike->start->pos;
		function->name = name;
		function->returnType = TypeToIRType(ir, returnType, function, this, generics, templates);

		for (size_t i = 0; i < funcDecl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = funcDecl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(function, param, i, generics, templates);
		}

		return function;
	}

	void BuildFunctionDeclaration(SpiteIR::Package* package, Stmnt* funcStmnt)
	{

		if (funcStmnt->function.generics)
		{
			BuildGenericFunction(package, funcStmnt);
			return;
		}


		SpiteIR::Function* function = BuildFunction(package, funcStmnt, funcStmnt->function.returnType,
			funcStmnt->function.decl, BuildFunctionName(funcStmnt));

		package->functions[function->name] = function;
	}

	void BuildGenericFunction(SpiteIR::Package* package, Stmnt* funcStmnt)
	{
		auto& generics = funcStmnt->function.generics->generics;
		auto& funcDecl = funcStmnt->function.decl->functionDecl;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			
			SpiteIR::Function* function = BuildFunction(package, funcStmnt, funcStmnt->function.returnType,
				funcStmnt->function.decl, BuildTemplatedFunctionName(funcStmnt, templates),
				generics.names, templates);

			package->functions[function->name] = function;
		}
	}

	void BuildArgumentForFunction(SpiteIR::Function* function, Stmnt* param, size_t index, 
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Argument* arg = ir->AllocateArgument();
		arg->parent = function;
		arg->pos = param->start->pos;
		arg->index = index;
		arg->type = TypeToIRType(ir, param->definition.type, arg, this, generics, templates);
		arg->name = param->definition.name->val.ToString();
		function->arguments[arg->name] = arg;
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