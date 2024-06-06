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
		eastl::string name = BuildStateName(stateStmnt);
		SpiteIR::State* state = EmplaceState(package, stateStmnt, name);

		for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
		{
			Stmnt* memberStmnt = stateStmnt->state.members->at(i);
			BuildMemberForState(state, memberStmnt, i);
		}

		for (Stmnt* method : stateSymbol.methods)
		{

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

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			// Name needs built after the members so naming is package qualified
			eastl::string name = BuildTemplatedStateName(stateStmnt, templates);
			SpiteIR::State* state = EmplaceState(package, stateStmnt, name);

			for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
			{
				Stmnt* memberStmnt = stateStmnt->state.members->at(i);
				BuildMemberForState(state, memberStmnt,i, generics.names, templates);
			}

			for (Stmnt* method : stateSymbol.methods)
			{

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

	SpiteIR::State* EmplaceState(SpiteIR::Package* package, Stmnt* stateStmnt, eastl::string& name)
	{
		SpiteIR::State* state = ir->AllocateState();

		state->parent = package;
		state->pos = stateStmnt->start->pos;
		state->name = package->name + ":" + name;
		state->metadata.flags = (int)*stateStmnt->state.insetFlags->flags;

		package->states[state->name] = state;
		return state;
	}

	void BuildFunctionDeclaration(SpiteIR::Package* package, Stmnt* funcStmnt)
	{

		if (funcStmnt->function.generics)
		{
			BuildGenericFunction(package, funcStmnt);
			return;
		}

		auto& funcDecl = funcStmnt->function.decl->functionDecl;
		SpiteIR::Function* function = ir->AllocateFunction();
		function->parent = package;
		function->pos = funcStmnt->start->pos;
		function->name = BuildFunctionName(funcStmnt);
		function->returnType = TypeToIRType(ir, funcStmnt->function.returnType, function, this);

		for (size_t i = 0; i < funcDecl.parameters->size(); i++)
		{
			Stmnt* param = funcDecl.parameters->at(i);
			BuildArgumentForFunction(function, param, i);
		}

		package->functions[function->name] = function;
	}

	void BuildGenericFunction(SpiteIR::Package* package, Stmnt* funcStmnt)
	{
		auto& generics = funcStmnt->function.generics->generics;
		auto& funcDecl = funcStmnt->function.decl->functionDecl;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			SpiteIR::Function* function = ir->AllocateFunction();
			function->parent = package;
			function->pos = funcStmnt->start->pos;
			function->name = BuildTemplatedFunctionName(funcStmnt, templates);
			function->returnType = TypeToIRType(ir, 
				funcStmnt->function.returnType, 
				function, this, generics.names, templates);

			for (size_t i = 0; i < funcDecl.parameters->size(); i++)
			{
				Stmnt* param = funcDecl.parameters->at(i);
				BuildArgumentForFunction(function, param, i, generics.names, templates);
			}

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