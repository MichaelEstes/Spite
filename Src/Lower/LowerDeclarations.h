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
			BuildMemberForState(state, memberStmnt->start->pos, memberStmnt->definition.name->ToString(),
				memberStmnt->definition.type, memberStmnt->definition.assignment, i);
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
			eastl::string name = BuildTemplatedStateName(stateStmnt, templates);
			SpiteIR::State* state = EmplaceState(package, stateStmnt, name);

			for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
			{
				Stmnt* memberStmnt = stateStmnt->state.members->at(i);
				Type* type = ReplaceTypeWithTemplateType(memberStmnt->definition.type, generics.names, templates);
				BuildMemberForState(state, memberStmnt->start->pos, memberStmnt->definition.name->ToString(),
					type, memberStmnt->definition.assignment, i);
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

	void BuildMemberForState(SpiteIR::State* state, Position& pos, const eastl::string& name, Type* type,
		Expr* assignment, size_t index)
	{
		SpiteIR::Member* member = ir->AllocateMember();
		member->parent = state;
		member->pos = pos;
		member->type = TypeToIRType(ir, type, member);
		member->name = name;
		member->index = index;
		state->members[name] = member;
	}

	Type* ReplaceTypeWithTemplateType(Type* type, eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
	{
		Type* returnType = type;
		if (type->typeID == TypeID::NamedType)
		{
			for (size_t i = 0; i < generics->size(); i++)
			{
				Token* genericName = generics->at(i);
				if (type->namedType.typeName->val == genericName->val)
				{
					Expr* templ = templates->at(i);
					if (templ && templ->typeID == ExprID::TypeExpr)
					{
						returnType = templ->typeExpr.type;
						break;
					}
					else AddError(genericName, "Lower:BuildGenericState Invalid expression used as type template");
				}
			}
		}

		return returnType;
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
		function->returnType = TypeToIRType(ir, funcStmnt->function.returnType, function);

		for (size_t i = 0; i < funcDecl.parameters->size(); i++)
		{
			Stmnt* param = funcDecl.parameters->at(i);
			BuildArgumentForFunction(function, param, param->definition.type, i);
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
				ReplaceTypeWithTemplateType(funcStmnt->function.returnType, generics.names, templates), 
				function);

			for (size_t i = 0; i < funcDecl.parameters->size(); i++)
			{
				Stmnt* param = funcDecl.parameters->at(i);
				Type* type = ReplaceTypeWithTemplateType(param->definition.type, generics.names, templates);
				BuildArgumentForFunction(function, param, type, i);
			}

			package->functions[function->name] = function;
		}
	}

	void BuildArgumentForFunction(SpiteIR::Function* function, Stmnt* param, Type* type, size_t index)
	{
		SpiteIR::Argument* arg = ir->AllocateArgument();
		arg->parent = function;
		arg->pos = param->start->pos;
		arg->index = index;
		arg->type = TypeToIRType(ir, type, arg);
		arg->name = param->definition.name->val.ToString();
		function->arguments[arg->name] = arg;
	}

	void BuildGlobalVariableDeclaration(SpiteIR::Package* package, Stmnt* globalVarStmnt)
	{
		auto& def = globalVarStmnt->definition;
		SpiteIR::Value* globalVar = ir->AllocateValue();
		globalVar->parent = SpiteIR::Parent(package);
		globalVar->pos = globalVarStmnt->start->pos;
		globalVar->name = package->name + ":" + def.name->val.ToString();
		globalVar->type = TypeToIRType(ir, def.type, globalVar);
		package->globalVariables[globalVar->name] = globalVar;
	}

};