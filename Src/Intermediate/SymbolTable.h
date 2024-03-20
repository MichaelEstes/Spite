#pragma once

#include "EASTL/hash_map.h"
#include "EASTL/vector.h"

#include "../Containers/InplaceString.h"
#include "../Containers/Table.h"
#include "Node.h"

struct StateSymbol
{
	Node* state = nullptr;

	eastl::vector<Node*> constructors;
	eastl::vector<Node*> methods;
	eastl::vector<Node*> operators;
	Node* destructor = nullptr;
};

struct SymbolTable
{
	Node* package;
	eastl::hash_map<InplaceString, StateSymbol, InplaceStringHash> stateMap;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash> functionMap;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash> globalValMap;
	eastl::vector<Node*> onCompiles;

	StateSymbol& GetOrCreateState(const InplaceString& name)
	{
		if (stateMap.find(name) != stateMap.end())
		{
			return stateMap[name];
		}
		else
		{
			stateMap[name] = StateSymbol();
			return stateMap[name];
		}
	}
	
	void AddState(Node* state)
	{
		StateSymbol& symbol = GetOrCreateState(state->state.name->val);
		symbol.state = state;
	}

	void AddConstructor(Node* constructor)
	{
		StateSymbol& symbol = GetOrCreateState(constructor->constructor.stateName->val);
		symbol.constructors.push_back(constructor);
	}

	void AddMethod(Node* method)
	{
		StateSymbol& symbol = GetOrCreateState(method->method.stateName->val);
		symbol.methods.push_back(method);
	}

	void AddOperator(Node* stateOperator)
	{
		StateSymbol& symbol = GetOrCreateState(stateOperator->stateOperator.stateName->val);
		symbol.operators.push_back(stateOperator);
	}

	void SetDestructor(Node* destructor)
	{
		StateSymbol& symbol = GetOrCreateState(destructor->destructor.stateName->val);
		symbol.destructor = destructor;
	}

	void AddFunction(Node* function)
	{
		functionMap[function->function.name->val] = function;
	}

	void AddGlobalVal(Node* globalVal)
	{
		globalValMap[globalVal->definition.name->val] = globalVal;
	}

	void AddOnCompile(Node* compile)
	{
		onCompiles.push_back(compile);
	}
};