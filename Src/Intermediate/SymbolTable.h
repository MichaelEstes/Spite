#pragma once

#include "EASTL/hash_map.h"
#include "EASTL/vector.h"

#include "../Containers/InplaceString.h"
#include "../Containers/Table.h"
#include "Node.h"

struct StateSymbol
{
	Node* state;

	eastl::vector<Node*> constructors;
	eastl::vector<Node*> methods;
	eastl::vector<Node*> operators;
	Node* destructor;
};

struct SymbolTable
{
	Node* package;
	eastl::hash_map<InplaceString, StateSymbol, InplaceStringHash> stateMap;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash> functionMap;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash> globalValMap;
	eastl::vector<Node*> onCompiles;

	void AddState(Node* state)
	{
		StateSymbol symbol = StateSymbol();
		symbol.state = state;
		stateMap[state->state.name->val] = symbol;
	}

	void AddConstructor(Node* constructor)
	{
		stateMap[constructor->constructor.stateName->val].constructors.push_back(constructor);
	}

	void AddMethod(Node* method)
	{
		stateMap[method->method.stateName->val].methods.push_back(method);
	}

	void AddOperator(Node* stateOperator)
	{
		stateMap[stateOperator->stateOperator.stateName->val].operators.push_back(stateOperator);
	}

	void SetDestructor(Node* destructor)
	{
		stateMap[destructor->destructor.stateName->val].destructor = destructor;
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