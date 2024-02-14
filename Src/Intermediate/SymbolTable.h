#pragma once

#include "EASTL/hash_map.h"
#include "EASTL/vector.h"

#include "../Containers/InplaceString.h"
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

	void AddState(Node* state)
	{
		StateSymbol symbol = StateSymbol();
		symbol.state = state;
		stateMap[state->state.name->val] = symbol;
	}
};