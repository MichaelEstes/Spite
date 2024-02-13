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
	eastl::hash_map<InplaceString, StateSymbol, InplaceStringHash> stateMap;

	void AddState(Node* state)
	{

	}
};