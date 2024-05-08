#pragma once
#include "SymbolTable.h"


struct GlobalTable
{
	eastl::hash_map<StringView, SymbolTable*, StringViewHash> symbolTableMap;
};