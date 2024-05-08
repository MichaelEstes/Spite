#pragma once
#include "EASTL/string.h"
#include "../Intermediate/SymbolTable.h"

class Parser
{

public:
	Parser() 
	{
	}

	SymbolTable* Parse(eastl::string& file);

private:

};