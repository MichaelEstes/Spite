#pragma once
#include "EASTL/string.h"
#include "../Intermediate/SymbolTable.h"
#include "Scanner.h"

struct Parser
{
	Tokens tokens;
	Scanner scanner;

	Parser(eastl::string& file) : scanner(file)
	{
	}

	SymbolTable* Parse(eastl::string& file);
};