#pragma once
#include "EASTL/string.h"
#include "../Intermediate/SymbolTable.h"
#include "Scanner.h"

struct Parser
{
	const eastl::string& file;
	Tokens tokens;
	Scanner scanner;

	Parser(const eastl::string& file) : file(file), scanner(file)
	{
	}

	SymbolTable* Parse();
};