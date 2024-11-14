#pragma once
#include "EASTL/string.h"
#include "../Syntax/SymbolTable.h"
#include "Scanner.h"

struct Parser
{
	const eastl::string& file;
	Tokens tokens;
	Scanner scanner;

	Parser(const eastl::string& file) : file(file)
	{
	}

	SymbolTable* Parse();

	~Parser() = default;
};