#pragma once
#include "DeferredChecker.h"
#include "../Syntax/GlobalTable.h"
#include "../Syntax/ScopeUtils.h"

struct CheckerContext
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	Stmnt* currentContext = nullptr;
	ScopeUtils scopeUtils;

	CheckerContext(GlobalTable* globalTable, SymbolTable* symbolTable):
		globalTable(globalTable), symbolTable(symbolTable), scopeUtils(globalTable, symbolTable)
	{}

	CheckerContext(GlobalTable* globalTable, SymbolTable* symbolTable, Stmnt* currentContext, 
		const ScopeUtils& scopeUtils) : globalTable(globalTable), symbolTable(symbolTable), 
		currentContext(currentContext), scopeUtils(scopeUtils)
	{}
};
