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
	DeferredContainer& deferred;

	CheckerContext(GlobalTable* globalTable, SymbolTable* symbolTable, DeferredContainer& deferred):
		globalTable(globalTable), symbolTable(symbolTable), scopeUtils(globalTable, symbolTable), 
		deferred(deferred) {}
};
