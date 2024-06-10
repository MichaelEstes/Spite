#pragma once
#include "../Intermediate/GlobalTable.h"
#include "EASTL/deque.h"
#include "DeferredChecker.h"

struct CheckerContext
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	Stmnt* currentContext = nullptr;
	Stmnt* currentStateContext = nullptr;
	eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>> scopeQueue;
	DeferredContainer& deferred;

	CheckerContext(GlobalTable* globalTable, SymbolTable* symbolTable, DeferredContainer& deferred):
		globalTable(globalTable), symbolTable(symbolTable), deferred(deferred) {}
};
