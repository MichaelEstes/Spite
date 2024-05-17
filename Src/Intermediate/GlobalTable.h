#pragma once
#include "SymbolTable.h"


struct GlobalTable
{
	eastl::hash_map<StringView, SymbolTable*, StringViewHash> packageToSymbolTable;

	void InsertTable(SymbolTable* symbolTable)
	{
		StringView& package = symbolTable->package->val;
		if (packageToSymbolTable.find(package) == packageToSymbolTable.end())
		{
			packageToSymbolTable[package] = symbolTable;
		}
		else
		{
			packageToSymbolTable[package]->Merge(symbolTable);
		}
	}

	inline bool IsPackage(StringView& package)
	{
		return packageToSymbolTable.find(package) != packageToSymbolTable.end();
	}

	inline StateSymbol* FindStateSymbol(StringView& package, StringView& name)
	{
		SymbolTable* symbolTable = FindSymbolTable(package);
		if (!symbolTable) return nullptr;

		return symbolTable->FindStateSymbol(name);
	}

	inline Stmnt* FindState(StringView& package, StringView& name)
	{
		SymbolTable* symbolTable = FindSymbolTable(package);
		if (!symbolTable) return nullptr;

		return symbolTable->FindState(name);
	}

	inline Stmnt* FindStateOrFunction(StringView& package, StringView& name)
	{
		SymbolTable* symbolTable = FindSymbolTable(package);
		if (!symbolTable) return nullptr;

		return symbolTable->FindStateOrFunction(name);
	}

	inline SymbolTable* FindSymbolTable(StringView& package)
	{

		return packageToSymbolTable[package];
	}
};