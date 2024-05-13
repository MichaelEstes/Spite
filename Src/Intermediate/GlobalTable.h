#pragma once
#include "SymbolTable.h"


struct GlobalTable
{
	eastl::hash_map<StringView, SymbolTable*, StringViewHash> packageToSymbolTable;

	void InsertTable(SymbolTable* symbolTable)
	{
		StringView& package = symbolTable->package->package.name->val;
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
};