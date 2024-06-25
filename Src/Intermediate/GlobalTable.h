#pragma once
#include "SymbolTable.h"

struct GlobalTable
{
	eastl::hash_map<StringView, SymbolTable*, StringViewHash> packageToSymbolTable;

	GlobalTable()
	{

	}

	~GlobalTable()
	{
		for (auto& [key, value] : packageToSymbolTable)
		{
			delete value;
		}
	}

	void Print()
	{
		for (auto& [key, value] : packageToSymbolTable)
		{
			value->Print();
		}
	}

	inline size_t GetSize()
	{
		size_t size = 0;

		for (auto& [key, value] : packageToSymbolTable)
		{
			size += value->GetSize();
		}

		return size;
	}

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

	inline SymbolTable* FindSymbolTable(StringView& package)
	{
		if (IsPackage(package)) 
		{
			return packageToSymbolTable[package];
		}

		return nullptr;
	}

	Stmnt* FindStateForType(Type* type, SymbolTable* symbolTable)
	{
		switch (type->typeID)
		{
		case NamedType:
			return FindScopedState(type->namedType.typeName, symbolTable);
		case ImportedType:
			return FindScopedState(type->importedType.typeName, symbolTable);
		case PointerType:
			return FindStateForType(type->pointerType.type, symbolTable);
		case ValueType:
			return FindStateForType(type->valueType.type, symbolTable);
		case ArrayType:
			return FindStateForType(type->arrayType.type, symbolTable);
		case TemplatedType:
			return FindStateForType(type->arrayType.type, symbolTable);
		case FixedArrayType:
			return FindStateForType(type->fixedArrayType.type, symbolTable);
		default:
			return nullptr;
		}
	}

	inline Stmnt* FindScopedState(Token* name, SymbolTable* symbolTable)
	{
		StringView& stateName = name->val;

		Stmnt* state = symbolTable->FindState(stateName);
		if (state) return state;

		for (Stmnt* import : symbolTable->imports)
		{
			StringView& package = import->importStmnt.packageName->val;
			SymbolTable* symbolTable = FindSymbolTable(package);
			if (symbolTable) return symbolTable->FindState(stateName);
		}

		return nullptr;
	}

	inline StateSymbol* FindScopedStateSymbol(Token* name, SymbolTable* symbolTable)
	{
		StringView& stateName = name->val;

		StateSymbol* state = symbolTable->FindStateSymbol(stateName);
		if (state) return state;

		for (Stmnt* import : symbolTable->imports)
		{
			StringView& package = import->importStmnt.packageName->val;
			SymbolTable* symbolTable = FindSymbolTable(package);
			if (symbolTable) return symbolTable->FindStateSymbol(stateName);
		}

		return nullptr;
	}

	inline Stmnt* FindScopedFunction(Token* name, SymbolTable* symbolTable)
	{
		StringView& functionName = name->val;

		Stmnt* stmnt = symbolTable->FindFunction(functionName);
		if (stmnt) return stmnt;

		for (Stmnt * import : symbolTable->imports)
		{
			StringView & package = import->importStmnt.packageName->val;
			SymbolTable* symbolTable = FindSymbolTable(package);
			if (symbolTable) return symbolTable->FindFunction(functionName);
		}

		return nullptr;
	}

	inline Stmnt* FindScopedGlobalVar(Token* name, SymbolTable* symbolTable)
	{
		StringView& globalVarName = name->val;

		Stmnt* stmnt = symbolTable->FindGlobalVariable(globalVarName);
		if (stmnt) return stmnt;

		for (Stmnt * import : symbolTable->imports)
		{
			StringView & package = import->importStmnt.packageName->val;
			SymbolTable* symbolTable = FindSymbolTable(package);
			if (symbolTable) return symbolTable->FindGlobalVariable(globalVarName);
		}

		return nullptr;
	}

	inline Stmnt* FindScopedValue(Token* name, SymbolTable* symbolTable)
	{
		Stmnt* found = FindScopedState(name, symbolTable);
		if (!found) found = FindScopedFunction(name, symbolTable);
		if (!found) found = FindScopedGlobalVar(name, symbolTable);
		return found;
	}
};