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

	inline StateSymbol* FindStateSymbolForState(Stmnt* state)
	{
		return FindStateSymbol(state->package->val, state->state.name->val);
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
			return FindLocalOrImportedState(type->namedType.typeName, symbolTable);
		case ImportedType:
			return FindState(type->importedType.packageName->val, type->importedType.typeName->val);
		case PointerType:
			return FindStateForType(type->pointerType.type, symbolTable);
		case ValueType:
			return FindStateForType(type->valueType.type, symbolTable);
		case ArrayType:
			return FindStateForType(type->arrayType.type, symbolTable);
		case TemplatedType:
			return FindStateForType(type->arrayType.type, symbolTable);
		default:
			return nullptr;
		}
	}

	inline Stmnt* FindLocalOrImportedState(Token* name, SymbolTable* symbolTable)
	{
		StringView& stateName = name->val;

		Stmnt* state = symbolTable->FindState(stateName);
		if (state) return state;

		for (Stmnt * import : symbolTable->imports)
		{
			StringView & package = import->importStmnt.packageName->val;
			state = FindState(package, stateName);
			if (state) return state;
		}

		return nullptr;
	}
};