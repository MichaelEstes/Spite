#pragma once
#include "SymbolTable.h"

struct GlobalTable
{
	eastl::hash_map<StringView, SymbolTable*, StringViewHash> packageToSymbolTable;
	SymbolTable* entryTable;
	Stmnt* entryFunc;

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

	inline Stmnt* FindStatementForPackage(Token* package, Token* name)
	{
		SymbolTable* symbolTable = FindSymbolTable(package->val);
		if (!symbolTable) return nullptr;
		return symbolTable->FindStatement(name->val);
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
			if (!symbolTable) continue;
			state = symbolTable->FindState(stateName);
			if (state) return state;
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
			if (!symbolTable) continue;
			state = symbolTable->FindStateSymbol(stateName);
			if (state) return state;
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
			if (!symbolTable) continue;
			stmnt = symbolTable->FindFunction(functionName);
			if (stmnt) return stmnt;
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
			if (!symbolTable) continue;
			stmnt = symbolTable->FindGlobalVariable(globalVarName);
			if (stmnt) return stmnt;
		}

		return nullptr;
	}

	inline Stmnt* FindScopedExternFun(Token* name, SymbolTable* symbolTable)
	{
		StringView& externFuncName = name->val;

		Stmnt* stmnt = symbolTable->FindExternalFunction(externFuncName);
		if (stmnt) return stmnt;

		for (Stmnt * import : symbolTable->imports)
		{
			StringView & package = import->importStmnt.packageName->val;
			SymbolTable* symbolTable = FindSymbolTable(package);
			if (!symbolTable) continue;
			stmnt = symbolTable->FindExternalFunction(externFuncName);
			if (stmnt) return stmnt;
		}

		return nullptr;
	}

	inline Stmnt* FindScopedValue(Token* name, SymbolTable* symbolTable)
	{
		Stmnt* found = FindScopedState(name, symbolTable);
		if (!found) found = FindScopedFunction(name, symbolTable);
		if (!found) found = FindScopedGlobalVar(name, symbolTable);
		if (!found) found = FindScopedExternFun(name, symbolTable);
		return found;
	}

	inline Stmnt* FindStateMemberOrMethodStmnt(Stmnt* state, Token* name, SymbolTable* symbolTable)
	{
		Stmnt* stmnt = FindStateMember(state, name->val);
		if (stmnt)
		{
			return FindStateForType(stmnt->definition.type, symbolTable);
		}

		StateSymbol* stateSymbol = FindScopedStateSymbol(state->state.name, symbolTable);
		stmnt = FindStateMethod(stateSymbol, name->val);
		return stmnt;
	}

	Token* GetStateNameForStmnt(Stmnt* stmnt)
	{
		switch (stmnt->nodeID)
		{
		case Method:
			return stmnt->method.stateName;
		case StateOperator:
			return stmnt->stateOperator.stateName;
		case Destructor:
			return stmnt->destructor.stateName;
		case Constructor:
			return stmnt->constructor.stateName;
		default:
			break;
		}

		return nullptr;
	}

	Stmnt* FindStateForStmnt(Stmnt* stmnt, SymbolTable* symbolTable)
	{
		Token* stateName = GetStateNameForStmnt(stmnt);
		if (!stateName) return nullptr;

		return FindScopedState(stateName, symbolTable);
	}

	bool IsGenericOfStmnt(Type* type, Stmnt* stmnt, SymbolTable* symbolTable)
	{
		if (!type || !stmnt || type->typeID != TypeID::NamedType) return false;

		Token* ident = type->namedType.typeName;
		if (stmnt && IsGeneric(ident, stmnt)) return true;

		Stmnt* state = FindStateForStmnt(stmnt, symbolTable);
		return state && IsGeneric(ident, state);
	}

	bool IsGenericOfStmnt(Expr* expr, Stmnt* stmnt, SymbolTable* symbolTable)
	{
		if (expr->typeID == ExprID::TypeExpr) 
			return IsGenericOfStmnt(expr->typeExpr.type, stmnt, symbolTable);

		if (!stmnt || expr->typeID != ExprID::IdentifierExpr) return false;

		Token* ident = expr->identifierExpr.identifier;
		if (stmnt && IsGeneric(ident, stmnt)) return true;
		
		Stmnt* state = FindStateForStmnt(stmnt, symbolTable);
		return state && IsGeneric(ident, state);
	}
};