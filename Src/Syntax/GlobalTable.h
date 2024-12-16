#pragma once
#include "SymbolTable.h"

struct GlobalTable
{
	eastl::hash_map<StringView, SymbolTable*, StringViewHash> packageToSymbolTable;
	SymbolTable* runtimeTable;
	SymbolTable* entryTable;
	StateSymbol* arraySymbol;
	StateSymbol* stringSymbol;
	Stmnt* entryFunc;

	StringView runtimePackage = StringView("_");
	eastl::vector<SymbolTable*> merged;

	GlobalTable()
	{

	}

	~GlobalTable()
	{
		for (auto& [key, value] : packageToSymbolTable)
		{
			delete value;
		}

		for (SymbolTable* mergedTable : merged)
		{
			delete mergedTable;
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
			merged.push_back(symbolTable);
		}
	}

	void SetRuntimeTable()
	{
		SymbolTable* runtime = packageToSymbolTable[runtimePackage];
		this->runtimeTable = runtime;
		StringView arrayStateName = "array";
		this->arraySymbol = this->runtimeTable->FindStateSymbol(arrayStateName);
		StringView stringStateName = "_string";
		this->stringSymbol = this->runtimeTable->FindStateSymbol(stringStateName);

		packageToSymbolTable.erase(runtimePackage);
	}

	inline Stmnt* GetArrayState()
	{
		return this->arraySymbol->state;
	}

	inline Stmnt* GetStringState()
	{
		return this->stringSymbol->state;
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
		else if (package == runtimePackage) return runtimeTable;

		return nullptr;
	}

	inline Stmnt* FindStatementForPackage(Token* package, Token* name)
	{
		SymbolTable* symbolTable = FindSymbolTable(package->val);
		if (symbolTable) return symbolTable->FindStatement(name->val);
		return runtimeTable->FindStatement(name->val);
	}

	Stmnt* FindStateForType(Type* type, SymbolTable* symbolTable)
	{
		switch (type->typeID)
		{
		case PrimitiveType:
			if (type->primitiveType.type == UniqueType::String) return GetStringState();
			break;
		case NamedType:
			return FindScopedState(type->namedType.typeName, symbolTable);
		case ImportedType:
			return FindState(type->importedType.packageName, type->importedType.typeName);
		case PointerType:
			return FindStateForType(type->pointerType.type, symbolTable);
		case ValueType:
			return FindStateForType(type->valueType.type, symbolTable);
		case TemplatedType:
			return FindStateForType(type->templatedType.type, symbolTable);
		case ArrayType:
			return GetArrayState();
		case FixedArrayType:
			return FindStateForType(type->fixedArrayType.type, symbolTable);
		default:
			break;
		}
		return nullptr;
	}

	Stmnt* FindState(Token* package, Token* name)
	{
		SymbolTable* symbolTable = FindSymbolTable(package->val);
		if (!symbolTable) return nullptr;
		return symbolTable->FindState(name->val);
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

		return runtimeTable->FindState(stateName);
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

		return runtimeTable->FindStateSymbol(stateName);;
	}

	Stmnt* FindEnumForType(Type* type, SymbolTable* symbolTable)
	{
		switch (type->typeID)
		{
		case NamedType:
			return FindScopedEnum(type->namedType.typeName, symbolTable);
		case ImportedType:
		{
			SymbolTable* symbolTable = FindSymbolTable(type->importedType.packageName->val);
			if (!symbolTable) return nullptr;
			return symbolTable->FindEnum(type->importedType.typeName->val);
		}
		default:
			break;
		}
		return nullptr;
	}

	inline Stmnt* FindScopedEnum(Token* name, SymbolTable* symbolTable)
	{
		StringView& enumName = name->val;

		Stmnt* enumStmnt = symbolTable->FindEnum(enumName);
		if (enumStmnt) return enumStmnt;

		for (Stmnt * import : symbolTable->imports)
		{
			StringView & package = import->importStmnt.packageName->val;
			SymbolTable* symbolTable = FindSymbolTable(package);
			if (!symbolTable) continue;
			enumStmnt = symbolTable->FindEnum(enumName);
			if (enumStmnt) return enumStmnt;
		}

		return runtimeTable->FindEnum(enumName);
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

		stmnt = runtimeTable->FindFunction(functionName);
		if (stmnt) return stmnt;

		return FindScopedExternFunc(name, symbolTable);
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

		return runtimeTable->FindGlobalVariable(globalVarName);;
	}

	inline Stmnt* FindScopedExternFunc(Token* name, SymbolTable* symbolTable)
	{
		StringView& externFuncName = name->val;

		Stmnt* stmnt = symbolTable->FindExternalFunction(externFuncName);
		if (stmnt) return stmnt;

		for (Stmnt* import : symbolTable->imports)
		{
			StringView & package = import->importStmnt.packageName->val;
			SymbolTable* symbolTable = FindSymbolTable(package);
			if (!symbolTable) continue;
			stmnt = symbolTable->FindExternalFunction(externFuncName);
			if (stmnt) return stmnt;
		}

		return runtimeTable->FindExternalFunction(externFuncName);
	}

	inline Stmnt* FindScopedValue(Token* name, SymbolTable* symbolTable)
	{
		Stmnt* found = FindScopedState(name, symbolTable);
		if (!found) found = FindScopedFunction(name, symbolTable);
		if (!found) found = FindScopedGlobalVar(name, symbolTable);
		if (!found) found = FindScopedEnum(name, symbolTable);
		if (!found) found = FindScopedExternFunc(name, symbolTable);
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

	Stmnt* FindStateForStmnt(Stmnt* stmnt, SymbolTable* symbolTable)
	{
		Token* stateName = GetStateName(stmnt);
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