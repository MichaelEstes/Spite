#pragma once
#include "EASTL/deque.h"
#include "GlobalTable.h"

struct ScopeUtils
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>> scopeQueue;

	ScopeUtils(GlobalTable* globalTable, SymbolTable* symbolTable)
	{
		this->globalTable = globalTable;
		this->symbolTable = symbolTable;
	}

	void AddScope()
	{
		scopeQueue.emplace_back();
	}

	void PopScope()
	{
		scopeQueue.pop_back();
	}

	bool HasInTopScope(StringView& name)
	{
		eastl::hash_map<StringView, Stmnt*, StringViewHash>& back = scopeQueue.back();
		return MapHas(back, name);
	}

	void AddToTopScope(const StringView& name, Stmnt* stmnt)
	{
		eastl::hash_map<StringView, Stmnt*, StringViewHash>& back = scopeQueue.back();
		back[name] = stmnt;
	}

	inline bool IsPackageExpr(Expr* expr)
	{
		return expr->typeID == ExprID::SelectorExpr && expr->selectorExpr.on->typeID == ExprID::IdentifierExpr &&
			globalTable->IsPackage(expr->selectorExpr.on->identifierExpr.identifier->val);
	}

	inline Token* GetPackageFromExpr(Expr* expr)
	{
		if (expr->typeID == ExprID::SelectorExpr && expr->selectorExpr.on->typeID == ExprID::IdentifierExpr &&
			globalTable->IsPackage(expr->selectorExpr.on->identifierExpr.identifier->val))
		{
			return expr->selectorExpr.on->identifierExpr.identifier;
		}

		return nullptr;
	}

	inline Stmnt* FindInScope(StringView& val)
	{
		for (auto it = scopeQueue.rbegin(); it != scopeQueue.rend(); it++)
		{
			if (auto entry = it->find(val); entry != it->end())
			{
				return entry->second;
			}
		}

		return globalTable->FindScopedNamedGlobalVar(val, symbolTable);
	}

	inline Stmnt* FindForName(Token* name)
	{
		Stmnt* stmnt = FindInScope(name->val);
		if (stmnt) return stmnt;

		return globalTable->FindScopedValue(name, symbolTable);
	}
};