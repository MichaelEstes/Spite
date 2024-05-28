#pragma once

#include "Stmnt.h"

bool operator==(const Expr& left, const Expr& right);
bool operator!=(const Expr& left, const Expr& right);
bool operator==(const Type& left, const Type& right);
bool operator!=(const Type& left, const Type& right);

inline size_t HashType(const Type* type);
inline size_t HashExpr(const Expr* expr);

struct TypeHash
{
	size_t operator()(const Type* type) const
	{
		return HashType(type);
	}
};

struct ExprHash
{
	size_t operator()(const Expr* expr) const
	{
		return HashExpr(expr);
	}
};

struct TypeArrHash
{
	size_t operator()(const eastl::vector<Type*>* types) const;
};

struct ExprArrHash
{
	size_t operator()(const eastl::vector<Expr*>* exprs) const;
};

struct ExprArrEqual
{
	bool operator()(const eastl::vector<Expr*>* l, const eastl::vector<Expr*>* r) const;
};

eastl::string ToString(Expr* expr);
eastl::string ToString(Type* type);
eastl::string ToString(Body& body);
eastl::string ToString(Stmnt* node);