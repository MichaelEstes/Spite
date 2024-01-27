#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"

typedef size_t NodeIndex;
typedef size_t TokenIndex;
typedef size_t ScopeIndex;

struct Node;
struct Type;

enum ExprID
{
	InvalidExpr,
	LiteralExpr,
	IdentifierExpr,
	PrimitiveExpr,
	SelectorExpr,
	IndexExpr,
	FunctionCallExpr,
	NewExpr,
	FixedExpr,
	AnonTypeExpr,
	BinaryExpr,
	UnaryExpr,
	GroupedExpr,
	GenericsExpr,
};

struct Expr
{
	ExprID typeID;
	TokenIndex start;

	union {
		struct
		{
			TokenIndex val;
			UniqueType type;
		} literalExpr;

		struct
		{
			TokenIndex identifier;
		} identfierExpr;

		struct
		{
			TokenIndex primitive;
		} primitiveExpr;

		struct
		{
			Expr* on;
			Expr* select;
		} selectorExpr;

		struct
		{
			Expr* of;
			Expr* index;
			TokenIndex lBrack;
			TokenIndex rBrack;
		} indexExpr;

		struct
		{
			Expr* function;
			eastl::vector<Expr*>* params;
			TokenIndex lParen;
			TokenIndex rParen;
		} functionCallExpr;

		struct
		{
			TokenIndex newIndex;
			Expr* primaryExpr;
			Expr* atExpr;
		} newExpr;

		struct
		{
			TokenIndex fixed;
			Expr* atExpr;
		} fixedExpr;

		struct
		{
			eastl::vector<Node*> definitions;
		} anonTypeExpr;

		struct
		{
			Expr* left;
			Expr* right;
			TokenIndex op;
			UniqueType opType;
		} binaryExpr;

		struct
		{
			Expr* expr;
			TokenIndex op;
			UniqueType opType;
		} unaryExpr;

		struct
		{
			Expr* expr;
			TokenIndex lParen;
			TokenIndex rParen;
		} groupedExpr;

		struct
		{
			Expr* expr;
			eastl::vector<Type>* types;
			TokenIndex open;
			TokenIndex close;
		} genericsExpr;
	};

	Expr(ExprID typeID, TokenIndex start)
	{
		this->typeID = typeID;
		this->start = start;
	}

	Expr()
	{
		typeID = ExprID::InvalidExpr;
	}

	Expr(const Expr& copy)
	{
		*this = copy;
	}

	Expr(TokenIndex start, Token* op, Expr* left, Expr* right)
	{
		typeID = ExprID::BinaryExpr;
		this->start = start;
		this->binaryExpr.op = op->index;
		this->binaryExpr.opType = op->uniqueType;
		this->binaryExpr.left = left;
		this->binaryExpr.right = right;
	}

	Expr& operator=(const Expr& copy)
	{
		typeID = copy.typeID;
		start = copy.start;

		switch (typeID)
		{
		case LiteralExpr:
			literalExpr = copy.literalExpr;
			break;
		case IdentifierExpr:
			identfierExpr = copy.identfierExpr;
			break;
		case PrimitiveExpr:
			primitiveExpr = copy.primitiveExpr;
			break;
		case SelectorExpr:
			selectorExpr = copy.selectorExpr;
			break;
		case IndexExpr:
			indexExpr = copy.indexExpr;
			break;
		case FunctionCallExpr:
			functionCallExpr = copy.functionCallExpr;
			break;
		case NewExpr:
			newExpr = copy.newExpr;
			break;
		case FixedExpr:
			fixedExpr = copy.fixedExpr;
			break;
		case AnonTypeExpr:
			anonTypeExpr = copy.anonTypeExpr;
			break;
		case BinaryExpr:
			binaryExpr = copy.binaryExpr;
			break;
		case UnaryExpr:
			unaryExpr = copy.unaryExpr;
			break;
		case GroupedExpr:
			groupedExpr = copy.groupedExpr;
			break;
		case GenericsExpr:
			genericsExpr = copy.genericsExpr;
			break;
		default:
			break;
		}
		return *this;
	}

	~Expr() {};
};