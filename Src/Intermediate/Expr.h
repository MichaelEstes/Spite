#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"

struct Node;
struct Body;
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
	AsExpr,
	DereferenceExpr,
	ReferenceExpr,
	BinaryExpr,
	UnaryExpr,
	GroupedExpr,
	GenericsExpr,
	FunctionTypeExpr,
	FunctionTypeDeclExpr,
	CompileExpr,
};

struct Expr
{
	ExprID typeID;
	Token* start;

	union {
		struct
		{
			Token* val;
			UniqueType type;
		} literalExpr;

		struct
		{
			Token* identifier;
		} identfierExpr;

		struct
		{
			Token* primitive;
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
			Token* lBrack;
			Token* rBrack;
		} indexExpr;

		struct
		{
			Expr* function;
			eastl::vector<Expr*>* params;
			Token* lParen;
			Token* rParen;
		} functionCallExpr;

		struct
		{
			Token* newIndex;
			Expr* primaryExpr;
			Expr* atExpr;
		} newExpr;

		struct
		{
			Token* fixed;
			Expr* atExpr;
		} fixedExpr;

		struct
		{
			eastl::vector<Expr*>* values;
		} anonTypeExpr;

		struct
		{
			Expr* of;
			Type* to;
		} asExpr;

		struct
		{
			Expr* of;
			Token* op;
		} dereferenceExpr;

		struct
		{
			Expr* of;
			Token* op;
		} referenceExpr;

		struct
		{
			Expr* left;
			Expr* right;
			Token* op;
			UniqueType opType;
		} binaryExpr;

		struct
		{
			Expr* expr;
			Token* op;
			UniqueType opType;
		} unaryExpr;

		struct
		{
			Expr* expr;
			Token* lParen;
			Token* rParen;
		} groupedExpr;

		struct
		{
			Expr* expr;
			eastl::vector<Type>* types;
			Token* open;
			Token* close;
		} genericsExpr;

		struct
		{
			Type* functionType;
		} functionTypeExpr;

		struct
		{
			Type* returnType;
			Node* functionDecl;
		} functionTypeDeclExpr;

		struct
		{
			Type* returnType;
			Body* body;
		} compileExpr;
	};

	Expr(ExprID typeID, Token* start)
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

	Expr(Token* start, Token* op, Expr* left, Expr* right)
	{
		typeID = ExprID::BinaryExpr;
		this->start = start;
		this->binaryExpr.op = op;
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
		case AsExpr:
			asExpr = copy.asExpr;
			break;
		case DereferenceExpr:
			dereferenceExpr = copy.dereferenceExpr;
			break;
		case ReferenceExpr:
			referenceExpr = copy.referenceExpr;
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
		case FunctionTypeExpr:
			functionTypeExpr = copy.functionTypeExpr;
			break;
		case FunctionTypeDeclExpr:
			functionTypeDeclExpr = copy.functionTypeDeclExpr;
			break;
		case CompileExpr:
			compileExpr = copy.compileExpr;
			break;
		default:
			break;
		}
		return *this;
	}

	~Expr() {};
};