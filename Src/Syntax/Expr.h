#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"

struct Stmnt;
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
	TypeLiteralExpr,
	ExplicitTypeExpr,
	AsExpr,
	DereferenceExpr,
	ReferenceExpr,
	BinaryExpr,
	UnaryExpr,
	GroupedExpr,
	TemplateExpr,
	TypeExpr,
	FunctionTypeDeclExpr,
	CompileExpr,

	// For evalulated constant int expressions
	ConstantIntExpr
};

enum FunctionCallKind
{
	UnknownCall,
	FunctionCall,
	PrimitiveCall,
	ConstructorCall,
	MemberMethodCall,
	UniformMethodCall,
	FunctionTypeCall,
	UnresolvedGenericCall,
	ExternalCall,
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
		} identifierExpr;

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
			FunctionCallKind callKind;
			Stmnt* functionStmnt;
		} functionCallExpr;

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
			bool array;
			eastl::vector<Expr*>* values;
		} typeLiteralExpr;

		struct
		{
			eastl::vector<Stmnt*>* values;
		} explicitTypeExpr;

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
			Expr* expr;
			eastl::vector<Expr*>* templateArgs;
		} templateExpr;

		struct
		{
			Type* type;
		} typeExpr;

		struct
		{
			Stmnt* anonFunction;
		} functionTypeDeclExpr;

		struct
		{
			Stmnt* compile;
		} compileExpr;

		struct
		{
			int value;
		} constantIntExpr;
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
			identifierExpr = copy.identifierExpr;
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
		case TypeLiteralExpr:
			typeLiteralExpr = copy.typeLiteralExpr;
			break;
		case ExplicitTypeExpr:
			explicitTypeExpr = copy.explicitTypeExpr;
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
		case TemplateExpr:
			templateExpr = copy.templateExpr;
			break;
		case TypeExpr:
			typeExpr = copy.typeExpr;
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
};