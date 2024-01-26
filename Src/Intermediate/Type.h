#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"

typedef size_t NodeIndex;
typedef size_t TokenIndex;
typedef size_t ScopeIndex;

struct Node;
struct Expr;

enum TypeID
{
	InvalidType,
	UnknownType,
	PrimitiveType,
	NamedType,
	ExplicitType,
	ImplicitType,
	PointerType,
	ArrayType,
	GenericsType,
	FunctionType,
	ImportedType,
};

struct Type
{
	TypeID typeID;

	union
	{
		struct
		{
			size_t size;
			TokenIndex name;
			bool isSigned;
		} primitiveType;

		struct
		{
			TokenIndex typeName;
		} namedType;

		struct
		{
			eastl::vector<Node*>* declarations;
		} explicitType;

		struct
		{
			eastl::vector<TokenIndex>* identifiers;
		} implicitType;

		struct
		{
			bool raw;
			TokenIndex ptr;
			Type* type;
		} pointerType;

		struct
		{
			TokenIndex arr;
			Type* type;
		} arrayType;

		struct
		{
			Expr* generics;
			Type* type;
		} genericsType;

		struct
		{
			Type* returnType;
			// Param types
			// Block?
		} functionType;

		struct
		{
			TokenIndex packageName;
			TokenIndex typeName;
		} importedType;

	};

	Type()
	{
		typeID = TypeID::InvalidType;
	}

	Type(TypeID typeID)
	{
		this->typeID = typeID;
	}

	Type(const Type& copy)
	{
		*this = copy;
	}

	Type& operator=(const Type& copy)
	{
		typeID = copy.typeID;

		switch (typeID)
		{
		case PrimitiveType:
			primitiveType = copy.primitiveType;
			break;
		case NamedType:
			namedType = copy.namedType;
			break;
		case ExplicitType:
			explicitType = copy.explicitType;
			break;
		case ImplicitType:
			implicitType = copy.implicitType;
			break;
		case PointerType:
			pointerType = copy.pointerType;
			break;
		case ArrayType:
			arrayType = copy.arrayType;
			break;
		case GenericsType:
			genericsType = copy.genericsType;
			break;
		case FunctionType:
			functionType = copy.functionType;
			break;
		case ImportedType:
			importedType = copy.importedType;
			break;
		default:
			break;
		}
		return *this;
	}
};