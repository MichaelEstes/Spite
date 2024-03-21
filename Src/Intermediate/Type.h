#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"

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
	ValueType,
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
			UniqueType type;
			bool isSigned;
		} primitiveType;

		struct
		{
			Token* typeName;
		} namedType;

		struct
		{
			eastl::vector<Node*>* declarations;
		} explicitType;

		struct
		{
			eastl::vector<Token*>* identifiers;
		} implicitType;

		struct
		{
			Token* ptr;
			Type* type;
		} pointerType;

		struct
		{
			Token* valueOp;
			Type* type;
		} valueType;

		struct
		{
			Token* arr;
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
			eastl::vector<Type*>* paramTypes;
		} functionType;

		struct
		{
			Token* packageName;
			Token* typeName;
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
		case ValueType:
			valueType = copy.valueType;
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
