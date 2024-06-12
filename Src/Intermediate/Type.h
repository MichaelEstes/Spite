#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"

struct Stmnt;
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
	TemplatedType,
	FunctionType,
	ImportedType,
	// Only for use of type checking before template expansion, essentially an 'any' type
	GenericNamedType,
	// Type container for use inferring anonymous type expressions in non assignment cases
	AnonymousType,
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
			eastl::vector<Stmnt*>* declarations;
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
			Expr* templates;
			Type* type;
		} templatedType;

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

		struct
		{
			eastl::vector<Type*>* types;
		} anonType;
	};

	Type()
	{
		typeID = TypeID::InvalidType;
	}

	Type(TypeID typeID)
	{
		this->typeID = typeID;
	}

	Type(size_t size, UniqueType type, bool isSigned)
	{
		typeID = TypeID::PrimitiveType;
		primitiveType.size = size;
		primitiveType.type = type;
		primitiveType.isSigned = isSigned;
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
		case TemplatedType:
			templatedType = copy.templatedType;
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
