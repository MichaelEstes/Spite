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

	// For type checking/inference
	// Only for use of type checking before template expansion, essentially an 'any' type
	GenericNamedType,
	// Type container for use inferring anonymous type expressions in non assignment cases
	AnonymousType,
	// Fixed array, returned as pointer, no count field
	FixedArrayType,
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
			Expr* size;
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

		struct
		{
			Type* type;
			intmax_t size;
		} fixedArrayType;
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
};
