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

inline bool IsInt(Type* primitive)
{
	switch (primitive->primitiveType.type)
	{
	case UniqueType::Bool:
	case UniqueType::Byte:
	case UniqueType::Int:
	case UniqueType::Int16:
	case UniqueType::Int32:
	case UniqueType::Int64:
	case UniqueType::Int128:
	case UniqueType::Ubyte:
	case UniqueType::Uint:
	case UniqueType::Uint16:
	case UniqueType::Uint32:
	case UniqueType::Uint64:
	case UniqueType::Uint128:
		return true;
	default:
		return false;
	}
}

inline bool IsIntLike(Type* type)
{
	if (type->typeID == TypeID::PrimitiveType) return IsInt(type);
	else if (type->typeID == TypeID::PointerType) return true;
	else if (type->typeID == TypeID::ValueType) return IsIntLike(type->valueType.type);
}

inline bool IsFloat(Type* primitive)
{
	switch (primitive->primitiveType.type)
	{
	case UniqueType::Float:
	case UniqueType::Float32:
	case UniqueType::Float64:
		return true;
	default:
		return false;
	}
}

inline bool IsComparableToZero(Type* type)
{
	return (type->typeID == TypeID::PrimitiveType && (IsInt(type) || IsFloat(type)))
		|| IsIntLike(type);
}

inline bool IsString(Type* primitive)
{
	return primitive->primitiveType.type == UniqueType::String;
}
