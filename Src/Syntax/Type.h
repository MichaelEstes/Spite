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
	UnionType,
	AnyType,

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
			eastl::vector<Stmnt*>* declarations;
		} unionType;

		struct
		{
			eastl::vector<Token*>* identifiers;
		} implicitType;

		struct
		{
			Type* type;
		} pointerType;

		struct
		{
			Type* type;
		} valueType;

		struct
		{
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

inline bool IsAny(Type* type)
{
	return type->typeID == TypeID::AnyType ||
		(type->typeID == TypeID::TemplatedType && IsAny(type->templatedType.type)) ||
		(type->typeID == TypeID::ValueType && IsAny(type->valueType.type));
}

inline bool IsIntLike(Type* type)
{
	if (type->typeID == TypeID::PrimitiveType) return IsInt(type);
	else if (type->typeID == TypeID::PointerType) return true;
	else if (type->typeID == TypeID::ValueType) return IsIntLike(type->valueType.type);

	return IsAny(type);
}

inline bool IsFunctionType(Type* type)
{
	return type->typeID == TypeID::FunctionType;
}

inline bool IsComparableToZero(Type* type)
{
	return (type->typeID == TypeID::PrimitiveType && (IsInt(type) || IsFloat(type)))
		|| IsIntLike(type) || IsAny(type) || IsFunctionType(type);
}

inline bool IsString(Type* primitive)
{
	return primitive->primitiveType.type == UniqueType::String || IsAny(primitive);
}

inline bool IsVoidPtr(Type* type)
{
	return type->typeID == TypeID::PointerType &&
		type->pointerType.type->typeID == PrimitiveType &&
		type->pointerType.type->primitiveType.type == UniqueType::Void;
}
