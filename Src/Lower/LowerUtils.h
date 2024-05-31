#pragma once
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"

eastl::string BuildTemplatedString(eastl::vector<Expr*>* templates)
{
	eastl::string str = "<";

	size_t size = templates->size();
	for (size_t i = 0; i < size; i++)
	{
		str += ToString(templates->at(i)) + ",";
	}
	str.back() = '>';

	return str;
}

inline eastl::string BuildPackageName(Token* package)
{
	return package->val.ToString();
}

inline eastl::string BuildStateName(Stmnt* state)
{
	return BuildPackageName(state->package) + ':' + state->state.name->val.ToString();
}

inline eastl::string BuildTemplatedStateName(Stmnt* state, eastl::vector<Expr*>* templates)
{
	return BuildStateName(state) + BuildTemplatedString(templates);
}

inline eastl::string BuildFunctionName(Stmnt* func)
{
	return BuildPackageName(func->package) + ':' + func->function.name->val.ToString();
}

inline eastl::string BuildTemplatedFunctionName(Stmnt* func, eastl::vector<Expr*>* templates)
{
	return BuildFunctionName(func) + BuildTemplatedString(templates);
}

template<typename P>
SpiteIR::Type* TypeToIRType(SpiteIR::IR* ir, Type* type, P* parent)
{
	SpiteIR::Type* irType = ir->AllocateType();

	irType->parent = SpiteIR::Parent(parent);

	switch (type->typeID)
	{
	case InvalidType:
	case UnknownType:
	case ImplicitType:
		AddError("Lower:TypeToIRType Invalid type for conversion");
		break;
	case PrimitiveType:
	{
		irType->kind = SpiteIR::TypeKind::PrimitiveType;
		auto& prim = irType->primitive;
		prim.size = type->primitiveType.size;
		prim.isSigned = type->primitiveType.isSigned;
		switch (type->primitiveType.type)
		{
		case Void:
			prim.kind = SpiteIR::PrimitiveKind::Void;
			break;
		case Bool:
		case Byte:
		case Ubyte:
		case Int:
		case Int16:
		case Int32:
		case Int64:
		case Int128:
		case Uint:
		case Uint16:
		case Uint32:
		case Uint64:
		case Uint128:
			prim.kind = SpiteIR::PrimitiveKind::Int;
			break;
		case Float:
		case Float32:
		case Float64:
		case Float128:
			prim.kind = SpiteIR::PrimitiveKind::Float;
		case String:
			prim.kind = SpiteIR::PrimitiveKind::String;
			break;
		default:
			break;
		}
		break;
	}
	case ImportedType:
	case NamedType:
	{
		irType->kind = SpiteIR::TypeKind::NamedType;
		Stmnt* state = globalTable->FindStateForType(type, symbolTable);
		irType->namedType.name = ir->AllocateString();
		*irType->namedType.name = BuildStateName(state);
		break;
	}
	case ExplicitType:
	{
		irType->kind = SpiteIR::TypeKind::AnonymousType;
		irType->anonymousType.members = ir->AllocateArray<SpiteIR::AnonymousTypeMember*>();
		for (size_t i = 0; i < type->explicitType.declarations->size(); i++)
		{
			Stmnt* stmnt = type->explicitType.declarations->at(i);
			auto& def = stmnt->definition;
			SpiteIR::AnonymousTypeMember* member = ir->AllocateAnonymousTypeMember();
			member->parent = irType;
			member->pos = stmnt->start->pos;
			member->name = def.name->val.ToString();
			member->type = TypeToIRType(def.type, member);
			irType->anonymousType.members->push_back(member);
		}
		break;
	}
	case PointerType:
		irType->kind = SpiteIR::TypeKind::PointerType;
		irType->pointer.type = TypeToIRType(type->pointerType.type, irType);
		break;
	case ValueType:
		irType->kind = SpiteIR::TypeKind::ValueType;
		irType->value.type = TypeToIRType(type->valueType.type, irType);
		break;
	case ArrayType:
		irType->kind = SpiteIR::TypeKind::ArrayType;
		irType->array.type = TypeToIRType(type->arrayType.type, irType);
		break;
	case GenericsType:
		break;
	case FunctionType:
	{
		irType->kind = SpiteIR::TypeKind::FunctionType;
		irType->function.params = ir->AllocateArray<SpiteIR::Type*>();
		irType->function.returnType = TypeToIRType(type->functionType.returnType, irType);
		for (Type* param : *type->functionType.paramTypes)
		{
			irType->function.params->push_back(TypeToIRType(param, irType));
		}
		break;
	}
	default:
		break;
	}

	return irType;
}
