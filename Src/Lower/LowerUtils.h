#pragma once
#include "../Syntax/GlobalTable.h"
#include "../Syntax/SyntaxUtils.h"
#include "../IR/IR.h"
#include "../Checking/GenericInference.h"

extern Config config;

eastl::string BuildExprString(Expr* expr);
int IsIRTypeAssignable(SpiteIR::Type* left, SpiteIR::Type* right);
inline bool IsStringType(SpiteIR::Type* type);

SpiteIR::State* stringState = nullptr;
SpiteIR::State* arrayState = nullptr;
SpiteIR::State* typeMetaState = nullptr;

SpiteIR::State* GetStateForType(SpiteIR::Type* type)
{
	if (type->kind == SpiteIR::TypeKind::ReferenceType)
	{
		return GetStateForType(type->reference.type);
	}
	else if (type->kind == SpiteIR::TypeKind::StateType)
	{
		return type->stateType.state;
	}
	else if (type->kind == SpiteIR::TypeKind::DynamicArrayType)
	{
		return arrayState;
	}
	else if (IsStringType(type))
	{
		return stringState;
	}

	return nullptr;
}

eastl::vector<SpiteIR::Type*> GetStateTypes(SpiteIR::State* state)
{
	eastl::vector<SpiteIR::Type*> types;
	if (!state) return types;
	for (SpiteIR::Member* member : state->members)
	{
		types.push_back(member->value.type);
	}

	return types;
}

size_t HashIRType(const SpiteIR::Type* type)
{
	switch (type->kind)
	{
	case SpiteIR::TypeKind::PrimitiveType:
		return type->size + type->alignment + type->primitive.isSigned;
	case SpiteIR::TypeKind::StateType:
		return (size_t)type->stateType.state;
	case SpiteIR::TypeKind::UnionType:
	case SpiteIR::TypeKind::StructureType:
	{
		size_t hash = 0;
		eastl::string_hash<eastl::string> strHash;
		for (SpiteIR::Member* member : *type->structureType.members)
		{
			hash += HashIRType(member->value.type) + member->offset;
			hash += strHash(member->value.name);
		}
		return hash;
	}
	case SpiteIR::TypeKind::PointerType:
		return 3 + HashIRType(type->pointer.type);
	case SpiteIR::TypeKind::ReferenceType:
		return 4 + HashIRType(type->reference.type);
	case SpiteIR::TypeKind::DynamicArrayType:
		return 5 + HashIRType(type->dynamicArray.type);
	case SpiteIR::TypeKind::FixedArrayType:
		return 6 + type->fixedArray.count + HashIRType(type->fixedArray.type);
	case SpiteIR::TypeKind::FunctionType:
	{
		size_t hash = 7 + HashIRType(type->function.returnType);
		for (SpiteIR::Type* param : *type->function.params)
		{
			hash += HashIRType(param);
		}
		return hash;
	}
	default:
		break;
	}

	return (size_t)-1;
}

struct IRTypeHash
{
	size_t operator()(const SpiteIR::Type* type) const
	{
		return HashIRType(type);
	}
};

bool IsIRTypeEqual(const SpiteIR::Type* l, const SpiteIR::Type* r)
{
	if (l->kind != r->kind || l->size != r->size || l->alignment != r->alignment ||
		l->byValue != r->byValue) return false;

	switch (l->kind)
	{
	case SpiteIR::TypeKind::PrimitiveType:
		return l->primitive.isSigned == r->primitive.isSigned &&
			l->primitive.kind == r->primitive.kind;
	case SpiteIR::TypeKind::StateType:
		return l->stateType.state == r->stateType.state;
	case SpiteIR::TypeKind::UnionType:
	case SpiteIR::TypeKind::StructureType:
	{
		size_t count = l->structureType.members->size();
		if (count != r->structureType.members->size()) return false;

		for (size_t i = 0; i < count; i++)
		{
			SpiteIR::Member* lMember = l->structureType.members->at(i);
			SpiteIR::Member* rMember = r->structureType.members->at(i);
			if (lMember->value.name != rMember->value.name ||
				!IsIRTypeEqual(lMember->value.type, rMember->value.type)) return false;
		}

		return true;
	}
	case SpiteIR::TypeKind::PointerType:
		return IsIRTypeEqual(l->pointer.type, r->pointer.type);
	case SpiteIR::TypeKind::ReferenceType:
		return IsIRTypeEqual(l->reference.type, r->reference.type);
	case SpiteIR::TypeKind::DynamicArrayType:
		return IsIRTypeEqual(l->dynamicArray.type, r->dynamicArray.type);
	case SpiteIR::TypeKind::FixedArrayType:
		return l->fixedArray.count == r->fixedArray.count &&
			IsIRTypeEqual(l->fixedArray.type, r->fixedArray.type);
	case SpiteIR::TypeKind::FunctionType:
	{
		size_t count = l->function.params->size();
		if (count != r->function.params->size() ||
			!IsIRTypeEqual(l->function.returnType, r->function.returnType)) return false;

		for (size_t i = 0; i < count; i++)
		{
			SpiteIR::Type* lParam = l->function.params->at(i);
			SpiteIR::Type* rParam = r->function.params->at(i);
			if (!IsIRTypeEqual(lParam, rParam)) return false;
		}

		return true;
	}
	default:
		break;
	}

	return false;
}

struct IRTypeEqual
{
	bool operator()(const SpiteIR::Type* l, const SpiteIR::Type* r) const
	{
		return IsIRTypeEqual(l, r);
	}
};

bool IRTypesAssignable(const eastl::vector<SpiteIR::Type*>& left, const eastl::vector<SpiteIR::Type*>& right)
{
	if (left.size() != right.size()) return false;

	for (size_t i = 0; i < left.size(); i++)
	{
		if (IsIRTypeAssignable(left.at(i), right.at(i)) == 0) return false;
	}

	return true;
}

bool IsStructuredType(SpiteIR::Type* type)
{
	return type->kind == SpiteIR::TypeKind::StateType || type->kind == SpiteIR::TypeKind::StructureType;
}

bool IsAnyType(SpiteIR::Type* type)
{
	return type->kind == SpiteIR::TypeKind::ReferenceType &&
		type->reference.type->kind == SpiteIR::TypeKind::PrimitiveType &&
		type->reference.type->primitive.kind == SpiteIR::PrimitiveKind::Void;
}

inline bool IsStringType(SpiteIR::Type* type)
{
	return type->kind == SpiteIR::TypeKind::PrimitiveType &&
		type->primitive.kind == SpiteIR::PrimitiveKind::String;
}

inline bool IsAssignableString(SpiteIR::Type* type)
{
	return IsStringType(type) || (type->kind == SpiteIR::TypeKind::StateType && type->stateType.state &&
		type->stateType.state->name == "___string");
}

inline bool IsBoolType(SpiteIR::Type* type)
{
	return type->kind == SpiteIR::TypeKind::PrimitiveType &&
		type->primitive.kind == SpiteIR::PrimitiveKind::Bool;
}

inline bool IsIntLikeType(SpiteIR::Type* type)
{
	return type->kind == SpiteIR::TypeKind::PrimitiveType &&
		type->primitive.kind != SpiteIR::PrimitiveKind::Void &&
		type->primitive.kind <= SpiteIR::PrimitiveKind::Int;
}

inline bool IsFloatLikeType(SpiteIR::Type* type)
{
	return type->kind == SpiteIR::TypeKind::PrimitiveType &&
		(type->primitive.kind == SpiteIR::PrimitiveKind::F32 ||
		type->primitive.kind == SpiteIR::PrimitiveKind::Float);
}

inline bool IsPointerLikeType(SpiteIR::Type* type)
{
	return type->kind == SpiteIR::TypeKind::PointerType ||
		type->kind == SpiteIR::TypeKind::FunctionType;
}

eastl::vector<SpiteIR::Type*> GetStructuredTypes(SpiteIR::Type* type)
{
	if (type->kind == SpiteIR::TypeKind::StateType) return GetStateTypes(type->stateType.state);
	else
	{
		eastl::vector<SpiteIR::Type*> types = eastl::vector<SpiteIR::Type*>();
		for (SpiteIR::Member* member : *type->structureType.members)
			types.push_back(member->value.type);

		return types;
	}
}

int IsIRTypeAssignable(SpiteIR::Type* left, SpiteIR::Type* right)
{
	if (IsAnyType(left)) return 1;
	if (left->kind == SpiteIR::TypeKind::ReferenceType) return IsIRTypeAssignable(left->reference.type, right);
	if (right->kind == SpiteIR::TypeKind::ReferenceType) return IsIRTypeAssignable(left, right->reference.type);

	if (left->kind == SpiteIR::TypeKind::PrimitiveType &&
		right->kind == SpiteIR::TypeKind::PrimitiveType)
	{
		if (left->primitive.isSigned == right->primitive.isSigned &&
			left->primitive.kind == right->primitive.kind &&
			left->size == right->size) return 1;

		//Both types must be void to be assignable, which would have been caught in the above check
		if (left->primitive.kind == SpiteIR::PrimitiveKind::Void ||
			right->primitive.kind == SpiteIR::PrimitiveKind::Void)
			return 0;

		//Both types must be strings to be assignable, which would have been caught in the above check
		if (left->primitive.kind == SpiteIR::PrimitiveKind::String ||
			right->primitive.kind == SpiteIR::PrimitiveKind::String)
			return 0;

		return 2;
	}

	if (left->kind == SpiteIR::TypeKind::PrimitiveType &&
		right->kind == SpiteIR::TypeKind::PointerType)
	{
		if (IsIntLikeType(left)) return 2;
		return 0;
	}

	if (left->kind == SpiteIR::TypeKind::PointerType &&
		right->kind == SpiteIR::TypeKind::PrimitiveType)
	{
		if (IsIntLikeType(right)) return 2;
		return 0;
	}

	if (left->kind == SpiteIR::TypeKind::StateType &&
		right->kind == SpiteIR::TypeKind::StateType)
	{
		if (left->stateType.state == right->stateType.state) return 1;
	}

	if (left->kind == SpiteIR::TypeKind::StructureType &&
		right->kind == SpiteIR::TypeKind::StructureType)
	{
		if (IRTypesAssignable(GetStructuredTypes(left), GetStructuredTypes(right))) return 1;
	}

	if (IsStructuredType(left) && IsStructuredType(right))
	{
		if (IRTypesAssignable(GetStructuredTypes(left), GetStructuredTypes(right)))
			return 2;
	}

	if (IsAssignableString(left) && IsAssignableString(right))
	{
		return 1;
	}

	if (left->kind == SpiteIR::TypeKind::DynamicArrayType &&
		right->kind == SpiteIR::TypeKind::DynamicArrayType)
	{
		if (IsIRTypeAssignable(left->dynamicArray.type, right->dynamicArray.type) == 1) return 1;
	}

	if (left->kind == SpiteIR::TypeKind::DynamicArrayType &&
		right->kind == SpiteIR::TypeKind::FixedArrayType)
	{
		if (IsIRTypeAssignable(left->dynamicArray.type, right->fixedArray.type) == 1) return 2;
	}

	if (left->kind == SpiteIR::TypeKind::FixedArrayType &&
		right->kind == SpiteIR::TypeKind::FixedArrayType)
	{
		if (left->fixedArray.count == right->fixedArray.count &&
			IsIRTypeAssignable(left->fixedArray.type, right->fixedArray.type) == 1) return 1;
	}

	if (left->kind == SpiteIR::TypeKind::PointerType &&
		right->kind == SpiteIR::TypeKind::PointerType)
	{
		if (IsIRTypeAssignable(left->pointer.type, right->pointer.type) == 1) return 1;
	}

	if (left->kind == SpiteIR::TypeKind::FunctionType &&
		right->kind == SpiteIR::TypeKind::FunctionType)
	{
		if (IsIRTypeAssignable(left->function.returnType, right->function.returnType) != 1) return 0;
		if (left->function.params->size() != right->function.params->size()) return 0;
		for (size_t i = 0; i < left->function.params->size(); i++)
		{
			if (IsIRTypeAssignable(left->function.params->at(i), right->function.params->at(i)) != 1)
				return 0;
		}

		return 1;
	}


	return 0;
}

inline eastl::string OperatorToString(UniqueType op)
{
	switch (op)
	{
	case UniqueType::Add:
		return "add";
	case UniqueType::Subtract:
		return "sub";
	case UniqueType::Multiply:
		return "mul";
	case UniqueType::Divide:
		return "div";
	case UniqueType::Modulo:
		return "mod";
	case UniqueType::And:
		return "and";
	case UniqueType::Or:
		return "or";
	case UniqueType::Xor:
		return "xor";
	case UniqueType::Shiftl:
		return "lshift";
	case UniqueType::Shiftr:
		return "rshift";
	case UniqueType::AndNot:
		return "andNot";
	case UniqueType::AddAssign:
		return "addAssign";
	case UniqueType::SubtractAssign:
		return "subAssign";
	case UniqueType::MultiplyAssign:
		return "multAssign";
	case UniqueType::DivideAssign:
		return "divAssign";
	case UniqueType::ModuloAssign:
		return "modAssign";
	case UniqueType::AndAssign:
		return "andAssign";
	case UniqueType::OrAssign:
		return "orAssign";
	case UniqueType::XorAssign:
		return "xorAssign";
	case UniqueType::ShiftlAssign:
		return "shiftlAssign";
	case UniqueType::ShiftrAssign:
		return "shiftrAssign";
	case UniqueType::AndNotAssign:
		return "andNotAssign";
	case UniqueType::LogicAnd:
		return "logicAnd";
	case UniqueType::LogicOr:
		return "logicOr";
	case UniqueType::Arrow:
		return "arrow";
	case UniqueType::Increment:
		return "increment";
	case UniqueType::Decrement:
		return "decrement";
	case UniqueType::Equal:
		return "equal";
	case UniqueType::Less:
		return "less";
	case UniqueType::Greater:
		return "greater";
	case UniqueType::Assign:
		return "assign";
	case UniqueType::Not:
		return "not";
	case UniqueType::NotEql:
		return "notEql";
	case UniqueType::LessEqual:
		return "lessEqual";
	case UniqueType::GreaterEqual:
		return "greaterEqual";
	case UniqueType::In:
		return "in";
	case UniqueType::To:
		return "to";
	case UniqueType::Array:
		return "index";
	default:
		break;
	}

	AddError("LowerUtils:OperatorToString Invalid operator");
	return "";
}

SpiteIR::Type* GetDereferencedType(SpiteIR::Type* type)
{
	switch (type->kind)
	{
	case SpiteIR::TypeKind::PointerType:
		return type->pointer.type;
	case SpiteIR::TypeKind::ReferenceType:
		return type->reference.type;
	default:
		break;
	}
	return type;
}

SpiteIR::Type* CreateVoidType(SpiteIR::IR* ir)
{
	SpiteIR::Type* type = ir->AllocateType();
	type->size = 0;
	type->alignment = 1;
	type->kind = SpiteIR::TypeKind::PrimitiveType;
	type->byValue = true;
	type->primitive.isSigned = true;
	type->primitive.kind = SpiteIR::PrimitiveKind::Void;
	return type;
}

SpiteIR::Type* CreateBoolType(SpiteIR::IR* ir)
{
	SpiteIR::Type* type = ir->AllocateType();
	type->size = 1;
	type->alignment = 1;
	type->kind = SpiteIR::TypeKind::PrimitiveType;
	type->byValue = true;
	type->primitive.isSigned = true;
	type->primitive.kind = SpiteIR::PrimitiveKind::Bool;
	return type;
}

SpiteIR::Type* CreateByteType(SpiteIR::IR* ir)
{
	SpiteIR::Type* type = ir->AllocateType();
	type->size = 1;
	type->alignment = 1;
	type->kind = SpiteIR::TypeKind::PrimitiveType;
	type->byValue = true;
	type->primitive.isSigned = true;
	type->primitive.kind = SpiteIR::PrimitiveKind::Byte;
	return type;
}

SpiteIR::Type* CreateIntType(SpiteIR::IR* ir)
{
	SpiteIR::Type* type = ir->AllocateType();
	type->size = config.targetArchByteWidth;
	type->alignment = config.targetArchByteWidth;
	type->kind = SpiteIR::TypeKind::PrimitiveType;
	type->byValue = true;
	type->primitive.isSigned = true;
	type->primitive.kind = SpiteIR::PrimitiveKind::Int;
	return type;
}

SpiteIR::Type* CreateUnsignedIntType(SpiteIR::IR* ir)
{
	SpiteIR::Type* type = ir->AllocateType();
	type->size = config.targetArchByteWidth;
	type->alignment = config.targetArchByteWidth;
	type->kind = SpiteIR::TypeKind::PrimitiveType;
	type->byValue = true;
	type->primitive.isSigned = false;
	type->primitive.kind = SpiteIR::PrimitiveKind::Int;
	return type;
}

SpiteIR::Type* CreateVoidPtrType(SpiteIR::IR* ir)
{
	SpiteIR::Type* type = ir->AllocateType();
	type->size = config.targetArchByteWidth;
	type->alignment = config.targetArchByteWidth;
	type->kind = SpiteIR::TypeKind::PointerType;
	type->byValue = true;
	type->pointer.type = CreateVoidType(ir);
	return type;
}

SpiteIR::Type* CreateStateType(SpiteIR::IR* ir, SpiteIR::State* state)
{
	SpiteIR::Type* type = ir->AllocateType();
	type->size = state->size;
	type->alignment = state->alignment;
	type->kind = SpiteIR::TypeKind::StateType;
	type->byValue = state->IsValueType();
	type->stateType.state = state;
	return type;
}

SpiteIR::Type* MakeReferenceType(SpiteIR::Type* type, SpiteIR::IR* ir)
{
	if (type->kind == SpiteIR::TypeKind::ReferenceType) return type;

	SpiteIR::Type* refType = ir->AllocateType();
	refType->kind = SpiteIR::TypeKind::ReferenceType;
	refType->size = config.targetArchByteWidth;
	refType->alignment = config.targetArchByteWidth;
	refType->byValue = true;
	refType->reference.type = type;
	return refType;
}

SpiteIR::Type* MakePointerType(SpiteIR::Type* type, SpiteIR::IR* ir)
{
	SpiteIR::Type* ptrType = ir->AllocateType();
	ptrType->kind = SpiteIR::TypeKind::PointerType;
	ptrType->size = config.targetArchByteWidth;
	ptrType->alignment = config.targetArchByteWidth;
	ptrType->byValue = true;
	ptrType->pointer.type = type;
	return ptrType;
}

SpiteIR::BinaryOpKind BinaryOpToIR(UniqueType type)
{
	switch (type)
	{
	case Add:
		return SpiteIR::BinaryOpKind::Add;
	case Subtract:
		return SpiteIR::BinaryOpKind::Subtract;
	case Multiply:
		return SpiteIR::BinaryOpKind::Multiply;
	case Divide:
		return SpiteIR::BinaryOpKind::Divide;
	case Modulo:
		return SpiteIR::BinaryOpKind::Modulo;
	case And:
		return SpiteIR::BinaryOpKind::And;
	case Or:
		return SpiteIR::BinaryOpKind::Or;
	case Xor:
		return SpiteIR::BinaryOpKind::Xor;
	case Shiftl:
		return SpiteIR::BinaryOpKind::ShiftLeft;
	case Shiftr:
		return SpiteIR::BinaryOpKind::ShiftRight;
	case AndNot:
		return SpiteIR::BinaryOpKind::AndNot;
	case LogicAnd:
		return SpiteIR::BinaryOpKind::LogicAnd;
	case LogicOr:
		return SpiteIR::BinaryOpKind::LogicOr;
	case Equal:
		return SpiteIR::BinaryOpKind::Equal;
	case NotEql:
		return SpiteIR::BinaryOpKind::NotEql;
	case Less:
		return SpiteIR::BinaryOpKind::Less;
	case Greater:
		return SpiteIR::BinaryOpKind::Greater;
	case LessEqual:
		return SpiteIR::BinaryOpKind::LessEqual;
	case GreaterEqual:
		return SpiteIR::BinaryOpKind::GreaterEqual;
	default:
		return SpiteIR::BinaryOpKind::Add;
	}
}

SpiteIR::UnaryOpKind UnaryOpToIR(UniqueType type)
{
	switch (type)
	{
	case Subtract:
		return SpiteIR::UnaryOpKind::Subtract;
	case Not:
		return SpiteIR::UnaryOpKind::Not;
	case Xor:
		return SpiteIR::UnaryOpKind::XOr;
	default:
		return SpiteIR::UnaryOpKind::Subtract;
	}
}

Expr* _ExpandTemplate(Expr* expr, eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
{
	if (!generics || !generics->size()) return expr;

	size_t genericsCount = generics->size();
	Token* exprToken = GetTokenForTemplate(expr);
	if (exprToken)
	{
		for (int i = 0; i < genericsCount; i++)
		{
			Token* token = generics->at(i);
			if (token->val == exprToken->val)
			{
				return templates->at(i);
			}
		}
	}

	return expr;
}

Type* _ExpandTypeTemplates(Type* type, eastl::vector<Token*>* generics,
	eastl::vector<Expr*>* templates, SymbolTable* symbolTable)
{
	if (!generics || !generics->size()) return type;

	return InferGenericType(generics, symbolTable->CloneType(type), templates);
}

eastl::vector<Expr*> _ExpandTemplates(eastl::vector<Expr*>* exprs, eastl::vector<Token*>* generics, 
	eastl::vector<Expr*>* templates, SymbolTable* symbolTable)
{
	if (!generics || !generics->size()) return *exprs;

	eastl::vector<Expr*> expanded;
	for (Expr* expr : *exprs)
	{
		expanded.push_back(InferGenericExpr(generics, symbolTable->CloneExpr(expr), templates));
	}

	return expanded;
}

eastl::string BuildTemplatedString(eastl::vector<Expr*>* templates)
{
	if (!templates || !templates->size()) return "";
	eastl::string str = "__";

	size_t size = templates->size();
	for (size_t i = 0; i < size; i++)
	{
		str += BuildExprString(templates->at(i)) + "_";
	}
	str += '_';

	return str;
}

eastl::string BuildTypeString(Type* type)
{
	switch (type->typeID)
	{
	case PrimitiveType:
		return PrimitiveToString(type->primitiveType.type);
	case NamedType:
		return type->namedType.typeName->val.ToString();
	case ExplicitType:
	{
		eastl::string explTypeStr = "expl_";
		for (Stmnt* stmnt : *type->explicitType.declarations)
		{
			explTypeStr += BuildTypeString(stmnt->definition.type) + '_'
				+ stmnt->definition.name->val.ToString() + '_';
		}
		return explTypeStr;
	}
	case UnionType:
	{
		eastl::string unionTypeStr = "union_";
		for (Stmnt* stmnt : *type->unionType.declarations)
		{
			unionTypeStr += BuildTypeString(stmnt->definition.type) + '_'
				+ stmnt->definition.name->val.ToString() + '_';
		}
		return unionTypeStr;
	}
	case PointerType:
		return "ptr_" + BuildTypeString(type->pointerType.type);
	case ValueType:
		return "val_" + BuildTypeString(type->valueType.type);
	case RefType:
		return "ref_" + BuildTypeString(type->refType.type);
	case ArrayType:
	{
		if (type->arrayType.size)
		{
			return "arr_" + BuildExprString(type->arrayType.size) + "_" + BuildTypeString(type->arrayType.type);
		}
		return "arr_" + BuildTypeString(type->arrayType.type);
	}
	case TemplatedType:
		return BuildTypeString(type->templatedType.type) +
			BuildTemplatedString(type->templatedType.templates->templateExpr.templateArgs);
	case FunctionType:
	{
		eastl::string funcTypeStr = "func_" + BuildTypeString(type->functionType.returnType);

		for (Type* param : *type->functionType.paramTypes)
		{
			funcTypeStr += BuildTypeString(param) + '_';
		}

		return funcTypeStr;
	}
	case ImportedType:
		return type->importedType.packageName->val.ToString() + '_' +
			type->importedType.typeName->val.ToString();
	case AnyType:
		return "any";
	default:
		break;
	}

	return "";
}

eastl::string BuildExprString(Expr* expr)
{
	switch (expr->typeID)
	{
	case LiteralExpr:
		return expr->literalExpr.val->ToString();
	case IdentifierExpr:
		return expr->identifierExpr.identifier->val.ToString();
	case PrimitiveExpr:
		return expr->primitiveExpr.primitive->val.ToString();
	case SelectorExpr:
		return "sel_" + BuildExprString(expr->selectorExpr.on) + '_' + BuildExprString(expr->selectorExpr.select);
	case IndexExpr:
		return "ind_" + BuildExprString(expr->indexExpr.of) + '_' + BuildExprString(expr->indexExpr.index);
	case FunctionCallExpr:
	{
		eastl::string funcCallStr = BuildExprString(expr->functionCallExpr.function) + "__";

		size_t size = expr->functionCallExpr.params->size();
		for (size_t i = 0; i < size; i++)
		{
			Expr* param = expr->functionCallExpr.params->at(i);
			funcCallStr += BuildExprString(param) + '_';
		}
		if (size > 0)
			funcCallStr += '_';
		else funcCallStr += "__";

		return funcCallStr;
	}
	case NewExpr:
		return "new_" + BuildExprString(expr->newExpr.primaryExpr) +
			(expr->newExpr.atExpr ? '_' + BuildExprString(expr->newExpr.atExpr) : "");
	case FixedExpr:
		return "fixed_" + BuildExprString(expr->fixedExpr.atExpr);
	case TypeLiteralExpr:
	{
		eastl::string anonTypeStr = "anon_";

		if (expr->typeLiteralExpr.typed)
		{
			anonTypeStr += BuildExprString(expr->typeLiteralExpr.typed) + "_";
		}

		size_t size = expr->typeLiteralExpr.values->size();
		for (size_t i = 0; i < size; i++)
		{
			Expr* val = expr->typeLiteralExpr.values->at(i);
			anonTypeStr += BuildExprString(val);
			if (i < size - 1) anonTypeStr += '_';
		}

		return anonTypeStr;
	}
	case ExplicitTypeExpr:
	{
		eastl::string expTypeStr = "expl_";

		size_t size = expr->explicitTypeExpr.values->size();
		for (size_t i = 0; i < size; i++)
		{
			Stmnt* val = expr->explicitTypeExpr.values->at(i);
			expTypeStr += BuildTypeString(val->definition.type) + '_' + val->definition.name->val.ToString();
			if (val->definition.assignment) expTypeStr += '_' + BuildExprString(val->definition.assignment);
			expTypeStr += '_';
		}

		return expTypeStr;
	}
	case AsExpr:
		return BuildExprString(expr->asExpr.of) + "_as_" + BuildTypeString(expr->asExpr.to);
	case DereferenceExpr:
		return "deref_" + BuildExprString(expr->dereferenceExpr.of);
	case ReferenceExpr:
		return "ref_" + BuildExprString(expr->referenceExpr.of);
	case BinaryExpr:
		return BuildExprString(expr->binaryExpr.left) + '_' + OperatorToString(expr->binaryExpr.op->uniqueType)
			+ '_' + BuildExprString(expr->binaryExpr.right);
	case UnaryExpr:
		return OperatorToString(expr->unaryExpr.op->uniqueType) + '_' + BuildExprString(expr->unaryExpr.expr);
	case GroupedExpr:
		return "group_" + BuildExprString(expr->groupedExpr.expr);
	case TemplateExpr:
		return BuildExprString(expr->templateExpr.expr) + BuildTemplatedString(expr->templateExpr.templateArgs);
	case TypeExpr:
		return BuildTypeString(expr->typeExpr.type);
	case FunctionTypeDeclExpr:
	{
		auto& anonFunction = expr->functionTypeDeclExpr.anonFunction->anonFunction;
		eastl::string funcDeclStr = "funcD_" + BuildTypeString(anonFunction.returnType);

		for (Stmnt* param : *anonFunction.decl->functionDecl.parameters)
		{
			funcDeclStr += BuildTypeString(param->definition.type) + '_';
		}

		return funcDeclStr;
	}
	case CompileExpr:
		// Error
		break;
	case SizeOfExpr:
		return "sizeof_" + BuildExprString(expr->sizeOfExpr.expr);
	case AlignOfExpr:
		return "alignof_" + BuildExprString(expr->alignOfExpr.expr);
	case OffsetOfExpr:
		return "offsetof_" + BuildExprString(expr->offsetOfExpr.type) + BuildExprString(expr->offsetOfExpr.expr);
	case TypeOfExpr:
		return "typeof_" + BuildExprString(expr->typeOfExpr.expr);
	default:
		break;
	}

	return "";
}

inline eastl::string BuildConOpParamsTypeString(Stmnt* funcDecl,
	eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
{
	eastl::string paramTypes = "";

	// Skip 'this' parameters, get the type directly from state
	for (size_t i = 1; i < funcDecl->functionDecl.parameters->size(); i++)
	{
		Stmnt* def = funcDecl->functionDecl.parameters->at(i);
		if (generics && def->definition.type->typeID == TypeID::NamedType)
		{
			for (size_t i = 0; i < generics->size(); i++)
			{
				Expr* templ = templates->at(i);
				if (templ->typeID == ExprID::TypeExpr)
				{
					if (generics->at(i)->val == def->definition.type->namedType.typeName->val)
					{
						paramTypes += BuildTypeString(templ->typeExpr.type) + '_';
						goto genericFound;
					}
				}
			}
		}
		paramTypes += BuildTypeString(def->definition.type) + '_';
	genericFound:;
	}

	return paramTypes;
}

inline eastl::string BuildPackageName(Token* package)
{
	return package->val.ToString();
}

inline eastl::string _BuildStateName(Token* package, Token* name)
{
	return BuildPackageName(package) + '_' + name->val.ToString();
}

inline eastl::string BuildStateName(Stmnt* state)
{
	return _BuildStateName(state->package, state->state.name);
}

inline eastl::string BuildTemplatedStateName(Stmnt* state, eastl::vector<Expr*>* templates)
{
	return BuildStateName(state) + BuildTemplatedString(templates);
}

inline eastl::string BuildFunctionName(Stmnt* func)
{
	return BuildPackageName(func->package) + '_' + func->function.name->val.ToString();
}

inline eastl::string BuildTemplatedFunctionName(Stmnt* func, eastl::vector<Expr*>* templates)
{
	return BuildFunctionName(func) + BuildTemplatedString(templates);
}

inline eastl::string BuildMethodName(SpiteIR::State* state, Stmnt* method)
{
	return  state->name + '_' + method->method.name->val.ToString();
}

inline eastl::string BuildTemplatedMethodName(SpiteIR::State* state, Stmnt* method, eastl::vector<Expr*>* templates)
{
	return BuildMethodName(state, method) + BuildTemplatedString(templates);
}

inline eastl::string BuildDefaultConstructorName(Stmnt* state, eastl::vector<Expr*>* templates = nullptr)
{
	return "default_" + BuildStateName(state) + BuildTemplatedString(templates);
}

inline eastl::string BuildConstructorName(Stmnt* con,
	eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
{
	return "con_" + _BuildStateName(con->package, con->constructor.stateName) + BuildTemplatedString(templates) + 
		'_' + BuildConOpParamsTypeString(con->constructor.decl, generics, templates);
}

inline eastl::string BuildOperatorMethodName(Stmnt* op, eastl::string& stateName,
	eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
{
	return OperatorToString(op->stateOperator.op->uniqueType) + stateName + '_' +
		BuildConOpParamsTypeString(op->stateOperator.decl, generics, templates);
}

inline eastl::string BuildDestructorName(SpiteIR::State* state)
{
	return "dest_" + state->name;
}

inline eastl::string BuildGlobalVariableName(Stmnt* global)
{
	return BuildPackageName(global->package) + '_' + global->definition.name->val.ToString();
}

inline bool IsVoidType(SpiteIR::Type* type)
{
	return type->kind == SpiteIR::TypeKind::PrimitiveType &&
		type->primitive.kind == SpiteIR::PrimitiveKind::Void;
}

template<typename Low>
SpiteIR::State* FindState(Low* lower, const eastl::string& val, SpiteIR::Type* type, bool allowResolve = true)
{
	SpiteIR::State* state = lower->context.FindState(val);
	if (!state)
	{
		if (allowResolve) lower->context.toResolveStateType.push_back({ val, type });
	}
	else if (!state->size && type) lower->context.toResolveSizeAndAlignment.insert(type);
	else if (type)
	{
		type->size = state->size;
		type->alignment = state->alignment;
		type->byValue = state->IsValueType();
	}

	return state;
}

SpiteIR::Type* IRFunctionToFunctionType(SpiteIR::IR* ir, SpiteIR::Function* function)
{
	SpiteIR::Type* funcType = ir->AllocateType();
	funcType->kind = SpiteIR::TypeKind::FunctionType;
	funcType->size = config.targetArchByteWidth;
	funcType->alignment = config.targetArchByteWidth;
	funcType->byValue = true;
	funcType->function.params = ir->AllocateArray<SpiteIR::Type*>();
	funcType->function.returnType = function->returnType;

	for (SpiteIR::Argument* arg : function->arguments)
	{
		funcType->function.params->push_back(arg->value.type);
	}
	return funcType;
}

SpiteIR::Type* BuildFixedArray(SpiteIR::IR* ir, size_t count, SpiteIR::Type* type)
{
	SpiteIR::Type* fixedArray = ir->AllocateType();
	fixedArray->kind = SpiteIR::TypeKind::FixedArrayType;
	fixedArray->fixedArray.count = count;
	fixedArray->fixedArray.type = type;
	fixedArray->size = (type->size * count);
	fixedArray->alignment = type->alignment;
	return fixedArray;
}

struct SizeAndAlignment { size_t size; size_t alignment; };
SizeAndAlignment CalculateSizeAndAlignForMembers(eastl::vector<SpiteIR::Member*>* members)
{
	size_t offset = 0;
	size_t alignment = 1;

	for (SpiteIR::Member* member : *members)
	{
		SpiteIR::Type* memberType = member->value.type;
		if (!memberType->alignment) return { 0, 0 };
		size_t memberAlignment = memberType->alignment;
		size_t padding = (memberAlignment - (offset % memberAlignment)) % memberAlignment;
		offset += padding;

		member->offset = offset;
		if (memberAlignment > alignment) alignment = memberAlignment;
		offset += memberType->size;
	}

	return { (offset + alignment - 1) & ~(alignment - 1), alignment };
}

template<typename Low>
void SetStructuredTypeSizeAndAlign(SpiteIR::Type* type, Low* lower)
{
	if (type->kind == SpiteIR::TypeKind::StructureType)
	{
		SizeAndAlignment sa = CalculateSizeAndAlignForMembers(type->structureType.members);
		if (!sa.alignment) lower->context.toResolveSizeAndAlignment.insert(type);

		type->size = sa.size;
		type->alignment = sa.alignment;
	}
}

bool IsConstantIntExpr(Expr* expr, ScopeUtils& scopeUtils, 
	eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr, 
	bool checkIdent = true)
{
	expr = _ExpandTemplate(expr, generics, templates);
	switch (expr->typeID)
	{
	case LiteralExpr:
		return expr->literalExpr.val->uniqueType == UniqueType::IntLiteral ||
			expr->literalExpr.val->uniqueType == UniqueType::HexLiteral ||
			expr->literalExpr.val->uniqueType == UniqueType::ByteLiteral;
	case IdentifierExpr:
	{
		//Only allow one degree of seperation to be considered a constant
		// Constant --
		// a := 1
		// arr := [a]int
		// Not Constant --
		// a := 1; b := a
		// arr := [b]int
		if (!checkIdent) return false;

		Stmnt* def = scopeUtils.FindInScope(expr->identifierExpr.identifier->val);
		if (!def || !def->definition.assignment) return false;
		return IsConstantIntExpr(def->definition.assignment, scopeUtils,
			generics, templates, false);
	}
	case BinaryExpr:
		return IsConstantIntExpr(expr->binaryExpr.left, scopeUtils, generics, templates)
			&& IsConstantIntExpr(expr->binaryExpr.right, scopeUtils, generics, templates);
	case UnaryExpr:
		return IsConstantIntExpr(expr->unaryExpr.expr, scopeUtils, generics, templates);
	case GroupedExpr:
		return IsConstantIntExpr(expr->groupedExpr.expr, scopeUtils, generics, templates);
	case SelectorExpr:
	{
		// Check if enum selection
		if (expr->selectorExpr.on->typeID == IdentifierExpr)
		{
			Token* name = expr->selectorExpr.on->identifierExpr.identifier;
			return scopeUtils.globalTable->FindScopedEnum(name, scopeUtils.symbolTable);
		}
		else if (expr->selectorExpr.on->typeID == SelectorExpr)
		{
			if (scopeUtils.IsPackageExpr(expr->selectorExpr.on))
			{
				Token* package = expr->selectorExpr.on->selectorExpr.on->identifierExpr.identifier;
				Token* name = expr->selectorExpr.on->selectorExpr.select->identifierExpr.identifier;
				SymbolTable* symbolTable = scopeUtils.globalTable->FindSymbolTable(package->val);
				return symbolTable->FindEnum(name->val);
			}
		}
	}
	case SizeOfExpr:
	case AlignOfExpr:
		return true;
	default:
		break;
	}

	return false;
}

template<typename Low>
intmax_t EvaluateConstantIntExpr(Expr* expr, Low* lower,
	eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
{
	expr = _ExpandTemplate(expr, generics, templates);
	ScopeUtils& scopeUtils = lower->GetScopeUtils();
	switch (expr->typeID)
	{
	case LiteralExpr:
	{
		StringView& str = expr->literalExpr.val->val;
		if (expr->literalExpr.val->uniqueType == UniqueType::IntLiteral)
		{
			return IntLiteralStringToInt(str);
		}
		else if (expr->literalExpr.val->uniqueType == UniqueType::HexLiteral)
		{
			return std::stoll(str.ToString().c_str(), nullptr, 0);
		}
		else if (expr->literalExpr.val->uniqueType == UniqueType::ByteLiteral)
		{
			return str[0];
		}

		break;
	}
	case IdentifierExpr:
	{
		Stmnt* def = scopeUtils.FindInScope(expr->identifierExpr.identifier->val);
		if (def && def->definition.assignment)
		{
			return EvaluateConstantIntExpr(def->definition.assignment, lower, generics, templates);
		}

		break;
	}
	case BinaryExpr:
	{
		intmax_t left = EvaluateConstantIntExpr(expr->binaryExpr.left, lower, generics, templates);
		intmax_t right = EvaluateConstantIntExpr(expr->binaryExpr.right, lower, generics, templates);
		switch (expr->binaryExpr.op->uniqueType)
		{
		case UniqueType::Add:
			return left + right;
		case UniqueType::Subtract:
			return left - right;
		case UniqueType::Multiply:
			return left * right;
		case UniqueType::Divide:
			return left / right;
		case UniqueType::Modulo:
			return left % right;
		case UniqueType::And:
			return left & right;
		case UniqueType::Or:
			return left | right;
		case UniqueType::Xor:
			return left ^ right;
		case UniqueType::Shiftl:
			return left << right;
		case UniqueType::Shiftr:
			return left >> right;
		case UniqueType::AndNot:
			return left & ~right;
		case UniqueType::LogicAnd:
			return left && right;
		case UniqueType::LogicOr:
			return left || right;
		case UniqueType::Equal:
			return left == right;
		case UniqueType::Less:
			return left < right;
		case UniqueType::Greater:
			return left > right;
		case UniqueType::NotEql:
			return left != right;
		case UniqueType::LessEqual:
			return left <= right;
		case UniqueType::GreaterEqual:
			return left >= right;
		default:
			break;
		}

		break;
	}
	case UnaryExpr:
	{
		intmax_t value = EvaluateConstantIntExpr(expr->unaryExpr.expr, lower, generics, templates);
		switch (expr->unaryExpr.op->uniqueType)
		{
		case UniqueType::Subtract:
			return -value;
		case UniqueType::Not:
			return !value;
		case UniqueType::Xor:
			return ~value;
		default:
			break;
		}

		break;
	}
	case GroupedExpr:
		return EvaluateConstantIntExpr(expr->groupedExpr.expr, lower, generics, templates);
	case SelectorExpr:
	{
		// Check if enum selection
		if (expr->selectorExpr.on->typeID == IdentifierExpr)
		{
			Token* member = expr->selectorExpr.select->identifierExpr.identifier;
			Token* name = expr->selectorExpr.on->identifierExpr.identifier;
			Stmnt* enumStmnt = scopeUtils.globalTable->FindScopedEnum(name, scopeUtils.symbolTable);
			return lower->context.FindEnumValue(enumStmnt, member->val);
		}
		else if (expr->selectorExpr.on->typeID == SelectorExpr)
		{
			if (scopeUtils.IsPackageExpr(expr->selectorExpr.on))
			{
				Token* member = expr->selectorExpr.select->identifierExpr.identifier;
				Token* package = expr->selectorExpr.on->selectorExpr.on->identifierExpr.identifier;
				Token* name = expr->selectorExpr.on->selectorExpr.select->identifierExpr.identifier;
				SymbolTable* symbolTable = scopeUtils.globalTable->FindSymbolTable(package->val);
				Stmnt* enumStmnt = scopeUtils.symbolTable->FindEnum(name->val);
				return lower->context.FindEnumValue(enumStmnt, member->val);
			}
		}

		break;
	}
	case SizeOfExpr:
		return lower->GetSizeOf(expr, generics, templates);
	case AlignOfExpr:
		return lower->GetAlignOf(expr, generics, templates);
	default:
		break;
	}

	return 0;
}

template<typename Low>
SpiteIR::Type* TypeToIRType(SpiteIR::IR* ir, Type* type, Low* lower,
	eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
{
	switch (type->typeID)
	{
	case InvalidType:
	case UnknownType:
	case ImplicitType:
		AddError("Lower:TypeToIRType Invalid type for conversion: " + eastl::to_string(type->typeID));
		break;
	case PrimitiveType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::PrimitiveType;
		irType->size = type->primitiveType.size;
		irType->alignment = type->primitiveType.size;
		irType->byValue = true;
		irType->primitive.isSigned = type->primitiveType.isSigned;

		switch (type->primitiveType.type)
		{
		case Void:
			irType->alignment = 1;
			irType->primitive.kind = SpiteIR::PrimitiveKind::Void;
			break;
		case Bool:
			irType->primitive.kind = SpiteIR::PrimitiveKind::Bool;
			break;
		case Byte:
			irType->primitive.kind = SpiteIR::PrimitiveKind::Byte;
			break;
		case Ubyte:
			irType->primitive.kind = SpiteIR::PrimitiveKind::Byte;
			break;
		case Uint:
		case Int:
		case Uint128:
		case Int128:
			irType->primitive.kind = SpiteIR::PrimitiveKind::Int;
			break;
		case Uint16:
		case Int16:
			irType->primitive.kind = SpiteIR::PrimitiveKind::I16;
			break;
		case Uint32:
		case Int32:
			irType->primitive.kind = SpiteIR::PrimitiveKind::I32;
			break;
		case Uint64:
		case Int64:
			irType->primitive.kind = SpiteIR::PrimitiveKind::I64;
			break;
		case Float:
		case Float64:
		case Float128:
			irType->primitive.kind = SpiteIR::PrimitiveKind::Float;
			break;
		case Float32:
			irType->primitive.kind = SpiteIR::PrimitiveKind::F32;
			break;
		case String:
			irType->alignment = config.targetArchByteWidth;
			irType->primitive.kind = SpiteIR::PrimitiveKind::String;
			break;
		default:
			break;
		}

		return irType;
	}
	case NamedType:
	{
		if (generics && templates)
		{
			for (size_t i = 0; i < generics->size(); i++)
			{
				Token* genericName = generics->at(i);
				if (type->namedType.typeName->val == genericName->val)
				{
					Expr* templ = _ExpandTemplate(templates->at(i), generics, templates);
					if (templ && templ->typeID == ExprID::TypeExpr)
					{
						return TypeToIRType(ir, templ->typeExpr.type, lower, generics, templates);
					}
					else AddError(templ->start, "Lower:ReplaceTypeWithTemplateType Invalid expression used as type template");
				}
			}
		}

		AddError(type->namedType.typeName, "Lower:ReplaceTypeWithTemplateType Named types should have been qualified by now");
		return nullptr;
	}
	case ImportedType:
	{
		Stmnt* enumStmnt = lower->context.globalTable->FindEnumForType(type, lower->symbolTable);
		if (enumStmnt)
		{
			return TypeToIRType(ir, enumStmnt->enumStmnt.type, lower, generics, templates);
		}

		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::StateType;
		eastl::string typeName = BuildTypeString(type);
		irType->stateType.state = FindState(lower, typeName, irType);
		return irType;
	}
	case TemplatedType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::StateType;
		eastl::vector<Expr*>* templateArgs = type->templatedType.templates->templateExpr.templateArgs;
		eastl::vector<Expr*> expandedArgs = _ExpandTemplates(templateArgs, generics, templates, lower->symbolTable);
		eastl::string typeName = BuildTypeString(_ExpandTypeTemplates(type->templatedType.type, generics, templates, lower->symbolTable)) + 
			BuildTemplatedString(&expandedArgs);
		irType->stateType.state = FindState(lower, typeName, irType);
		return irType;
	}
	case ExplicitType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::StructureType;
		irType->structureType.members = ir->AllocateArray<SpiteIR::Member*>();
		for (size_t i = 0; i < type->explicitType.declarations->size(); i++)
		{
			Stmnt* stmnt = type->explicitType.declarations->at(i);
			auto& def = stmnt->definition;
			SpiteIR::Type* memberType = TypeToIRType(ir, def.type, lower, generics, templates);
			SpiteIR::Member* member = ir->AllocateMember();
			member->value.type = memberType;
			member->value.name = def.name->val.ToString();
			irType->structureType.members->push_back(member);
		}
		SetStructuredTypeSizeAndAlign(irType, lower);
		return irType;
	}
	case AnonymousType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::StructureType;
		irType->structureType.members = ir->AllocateArray<SpiteIR::Member*>();
		for (size_t i = 0; i < type->anonType.types->size(); i++)
		{
			SpiteIR::Type* memberType = TypeToIRType(ir, type->anonType.types->at(i), lower, generics, templates);
			SpiteIR::Member* member = ir->AllocateMember();
			member->value.type = memberType;
			irType->structureType.members->push_back(member);
		}
		SetStructuredTypeSizeAndAlign(irType, lower);
		return irType;
	}
	case UnionType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::UnionType;
		irType->size = 0;
		irType->alignment = 0;
		irType->structureType.members = ir->AllocateArray<SpiteIR::Member*>();
		for (size_t i = 0; i < type->unionType.declarations->size(); i++)
		{
			Stmnt* stmnt = type->unionType.declarations->at(i);
			auto& def = stmnt->definition;
			SpiteIR::Type* memberType = TypeToIRType(ir, def.type, lower, generics, templates);
			if (!memberType->alignment)
			{
				lower->context.toResolveSizeAndAlignment.insert(irType);
			}
			if (memberType->size > irType->size) irType->size = memberType->size;
			if (memberType->alignment > irType->alignment) irType->alignment = memberType->alignment;
			SpiteIR::Member* member = ir->AllocateMember();
			member->value.type = memberType;
			member->value.name = def.name->val.ToString();
			irType->structureType.members->push_back(member);
		}
		return irType;
	}
	case PointerType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::PointerType;
		irType->size = config.targetArchByteWidth;
		irType->alignment = config.targetArchByteWidth;
		irType->byValue = true;
		irType->pointer.type = TypeToIRType(ir, type->pointerType.type, lower, generics, templates);
		return irType;
	}
	case ValueType:
	{
		SpiteIR::Type* irType = TypeToIRType(ir, type->valueType.type, lower, generics, templates);
		irType->byValue = true;
		return irType;
	}
	case RefType:
	{
		SpiteIR::Type* irType = TypeToIRType(ir, type->refType.type, lower, generics, templates);
		return MakeReferenceType(irType, ir);
	}
	case ArrayType:
	{
		if (type->arrayType.size)
		{
			if (IsConstantIntExpr(type->arrayType.size, lower->GetScopeUtils(), generics, templates))
			{
				intmax_t constantSize = EvaluateConstantIntExpr(type->arrayType.size, lower, generics, templates);
				SpiteIR::Type* irType = BuildFixedArray(ir, constantSize,
					TypeToIRType(ir, type->arrayType.type, lower, generics, templates));
				if (!irType->alignment)
				{
					lower->context.toResolveSizeAndAlignment.insert(irType);
				}
				return irType;
			}
		}

		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::DynamicArrayType;
		irType->size = config.targetArchByteWidth * 4;
		irType->alignment = config.targetArchByteWidth;
		irType->dynamicArray.type = TypeToIRType(ir, type->arrayType.type, lower, generics, templates);
		return irType;
	}
	case FunctionType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::FunctionType;
		irType->size = config.targetArchByteWidth;
		irType->alignment = config.targetArchByteWidth;
		irType->byValue = true;
		irType->function.params = ir->AllocateArray<SpiteIR::Type*>();
		irType->function.returnType = TypeToIRType(ir, type->functionType.returnType, lower, generics, templates);
		for (Type* param : *type->functionType.paramTypes)
		{
			SpiteIR::Type* paramType = TypeToIRType(ir, param, lower, generics, templates);
			if (!paramType->alignment)
			{
				lower->context.toResolveSizeAndAlignment.insert(paramType);
			}
			if (!paramType->byValue)
				paramType = MakeReferenceType(paramType, ir);
			irType->function.params->push_back(paramType);
		}
		return irType;
	}
	case AnyType:
	{
		return MakeReferenceType(CreateVoidType(ir), ir);
	}
	default:
		break;
	}

	return nullptr;
}
