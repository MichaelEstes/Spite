#pragma once
#include "../Syntax/GlobalTable.h"
#include "../Syntax/SyntaxUtils.h"
#include "../IR/IR.h"

extern Config config;

eastl::string BuildExprString(Expr* expr);
int IsIRTypeAssignable(SpiteIR::Type* left, SpiteIR::Type* right);

eastl::vector<SpiteIR::Type*> GetStateTypes(SpiteIR::State* state)
{
	eastl::vector<SpiteIR::Type*> types;
	for (SpiteIR::Member* member : state->members)
	{
		types.push_back(member->value->type);
	}

	return types;
}

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
		type->primitive.kind <= SpiteIR::PrimitiveKind::Float);
}

eastl::vector<SpiteIR::Type*> GetStructuredTypes(SpiteIR::Type* type)
{
	if (type->kind == SpiteIR::TypeKind::StateType) return GetStateTypes(type->stateType.state);
	else return *type->structureType.types;
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

	if (IsStructuredType(left) && IsStructuredType(right))
	{
		if (IRTypesAssignable(GetStructuredTypes(left), GetStructuredTypes(right)))
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

SpiteIR::Type* CreateVoidType(SpiteIR::IR* ir)
{
	SpiteIR::Type* type = ir->AllocateType();
	type->size = 0;
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
	type->kind = SpiteIR::TypeKind::PointerType;
	type->byValue = true;
	type->pointer.type = CreateVoidType(ir);
	return type;
}

SpiteIR::Type* MakeReferenceType(SpiteIR::Type* type, SpiteIR::IR* ir)
{
	SpiteIR::Type* refType = ir->AllocateType();
	refType->kind = SpiteIR::TypeKind::ReferenceType;
	refType->size = config.targetArchByteWidth;
	refType->byValue = true;
	refType->reference.type = type;
	return refType;
}

SpiteIR::Type* MakePointerType(SpiteIR::Type* type, SpiteIR::IR* ir)
{
	SpiteIR::Type* ptrType = ir->AllocateType();
	ptrType->kind = SpiteIR::TypeKind::PointerType;
	ptrType->size = config.targetArchByteWidth;
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

eastl::vector<Expr*> _ExpandTemplates(eastl::vector<Expr*>* exprs, eastl::vector<Token*>* generics, 
	eastl::vector<Expr*>* templates)
{
	eastl::vector<Expr*> expanded;
	for (Expr* expr : *exprs)
	{
		expanded.push_back(_ExpandTemplate(expr, generics, templates));
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
	case PointerType:
		return "ptr_" + BuildTypeString(type->pointerType.type);
	case ValueType:
		return BuildTypeString(type->valueType.type);
	case ArrayType:
		return "arr_" + BuildTypeString(type->arrayType.type);
	case FixedArrayType:
		return "arr_" + eastl::to_string(type->fixedArrayType.size) + "_" +
			BuildTypeString(type->fixedArrayType.type);
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
	return "con_" + _BuildStateName(con->package, con->constructor.stateName) + '_' +
		BuildConOpParamsTypeString(con->constructor.decl, generics, templates);
}

inline eastl::string BuildOperatorMethodName(Stmnt* op,
	eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
{
	return OperatorToString(op->stateOperator.op->uniqueType) +
		_BuildStateName(op->package, op->stateOperator.stateName) + '_' +
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
SpiteIR::State* FindState(Low* lower, const eastl::string& val, SpiteIR::Type* type)
{
	SpiteIR::State* state = lower->context.FindState(val);
	if (!state) lower->context.toResolveStateType.push_back({ val, type });
	else if (!state->size && type) lower->context.toResolveStateSize.push_back(type);
	else if (type) type->size = state->size;
	return state;
}

SpiteIR::Type* IRFunctionToFunctionType(SpiteIR::IR* ir, SpiteIR::Function* function)
{
	SpiteIR::Type* funcType = ir->AllocateType();
	funcType->kind = SpiteIR::TypeKind::FunctionType;
	funcType->size = config.targetArchByteWidth;
	funcType->byValue = true;
	funcType->function.params = ir->AllocateArray<SpiteIR::Type*>();
	funcType->function.returnType = function->returnType;

	for (SpiteIR::Argument* arg : function->arguments)
	{
		funcType->function.params->push_back(arg->value->type);
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
	return fixedArray;
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
		irType->byValue = true;
		irType->primitive.isSigned = type->primitiveType.isSigned;

		switch (type->primitiveType.type)
		{
		case Void:
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
					else AddError(genericName, "Lower:ReplaceTypeWithTemplateType Invalid expression used as type template");
				}
			}
		}

		AddError(type->namedType.typeName, "Lower:ReplaceTypeWithTemplateType Named types should have been qualified by now");
		return nullptr;
	}
	case ImportedType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::StateType;
		eastl::string typeName = BuildTypeString(type);
		irType->stateType.state = FindState(lower, typeName, irType);
		return irType;
	}
	case ExplicitType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::StructureType;
		irType->size = 0;
		irType->structureType.types = ir->AllocateArray<SpiteIR::Type*>();
		irType->structureType.names = ir->AllocateArray<eastl::string>();
		for (size_t i = 0; i < type->explicitType.declarations->size(); i++)
		{
			Stmnt* stmnt = type->explicitType.declarations->at(i);
			auto& def = stmnt->definition;
			SpiteIR::Type* member = TypeToIRType(ir, def.type, lower, generics, templates);
			irType->size += member->size;
			irType->structureType.types->push_back(member);
			irType->structureType.names->push_back(def.name->val.ToString());
		}
		return irType;
	}
	case AnonymousType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::StructureType;
		irType->size = 0;
		irType->structureType.names = nullptr;
		irType->structureType.types = ir->AllocateArray<SpiteIR::Type*>();
		for (size_t i = 0; i < type->anonType.types->size(); i++)
		{
			SpiteIR::Type* member = TypeToIRType(ir, type->anonType.types->at(i), lower, generics, templates);
			irType->size += member->size;
			irType->structureType.types->push_back(member);
		}
		return irType;
	}
	case PointerType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::PointerType;
		irType->size = config.targetArchByteWidth;
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
	case ArrayType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::DynamicArrayType;
		irType->size = config.targetArchByteWidth * 4;
		irType->dynamicArray.type = TypeToIRType(ir, type->arrayType.type, lower, generics, templates);
		return irType;
	}
	case FixedArrayType:
	{
		SpiteIR::Type* irType = BuildFixedArray(ir, type->fixedArrayType.size,
			TypeToIRType(ir, type->fixedArrayType.type, lower, generics, templates));
		return irType;
	}
	case TemplatedType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::StateType;
		eastl::vector<Expr*>* templateArgs = type->templatedType.templates->templateExpr.templateArgs;
		eastl::vector<Expr*> expandedArgs = _ExpandTemplates(templateArgs, generics, templates);
		eastl::string typeName = BuildTypeString(type->templatedType.type) + BuildTemplatedString(&expandedArgs);
		irType->stateType.state = FindState(lower, typeName, irType);
		return irType;
	}
	case FunctionType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::FunctionType;
		irType->size = config.targetArchByteWidth;
		irType->byValue = true;
		irType->function.params = ir->AllocateArray<SpiteIR::Type*>();
		irType->function.returnType = TypeToIRType(ir, type->functionType.returnType, lower, generics, templates);
		for (Type* param : *type->functionType.paramTypes)
		{
			SpiteIR::Type* paramType = TypeToIRType(ir, param, lower, generics, templates);
			if (!paramType->byValue)
				paramType = MakeReferenceType(paramType, ir);
			irType->function.params->push_back(paramType);
		}
		return irType;
	}
	case AnyType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::ReferenceType;
		irType->size = config.targetArchByteWidth;
		irType->byValue = true;
		irType->reference.type = CreateVoidType(ir);
		return irType;
	}
	default:
		break;
	}

	return nullptr;
}
