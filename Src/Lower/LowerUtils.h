#pragma once
#include "../Syntax/GlobalTable.h"
#include "../Syntax/SyntaxUtils.h"
#include "../IR/IR.h"

extern Config config;

eastl::string BuildExprString(Expr* expr);

inline eastl::string OperatorToString(Token* op)
{
	switch (op->uniqueType)
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

	AddError(op, "LowerUtils:OperatorToString Invalid operator");
	return "";
}

SpiteIR::Type _voidType = {
	0,
	SpiteIR::TypeKind::PrimitiveType,
	true,
	{
		true,
		SpiteIR::PrimitiveKind::Void
	}
};

SpiteIR::Type _boolType = {
	1,
	SpiteIR::TypeKind::PrimitiveType,
	true,
	{
		true,
		SpiteIR::PrimitiveKind::Bool
	}
};

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

eastl::string BuildTemplatedString(eastl::vector<Expr*>* templates)
{
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
		return "val_" + BuildTypeString(type->valueType.type);
	case ArrayType:
		return "arr_" + BuildTypeString(type->arrayType.type);
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
		return BuildExprString(expr->binaryExpr.left) + '_' + OperatorToString(expr->binaryExpr.op) + '_' +
			BuildExprString(expr->binaryExpr.right);
	case UnaryExpr:
		return OperatorToString(expr->unaryExpr.op) + '_' + BuildExprString(expr->unaryExpr.expr);
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

inline eastl::string BuildStateName(Stmnt* state)
{
	return BuildPackageName(state->package) + '_' + state->state.name->val.ToString();
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
	return state->name + '_' + method->method.name->val.ToString();
}

inline eastl::string BuildTemplatedMethodName(SpiteIR::State* state, Stmnt* method, eastl::vector<Expr*>* templates)
{
	return BuildMethodName(state, method) + BuildTemplatedString(templates);
}

inline eastl::string BuildConstructorName(SpiteIR::State* state, Stmnt* con,
	eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
{
	return "con_" + state->name + '_' + BuildConOpParamsTypeString(con->constructor.decl, generics, templates);
}

inline eastl::string BuildOperatorMethodName(SpiteIR::State* state, Stmnt* op,
	eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
{
	return OperatorToString(op->stateOperator.op) + state->name + '_' +
		BuildConOpParamsTypeString(op->stateOperator.decl);
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

SpiteIR::Type* BuildFixedArray(SpiteIR::IR* ir, size_t count, SpiteIR::Type* type)
{
	SpiteIR::Type* fixedArray = ir->AllocateType();
	fixedArray->kind = SpiteIR::TypeKind::FixedArrayType;
	fixedArray->fixedArray.count = count;
	fixedArray->fixedArray.type = type;
	fixedArray->size = (type->size * count);
	return fixedArray;
}

SpiteIR::Type* BuildDynamicArray(SpiteIR::IR* ir, SpiteIR::Type* type)
{
	SpiteIR::Type* dynamicArray = ir->AllocateType();
	dynamicArray->kind = SpiteIR::TypeKind::DynamicArrayType;
	dynamicArray->dynamicArray.type = type;
	return dynamicArray;
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
		AddError("Lower:TypeToIRType Invalid type for conversion");
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
			irType->primitive.kind = SpiteIR::PrimitiveKind::Int;
			break;
		case Float:
		case Float32:
		case Float64:
		case Float128:
			irType->primitive.kind = SpiteIR::PrimitiveKind::Float;
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
					Expr* templ = templates->at(i);
					if (templ && templ->typeID == ExprID::TypeExpr)
					{
						return TypeToIRType(ir, templ->typeExpr.type, lower, generics, templates);
					}
					else AddError(genericName, "Lower:ReplaceTypeWithTemplateType Invalid expression used as type template");
				}
			}
		}

		AddError("Lower:ReplaceTypeWithTemplateType Named types should have been qualified by now");
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
	case PointerType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::PointerType;
		irType->size = config.targetArchBitWidth;
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
		irType->size = config.targetArchBitWidth * 2;
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
		eastl::string typeName = BuildTypeString(type->templatedType.type) +
			BuildTemplatedString(type->templatedType.templates->templateExpr.templateArgs);
		irType->stateType.state = FindState(lower, typeName, irType);
		return irType;
	}
	case FunctionType:
	{
		SpiteIR::Type* irType = ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::FunctionType;
		irType->size = config.targetArchBitWidth;
		irType->function.params = ir->AllocateArray<SpiteIR::Type*>();
		irType->function.returnType = TypeToIRType(ir, type->functionType.returnType, lower, generics, templates);
		for (Type* param : *type->functionType.paramTypes)
		{
			irType->function.params->push_back(TypeToIRType(ir, param, lower, generics, templates));
		}
		return irType;
	}
	default:
		break;
	}

	return nullptr;
}
