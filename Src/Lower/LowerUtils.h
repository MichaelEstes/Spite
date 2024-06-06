#pragma once
#include "../Intermediate/GlobalTable.h"
#include "../Intermediate/SyntaxUtils.h"
#include "../IR/IR.h"


eastl::string BuildExprString(Expr* expr);

eastl::string OperatorToString(UniqueType op)
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
		return "array";
	default:
		break;
	}

	return "";
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
		return type->importedType.packageName->val.ToString() + type->importedType.typeName->val.ToString();
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
	case AnonTypeExpr:
	{
		eastl::string anonTypeStr = "anon_";

		size_t size = expr->anonTypeExpr.values->size();
		for (size_t i = 0; i < size; i++)
		{
			Expr* val = expr->anonTypeExpr.values->at(i);
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
		return BuildExprString(expr->binaryExpr.left) + '_' + OperatorToString(expr->binaryExpr.opType) + '_' +
			BuildExprString(expr->binaryExpr.right);
	case UnaryExpr:
		return OperatorToString(expr->unaryExpr.opType) + '_' + BuildExprString(expr->unaryExpr.expr);
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

inline eastl::string BuildMethodName(Stmnt* method)
{
	return BuildPackageName(method->package) + '_' + method->method.stateName->val.ToString() + '_' +
		method->method.name->val.ToString();
}

inline eastl::string BuildTemplatedMethodName(Stmnt* method, eastl::vector<Expr*>* templates)
{
	return BuildMethodName(method) + BuildTemplatedString(templates);
}

inline eastl::string BuildOperatorName(Stmnt* op)
{
	return BuildPackageName(op->package) + '_' + op->stateOperator.stateName->val.ToString() + '_' +
		op->stateOperator.op->val.ToString();
}

inline eastl::string BuildGlobalVariableName(Stmnt* global)
{
	return BuildPackageName(global->package) + '_' + global->definition.name->val.ToString();
}

template<typename P, typename Low>
SpiteIR::Type* TypeToIRType(SpiteIR::IR* ir, Type* type, P* parent, Low* lower,
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
		SpiteIR::Type* irType = ir->AllocateType(parent);
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
						return TypeToIRType(ir, templ->typeExpr.type, parent, lower, generics, templates);
					}
					else AddError(genericName, "Lower:ReplaceTypeWithTemplateType Invalid expression used as type template");
				}
			}
		}
	}
	case ImportedType:
	{
		SpiteIR::Type* irType = ir->AllocateType(parent);
		irType->kind = SpiteIR::TypeKind::NamedType;
		Stmnt* state = lower->globalTable->FindStateForType(type, lower->symbolTable);
		irType->namedType.name = ir->AllocateString();
		*irType->namedType.name = BuildStateName(state);
		return irType;
	}
	case ExplicitType:
	{
		SpiteIR::Type* irType = ir->AllocateType(parent);
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
			member->type = TypeToIRType(ir, def.type, member, lower, generics, templates);
			irType->anonymousType.members->push_back(member);
		}
		return irType;
	}
	case PointerType:
	{
		SpiteIR::Type* irType = ir->AllocateType(parent);
		irType->kind = SpiteIR::TypeKind::PointerType;
		irType->pointer.type = TypeToIRType(ir, type->pointerType.type, irType, lower, generics, templates);
		return irType;
	}
	case ValueType:
	{
		SpiteIR::Type* irType = ir->AllocateType(parent);
		irType->kind = SpiteIR::TypeKind::ValueType;
		irType->value.type = TypeToIRType(ir, type->valueType.type, irType, lower, generics, templates);
		return irType;
	}
	case ArrayType:
	{
		SpiteIR::Type* irType = ir->AllocateType(parent);
		irType->kind = SpiteIR::TypeKind::ArrayType;
		irType->array.type = TypeToIRType(ir, type->arrayType.type, irType, lower, generics, templates);
		return irType;
	}
	case TemplatedType:
		break;
	case FunctionType:
	{
		SpiteIR::Type* irType = ir->AllocateType(parent);
		irType->kind = SpiteIR::TypeKind::FunctionType;
		irType->function.params = ir->AllocateArray<SpiteIR::Type*>();
		irType->function.returnType = TypeToIRType(ir, type->functionType.returnType, irType, lower, generics, templates);
		for (Type* param : *type->functionType.paramTypes)
		{
			irType->function.params->push_back(TypeToIRType(ir, param, irType, lower, generics, templates));
		}
		return irType;
	}
	default:
		break;
	}

	return nullptr;
}
