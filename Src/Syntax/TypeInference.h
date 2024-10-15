#pragma once
#include "Syntax.h"
#include "GlobalTable.h"
#include "ScopeUtils.h"

static Type boolType = Type(1, UniqueType::Bool, false);

struct TypeInferer
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	ScopeUtils scopeUtils;
	Stmnt* context;

	TypeInferer(GlobalTable* globalTable, SymbolTable* symbolTable, ScopeUtils& scopeUtils, Stmnt* context)
		: globalTable(globalTable), symbolTable(symbolTable), scopeUtils(scopeUtils), context(context)
	{
	}

	inline Type* GetGenericsType(Expr* expr, Type* of)
	{
		if (of->typeID == TypeID::FunctionType)
		{
			Stmnt* func = scopeUtils.GetDeclarationStmntForExpr(expr->templateExpr.expr);
			if (!func)
			{
				AddError(expr->start, "TypeInferer:GetGenericsType No function found for expression");
				return nullptr;
			}
			Stmnt* generics = GetGenerics(func);
			if (!generics)
			{
				AddError(expr->start, "TypeInferer:GetGenericsType Template expression used on non-templated function");
				return nullptr;
			}

			auto& funcType = of->functionType;

			eastl::vector<Expr*>* templateArgs = expr->templateExpr.templateArgs;
			if (templateArgs->size() != generics->generics.names->size())
			{
				AddError(expr->start, "TypeInferer:GetGenericsType Invalid number of template arguments for generic type");
				return nullptr;
			}
			for (size_t i = 0; i < generics->generics.names->size(); i++)
			{
				Token* name = generics->generics.names->at(i);
				if (funcType.returnType->typeID == TypeID::NamedType &&
					funcType.returnType->namedType.typeName->val == name->val)
				{
					funcType.returnType = InferType(templateArgs->at(i));
				}

				for (size_t j = 0; j < funcType.paramTypes->size(); j++)
				{
					Type*& param = funcType.paramTypes->at(i);
					if (param->typeID == TypeID::NamedType &&
						param->namedType.typeName->val == name->val)
					{
						param = InferType(templateArgs->at(i));
					}
				}

			}

			return of;
		}

		Type* type = symbolTable->CreateTypePtr(TypeID::TemplatedType);
		type->templatedType.templates = symbolTable->CreateExpr(expr->start, ExprID::TemplateExpr);
		type->templatedType.templates->templateExpr.expr = nullptr;
		type->templatedType.templates->templateExpr.templateArgs = expr->templateExpr.templateArgs;
		type->templatedType.type = of;
		return type;
	}

	inline Type* FillFunctionTypeParams(eastl::vector<Stmnt*>* parameters, Type* type)
	{
		type->functionType.paramTypes = symbolTable->CreateVectorPtr<Type>();
		for (Stmnt* stmnt : *parameters)
		{
			type->functionType.paramTypes->push_back(stmnt->definition.type);
		}

		return type;
	}

	inline Type* FunctionToFunctionType(Stmnt* stmnt)
	{
		Type* type = symbolTable->CreateTypePtr(TypeID::FunctionType);
		eastl::vector<Stmnt*>* parameters = nullptr;

		switch (stmnt->nodeID)
		{
		case FunctionStmnt:
		{
			type->functionType.returnType = stmnt->function.returnType;
			parameters = stmnt->function.decl->functionDecl.parameters;
			break;
		}
		case Method:
		{
			type->functionType.returnType = stmnt->method.returnType;
			parameters = stmnt->method.decl->functionDecl.parameters;
			break;
		}
		case ExternFunctionDecl:
		{
			type->functionType.returnType = stmnt->externFunction.returnType;
			parameters = stmnt->externFunction.parameters;
		}
		default:
			break;
		}

		if (!parameters)
		{
			AddError(stmnt->start, "TypeInferer:FunctionToFunctionType Unable to find function declaration for statment: " + ToString(stmnt));
			return nullptr;
		}
		return FillFunctionTypeParams(parameters, type);
	}

	inline Type* GetIdentType(Expr* expr, Token* name)
	{
		if (globalTable->IsGenericOfStmnt(expr, context, symbolTable))
		{
			return symbolTable->CreateTypePtr(TypeID::AnyType);
		}

		Stmnt* stmnt = scopeUtils.FindForName(name);
		if (!stmnt)
		{
			if (globalTable->IsPackage(name->val))
			{
				Type* importedType = symbolTable->CreateTypePtr(TypeID::ImportedType);
				importedType->importedType.packageName = expr->identifierExpr.identifier;
				importedType->importedType.typeName = nullptr;
				return importedType;
			}
			else
			{
				AddError(expr->start, "TypeInferer:GetIdentType Unable to get node for name: " + name->val);
				return nullptr;
			}
		}

		switch (stmnt->nodeID)
		{
		case StmntID::Definition:
			return stmnt->definition.type;
		case StmntID::FunctionStmnt:
		case StmntID::ExternFunctionDecl:
			return FunctionToFunctionType(stmnt);
		case StmntID::StateStmnt:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::ImportedType);
			type->importedType.packageName = stmnt->package;
			type->importedType.typeName = stmnt->state.name;
			return type;
		}
		}

		AddError("TypeInferer:GetIdentType unable to find type for name: " + name->val);
		return nullptr;
	}

	Type* GetImportedTypeForSelector(Expr* of, Type* type)
	{
		Token* package = type->importedType.packageName;
		Token* name = of->selectorExpr.select->identifierExpr.identifier;
		Stmnt* stmnt = globalTable->FindStatementForPackage(package, name);
		if (!stmnt)
		{
			AddError(of->start, "TypeInferer:GetImportedTypeForSelector No statement found for expression");
			return type;
		}

		switch (stmnt->nodeID)
		{
		case Definition:
			// Check if type hasn't been inferred yet?
			return stmnt->definition.type;
		case FunctionStmnt:
			return FunctionToFunctionType(stmnt);
		case StateStmnt:
			type->importedType.typeName = stmnt->state.name;
			return type;
		default:
			break;
		}

		AddError(of->start, "TypeInferer:GetImportedTypeForSelector Unable to infer type for selector expression");
		return type;
	}

	inline Type* GetSelectorType(Expr* of, Type* type)
	{
		auto& selector = of->selectorExpr;
		StringView& name = selector.select->identifierExpr.identifier->val;

		if (type->typeID == TypeID::ImportedType && !type->importedType.typeName)
		{
			return GetImportedTypeForSelector(of, type);
		}

		if (type->typeID == TypeID::ExplicitType)
		{
			Stmnt* explicitMember = FindTypeMember(type->explicitType.declarations, name);
			return explicitMember->definition.type;
		}

		Stmnt* state = nullptr;
		if (type->typeID == TypeID::ArrayType)
		{
			state = globalTable->GetArrayState();
		}
		else state = globalTable->FindStateForType(type, symbolTable);
		if (!state)
		{
			AddError(of->start, "TypeInferer:GetSelectorType No state found for type: " + ToString(type));
			return nullptr;
		}

		Stmnt* member = FindStateMember(state, name);
		if (member)
		{
			return member->definition.type;
		}
		else
		{
			StateSymbol* stateSymbol = globalTable->FindScopedStateSymbol(state->state.name, symbolTable);
			Stmnt* method = FindStateMethod(stateSymbol, name);
			if (!method)
			{
				AddError(of->start, "Unable to find member or method for type: " + ToString(type));
				return nullptr;
			}

			return FunctionToFunctionType(method);
		}
	}

	inline Type* GetIndexType(Expr* of, Type* type)
	{
		return GetIndexTypeAccessArray(of, type);
	}

	inline Type* GetIndexTypeAccessArray(Expr* of, Type* type)
	{
		if (!of->indexExpr.index)
		{
			AddError(of->start, "TypeInferer:GetIndexTypeAccessArray No expression for indexing array");
			return type;
		}

		switch (type->typeID)
		{
		case ImportedType:
		case NamedType:
		{
			Type* typeOfIndex = InferType(of->indexExpr.index);
			Type* indexType = GetStateOperatorType(of->start, UniqueType::Array, type, typeOfIndex);
			return indexType;
		}
		case PointerType:
			return type;
		case ValueType:
			return GetIndexTypeAccessArray(of, type->valueType.type);
		case ArrayType:
			return type->arrayType.type;
		case FixedArrayType:
			return type->fixedArrayType.type;
		case AnyType:
			return type;
		}

		AddError(of->start, "TypeInferer:GetIndexTypeAccessArray Not a valid type to access index of: " + ToString(type));
		return nullptr;
	}

	inline Type* GetFunctionCallType(Expr* of, Type* type)
	{
		auto& function = of->functionCallExpr;

		if (!type) return nullptr;
		switch (type->typeID)
		{
		case FunctionType:
			return type->functionType.returnType;
		case TemplatedType:
		{
			if (of->functionCallExpr.function->typeID == ExprID::TemplateExpr) return type;
			break;
		}
		default:
			return type;
		}

		return nullptr;
	}

	Type* EvalType(Expr* expr)
	{
		Type* type = nullptr;
		switch (expr->typeID)
		{
		case PrimitiveExpr:
			type = symbolTable->CreatePrimitive(expr->primitiveExpr.primitive->uniqueType);
			break;
		case TypeExpr:
			type = expr->typeExpr.type;
			break;
		case TemplateExpr:
			type = GetGenericsType(expr, EvalType(expr->templateExpr.expr));
			break;
		case IdentifierExpr:
			type = GetIdentType(expr, expr->identifierExpr.identifier);
			break;
		case SelectorExpr:
			type = GetSelectorType(expr, EvalType(expr->selectorExpr.on));
			break;
		case IndexExpr:
			type = GetIndexType(expr, EvalType(expr->indexExpr.of));
			break;
		case FunctionCallExpr:
			type = GetFunctionCallType(expr, EvalType(expr->functionCallExpr.function));
			break;
		default:
			break;
		}

		if (!type)
		{
			AddError(expr->start, "TypeInferer:EvalType unable to create type for expression: " + ToString(expr));
			return type;
		}

		if (type->typeID == TypeID::ValueType) type = type->valueType.type;
		return type;
	}

	Type* GetStateOperatorType(Token* token, UniqueType op, Type* namedType, Type* rhs = nullptr)
	{
		if (globalTable->IsGenericOfStmnt(namedType, context, symbolTable) ||
			globalTable->IsGenericOfStmnt(rhs, context, symbolTable))
		{
			return symbolTable->CreateTypePtr(TypeID::AnyType);
		}

		Stmnt* node = globalTable->FindStateForType(namedType, symbolTable);
		if (node)
		{
			StateSymbol* state = globalTable->FindScopedStateSymbol(node->state.name, symbolTable);
			if (state)
			{
				for (Stmnt* opNode : state->operators)
				{
					auto& stateOp = opNode->stateOperator;
					if (stateOp.op->uniqueType == op)
					{
						if (!rhs) return stateOp.returnType;
						else if (stateOp.decl->functionDecl.parameters->size() > 0 &&
							*stateOp.decl->functionDecl.parameters->at(1)->definition.type == *rhs)
							return stateOp.returnType;
					}
				}
			}

			AddError(token, "TypeInferer:GetStateOperatorType No operator found for named type: " + ToString(namedType));
		}
		else
		{
			AddError(token, "TypeInferer:GetStateOperatorType State not found for named type: " + ToString(namedType));
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	inline Type* GetPrimitiveOperatorType(Token* op, Type* left, Type* right)
	{
		if (IsBooleanOperator(op)) return &boolType;

		auto& lPrim = left->primitiveType;
		auto& rPrim = right->primitiveType;
		switch (lPrim.type)
		{
		case UniqueType::Void:
			return left;
		case UniqueType::Bool:
			return left;
		case UniqueType::Byte:
		case UniqueType::Int:
		case UniqueType::Int16:
		case UniqueType::Int32:
		case UniqueType::Int64:
		case UniqueType::Int128:
		{
			if (IsFloat(right)) return right;
			else if (IsString(right)) return right;
			else if (!rPrim.isSigned) return left;
			else if (lPrim.size > rPrim.size) return left;
			else return right;
		}
		case UniqueType::Ubyte:
		case UniqueType::Uint:
		case UniqueType::Uint16:
		case UniqueType::Uint32:
		case UniqueType::Uint64:
		case UniqueType::Uint128:
		{
			if (IsFloat(right)) return right;
			else if (IsString(right)) return right;
			else if (rPrim.isSigned) return right;
			else if (lPrim.size > rPrim.size) return left;
			else return right;
		}
		case UniqueType::Float:
		case UniqueType::Float32:
		case UniqueType::Float64:
		{
			if (IsInt(right)) return left;
			else if (IsString(right)) return right;
			else if (lPrim.size > rPrim.size) return left;
			else return right;
		}
		case UniqueType::String:
			return left;
		default:
			break;
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	Type* GetOperatorType(Token* op, Type* left, Type* right)
	{
		if (left->typeID == TypeID::ValueType)
			return GetOperatorType(op, left->valueType.type, right);

		if (right->typeID == TypeID::ValueType)
			return GetOperatorType(op, left, right->valueType.type);

		switch (left->typeID)
		{
		case PrimitiveType:
		{
			if (right->typeID == TypeID::PrimitiveType) return GetPrimitiveOperatorType(op, left, right);
			else if (IsIntLike(right)) return right;
			else AddError(op, "TypeInferer:GetOperatorType Expected right hand side to be a primitive for operator");

			break;
		}
		case ImportedType:
		case NamedType:
			return GetStateOperatorType(op, op->uniqueType, left, right);
		case ExplicitType:
		case ImplicitType:
			AddError(op, "Binary operators are not valid with explicit and implicit types");
			break;
		case PointerType:
		{
			bool booleanOp = IsBooleanOperator(op);
			if (IsInt(right))
			{
				if (booleanOp) return &boolType;
				else return left;
			}
			else if (IsIntLike(right))
			{
				if (booleanOp) return &boolType;
				else return left;
			}

			// AddError pointers are ints, can't operate with anything other than ints
			// To think about, if both operands are pointer should it treat them as the underlying types?
			break;
		}
		break;
		case ValueType:
			return GetOperatorType(op, left->valueType.type, right);
		case ArrayType:
			break;
		case TemplatedType:
			break;
		case FunctionType:
			AddError(op, "You can't multiply functions");
			break;
		default:
			break;
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	Type* GetUnaryType(Token* op, Type* type)
	{
		switch (type->typeID)
		{
		case PrimitiveType:
			if (op->uniqueType == UniqueType::Not) return symbolTable->CreatePrimitive(UniqueType::Bool);
			return type;
		case ImportedType:
		case NamedType:
			return GetStateOperatorType(op, op->uniqueType, type);
		case PointerType:
			return GetUnaryType(op, type->pointerType.type);
		case ValueType:
			return GetUnaryType(op, type->valueType.type);
		case TemplatedType:
			return GetUnaryType(op, type->templatedType.type);
		default:
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		}
	}

	Type* InferType(Expr* of)
	{
		switch (of->typeID)
		{
		case InvalidExpr:
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		case PrimitiveExpr:
		case IdentifierExpr:
		case SelectorExpr:
		case IndexExpr:
		case FunctionCallExpr:
		case TypeExpr:
		case TemplateExpr:
			return EvalType(of);
		case LiteralExpr:
		{
			auto& literal = of->literalExpr;
			UniqueType uniqueType;
			switch (literal.type)
			{
			case IntLiteral:
				uniqueType = UniqueType::Int;
				break;
			case FloatLiteral:
				uniqueType = UniqueType::Float;
				break;
			case HexLiteral:
				uniqueType = UniqueType::Int;
				break;
			case StringLiteral:
				uniqueType = UniqueType::String;
				break;
			case TrueLiteral:
			case FalseLiteral:
				uniqueType = UniqueType::Bool;
				break;
			default:
				uniqueType = UniqueType::Void;
				break;
			}

			return symbolTable->CreatePrimitive(uniqueType);
		}
		case NewExpr:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::PointerType);
			type->pointerType.type = InferType(of->newExpr.primaryExpr);
			return type;
		}
		case FixedExpr:
		{
			Type* fixedArrType = InferType(of->fixedExpr.atExpr);
			if (fixedArrType->typeID != TypeID::FixedArrayType)
			{
				AddError(of->start, "TypeInferer:InferType fixed expressions must evaluate to a fixed sized array types");
				return symbolTable->CreateTypePtr(TypeID::InvalidType);
			}

			if (fixedArrType->fixedArrayType.type->typeID == TypeID::ArrayType ||
				fixedArrType->fixedArrayType.type->typeID == TypeID::FixedArrayType)
			{
				AddError(of->start, "TypeInferer:InferType fixed expression cannot be used to create multidimensional arrays");
				return symbolTable->CreateTypePtr(TypeID::InvalidType);
			}

			Type* fixedType = symbolTable->CreateTypePtr(TypeID::PointerType);
			fixedType->pointerType.type = fixedArrType->fixedArrayType.type;
			return fixedType;
		}
		case TypeLiteralExpr:
		{
			if (!of->typeLiteralExpr.array)
			{
				Type* anonType = symbolTable->CreateTypePtr(TypeID::AnonymousType);
				anonType->anonType.types = symbolTable->CreateVectorPtr<Type>();

				for (Expr* value : *of->typeLiteralExpr.values)
				{
					anonType->anonType.types->push_back(InferType(value));
				}

				return anonType;
			}
			else
			{
				size_t size = of->typeLiteralExpr.values->size();
				if (size > 0)
				{
					Type* arrType = symbolTable->CreateTypePtr(TypeID::FixedArrayType);
					arrType->fixedArrayType.size = of->typeLiteralExpr.values->size();
					arrType->fixedArrayType.type = InferType(of->typeLiteralExpr.values->at(0));
					return arrType;
				}
			}

			break;
		}
		case ExplicitTypeExpr:
		{
			Type* explicitType = symbolTable->CreateTypePtr(TypeID::ExplicitType);
			explicitType->explicitType.declarations = symbolTable->CreateVectorPtr<Stmnt>();
			for (Stmnt* param : *of->explicitTypeExpr.values)
			{
				auto& def = param->definition;
				Stmnt* decl = symbolTable->CreateStmnt(param->start, StmntID::Definition, param->package, param->scope);
				decl->definition.assignment = nullptr;
				decl->definition.name = def.name;
				if (def.type->typeID == TypeID::UnknownType)
				{
					decl->definition.type = InferType(def.assignment);
				}
				else
				{
					decl->definition.type = def.type;
				}

				explicitType->explicitType.declarations->push_back(decl);
			}
			return explicitType;
		}
		case AsExpr:
			return of->asExpr.to;
		case DereferenceExpr:
		{
			Type* type = InferType(of->dereferenceExpr.of);
			if (type->typeID == TypeID::PointerType) return type->pointerType.type;
			return type;
		}
		case ReferenceExpr:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::PointerType);
			type->pointerType.type = InferType(of->referenceExpr.of);
			return type;
		}
		case BinaryExpr:
		{
			Type* left = InferType(of->binaryExpr.left);
			Type* right = InferType(of->binaryExpr.right);
			return GetOperatorType(of->binaryExpr.op, left, right);
		}
		case UnaryExpr:
		{
			Type* type = InferType(of->unaryExpr.expr);
			return GetUnaryType(of->unaryExpr.op, type);
		}
		case GroupedExpr:
			return InferType(of->groupedExpr.expr);
		case FunctionTypeDeclExpr:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::FunctionType);
			auto& anonFunction = of->functionTypeDeclExpr.anonFunction->anonFunction;
			type->functionType.returnType = anonFunction.returnType;
			type->functionType.paramTypes = symbolTable->CreateVectorPtr<Type>();
			for (Stmnt* param : *anonFunction.decl->functionDecl.parameters)
			{
				type->functionType.paramTypes->push_back(param->definition.type);
			}
			return type;
		}
		case CompileExpr:
			return of->compileExpr.compile->compileStmnt.returnType;
		default:
			break;
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}
};
