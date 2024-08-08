#pragma once

#include <EASTL/deque.h>
#include "../Intermediate/Syntax.h"
#include "../Intermediate/GlobalTable.h"
#include "CheckerContext.h"

static Type boolType = Type(1, UniqueType::Bool, false);

struct CheckerUtils
{
	CheckerContext& context;

	CheckerUtils(CheckerContext& context) : context(context) {}

	bool IsConstantIntExpr(Expr* expr)
	{
		switch (expr->typeID)
		{
		case LiteralExpr:
			return expr->literalExpr.type == UniqueType::IntLiteral || 
				expr->literalExpr.type == UniqueType::HexLiteral;
		case IdentifierExpr:
		{
			Stmnt* def = FindInScope(expr->identifierExpr.identifier->val);
			if (!def || !def->definition.assignment) return false;
			return IsConstantIntExpr(def->definition.assignment);
		}
		case BinaryExpr:
			return IsConstantIntExpr(expr->binaryExpr.left) && IsConstantIntExpr(expr->binaryExpr.right);
		case UnaryExpr:
			return IsConstantIntExpr(expr->unaryExpr.expr);
		case GroupedExpr:
			return IsConstantIntExpr(expr->groupedExpr.expr);
		default:
			break;
		}

		return false;
	}

	size_t EvaluateConstantIntExpr(Expr* expr)
	{
		switch (expr->typeID)
		{
		case LiteralExpr:
		{
			StringView& str = expr->literalExpr.val->val;
			if (expr->literalExpr.type == UniqueType::IntLiteral)
			{
				return IntLiteralStringToInt(str);
			}
			else if (expr->literalExpr.type == UniqueType::HexLiteral)
			{
				return std::stoi(str.ToString().c_str());
			}

			break;
		}
		case IdentifierExpr:
		{
			Stmnt* def = FindInScope(expr->identifierExpr.identifier->val);
			if (def && def->definition.assignment)
			{
				return EvaluateConstantIntExpr(def->definition.assignment);
			}
		}
		case BinaryExpr:
		{
			int left = EvaluateConstantIntExpr(expr->binaryExpr.left);
			int right = EvaluateConstantIntExpr(expr->binaryExpr.right);
			switch (expr->binaryExpr.opType)
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
			int value = EvaluateConstantIntExpr(expr->unaryExpr.expr);
			switch (expr->unaryExpr.opType)
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
			return EvaluateConstantIntExpr(expr->groupedExpr.expr);
		default:
			break;
		}

		return 0;
	}

	Stmnt* GetDeclarationStmntForExpr(Expr* expr, Token* package = nullptr)
	{
		switch (expr->typeID)
		{
		case IdentifierExpr:
		{
			Token* ident = expr->identifierExpr.identifier;
			Stmnt* stmnt = nullptr;
			if (package)
			{
				stmnt = context.globalTable->FindStatementForPackage(package, ident);
			}
			else
			{
				stmnt = FindNodeForName(ident);
			}
			if (!stmnt) return stmnt;

			switch (stmnt->nodeID)
			{
			case Definition:
				return context.globalTable->FindStateForType(stmnt->definition.type, context.symbolTable);
			case FunctionStmnt:
			case StateStmnt:
				return stmnt;
			default:
				return nullptr;
			}
		}
		case SelectorExpr:
		{
			Stmnt* stmnt = GetDeclarationStmntForExpr(expr->selectorExpr.on);
			if (stmnt && stmnt->nodeID == StmntID::StateStmnt)
			{
				return FindStateMemberOrMethodStmnt(stmnt, expr->selectorExpr.select->identifierExpr.identifier);
			}
			else if (IsPackageExpr(expr))
			{
				Token* package = expr->selectorExpr.on->identifierExpr.identifier;
				return GetDeclarationStmntForExpr(expr->selectorExpr.select, package);
			}
			else
			{
				return nullptr;
			}
		}
		case TemplateExpr:
			return GetDeclarationStmntForExpr(expr->templateExpr.expr);
		default:
			break;
		}

		return nullptr;
	}

	inline bool IsPackageExpr(Expr* expr)
	{
		return expr->typeID == ExprID::SelectorExpr && expr->selectorExpr.on->typeID == ExprID::IdentifierExpr &&
			context.globalTable->IsPackage(expr->selectorExpr.on->identifierExpr.identifier->val);
	}

	inline Type* GetOuterReturnType(Stmnt* node)
	{
		Stmnt* outer = GetOuterScope(node);
		if (outer) return GetReturnType(outer);
		return nullptr;
	}

	inline Type* GetReturnType(Stmnt* node)
	{
		switch (node->nodeID)
		{
		case StmntID::FunctionStmnt:
			return node->function.returnType;
		case StmntID::Method:
			return node->method.returnType;
		case StmntID::StateOperator:
			return node->stateOperator.returnType;
		case StmntID::AnonFunction:
			return node->anonFunction.returnType;
		case StmntID::CompileStmnt:
			return node->compileStmnt.returnType;
		default:
			break;
		}

		return nullptr;
	}

	inline bool IsOuterScope(Stmnt* node)
	{
		StmntID nodeID = node->nodeID;
		return nodeID == StmntID::FunctionStmnt || nodeID == StmntID::Method ||
			nodeID == StmntID::StateOperator || nodeID == StmntID::AnonFunction ||
			nodeID == StmntID::CompileStmnt;
	}

	Stmnt* GetOuterScope(Stmnt* node)
	{
		while (node && !IsOuterScope(node)) node = node->scope;
		return node;
	}

	inline Type* GetGenericsType(Expr* expr, Type* of)
	{
		if (of->typeID == TypeID::FunctionType)
		{
			Stmnt* func = GetDeclarationStmntForExpr(expr->templateExpr.expr);
			if (!func)
			{
				AddError(expr->start, "CheckerUtils:GetGenericsType No function found for expression");
				return nullptr;
			}
			Stmnt* generics = GetGenerics(func);
			if (!generics)
			{
				AddError(expr->start, "CheckerUtils:GetGenericsType Template expression used on non-templated function");
				return nullptr;
			}

			auto& funcType = of->functionType;

			eastl::vector<Expr*>* templateArgs = expr->templateExpr.templateArgs;
			if (templateArgs->size() != generics->generics.names->size())
			{
				AddError(expr->start, "CheckerUtils:GetGenericsType Invalid number of template arguments for generic type");
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

		Type* type = context.symbolTable->CreateTypePtr(TypeID::TemplatedType);
		type->templatedType.templates = context.symbolTable->CreateExpr(expr->start, ExprID::TemplateExpr);
		type->templatedType.templates->templateExpr.expr = nullptr;
		type->templatedType.templates->templateExpr.open = expr->templateExpr.open;
		type->templatedType.templates->templateExpr.close = expr->templateExpr.close;
		type->templatedType.templates->templateExpr.templateArgs = expr->templateExpr.templateArgs;
		type->templatedType.type = of;
		return type;
	}

	inline Type* FunctionToFunctionType(Stmnt* node)
	{
		Type* type = context.symbolTable->CreateTypePtr(TypeID::FunctionType);
		Stmnt* decl = nullptr;

		switch (node->nodeID)
		{
		case FunctionStmnt:
		{
			type->functionType.returnType = node->function.returnType;
			decl = node->function.decl;
			break;
		}
		case Method:
		{
			type->functionType.returnType = node->method.returnType;
			decl = node->method.decl;
			break;
		}
		default:
			break;
		}

		if (!decl)
		{
			AddError(node->start, "CheckerUtils:FunctionToFunctionType Unable to find function declaration for statment: " + ToString(node));
			return nullptr;
		}
		return FillFunctionTypeParams(decl, type);
	}

	inline Type* FillFunctionTypeParams(Stmnt* decl, Type* type)
	{
		type->functionType.paramTypes = context.symbolTable->CreateVectorPtr<Type>();
		for (Stmnt* node : *decl->functionDecl.parameters)
		{
			type->functionType.paramTypes->push_back(node->definition.type);
		}

		return type;
	}

	inline Stmnt* FindInScope(StringView& val)
	{
		for (auto it = context.scopeQueue.rbegin(); it != context.scopeQueue.rend(); it++)
		{
			if (auto entry = it->find(val); entry != it->end())
			{
				return entry->second;
			}
		}

		return nullptr;
	}

	inline Stmnt* FindStateMemberOrMethodStmnt(Stmnt* state, Token* name)
	{
		Stmnt* stmnt = FindStateMember(state, name->val);
		if (stmnt)
		{
			return context.globalTable->FindStateForType(stmnt->definition.type, context.symbolTable);
		}

		StateSymbol* stateSymbol = context.globalTable->FindScopedStateSymbol(state->state.name, context.symbolTable);
		stmnt = FindStateMethod(stateSymbol, name->val);
		return stmnt;
	}

	inline Stmnt* FindStateMethod(StateSymbol* of, StringView& val)
	{
		auto& methods = of->methods;
		for (Stmnt* node : methods)
		{
			if (node->method.name->val == val) return node;
		}

		return nullptr;
	}

	inline Stmnt* FindTypeMember(eastl::vector<Stmnt*>* members, StringView& val)
	{
		for (Stmnt* node : *members)
		{
			if (node->definition.name->val == val) return node;
		}

		return nullptr;
	}

	inline Stmnt* FindStateMember(Stmnt* of, StringView& val)
	{
		return FindTypeMember(of->state.members, val);
	}

	inline Stmnt* FindNodeForName(Token* name)
	{
		Stmnt* node = FindInScope(name->val);
		if (node) return node;

		return context.globalTable->FindScopedValue(name, context.symbolTable);
	}

	inline Type* GetIdentType(Expr* expr, Token* name)
	{
		if (IsGenericOfCurrentContext(expr))
		{
			return context.symbolTable->CreateTypePtr(TypeID::GenericNamedType);
		}

		Stmnt* node = FindNodeForName(name);
		if (!node)
		{
			if (context.globalTable->IsPackage(name->val))
			{
				Type* importedType = context.symbolTable->CreateTypePtr(TypeID::ImportedType);
				importedType->importedType.packageName = expr->identifierExpr.identifier;
				importedType->importedType.typeName = nullptr;
				return importedType;
			}
			else
			{
				AddError(expr->start, "CheckerUtils:GetIdentType Unable to get node for name: " + name->val);
				return nullptr;
			}
		}

		switch (node->nodeID)
		{
		case StmntID::Definition:
			return node->definition.type;
		case StmntID::FunctionStmnt:
			return FunctionToFunctionType(node);
		case StmntID::StateStmnt:
		{
			Type* type = context.symbolTable->CreateTypePtr(TypeID::ImportedType);
			type->importedType.packageName = node->package;
			type->importedType.typeName = node->state.name;
			return type;
		}
		}

		AddError("CheckerUtils:GetIdentType unable to find type for name: " + name->val);
		return nullptr;
	}

	Type* GetImportedTypeForSelector(Expr* of, Type* type)
	{
		Token* package = type->importedType.packageName;
		Token* name = of->selectorExpr.select->identifierExpr.identifier;
		Stmnt* stmnt = context.globalTable->FindStatementForPackage(package, name);
		if (!stmnt)
		{
			AddError(of->start, "CheckerUtils:GetImportedTypeForSelector No statement found for expression");
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

		AddError(of->start, "CheckerUtils:GetImportedTypeForSelector Unable to infer type for selector expression");
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

		Stmnt* state = context.globalTable->FindStateForType(type, context.symbolTable);
		if (!state)
		{
			AddError(of->start, "CheckerUtils:GetSelectorType No state found for type: " + ToString(type));
			return nullptr;
		}

		Stmnt* member = FindStateMember(state, name);
		if (member)
		{
			return member->definition.type;
		}
		else
		{
			StateSymbol* stateSymbol = context.globalTable->FindScopedStateSymbol(state->state.name, context.symbolTable);
			Stmnt* method = FindStateMethod(stateSymbol, name);
			if (!method)
			{
				AddError(of->start, "Unable to find member or method for type: " + ToString(type));
				return nullptr;
			}

			return FunctionToFunctionType(method);
		}
	}

	inline bool IsNameOfPrimitive(StringView& name)
	{
		// Early out if string is longer than any primitive names
		if (name.count > 8) return false;

		TokenTree<eastl::string, TokenType, UniqueType>::TokenNode* node = tokenTypeLookup.Find(name);
		if (!node) return false;
		else return node->type == TokenType::Primitive;
	}

	inline bool IsNameOfType(Token* name)
	{
		StateSymbol* state = context.globalTable->FindScopedStateSymbol(name, context.symbolTable);
		if (state) return true;
		else return IsNameOfPrimitive(name->val);
	}

	inline bool IsImportedType(Expr* expr)
	{
		auto& selector = expr->selectorExpr;
		Expr* on = selector.on;
		Expr* select = selector.select;
		return on->typeID == ExprID::IdentifierExpr && select->typeID == ExprID::IdentifierExpr &&
			context.globalTable->FindScopedStateSymbol(select->identifierExpr.identifier, context.symbolTable);
	}

	bool IsGeneric(Token* ident, Stmnt* stmnt)
	{
		Stmnt* generics = GetGenerics(stmnt);
		if (!generics) return false;

		for (Token* gen : *generics->generics.names)
		{
			if (ident->val == gen->val) return true;
		}

		return false;
	}

	bool IsGenericOf(Stmnt* stmnt, Type* type)
	{
		if (!stmnt) return IsGenericOfCurrentContext(type);
		if (!type || type->typeID != TypeID::NamedType) return false;

		return IsGeneric(type->namedType.typeName, stmnt);
	}

	bool IsGenericOfCurrentContext(Expr* expr)
	{
		if (expr->typeID == ExprID::TypeExpr) return IsGenericOfCurrentContext(expr->typeExpr.type);
		if (!context.currentContext || expr->typeID != ExprID::IdentifierExpr) return false;

		Token* ident = expr->identifierExpr.identifier;
		if (context.currentContext && IsGeneric(ident, context.currentContext)) return true;
		else return context.currentStateContext && IsGeneric(ident, context.currentStateContext);
	}

	bool IsGenericOfCurrentContext(Type* type)
	{
		if (!type || !context.currentContext || type->typeID != TypeID::NamedType) return false;

		Token* ident = type->namedType.typeName;
		if (context.currentContext && IsGeneric(ident, context.currentContext)) return true;
		else return context.currentStateContext && IsGeneric(ident, context.currentStateContext);
	}

	bool IsExprOfType(Expr* expr)
	{
		switch (expr->typeID)
		{
		case IdentifierExpr:
			return IsNameOfType(expr->identifierExpr.identifier);
		case PrimitiveExpr:
			return true;
		case SelectorExpr:
			return IsImportedType(expr);
		case IndexExpr:
			return IsExprOfType(expr->indexExpr.of);
		case FunctionCallExpr:
			return IsExprOfType(expr->functionCallExpr.function);
		case TemplateExpr:
			return IsExprOfType(expr->templateExpr.expr);
		case TypeExpr:
			return true;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		default:
			break;
		}

		return false;
	}

	inline Type* GetIndexType(Expr* of, Type* type)
	{
		if (of->indexExpr.forward)
		{
			return GetIndexTypeCreateArray(of, type);
		}
		else
		{
			return GetIndexTypeAccessArray(of, type);
		}
	}

	inline Type* GetIndexTypeCreateArray(Expr* of, Type* type)
	{
		// Has index expression
		if (of->indexExpr.index && IsConstantIntExpr(of->indexExpr.index))
		{
			Type* arrType = context.symbolTable->CreateTypePtr(TypeID::FixedArrayType);
			arrType->fixedArrayType.size = EvaluateConstantIntExpr(of->indexExpr.index);
			arrType->fixedArrayType.type = type;
			return arrType;
		}

		Type* arrType = context.symbolTable->CreateTypePtr(TypeID::ArrayType);
		arrType->arrayType.type = type;
		arrType->arrayType.size = of->indexExpr.index;
		return arrType;
	}

	inline Type* GetIndexTypeAccessArray(Expr* of, Type* type)
	{
		if (!of->indexExpr.index)
		{
			AddError(of->start, "CheckerUtils:GetIndexTypeAccessArray No expression for indexing array");
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
		case GenericNamedType:
			return type;
		}

		AddError(of->start, "CheckerUtils:GetIndexTypeAccessArray Not a valid type to access index of: " + ToString(type));
		return nullptr;
	}

	inline Type* GetFunctionCallType(Expr* of, Type* type)
	{
		auto& function = of->functionCallExpr;

		if (!type) return nullptr;
		switch (type->typeID)
		{
		case PrimitiveType:
			return type;
		case ImportedType:
		case NamedType:
			return type;
		case FunctionType:
			return type->functionType.returnType;
		case TemplatedType:
		{
			if (of->functionCallExpr.function->typeID == ExprID::TemplateExpr) return type;
		}
		case GenericNamedType:
			return type;
		default:
			break;
		}

		AddError(of->start, "CheckerUtils:GetFunctionCallType unable to create type for expression: " + ToString(of));
		return nullptr;
	}

	Type* EvalType(Expr* expr)
	{
		Type* type = nullptr;
		switch (expr->typeID)
		{
		case PrimitiveExpr:
			type = context.symbolTable->CreatePrimitive(expr->primitiveExpr.primitive->uniqueType);
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
			AddError(expr->start, "CheckerUtils:EvalType unable to create type for expression: " + ToString(expr));
			return type;
		}

		if (type->typeID == TypeID::ValueType) type = type->valueType.type;
		return type;
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

	inline bool IsComparableToZero(Type* type)
	{
		return (type->typeID == TypeID::PrimitiveType && (IsInt(type) || IsFloat(type))) 
			|| IsIntLike(type);
	}

	inline bool IsString(Type* primitive)
	{
		return primitive->primitiveType.type == UniqueType::String;
	}

	Type* GetStateOperatorType(Token* token, UniqueType op, Type* namedType, Type* rhs = nullptr)
	{
		if (IsGenericOfCurrentContext(namedType) || IsGenericOfCurrentContext(rhs))
		{
			return context.symbolTable->CreateTypePtr(TypeID::GenericNamedType);
		}

		Stmnt* node = context.globalTable->FindStateForType(namedType, context.symbolTable);
		if (node)
		{
			StateSymbol* state = context.globalTable->FindScopedStateSymbol(node->state.name, context.symbolTable);
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

			AddError(token, "CheckerUtils:GetStateOperatorType No operator found for named type: " + ToString(namedType));
		}
		else
		{
			AddError(token, "CheckerUtils:GetStateOperatorType State not found for named type: " + ToString(namedType));
		}

		return context.symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	inline bool IsBooleanOperator(Token* op)
	{
		switch (op->uniqueType) {
		case UniqueType::LogicOr:
		case UniqueType::LogicAnd:
		case UniqueType::Equal:
		case UniqueType::NotEql:
		case UniqueType::Less:
		case UniqueType::Greater:
		case UniqueType::LessEqual:
		case UniqueType::GreaterEqual:
			return true;
		default:
			return false;
		}
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

		return context.symbolTable->CreateTypePtr(TypeID::InvalidType);
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
			else AddError(op, "CheckerUtils:GetOperatorType Expected right hand side to be a primitive for operator");

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

		return context.symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	Type* GetUnaryType(Token* op, Type* type)
	{
		switch (type->typeID)
		{
		case PrimitiveType:
			if (op->uniqueType == UniqueType::Not) return context.symbolTable->CreatePrimitive(UniqueType::Bool);
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
			return context.symbolTable->CreateTypePtr(TypeID::InvalidType);
		}
	}

	Type* InferType(Expr* of)
	{
		switch (of->typeID)
		{
		case InvalidExpr:
			return context.symbolTable->CreateTypePtr(TypeID::InvalidType);
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

			return context.symbolTable->CreatePrimitive(uniqueType);
		}
		case NewExpr:
		{
			Type* type = context.symbolTable->CreateTypePtr(TypeID::PointerType);
			type->pointerType.type = InferType(of->newExpr.primaryExpr);
			return type;
		}
		case FixedExpr:
		{
			Type* fixedArrType = InferType(of->fixedExpr.atExpr);
			if (fixedArrType->typeID != TypeID::FixedArrayType)
			{
				AddError(of->start, "CheckerUtils:InferType fixed expressions must evaluate to a fixed sized array types");
				return context.symbolTable->CreateTypePtr(TypeID::InvalidType);
			}

			if (fixedArrType->fixedArrayType.type->typeID == TypeID::ArrayType || 
				fixedArrType->fixedArrayType.type->typeID == TypeID::FixedArrayType)
			{
				AddError(of->start, "CheckerUtils:InferType fixed expression cannot be used to create multidimensional arrays");
				return context.symbolTable->CreateTypePtr(TypeID::InvalidType);
			}

			Type* fixedType = fixedArrType->fixedArrayType.type;
			fixedArrType->typeID = TypeID::PointerType;
			fixedArrType->pointerType.type = fixedType;
			return fixedArrType;
		}
		case TypeLiteralExpr:
		{
			if (!of->typeLiteralExpr.array)
			{
				Type* anonType = context.symbolTable->CreateTypePtr(TypeID::AnonymousType);
				anonType->anonType.types = context.symbolTable->CreateVectorPtr<Type>();

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
					Type* arrType = context.symbolTable->CreateTypePtr(TypeID::FixedArrayType);
					arrType->fixedArrayType.size = of->typeLiteralExpr.values->size();
					arrType->fixedArrayType.type = InferType(of->typeLiteralExpr.values->at(0));
					return arrType;
				}
			}

			break;
		}
		case ExplicitTypeExpr:
		{
			Type* explicitType = context.symbolTable->CreateTypePtr(TypeID::ExplicitType);
			explicitType->explicitType.declarations = context.symbolTable->CreateVectorPtr<Stmnt>();
			for (Stmnt* param : *of->explicitTypeExpr.values)
			{
				auto& def = param->definition;
				Stmnt* decl = context.symbolTable->CreateStmnt(param->start, StmntID::Definition, param->package, param->scope);
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
			Type* type = context.symbolTable->CreateTypePtr(TypeID::PointerType);
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
			Type* type = context.symbolTable->CreateTypePtr(TypeID::FunctionType);
			auto& anonFunction = of->functionTypeDeclExpr.anonFunction->anonFunction;
			type->functionType.returnType = anonFunction.returnType;
			type->functionType.paramTypes = context.symbolTable->CreateVectorPtr<Type>();
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

		return context.symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	bool IsAssignable(Type* left, Type* right, Stmnt* context = nullptr)
	{
		if (*left == *right) return true;

		if (left->typeID == TypeID::ValueType)
			return IsAssignable(left->valueType.type, right);

		if (right->typeID == TypeID::ValueType)
			return IsAssignable(left, right->valueType.type);

		if (left->typeID == TypeID::PrimitiveType && right->typeID == TypeID::PrimitiveType)
		{
			// Maybe strings shouldn't be primitives, they are not assignable to other primitives
			bool isStringL = IsString(left);
			bool isStringR = IsString(right);
			if (isStringL || isStringR)
			{
				return isStringL && isStringR;
			}

			return true;
		}

		if (left->typeID == TypeID::PointerType && right->typeID == TypeID::PointerType)
		{
			return IsAssignable(left->pointerType.type, right->pointerType.type);
		}

		if (left->typeID == TypeID::ArrayType && right->typeID == TypeID::ArrayType)
		{
			return IsAssignable(left->arrayType.type, right->arrayType.type);
		}

		if (left->typeID == TypeID::FixedArrayType && right->typeID == TypeID::ArrayType)
		{
			return IsAssignable(left->fixedArrayType.type, right->arrayType.type);
		}

		if (left->typeID == TypeID::ArrayType && right->typeID == TypeID::FixedArrayType)
		{
			return IsAssignable(left->arrayType.type, right->fixedArrayType.type);
		}

		if (IsGenericOf(context, left) || IsGenericOf(context, right)) return true;

		if (IsComplexType(left) && IsComplexType(right))
		{
			eastl::vector<Type*> leftTypes = UnwrapComplexType(left);
			eastl::vector<Type*> rightTypes = UnwrapComplexType(right);

			// Can't assign to a partial type, but as long as the types are ordered the same
			// assigning a type with more members is fine
			if (rightTypes.size() < leftTypes.size()) return false;

			for (size_t i = 0; i < leftTypes.size(); i++)
			{
				Type* lType = leftTypes.at(i);
				Type* rType = rightTypes.at(i);
				if (!IsAssignable(lType, rType)) return false;
			}

			return true;
		}

		if (left->typeID == TypeID::GenericNamedType || right->typeID == TypeID::GenericNamedType)
		{
			return true;
		}

		return false;
	}

	inline bool IsComplexType(Type* type)
	{
		TypeID id = type->typeID;
		return id == TypeID::NamedType || id == TypeID::ImportedType || id == TypeID::ExplicitType || 
			id == TypeID::AnonymousType;
	}

	eastl::vector<Type*> UnwrapComplexType(Type* type)
	{
		if (type->typeID == TypeID::AnonymousType) return *type->anonType.types;
		eastl::vector<Type*> types = eastl::vector<Type*>();

		eastl::vector<Stmnt*>* decls = nullptr;
		if (type->typeID == TypeID::NamedType || type->typeID == TypeID::ImportedType)
		{
			Stmnt* state = context.globalTable->FindStateForType(type, context.symbolTable);
			if (!state)
			{
				AddError("Unable to find state for: " + ToString(type));
				return types;
			}
			decls = state->state.members;
		}
		else
		{
			decls = type->explicitType.declarations;
		}

		for (Stmnt* decl : *decls) types.push_back(decl->definition.type);
		return types;
	}
};