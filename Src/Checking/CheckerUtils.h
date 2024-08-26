#pragma once

#include <EASTL/deque.h>
#include "../Intermediate/Syntax.h"
#include "../Intermediate/GlobalTable.h"
#include "CheckerContext.h"
#include "../Intermediate/TypeInference.h"

struct CheckerUtils
{
	CheckerContext& context;

	CheckerUtils(CheckerContext& context) : context(context) {}

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

	bool IsGenericOfCurrentContext(Type* type)
	{
		return context.globalTable->IsGenericOfStmnt(type, context.currentContext, context.symbolTable);
	}

	bool IsGenericOfCurrentContext(Expr* expr)
	{
		return context.globalTable->IsGenericOfStmnt(expr, context.currentContext, context.symbolTable);
	}

	bool IsGenericOf(Stmnt* stmnt, Type* type)
	{
		if (!stmnt) return IsGenericOfCurrentContext(type);
		if (!type || type->typeID != TypeID::NamedType) return false;

		return IsGeneric(type->namedType.typeName, stmnt);
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

	Type* InferType(Expr* of)
	{
		return TypeInferer(context.globalTable, context.symbolTable, 
			context.scopeUtils, context.currentContext).InferType(of);
	}

	bool IsAssignable(Type* left, Type* right, Stmnt* context = nullptr)
	{
		if (*left == *right) return true;

		if (left->typeID == TypeID::ValueType)
			return IsAssignable(left->valueType.type, right, context);

		if (right->typeID == TypeID::ValueType)
			return IsAssignable(left, right->valueType.type, context);

		if (left->typeID == TypeID::PrimitiveType && right->typeID == TypeID::PrimitiveType)
		{
			// Void can only be assigned to void which would be caught in the type equality check above
			if (left->primitiveType.type == UniqueType::Void ||
				right->primitiveType.type == UniqueType::Void) return false;

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
			return IsAssignable(left->pointerType.type, right->pointerType.type, context);
		}

		if (left->typeID == TypeID::ArrayType && right->typeID == TypeID::ArrayType)
		{
			return IsAssignable(left->arrayType.type, right->arrayType.type, context);
		}

		if (left->typeID == TypeID::FixedArrayType && right->typeID == TypeID::ArrayType)
		{
			return IsAssignable(left->fixedArrayType.type, right->arrayType.type, context);
		}

		if (left->typeID == TypeID::ArrayType && right->typeID == TypeID::FixedArrayType)
		{
			return IsAssignable(left->arrayType.type, right->fixedArrayType.type, context);
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
				if (!IsAssignable(lType, rType, context)) return false;
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
				AddError("CheckerUtils:UnwrapComplexType Unable to find state for: " + ToString(type));
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