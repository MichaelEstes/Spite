#pragma once

#include <EASTL/deque.h>
#include "../Syntax/Syntax.h"
#include "../Syntax/GlobalTable.h"
#include "CheckerContext.h"
#include "../Syntax/TypeInference.h"

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

	bool IsTypeGenericOf(Stmnt* stmnt, Type* type)
	{
		if (!stmnt) return IsGenericOfCurrentContext(type);
		if (!type || type->typeID != TypeID::NamedType) return false;

		return IsGeneric(type->namedType.typeName, stmnt);
	}

	bool IsExprGenericOf(Stmnt* stmnt, Expr* expr)
	{
		if (!stmnt) return IsGenericOfCurrentContext(expr);
		if (!expr || expr->typeID != ExprID::IdentifierExpr) return false;

		return IsGeneric(expr->identifierExpr.identifier, stmnt);
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

	bool IsArrayStateType(Type* type)
	{
		return context.globalTable->FindStateForType(type, context.symbolTable) ==
			context.globalTable->GetArrayState();
	}

	bool IsAssignable(Type* left, Type* right, Stmnt* stmntContext = nullptr)
	{
		if (*left == *right) return true;

		if (left->typeID == TypeID::ValueType)
			return IsAssignable(left->valueType.type, right, stmntContext);

		if (right->typeID == TypeID::ValueType)
			return IsAssignable(left, right->valueType.type, stmntContext);

		if (left->typeID == TypeID::PrimitiveType && right->typeID == TypeID::PrimitiveType)
		{
			// Void can only be assigned to void which would be caught in the type equality check above
			if (left->primitiveType.type == UniqueType::Void ||
				right->primitiveType.type == UniqueType::Void) return false;

			// Maybe strings shouldn't be primitives, they are not assignable to other primitives
			bool isStringL = IsString(left);
			bool isStringR = IsString(right);
			/*if (isStringL || isStringR)
			{
				return isStringL && isStringR;
			}*/

			return isStringL == isStringR;
		}

		if (left->typeID == TypeID::PointerType && right->typeID == TypeID::PointerType)
		{
			return IsAssignable(left->pointerType.type, right->pointerType.type, stmntContext);
		}

		if (left->typeID == TypeID::ArrayType && right->typeID == TypeID::ArrayType)
		{
			return IsAssignable(left->arrayType.type, right->arrayType.type, stmntContext);
		}

		if (left->typeID == TypeID::ArrayType && right->typeID == TypeID::FixedArrayType)
		{
			return IsAssignable(left->arrayType.type, right->fixedArrayType.type, stmntContext);
		}

		if (IsArrayStateType(left))
		{
			return right->typeID == TypeID::ArrayType || right->typeID == TypeID::FixedArrayType;
		}

		if (left->typeID == TypeID::TemplatedType && right->typeID == TypeID::TemplatedType)
		{
			if (!IsAssignable(left->templatedType.type, right->templatedType.type, stmntContext))
				return false;

			eastl::vector<Expr*>* leftTemplateArgs = left->templatedType.templates->templateExpr.templateArgs;
			eastl::vector<Expr*>* rightTemplateArgs = right->templatedType.templates->templateExpr.templateArgs;

			if (leftTemplateArgs->size() != rightTemplateArgs->size()) return false;

			Stmnt* state = context.globalTable->FindStateForType(left, context.symbolTable);

			for (size_t i = 0; i < leftTemplateArgs->size(); i++)
			{
				Expr* lTempl = leftTemplateArgs->at(i);
				Expr* rTempl = rightTemplateArgs->at(i);
				if (IsExprGenericOf(state, lTempl) || IsExprGenericOf(state, rTempl) ||
					IsExprGenericOf(stmntContext, lTempl) || IsExprGenericOf(stmntContext, rTempl))
					continue;

				if (!IsAssignable(InferType(lTempl), InferType(rTempl))) return false;
			}

			return true;
		}

		if (IsTypeGenericOf(stmntContext, left) || IsTypeGenericOf(stmntContext, right)) return true;

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
				if (!IsAssignable(lType, rType, stmntContext)) return false;
			}

			return true;
		}

		if (left->typeID == TypeID::AnyType || right->typeID == TypeID::AnyType) return true;

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