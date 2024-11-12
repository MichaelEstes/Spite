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

	bool CheckValidFunctionCallParams(Stmnt* calledFor, eastl::vector<Stmnt*>* funcParams,
		eastl::vector<Expr*>* params)
	{

		size_t paramCount = params->size();
		size_t requiredParamCount = RequiredFunctionParamCount(funcParams);
		if (requiredParamCount > paramCount) return false;

		for (size_t i = 0; i < paramCount; i++)
		{
			Expr* exprParam = params->at(i);
			Stmnt* funcParam = funcParams->at(i);
			Type* defType = funcParam->definition.type;
			if (!IsAssignable(defType, InferType(exprParam), calledFor))
				return false;
		}

		return true;
	}

	size_t RequiredFunctionParamCount(eastl::vector<Stmnt*>* params)
	{
		size_t count = 0;

		for (Stmnt* param : *params)
		{
			if (param->definition.assignment) return count;
			count += 1;
		}

		return count;
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

	Stmnt* GetDeclarationStmntForExpr(Expr* expr)
	{
		return TypeInferer(context.globalTable, context.symbolTable,
			context.scopeUtils, context.currentContext).GetDeclarationStmntForExpr(expr);
	}

	bool IsAssignable(Type* left, Type* right, Stmnt* stmntContext = nullptr)
	{
		return TypeInferer(context.globalTable, context.symbolTable,
			context.scopeUtils, context.currentContext).IsAssignable(left, right, stmntContext);
	}
};
	