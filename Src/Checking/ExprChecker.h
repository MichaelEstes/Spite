#pragma once

#include <EASTL/deque.h>
#include "../Intermediate/SymbolTable.h"
#include "../Intermediate/Syntax.h"
#include "CheckerUtils.h"

struct ExprChecker
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>>& scopeQueue;
	CheckerUtils utils;

	ExprChecker(GlobalTable* globalTable, SymbolTable* symbolTable,
		eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>>& scopeQueue)
		: globalTable(globalTable), symbolTable(symbolTable), scopeQueue(scopeQueue), utils(globalTable, symbolTable, scopeQueue) {}

	void CheckExpr(Expr* expr, Stmnt* node, Expr* prev = nullptr)
	{
		switch (expr->typeID)
		{
		case InvalidExpr:
			break;
		case LiteralExpr:
			break;
		case IdentifierExpr:
			break;
		case PrimitiveExpr:
			break;
		case SelectorExpr:
			CheckExpr(expr->selectorExpr.on, node, prev);
			CheckExpr(expr->selectorExpr.select, node, prev);
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			CheckExpr(expr->functionCallExpr.function, node, expr);
			break;
		case NewExpr:
			CheckNew(expr, node, prev);
			break;
		case FixedExpr:
			CheckFixed(expr, node, prev);
			break;
		case AnonTypeExpr:
		{
			for (Expr* e : *expr->anonTypeExpr.values) CheckExpr(e, node, expr);
			break;
		}
		case AsExpr:
			CheckExpr(expr->asExpr.of, node, expr);
			break;
		case DereferenceExpr:
			CheckExpr(expr->dereferenceExpr.of, node, expr);
			break;
		case ReferenceExpr:
			CheckExpr(expr->referenceExpr.of, node, expr);
			break;
		case BinaryExpr:
		{
			auto& binaryExpr = expr->binaryExpr;
			CheckExpr(binaryExpr.left, node, prev);
			CheckExpr(binaryExpr.right, node, prev);
			break;
		}
		case UnaryExpr:
			CheckExpr(expr->unaryExpr.expr, node, expr);
			break;
		case GroupedExpr:
			CheckExpr(expr->groupedExpr.expr, node, expr);
			break;
		case GenericsExpr:
			CheckGenerics(expr, node, prev);
			break;
		case TypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		default:
			break;
		}
	}

	void CheckAssignmentStmnt(Stmnt* node)
	{
		CheckExpr(node->assignmentStmnt.assignTo, node);
		CheckExpr(node->assignmentStmnt.assignment, node);
	}

	void CheckGenerics(Expr* expr, Stmnt* node, Expr* prev)
	{
		auto& genericsExpr = expr->genericsExpr;
		eastl::vector<Expr*>* templateArgs = genericsExpr.templateArgs;
		Expr* ofExpr = genericsExpr.expr;

		Stmnt* genericsNode = nullptr;

		if (ofExpr->typeID == ExprID::IdentifierExpr)
		{
			Stmnt* node = symbolTable->FindStateOrFunction(ofExpr->identifierExpr.identifier->val);
			if (!node)
			{
				AddError(expr->start, "ExprChecker:CheckGenerics Unable to find statement for generics expression");
				return;
			}
			genericsNode = utils.GetGenerics(node);
		}
		else if (ofExpr->typeID == ExprID::SelectorExpr)
		{
			//TODO imported type and method checking
			auto& selectorExpr = ofExpr->selectorExpr;
			Expr* left = selectorExpr.on;
			Expr* right = selectorExpr.select;
		}

		if (!genericsNode)
		{
			AddError(expr->start, "ExprChecker:CheckGenerics Generic expression used on a type that doesn't define generics");
			return;
		}

		auto& generics = genericsNode->generics;
		if (generics.templatesToExpand->find(templateArgs) == generics.templatesToExpand->end())
		{
			generics.templatesToExpand->insert(templateArgs);
		}
	}

	void CheckNew(Expr* expr, Stmnt* node, Expr* prev)
	{
		CheckExpr(expr->newExpr.primaryExpr, node, expr);
		//CheckExpr(expr->newExpr.atExpr, node, expr);
	}

	void CheckFixed(Expr* expr, Stmnt* node, Expr* prev)
	{

	}
};