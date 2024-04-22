#pragma once

#include "../Intermediate/SymbolTable.h"
#include <EASTL/deque.h>

struct ExprChecker
{
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<InplaceString, Node*, InplaceStringHash>>& scopeQueue;

	ExprChecker(SymbolTable* symbolTable,
		eastl::deque<eastl::hash_map<InplaceString, Node*, InplaceStringHash>>& scopeQueue)
		: symbolTable(symbolTable), scopeQueue(scopeQueue) {}

	void CheckExpr(Expr* expr, Node* node, Expr* prev = nullptr)
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
		case FunctionTypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		default:
			break;
		}
	}

	void CheckAssignmentStmnt(Node* node)
	{
		CheckExpr(node->assignmentStmnt.assignTo, node);
		CheckExpr(node->assignmentStmnt.assignment, node);
	}

	void CheckGenerics(Expr* expr, Node* node, Expr* prev)
	{
		auto& generics = expr->genericsExpr;
	}

	void CheckNew(Expr* expr, Node* node, Expr* prev)
	{

	}

	void CheckFixed(Expr* expr, Node* node, Expr* prev)
	{

	}
};