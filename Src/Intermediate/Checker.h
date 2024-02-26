#pragma once
#include "Syntax.h"

struct Checker
{
	Syntax& syntax;

	Checker(Syntax& syntax) : syntax(syntax) {}

	void Check()
	{
		for (Node* node : syntax.nodes)
		{
			CheckNode(node);
		}
	}

	void CheckNode(Node* node)
	{
		switch (node->nodeID)
		{
		case InvalidNode:
		case CommentStmnt:
		case UsingStmnt:
		case PackageStmnt:
			break;

		case ExpressionStmnt:
			CheckExpr(node->expressionStmnt.expression);
			break;
		case Definition:
			CheckDefinition(node);
			break;
		case InlineDefinition:
			break;
		case Function:
			break;
		case FunctionDecl:
			break;
		case StateStmnt:
			break;
		case GenericsDecl:
			break;
		case WhereStmnt:
			break;
		case Method:
			break;
		case StateOperator:
			break;
		case Destructor:
			break;
		case Constructor:
			break;
		case Conditional:
			break;
		case AssignmentStmnt:
			break;
		case IfStmnt:
			break;
		case ForStmnt:
			break;
		case WhileStmnt:
			break;
		case SwitchStmnt:
			break;
		case TernaryStmnt:
			break;
		case DeleteStmnt:
			break;
		case DeferStmnt:
			break;
		case ContinueStmnt:
			break;
		case BreakStmnt:
			break;
		case ReturnStmnt:
			break;
		case CompileStmnt:
			break;
		case CompileDebugStmnt:
			break;
		case Block:
			break;
		default:
			break;
		}
	}

	void CheckExpr(Expr* expr)
	{

	}

	void CheckDefinition(Node* node, bool declarationOnly = false)
	{
		auto& definition = node->definition;
		Type& type = definition.type;

	}

	Type DeduceType(Expr* of)
	{

	}

};