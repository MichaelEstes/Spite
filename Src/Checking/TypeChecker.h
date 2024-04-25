#pragma once

#include <EASTL/deque.h>

#include "CheckerUtils.h"
#include "../Intermediate/SymbolTable.h"
#include "../Intermediate/Syntax.h"

struct TypeChecker
{
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<StringView, Node*, StringViewHash>>& scopeQueue;
	CheckerUtils utils;

	TypeChecker(SymbolTable* symbolTable,
		eastl::deque<eastl::hash_map<StringView, Node*, StringViewHash>>& scopeQueue)
		: symbolTable(symbolTable), scopeQueue(scopeQueue), utils(symbolTable, scopeQueue)  {}

	void CheckDefinitionType(Node* node)
	{
		auto& definition = node->definition;
		Type* type = definition.type;
		if (type->typeID == TypeID::UnknownType)
		{
			Type* inferredType = utils.InferType(definition.assignment, node);
			if (!inferredType)
			{
				AddError(definition.assignment->start, "Checker:CheckDefinition Unable to infer type of implicit definition for expression: " + ToString(definition.assignment));
			}
			definition.type = utils.InferType(definition.assignment, node);
		}
		else if (definition.assignment)
		{
			if (type->typeID == TypeID::ExplicitType)
			{
				CheckAnonType(node, type, definition.assignment);
			}
			else
			{
				Type* inferredType = utils.InferType(definition.assignment, node);
				if (!inferredType)
				{
					AddError(definition.assignment->start, "Checker:CheckDefinition Unable to infer type of definition for expression: " + ToString(definition.assignment));
				}
				else if (*definition.type != *inferredType)
				{
					AddError(node->start, "Expression evaluates to type:" + ToString(inferredType) + " which doesn't evaluate to type " + ToString(definition.type));
					return;
				}
			}
		}
	}

	void CheckAnonType(Node* node, Type* type, Expr* expr)
	{
		if (expr->typeID != ExprID::AnonTypeExpr)
		{
			AddError(node->start, "Can only assign anonymous type expressions to inline types");
			return;
		}

		auto& anonExpr = expr->anonTypeExpr;
		if (type->typeID == TypeID::ImplicitType)
		{
			eastl::vector<Token*>* identifiers = type->implicitType.identifiers;
			if (anonExpr.values->size() != identifiers->size())
			{
				AddError(node->start, "Incorrect number of anonymous type expression compared to implicit type values");
				return;
			}

			type->typeID = TypeID::ExplicitType;
			type->explicitType.declarations = symbolTable->CreateVectorPtr<Node*>();
			for (int i = 0; i < identifiers->size(); i++)
			{
				Expr* itemExpr = anonExpr.values->at(i);
				Token* token = identifiers->at(i);
				Node* decl = symbolTable->CreateNode(token, NodeID::Definition, node);
				decl->definition.assignment = nullptr;
				decl->definition.name = token;
				decl->definition.type = utils.InferType(itemExpr, node);
				type->explicitType.declarations->push_back(decl);
			}
		}
		else if (type->typeID == TypeID::ExplicitType)
		{
			eastl::vector<Node*>* decls = type->explicitType.declarations;
			if (anonExpr.values->size() != decls->size())
			{
				AddError(node->start, "Incorrect number of anonymous type expression compared to explicit type declarations");
				return;
			}

			for (int i = 0; i < decls->size(); i++)
			{
				Expr* itemExpr = anonExpr.values->at(i);
				Node* decl = decls->at(i);

				Type* inferredType = utils.InferType(itemExpr, node);
				if (*decl->definition.type != *inferredType)
				{
					AddError(node->start, "Anonymous expression doesn't evaluate to type " + ToString(decl->definition.type));
					return;
				}
			}
		}
		else
		{
			AddError(node->start, "Inline definition type can only be an implicit or explicit type");
		}
	}

	inline void CheckAssignmentStmnt(Node* node)
	{
		auto& assignment = node->assignmentStmnt;
		Type* to = utils.InferType(assignment.assignTo, node);
		Type* from = utils.InferType(assignment.assignment, node);
		if (*to != *from)
		{
			AddError(node->start, "Invalid type evaluation for assignment expression: " + ToString(to));
		}
	}

	inline void CheckConditionalType(Node* node)
	{
		auto& conditional = node->conditional;
		Type* inferred = utils.InferType(conditional.condition, node);
		if (!utils.IsBoolLike(inferred))
		{
			AddError(node->start, "Conditional expression doesn't evaluate to a conditional value");
		}
	}

	inline void CheckForType(Node* node)
	{
		auto& forStmnt = node->forStmnt;
		if (!forStmnt.isDeclaration)
		{
			Token* identifier = forStmnt.iterated.identifier;
			Node* decl = symbolTable->CreateNode(identifier, NodeID::Definition, node);
			decl->definition.assignment = nullptr;
			decl->definition.name = identifier;
			Type* type = utils.InferType(forStmnt.toIterate, node);
			if (forStmnt.rangeFor)
			{
				if (!utils.IsInt(type))
					AddError(forStmnt.toIterate->start, "Range based for loop expressions must evaluate to an integer");
			}
			else
			{
				if (type->typeID == TypeID::ArrayType)
					type = type->arrayType.type;
			}

			decl->definition.type = type;
			forStmnt.isDeclaration = true;
			forStmnt.iterated.declaration = decl;
		}
	}

	inline void CheckSwitchType(Node* node)
	{
		auto& switchStmnt = node->switchStmnt;
		if (!utils.IsInt(utils.InferType(switchStmnt.switchOn, node)))
		{
			AddError(switchStmnt.switchOn->start, "Switch expressions must evaluate to an int type");
		}
	}

	inline void CheckReturnType(Node* node)
	{
		if (node->returnStmnt.voidReturn) return;

		Type* returnType = utils.GetOuterReturnType(node);
		if (!returnType)
		{
			AddError(node->start, "TypeChecker:CheckReturnType Unable to get return type node");
			return;
		}

		Type* inferred = utils.InferType(node->returnStmnt.expr, node);
		if (*inferred != *returnType)
		{
			AddError(node->start, "TypeChecker:CheckerReturnType Expected return type: " + ToString(returnType) +
				", return expression evaluated to: " + ToString(inferred));
		}
	}
};