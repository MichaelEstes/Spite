#pragma once

#include <EASTL/deque.h>

#include "../Syntax/SymbolTable.h"
#include "../Syntax/GlobalTable.h"
#include "TypeInference.h"
#include "CheckerContext.h"

struct TypeChecker
{
	CheckerContext& context;
	TypeInferer inferer;

	TypeChecker(CheckerContext& context) : context(context), inferer(context)
	{}

	bool IsGenericOfCurrentContext(Type* type)
	{
		return context.globalTable->IsGenericOfStmnt(type, context.currentContext, context.symbolTable);
	}

	bool CheckImportedType(Type* type, Expr* templates, bool error = true)
	{
		Stmnt* state = context.globalTable->FindState(type->importedType.packageName,
			type->importedType.typeName);

		if (!state)
		{
			SymbolTable* symbolTable = context.globalTable->FindSymbolTable(type->importedType.packageName->val);
			if (symbolTable)
			{
				Stmnt* enumStmnt = context.globalTable->FindScopedEnum(type->importedType.typeName, symbolTable);
				if (enumStmnt) return true;
			}
			if (error) AddError(type->importedType.typeName, "TypeChecker:CheckImportedType Could not find imported type");
			return false;
		}

		return true;
	}

	bool CheckNamedType(Type* type, Expr* templates, bool error = true)
	{
		Token* name = type->namedType.typeName;
		Stmnt* state = context.globalTable->FindScopedState(name, context.symbolTable);
		if (state)
		{
			type->typeID = TypeID::ImportedType;
			type->importedType.packageName = state->package;
			type->importedType.typeName = name;

			return true;
		}
		else
		{
			Stmnt* enumStmnt = context.globalTable->FindScopedEnum(name, context.symbolTable);
			if (enumStmnt)
			{
				type->typeID = TypeID::ImportedType;
				type->importedType.packageName = enumStmnt->package;
				type->importedType.typeName = name;
			}
			else if (!IsGenericOfCurrentContext(type))
			{
				if (error)
					AddError(type->namedType.typeName, "TypeChecker:CheckNamedType Could not find named type");
				return false;
			}
			
			return true;
		}

		return false;
	}

	void InferUnknownType(Type* type, Expr* assignment)
	{
		Type* inferredType = inferer.InferType(assignment);
		if (!inferredType || inferredType->typeID == TypeID::InvalidType)
		{
			AddError(assignment->start, "TypeChecker:CheckDefinition Unable to infer type of implicit definition for expression: "
				+ ToString(assignment));
			return;
		}
		*type = *inferredType;
	}

	void CheckDefinitionType(Stmnt* node)
	{
		auto& definition = node->definition;
		Type* type = definition.type;
		if (definition.assignment)
		{
			Type* inferredType = inferer.InferType(definition.assignment);
			if (!inferredType || inferredType->typeID == TypeID::InvalidType)
			{
				AddError(definition.assignment->start, "TypeChecker:CheckDefinition Unable to infer type of definition for expression: " + ToString(definition.assignment));
			}
			else if (!inferer.IsAssignable(definition.type, inferredType))
			{
				AddError(node->start, "TypeChecker: Expression evaluates to type:" + ToString(inferredType) + " which doesn't evaluate to type " + ToString(definition.type));
			}
		}
	}

	void CheckAnonType(Stmnt* node, Type* type, Expr* expr)
	{
		if (expr->typeID != ExprID::TypeLiteralExpr)
		{
			AddError(node->start, "Can only assign anonymous type expressions to inline types");
			return;
		}

		auto& anonExpr = expr->typeLiteralExpr;
		if (type->typeID == TypeID::ImplicitType)
		{
			eastl::vector<Token*>* identifiers = type->implicitType.identifiers;
			if (anonExpr.values->size() != identifiers->size())
			{
				AddError(node->start, "Incorrect number of anonymous type expression compared to implicit type values");
				return;
			}

			type->typeID = TypeID::ExplicitType;
			type->explicitType.declarations = context.symbolTable->CreateVectorPtr<Stmnt>();
			for (int i = 0; i < identifiers->size(); i++)
			{
				Expr* itemExpr = anonExpr.values->at(i);
				Token* token = identifiers->at(i);
				Stmnt* decl = context.symbolTable->CreateStmnt(token, StmntID::Definition, node->package, node);
				decl->definition.assignment = nullptr;
				decl->definition.name = token;
				decl->definition.type = inferer.InferType(itemExpr);
				type->explicitType.declarations->push_back(decl);
			}
		}
		else if (type->typeID == TypeID::ExplicitType)
		{
			eastl::vector<Stmnt*>* decls = type->explicitType.declarations;
			if (anonExpr.values->size() != decls->size())
			{
				AddError(node->start, "Incorrect number of anonymous type expression compared to explicit type declarations");
				return;
			}

			for (int i = 0; i < decls->size(); i++)
			{
				Expr* itemExpr = anonExpr.values->at(i);
				Stmnt* decl = decls->at(i);

				Type* inferredType = inferer.InferType(itemExpr);
				if (!inferer.IsAssignable(decl->definition.type, inferredType))
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

	inline void CheckAssignmentStmnt(Stmnt* node)
	{
		auto& assignment = node->assignmentStmnt;
		Type* to = inferer.InferType(assignment.assignTo);
		Type* from = inferer.InferType(assignment.assignment);
		if (!inferer.IsAssignable(to, from))
		{
			AddError(node->start, "Invalid type evaluation for assignment expression, expected type: " + ToString(to) +
				", inferred assignment type: " + ToString(from));
		}
	}

	inline void CheckConditionalType(Stmnt* node)
	{
		auto& conditional = node->conditional;
		Type* inferred = inferer.InferType(conditional.condition);
		if (!IsComparableToZero(inferred))
		{
			AddError(node->start, "Conditional expression doesn't evaluate to a conditional value");
		}
	}

	inline void CheckForType(Stmnt* node)
	{
		auto& forStmnt = node->forStmnt;
		if (!forStmnt.isDeclaration)
		{
			Token* identifier = forStmnt.iterated.identifier;
			Stmnt* decl = context.symbolTable->CreateStmnt(identifier, StmntID::Definition, node->package, node);
			decl->definition.assignment = nullptr;
			decl->definition.name = identifier;
			Type* type = inferer.InferType(forStmnt.toIterate);
			if (forStmnt.rangeFor)
			{
				if (!IsIntLike(type))					
					AddError(forStmnt.toIterate->start, "Range based for loop expressions must evaluate to an integer type");
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

		if (!forStmnt.rangeFor && forStmnt.iterated.declaration->definition.assignment)
		{
			AddError(node->start, "For in loop variable declarations cannot have assignments");
		}
	}

	inline void CheckSwitchType(Stmnt* node)
	{
		auto& switchStmnt = node->switchStmnt;
		Type* switchOnType = inferer.InferType(switchStmnt.switchOn);
		if (!IsInt(switchOnType) && !context.globalTable->FindEnumForType(switchOnType, context.symbolTable))
		{
			AddError(switchStmnt.switchOn->start, "Switch expressions must evaluate to an int type");
		}
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

	inline Type* GetOuterReturnType(Stmnt* node)
	{
		Stmnt* outer = GetOuterScope(node);
		if (outer) return GetReturnType(outer);
		return nullptr;
	}

	inline void CheckReturnType(Stmnt* node)
	{
		Type* returnType = GetOuterReturnType(node);
		if (!returnType)
		{
			AddError(node->start, "TypeChecker:CheckReturnType Unable to get return type node");
			return;
		}

		Type* inferred = inferer.InferType(node->returnStmnt.expr);
		if (!inferer.IsAssignable(inferred, returnType))
		{
			AddError(node->start, "TypeChecker:CheckReturnType Expected return type: " + ToString(returnType) +
				", return expression evaluated to: " + ToString(inferred));
		}
	}
};