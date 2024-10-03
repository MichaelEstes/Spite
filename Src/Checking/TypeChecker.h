#pragma once

#include <EASTL/deque.h>

#include "CheckerUtils.h"
#include "../Intermediate/SymbolTable.h"
#include "../Intermediate/GlobalTable.h"

struct TypeChecker
{
	CheckerContext& context;
	CheckerUtils utils;

	TypeChecker(CheckerContext& context) : context(context), utils(context) {}

	void CheckTypeGenerics(Stmnt* state, Expr* templates, Token* token)
	{
		Stmnt* generics = GetGenerics(state);
		if (generics)
		{
			if (!templates)
			{
				AddError(token, "TypeChecker:CheckTypeGenerics No templates provided for generic type");
				return;
			}

			size_t genericsCount = generics->generics.names->size();
			size_t templatesCount = templates->templateExpr.templateArgs->size();
			if (genericsCount != templatesCount)
			{
				AddError(token, "TypeChecker:CheckTypeGenerics Expected " 
					+ eastl::to_string(genericsCount) 
					+ " templates, but was provided "
					+ eastl::to_string(templatesCount) 
					+ " templates");
			}
		}
		else if (templates)
		{
			AddError(token, "TypeChecker:CheckTypeGenerics Templates provided for non-generic type");
		}
	}

	void CheckImportedType(Type* type, Expr* templates)
	{
		Stmnt* state = context.globalTable->FindState(type->importedType.packageName,
			type->importedType.typeName);

		if (!state)
		{
			AddError(type->importedType.typeName, "TypeChecker:CheckImportedType Could not find imported type");
			return;
		}

		CheckTypeGenerics(state, templates, type->importedType.typeName);
	}

	void CheckNamedType(Type* type, Expr* templates)
	{
		Token* name = type->namedType.typeName;
		Stmnt* state = context.globalTable->FindScopedState(name, context.symbolTable);
		if (state)
		{
			CheckTypeGenerics(state, templates, type->namedType.typeName);

			type->typeID = TypeID::ImportedType;
			type->importedType.packageName = state->package;
			type->importedType.typeName = name;
		}
		else if (!utils.IsGenericOfCurrentContext(type))
		{
			AddError(type->namedType.typeName, "TypeChecker:CheckNamedType Could not find named type");
		}
	}

	void InferUnknownType(Type* type, Expr* assignment)
	{
		Type* inferredType = utils.InferType(assignment);
		if (!inferredType || inferredType->typeID == TypeID::InvalidType)
		{
			AddError(assignment->start, "TypeChecker:CheckDefinition Unable to infer type of implicit definition for expression: "
				+ ToString(assignment));
		}
		*type = *inferredType;
	}

	void CheckArrayType(Type* type)
	{
		auto& arr = type->arrayType;
		if (arr.size)
		{
			if (context.scopeUtils.IsConstantIntExpr(arr.size))
			{
				size_t size = context.scopeUtils.EvaluateConstantIntExpr(arr.size);
				if (size > 0)
				{
					Type* arrType = arr.type;
					type->typeID = TypeID::FixedArrayType;
					type->fixedArrayType.size = size;
					type->fixedArrayType.type = arrType;
				}
				else
				{
					AddError(arr.arr, "TypeChecker:CheckArrayType Array size expression must evaluate to a positive number");
				}
			}
			else
			{
				AddError(arr.arr, "TypeChecker:CheckArrayType Array size expression must be a constant value");
			}
		}
	}

	void CheckDefinitionType(Stmnt* node)
	{
		auto& definition = node->definition;
		Type* type = definition.type;
		if (definition.assignment)
		{
			Type* inferredType = utils.InferType(definition.assignment);
			if (!inferredType || inferredType->typeID == TypeID::InvalidType)
			{
				AddError(definition.assignment->start, "TypeChecker:CheckDefinition Unable to infer type of definition for expression: " + ToString(definition.assignment));
			}
			else if (!utils.IsAssignable(definition.type, inferredType))
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
				decl->definition.type = utils.InferType(itemExpr);
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

				Type* inferredType = utils.InferType(itemExpr);
				if (!utils.IsAssignable(decl->definition.type, inferredType))
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
		Type* to = utils.InferType(assignment.assignTo);
		Type* from = utils.InferType(assignment.assignment);
		if (!utils.IsAssignable(to, from))
		{
			AddError(node->start, "Invalid type evaluation for assignment expression: " + ToString(to));
		}
	}

	inline void CheckConditionalType(Stmnt* node)
	{
		auto& conditional = node->conditional;
		Type* inferred = utils.InferType(conditional.condition);
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
			Type* type = utils.InferType(forStmnt.toIterate);
			if (forStmnt.rangeFor)
			{
				if (!IsInt(type))
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

	inline void CheckSwitchType(Stmnt* node)
	{
		auto& switchStmnt = node->switchStmnt;
		if (!IsInt(utils.InferType(switchStmnt.switchOn)))
		{
			AddError(switchStmnt.switchOn->start, "Switch expressions must evaluate to an int type");
		}
	}

	inline void CheckReturnType(Stmnt* node)
	{
		Type* returnType = utils.GetOuterReturnType(node);
		if (!returnType)
		{
			AddError(node->start, "TypeChecker:CheckReturnType Unable to get return type node");
			return;
		}

		Type* inferred = utils.InferType(node->returnStmnt.expr);
		if (!utils.IsAssignable(inferred, returnType))
		{
			AddError(node->start, "TypeChecker:CheckReturnType Expected return type: " + ToString(returnType) +
				", return expression evaluated to: " + ToString(inferred));
		}
	}
};