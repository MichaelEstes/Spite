#pragma once
#include "../Syntax/GlobalTable.h"
#include "TypeChecker.h"
#include "ExprChecker.h"
#include "CheckerContext.h"

struct ASTNodeChecker
{
	CheckerContext& context;

	TypeChecker typeChecker;
	ExprChecker exprChecker;

	ASTNodeChecker(CheckerContext& context, DeferredContainer& deferred)
		: context(context), typeChecker(context), exprChecker(context, deferred) {}

	inline void CheckFunctionDecl(Stmnt* functionDecl, Stmnt* of)
	{
		auto& params = functionDecl->functionDecl.parameters;
		auto& body = functionDecl->functionDecl.body;
		CheckFuncBody(body, params);
	}

	void AddScope()
	{
		context.scopeUtils.AddScope();
	}

	void PopScope()
	{
		context.scopeUtils.PopScope();
		if (context.scopeUtils.scopeQueue.size() == 0)
		{
			Logger::FatalError("PackageChecker::PopScope Global scope removed, possible compiler error");
		}
	}

	inline void CheckBody(Body& body)
	{
		AddScope();
		CheckStmnt(body.body);
		PopScope();
	}

	inline void CheckFuncBody(Body& body, eastl::vector<Stmnt*>* params = nullptr)
	{
		AddScope();
		if (params)
		{
			for (Stmnt* node : *params)
			{
				CheckDefinition(node);
			}
		}

		// If single statement inline function make statement a return
		if (body.statement && body.body->nodeID == StmntID::ExpressionStmnt)
		{
			Expr* expr = body.body->expressionStmnt.expression;
			body.body->nodeID = StmntID::ReturnStmnt;
			body.body->returnStmnt.expr = expr;
		}

		CheckStmnt(body.body);
		PopScope();
	}

	void CheckStmnt(Stmnt* node)
	{
		Assert(node);
		switch (node->nodeID)
		{
		case ExpressionStmnt:
			CheckExpr(node->expressionStmnt.expression);
			break;
		case Definition:
			CheckDefinition(node);
			break;
		case InlineDefinition:
			CheckInlineDefinition(node);
			break;
		case AnonFunction:
			CheckType(node->anonFunction.returnType, node->start);
			CheckFunctionDecl(node->anonFunction.decl, node);
			break;
		case Conditional:
		{
			auto& conditional = node->conditional;
			CheckExpr(conditional.condition);
			typeChecker.CheckConditionalType(node);
			CheckBody(conditional.body);
			break;
		}
		case AssignmentStmnt:
		{
			CheckExpr(node->assignmentStmnt.assignTo);
			CheckExpr(node->assignmentStmnt.assignment);
			typeChecker.CheckAssignmentStmnt(node);
			break;
		}
		case IfStmnt:
		{
			auto& ifStmnt = node->ifStmnt;
			CheckStmnt(ifStmnt.condition);
			for (Stmnt* elif : *ifStmnt.elifs)
			{
				CheckStmnt(elif);
			}

			if (ifStmnt.elseCondition) CheckBody(ifStmnt.elseCondition);
			break;
		}
		case ForStmnt:
		{
			auto& forStmnt = node->forStmnt;
			CheckExpr(forStmnt.toIterate);
			typeChecker.CheckForType(node);
			// For loop index variable names can be redeclared
			CheckDefinition(forStmnt.iterated.declaration, false);
			context.scopeUtils.AddToTopScope(forStmnt.iterated.declaration->definition.name->val,
				forStmnt.iterated.declaration);
			CheckBody(forStmnt.body);
			break;
		}
		case WhileStmnt:
		{
			CheckStmnt(node->whileStmnt.conditional);
			break;
		}
		case SwitchStmnt:
		{

			auto& switchStmnt = node->switchStmnt;
			CheckExpr(switchStmnt.switchOn);
			typeChecker.CheckSwitchType(node);

			for (Stmnt* caseStmnt : *switchStmnt.cases)
			{
				CheckStmnt(caseStmnt);
			}

			if (switchStmnt.defaultCase) CheckBody(switchStmnt.defaultCase);
			break;
		}
		case DeleteStmnt:
		{
			CheckExpr(node->deleteStmnt.primaryExpr);
			exprChecker.CheckDelete(node->deleteStmnt.primaryExpr);
			break;
		}
		case DeferStmnt:
		{
			auto& deferStmnt = node->deferStmnt;
			if (deferStmnt.deferIf) CheckStmnt(deferStmnt.conditional);
			else CheckBody(deferStmnt.body);
			break;
		}
		case ContinueStmnt:
			break;
		case BreakStmnt:
			break;
		case ReturnStmnt:
		{
			CheckExpr(node->returnStmnt.expr);
			typeChecker.CheckReturnType(node);
			break;
		}
		case Block:
		{
			for (Stmnt* n : *node->block.inner) CheckStmnt(n);
			break;
		}
		case CompileStmnt:
			CheckType(node->compileStmnt.returnType, node->start);
			CheckFuncBody(node->compileStmnt.body);
			break;
		case CompileDebugStmnt:
			CheckFuncBody(node->compileDebugStmnt.body);
			break;
		case LogStmnt:
		{
			for (Expr* expr : *node->logStmnt.exprs)
			{
				CheckExpr(expr);
			}
			break;
		}
		case AssertStmnt:
			CheckExpr(node->assertStmnt.expr);
			break;
		default:
			break;
		}
	}

	void CheckTypeExprIsType(Expr* expr)
	{
		Type* type = expr->typeExpr.type;
		switch (type->typeID)
		{
		case NamedType:
		{
			if (!typeChecker.CheckNamedType(type, nullptr, false))
			{
				*expr = *context.symbolTable->TypeExprToExpr(expr);
			}
			break;
		}
		case ImportedType:
		{
			if (!typeChecker.CheckImportedType(type, nullptr, false))
			{
				*expr = *context.symbolTable->TypeExprToExpr(expr);
			}
			break;
		}
		case TemplatedType:
		{
			Type* templatedType = type->templatedType.type;
			if (templatedType->typeID == NamedType)
			{
				if (!typeChecker.CheckNamedType(templatedType, type->templatedType.templates, false))
				{
					*expr = *context.symbolTable->TypeExprToExpr(expr);
				}
			}
			else if (templatedType->typeID == ImportedType)
			{
				if (!typeChecker.CheckImportedType(templatedType, type->templatedType.templates, false))
				{
					*expr = *context.symbolTable->TypeExprToExpr(expr);
				}
			}
		}
		default:
			break;
		}
	}

	void CheckExpr(Expr* expr)
	{
		Assert(expr);
		switch (expr->typeID)
		{
		case InvalidExpr:
		case LiteralExpr:
		case IdentifierExpr:
		case PrimitiveExpr:
			break;
		case SelectorExpr:
			CheckExpr(expr->selectorExpr.on);
			CheckExpr(expr->selectorExpr.select);
			break;
		case IndexExpr:
			CheckExpr(expr->indexExpr.of);
			if (expr->indexExpr.index) CheckExpr(expr->indexExpr.index);
			break;
		case FunctionCallExpr:
			CheckExpr(expr->functionCallExpr.function);
			for (Expr* param : *expr->functionCallExpr.params)
			{
				CheckExpr(param);
			}
			exprChecker.CheckFunctionCallExpr(expr);
			break;
		case NewExpr:
			CheckExpr(expr->newExpr.primaryExpr);
			if (expr->newExpr.atExpr) CheckExpr(expr->newExpr.atExpr);
			exprChecker.CheckNew(expr);
			break;
		case FixedExpr:
			CheckExpr(expr->fixedExpr.atExpr);
			break;
		case TypeLiteralExpr:
		{
			if (expr->typeLiteralExpr.typed)
			{
				CheckTypeExprIsType(expr->typeLiteralExpr.typed);
				CheckExpr(expr->typeLiteralExpr.typed);
			}
			for (Expr* e : *expr->typeLiteralExpr.values) CheckExpr(e);
			break;
		}
		case ExplicitTypeExpr:
		{
			for (Stmnt* stmnt : *expr->explicitTypeExpr.values) CheckDefinition(stmnt, false);
			break;
		}
		case AsExpr:
			CheckExpr(expr->asExpr.of);
			CheckType(expr->asExpr.to, expr->start);
			break;
		case DereferenceExpr:
			CheckExpr(expr->dereferenceExpr.of);
			break;
		case ReferenceExpr:
			CheckExpr(expr->referenceExpr.of);
			break;
		case BinaryExpr:
		{
			auto& binaryExpr = expr->binaryExpr;
			CheckExpr(binaryExpr.left);
			CheckExpr(binaryExpr.right);
			break;
		}
		case UnaryExpr:
			CheckExpr(expr->unaryExpr.expr);
			break;
		case GroupedExpr:
			CheckExpr(expr->groupedExpr.expr);
			break;
		case TemplateExpr:
		{
			if (expr->templateExpr.expr) CheckExpr(expr->templateExpr.expr);
			for (Expr*& templArg : *expr->templateExpr.templateArgs)
			{
				CheckExpr(templArg);
			}
			exprChecker.CheckGenerics(expr);
			break;
		}
		case TypeExpr:
			CheckTypeExprIsType(expr);
			if (expr->typeID != TypeExpr)
			{
				CheckExpr(expr);
				break;
			}
			CheckType(expr->typeExpr.type, expr->start);
			break;
		case FunctionTypeDeclExpr:
			CheckStmnt(expr->functionTypeDeclExpr.anonFunction);
			break;
		case CompileExpr:
			CheckStmnt(expr->compileExpr.compile);
			break;
		case SizeOfExpr:
			CheckExpr(expr->sizeOfExpr.expr);
			break;
		case AlignOfExpr:
			CheckExpr(expr->alignOfExpr.expr);
			break;
		case OffsetOfExpr:
			CheckExpr(expr->offsetOfExpr.type);
			CheckExpr(expr->offsetOfExpr.expr);
			break;
		case TypeOfExpr:
			CheckExpr(expr->typeOfExpr.expr);
			break;
		default:
			break;
		}
	}

	void CheckType(Type* type, Token* start, Expr* templates = nullptr)
	{
		switch (type->typeID)
		{
		case InvalidType:
		case ImplicitType:
		case UnknownType:
			AddError(start, "Invalid type found");
			break;
		case ImportedType:
			typeChecker.CheckImportedType(type, templates);
			break;
		case NamedType:
		{
			typeChecker.CheckNamedType(type, templates);
			break;
		}
		case ExplicitType:
		{
			for (Stmnt* def : *type->explicitType.declarations)
			{
				CheckDefinition(def, false);
			}
			break;
		}
		case UnionType:
		{
			for (Stmnt* def : *type->unionType.declarations)
			{
				CheckDefinition(def, false);
			}
			break;
		}
		case PointerType:
			CheckType(type->pointerType.type, start, templates);
			break;
		case ValueType:
			CheckType(type->valueType.type, start, templates);
			break;
		case ArrayType:
		{
			CheckType(type->arrayType.type, start, templates);
			break;
		}
		case TemplatedType:
		{
			Expr* templs = type->templatedType.templates;
			CheckType(type->templatedType.type, start, templs);
			for (Expr* templ : *templs->templateExpr.templateArgs)
			{
				CheckExpr(templ);
			}
			exprChecker.AddTemplatesFromTemplatedType(type);
			break;
		}
		case FunctionType:
		{
			CheckType(type->functionType.returnType, start, templates);

			for (Type* param : *type->functionType.paramTypes)
			{
				CheckType(param, start, templates);
			}
			break;
		}
		default:
			break;
		}
	}

	void CheckDefinition(Stmnt* stmnt, bool scopeDefinition = true)
	{
		auto& definition = stmnt->definition;

		if (definition.assignment) CheckExpr(definition.assignment);

		if (stmnt->definition.type->typeID == UnknownType)
			typeChecker.InferUnknownType(stmnt->definition.type, definition.assignment);
		CheckType(stmnt->definition.type, stmnt->start);
		typeChecker.CheckDefinitionType(stmnt);

		if (scopeDefinition)
		{
			StringView& name = definition.name->val;
			if (context.scopeUtils.HasInTopScope(name))
			{
				AddError(stmnt->start, "PackageChecker:CheckDefinition Re-definition of variable name: " + name);
			}
			else context.scopeUtils.AddToTopScope(name, stmnt);
		}
	}

	void CheckInlineDefinition(Stmnt* node)
	{
		auto& inlineDefinition = node->inlineDefinition;
		Type* type = inlineDefinition.type;
		Expr* expr = inlineDefinition.assignment;
		typeChecker.CheckAnonType(node, type, expr);
		CheckExpr(expr);

		if (type->typeID == TypeID::ExplicitType)
		{
			auto& explicitType = type->explicitType;
			for (Stmnt* decl : *explicitType.declarations)
			{
				StringView& name = decl->definition.name->val;
				if (context.scopeUtils.HasInTopScope(name))
				{
					AddError(decl->start, "PackageChecker:CheckInlineDefinition Re-definition of variable name: " + name);
				}
				else context.scopeUtils.AddToTopScope(name, decl);;
				CheckType(decl->definition.type, decl->start);
			}
		}
		else
		{
			AddError(node->start, "Unable to create a valid inline definition");
		}
	}
};