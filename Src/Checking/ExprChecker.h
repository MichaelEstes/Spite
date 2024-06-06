#pragma once

#include <EASTL/deque.h>
#include "../Intermediate/SymbolTable.h"
#include "../Intermediate/Syntax.h"
#include "CheckerUtils.h"
#include "DeferredChecker.h"

struct ExprChecker
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	DeferredContainer& deferred;
	eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>>& scopeQueue;
	Stmnt*& currentContext;
	CheckerUtils utils;

	ExprChecker(GlobalTable* globalTable, SymbolTable* symbolTable,
		eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>>& scopeQueue,
		Stmnt*& currentContext, DeferredContainer& deferred)
		: globalTable(globalTable), symbolTable(symbolTable), scopeQueue(scopeQueue), currentContext(currentContext),
		utils(globalTable, symbolTable, scopeQueue, currentContext), deferred(deferred) {}

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
			CheckExpr(expr->selectorExpr.on, node, expr);
			CheckExpr(expr->selectorExpr.select, node, expr);
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			CheckExpr(expr->functionCallExpr.function, node, expr);
			CheckFunctionCallExpr(expr, node, prev);
			break;
		case NewExpr:
			CheckExpr(expr->newExpr.primaryExpr, node, expr);
			CheckNew(expr, node, prev);
			break;
		case FixedExpr:
			CheckExpr(expr->fixedExpr.atExpr, node, expr);
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
			CheckExpr(binaryExpr.left, node, expr);
			CheckExpr(binaryExpr.right, node, expr);
			break;
		}
		case UnaryExpr:
			CheckExpr(expr->unaryExpr.expr, node, expr);
			break;
		case GroupedExpr:
			CheckExpr(expr->groupedExpr.expr, node, expr);
			break;
		case TemplateExpr:
		{
			CheckExpr(expr->templateExpr.expr, node, expr);
			for (Expr* templArg : *expr->templateExpr.templateArgs) CheckExpr(templArg, node, expr);
			CheckGenerics(expr, node, prev);
			break;
		}
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

	bool TemplateIsForwardedGeneric(eastl::vector<Expr*>* templates)
	{
		for (Expr* expr : *templates)
		{
			if (utils.IsGenericOfCurrentContext(expr)) return true;
		}

		return false;
	}

	void CheckGenerics(Expr* expr, Stmnt* node, Expr* prev)
	{
		auto& templateExpr = expr->templateExpr;
		eastl::vector<Expr*>* templateArgs = templateExpr.templateArgs;
		Expr* ofExpr = templateExpr.expr;

		Stmnt* stmnt = utils.GetDeclarationStmntForExpr(ofExpr);
		if (!stmnt)
		{
			AddError(expr->start, "ExprChecker:CheckGenerics Unable to find statement for generics expression");
			return;
		}

		Stmnt* genericsNode = utils.GetGenerics(stmnt);
		if (!genericsNode)
		{
			AddError(expr->start, "ExprChecker:CheckGenerics Generic expression used on a type that doesn't define generics");
			return;
		}

		if (templateArgs->size() != genericsNode->generics.names->size())
		{
			AddError(expr->start, "ExprChecker:CheckGenerics Expected " +
				eastl::to_string(genericsNode->generics.names->size()) +
				" template arguments, got " + eastl::to_string(templateArgs->size()));
			return;
		}

		if (TemplateIsForwardedGeneric(templateArgs))
		{
			DeferredTemplateInstantiation toDefer = DeferredTemplateInstantiation();
			toDefer.forwardTo = genericsNode;
			toDefer.templatesToForward = templateArgs;
			deferred.deferredTemplates[utils.GetGenerics(currentContext)].push_back(toDefer);
			return;
		}

		auto& generics = genericsNode->generics;
		generics.templatesToExpand->insert(templateArgs);
	}

	void CheckNew(Expr* expr, Stmnt* node, Expr* prev)
	{
		//CheckExpr(expr->newExpr.atExpr, node, expr);
	}

	void CheckFixed(Expr* expr, Stmnt* node, Expr* prev)
	{

	}

	void CheckFunctionCallExpr(Expr* expr, Stmnt* node, Expr* prev)
	{
		auto& functionCall = expr->functionCallExpr;
		Expr* function = functionCall.function;
		eastl::vector<Expr*>* params = functionCall.params;
		size_t paramCount = params->size();
		Stmnt* functionStmnt = utils.GetDeclarationStmntForExpr(function);

		if (functionStmnt)
		{
			switch (functionStmnt->nodeID)
			{
				// Constructor being called
			case StmntID::StateStmnt:
			{
				// Every state has a default constructor
				if (paramCount == 0) return;

				StateSymbol* stateSymbol = globalTable->FindStateSymbolForState(functionStmnt);

				// Treat state 'this' param as an any variable becuase of generic states
				// Will be checked lowering
				Type anyThis = Type(TypeID::GenericNamedType);
				Expr thisIdent = Expr(ExprID::TypeExpr, stateSymbol->state->state.name);
				thisIdent.typeExpr.type = &anyThis;
				
				eastl::vector<Expr*> conParams = eastl::vector<Expr*>();
				conParams.push_back(&thisIdent);
				for (Expr* param : *params) conParams.push_back(param);

				for (Stmnt* con : stateSymbol->constructors)
				{
					Stmnt* conDecl = con->constructor.decl;
					if (CheckValidFunctionCallParams(con, conDecl, &conParams)) return;
				}

				AddError(expr->start, "ExprChecker:CheckFunctionCallExpr No constructor found with matching paramters");
				return;
			}
			case StmntID::FunctionStmnt:
			{
				if (!CheckValidFunctionCallParams(functionStmnt, functionStmnt->function.decl, params))
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Invalid parameters passed for call signature for function");
				}
				return;
			}
			case StmntID::Method:
			{
				// Add call expression as first parameter
				eastl::vector<Expr*> methodParams = eastl::vector<Expr*>();
				methodParams.push_back(GetStateParamForMethodCall(function));
				for (Expr* param : *params) methodParams.push_back(param);

				if (!CheckValidFunctionCallParams(functionStmnt, functionStmnt->method.decl, &methodParams))
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Invalid parameters passed for call signature for method");
				}
				return;
			}
			default:
				AddError(expr->start, "ExprChecker:CheckFunctionCallExpr No callable statement found");
				return;
			}
		}
		else
		{
			Type* functionType = utils.InferType(function);
			switch (functionType->typeID)
			{
				// Primitive constrtuctor
			case TypeID::PrimitiveType:
			{
				// Default primitive constructor
				if (paramCount == 0) return;
				else if (paramCount == 1)
				{
					if (!utils.IsAssignable(functionType, utils.InferType(params->at(0))))
					{
						AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Non primitive parameter passed into primitive constructor");
						return;
					}
				}
			}
			break;
			case TypeID::FunctionType:
			{
				auto& func = functionType->functionType;
				if (paramCount != func.paramTypes->size())
				{
					AddError(expr->start,
						"ExprChecker:CheckFunctionCallExpr Expected " +
						eastl::to_string(func.paramTypes->size()) +
						" parameters, " + eastl::to_string(paramCount) +
						" parameters found");
					return;
				}

				for (size_t i = 0; i < paramCount; i++)
				{
					if (!utils.IsAssignable(func.paramTypes->at(i), utils.InferType(params->at(i))))
					{
						AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Invalid parameter passed into function type");
						return;
					}
				}
				break;
			}
			default:
				AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Not a callable expression");
				break;
			}
		}
	}

	Expr* GetStateParamForMethodCall(Expr* expr)
	{
		Expr* param = expr;
		if (param->typeID == ExprID::TemplateExpr)
		{
			param = param->templateExpr.expr;
		}

		return param->selectorExpr.on;
	}

	bool CheckValidFunctionCallParams(Stmnt* calledFor, Stmnt* funcDecl, eastl::vector<Expr*>* params)
	{
		eastl::vector<Stmnt*>* funcParams = funcDecl->functionDecl.parameters;

		size_t paramCount = params->size();
		size_t requiredParamCount = RequiredFunctionParamCount(funcDecl);
		if (requiredParamCount > paramCount) return false;

		for (size_t i = 0; i < paramCount; i++)
		{
			Expr* exprParam = params->at(i);
			Stmnt* funcParam = funcParams->at(i);
			Type* defType = funcParam->definition.type;
			if (!utils.IsAssignable(defType, utils.InferType(exprParam), calledFor))
				return false;
		}

		return true;
	}

	size_t RequiredFunctionParamCount(Stmnt* funcDecl)
	{
		size_t count = 0;
		eastl::vector<Stmnt*>* params = funcDecl->functionDecl.parameters;

		for (Stmnt* param : *params)
		{
			if (param->definition.assignment) return count;
			count++;
		}

		return count;
	}

	void Finalize()
	{

	}
};