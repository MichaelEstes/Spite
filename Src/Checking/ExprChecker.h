#pragma once

#include <EASTL/deque.h>
#include "../Syntax/SymbolTable.h"
#include "../Syntax/Syntax.h"
#include "CheckerUtils.h"
#include "DeferredChecker.h"

struct ExprChecker
{
	CheckerContext& context;

	CheckerUtils utils;
	DeferredContainer& deferred;

	ExprChecker(CheckerContext& context, DeferredContainer& deferred) :
		context(context), utils(context), deferred(deferred) {}

	bool TemplatesContainForwardedGeneric(eastl::vector<Expr*>* templates)
	{
		for (Expr* expr : *templates)
		{
			if (utils.IsGenericOfCurrentContext(expr)) return true;
		}

		return false;
	}

	void AddTemplatesToExpand(Stmnt* stmnt, eastl::vector<Expr*>* templateArgs, Token* start,
		Expr* ofExpr = nullptr)
	{
		if (!stmnt)
		{
			if (ofExpr && context.globalTable->IsGenericOfStmnt(ofExpr, context.currentContext, context.symbolTable))
			{
				Token* genericTok = GetTokenForTemplate(ofExpr);
				DeferredTemplateForwarded toDefer = DeferredTemplateForwarded();
				toDefer.genericName = genericTok;
				toDefer.templatesToForward = templateArgs;
				deferred.deferredForwardedTemplates[context.currentContext].push_back(toDefer);
				return;
			}

			AddError(start, "ExprChecker:CheckGenerics Unable to find statement for generics expression");
			return;
		}

		Stmnt* genericsNode = GetGenerics(stmnt);
		if (!genericsNode)
		{
			AddError(start, "ExprChecker:CheckGenerics Generic expression used on a type that doesn't define generics");
			return;
		}

		size_t argSize = templateArgs->size();
		size_t genericsCount = genericsNode->generics.names->size();
		if (argSize < RequiredGenericsCount(genericsNode) ||
			argSize > genericsCount)
		{
			AddError(start, "ExprChecker:CheckGenerics Expected " +
				eastl::to_string(genericsCount) +
				" template arguments, got " + eastl::to_string(templateArgs->size()));
			return;
		}

		if (argSize < genericsCount)
		{
			for (size_t i = argSize; i < genericsCount; i++)
			{
				templateArgs->push_back(genericsNode->generics.defaultValues->at(i));
			}
		}

		if (TemplatesContainForwardedGeneric(templateArgs))
		{
			DeferredTemplateInstantiation toDefer = DeferredTemplateInstantiation();
			toDefer.forwardTo = genericsNode;
			toDefer.templatesToForward = templateArgs;
			Stmnt* currGenerics = GetGenerics(context.currentContext);
			if (!currGenerics && IsStateFunction(context.currentContext))
			{
				Stmnt* stateStmnt = context.globalTable->FindStateForStmnt(context.currentContext, context.symbolTable);
				currGenerics = GetGenerics(stateStmnt);
			}

			if (!currGenerics)
			{
				AddError(start, "ExprChecker:CheckGenerics Unable to find statement with generics");
				return;
			}

			deferred.deferredTemplates[currGenerics].push_back(toDefer);
			return;
		}

		auto& generics = genericsNode->generics;
		generics.templatesToExpand->insert(templateArgs);
	}

	void AddTemplatesFromTemplatedType(Type* type)
	{
		Stmnt* state = context.globalTable->FindStateForType(type, context.symbolTable);
		Expr* templateExpr = type->templatedType.templates;
		eastl::vector<Expr*>* templateArgs = templateExpr->templateExpr.templateArgs;
		AddTemplatesToExpand(state, templateArgs, templateExpr->start);
	}

	void CheckGenerics(Expr* expr)
	{
		auto& templateExpr = expr->templateExpr;
		eastl::vector<Expr*>* templateArgs = templateExpr.templateArgs;
		Expr* ofExpr = templateExpr.expr;

		Stmnt* stmnt = utils.GetDeclarationStmntForExpr(ofExpr);
		AddTemplatesToExpand(stmnt, templateArgs, expr->start, ofExpr);
	}

	void CheckNew(Expr* expr)
	{
		//CheckExpr(expr->newExpr.atExpr, node, expr);
	}

	void CheckDelete(Expr* expr)
	{
		/*if (utils.InferType(expr)->typeID != TypeID::PointerType)
		{
			AddError(expr->start, "ExprChecker::CheckDelete Delete called on non pointer type");
		}*/
	}

	void CheckFunctionCallExpr(Expr* expr)
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
				functionCall.callKind = FunctionCallKind::ConstructorCall;
				StateSymbol* stateSymbol = context.globalTable->FindScopedStateSymbol(functionStmnt->state.name, context.symbolTable);

				// Every state has a default constructor
				if (paramCount == 0)
				{
					for (Stmnt* con : stateSymbol->constructors)
					{
						Stmnt* conDecl = con->constructor.decl;
						if (conDecl->functionDecl.parameters->size() == 1)
						{
							functionCall.functionStmnt = con;
							return;
						}
					}
					functionCall.functionStmnt = stateSymbol->state;
					return;
				}

				eastl::vector<Expr*> conParams = eastl::vector<Expr*>();
				Type thisType = Type(TypeID::NamedType);
				Expr thisIdent = Expr(ExprID::TypeExpr, stateSymbol->state->state.name);
				thisIdent.typeExpr.type = &thisType;

				if (stateSymbol->state->state.generics)
				{
					thisType.typeID = TypeID::AnyType;
				}
				else thisType.namedType.typeName = stateSymbol->state->state.name;

				conParams.push_back(&thisIdent);

				for (Expr* param : *params) conParams.push_back(param);

				for (Stmnt* con : stateSymbol->constructors)
				{
					Stmnt* conDecl = con->constructor.decl;
					if (utils.CheckValidFunctionCallParams(con, conDecl->functionDecl.parameters, &conParams))
					{
						functionCall.functionStmnt = con;
						return;
					}
				}

				AddError(expr->start, "ExprChecker:CheckFunctionCallExpr No constructor found with matching paramters");
				return;
			}
			case StmntID::FunctionStmnt:
			{
				functionCall.callKind = FunctionCallKind::FunctionCall;
				functionCall.functionStmnt = functionStmnt;
				if (!utils.CheckValidFunctionCallParams(functionStmnt, functionStmnt->function.decl->functionDecl.parameters,
					params))
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Invalid parameters passed for call signature for function");
				}
				return;
			}
			case StmntID::Method:
			{
				Stmnt* state = context.globalTable->FindScopedState(functionStmnt->method.stateName, context.symbolTable);
				eastl::vector<Expr*> methodParams = eastl::vector<Expr*>();
				Expr* caller = GetCallerExprMethodCall(function);

				Expr thisIdent = Expr(ExprID::TypeExpr, functionStmnt->method.stateName);
				if (IsUniformCall(caller, functionStmnt))
				{
					functionCall.callKind = FunctionCallKind::UniformMethodCall;
				}
				else
				{
					Type* type = utils.InferType(caller);
					// Methods can be called from the . syntax for pointers since 'this' param is always a reference
					if (type->typeID == TypeID::PointerType) type = type->pointerType.type;
					thisIdent.typeExpr.type = type;
					methodParams.push_back(&thisIdent);
					functionCall.callKind = FunctionCallKind::MemberMethodCall;
				}
				functionCall.functionStmnt = functionStmnt;

				for (Expr* param : *params) methodParams.push_back(param);

				if (!utils.CheckValidFunctionCallParams(functionStmnt, functionStmnt->method.decl->functionDecl.parameters,
					&methodParams))
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Invalid parameters passed for call signature for method");
				}
				return;
			}
			case StmntID::ExternFunctionDecl:
			{
				functionCall.callKind = FunctionCallKind::ExternalCall;
				functionCall.functionStmnt = functionStmnt;

				if (!utils.CheckValidFunctionCallParams(functionStmnt, functionStmnt->externFunction.parameters, params))
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Invalid parameters passed for call signature for external function declaration");
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
			while (functionType->typeID == TypeID::TemplatedType) functionType = functionType->templatedType.type;
			switch (functionType->typeID)
			{
				// Primitive constrtuctor
			case TypeID::PrimitiveType:
			{
				functionCall.callKind = FunctionCallKind::PrimitiveCall;
				// Default primitive constructor
				if (paramCount == 0) return;

				if (functionType->primitiveType.type == UniqueType::String)
				{
					Type thisType = Type(TypeID::AnyType);
					Expr thisIdent = Expr(ExprID::TypeExpr, function->start);
					thisIdent.typeExpr.type = &thisType;

					eastl::vector<Expr*> strConParams = eastl::vector<Expr*>();
					strConParams.push_back(&thisIdent);
					for (Expr* param : *params) strConParams.push_back(param);

					for (Stmnt* con : context.globalTable->stringSymbol->constructors)
					{
						Stmnt* conDecl = con->constructor.decl;
						if (utils.CheckValidFunctionCallParams(con, conDecl->functionDecl.parameters, &strConParams))
						{
							return;
						}
					}

					AddError(function->start, "ExprChecker:CheckFunctionCallExpr No string constructor overload found");
					return;
				}

				if (paramCount == 1)
				{
					if (!utils.IsAssignable(functionType, utils.InferType(params->at(0))))
					{
						AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Non primitive parameter passed into primitive constructor");
						return;
					}
				}
				else
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Primitive constructors can have at most one parameter");
				}
			}
			break;
			case TypeID::FunctionType:
			{
				functionCall.callKind = FunctionCallKind::FunctionTypeCall;
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
			case TypeID::AnyType:
				functionCall.callKind = FunctionCallKind::UnresolvedGenericCall;
				return;
			default:
				AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Not a callable expression");
				break;
			}
		}
	}

	bool IsUniformCall(Expr* caller, Stmnt* method)
	{
		StringView& stateName = method->method.stateName->val;

		if (caller->typeID == ExprID::IdentifierExpr)
		{
			return caller->identifierExpr.identifier->val == stateName;
		}
		else if (caller->typeID == ExprID::SelectorExpr)
		{
			return caller->selectorExpr.select->identifierExpr.identifier->val == stateName;
		}

		return false;
	}
};