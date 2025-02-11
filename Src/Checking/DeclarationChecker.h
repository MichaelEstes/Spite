#pragma once
#include "../Syntax/GlobalTable.h"
#include "EASTL/deque.h"
#include "DeferredChecker.h"
#include "CheckerContext.h"
#include "ASTNodeChecker.h"

struct DeclarationChecker
{
	CheckerContext context;

	ASTNodeChecker nodeChecker;

	DeclarationChecker(GlobalTable* globalTable, SymbolTable* symbolTable, DeferredContainer& deferred)
		: context(globalTable, symbolTable), nodeChecker(context, deferred)
	{}

	void Check()
	{
		nodeChecker.AddScope();
		for (Stmnt* value : context.symbolTable->globalVals)
		{
			CheckGlobalVal(value);
		}

		for (auto& [key, value] : context.symbolTable->stateMap)
		{
			// Keep in state in context for method checking
			context.currentContext = value.state;
			CheckState(key, value);
			CheckConstructors(value.constructors);
			CheckMethods(value.methods);
			CheckOperators(value.operators);
			CheckDestructor(value.destructor);
		}

		for (auto& [key, value] : context.symbolTable->functionMap)
		{
			CheckFunction(value);
		}

		for (auto& [key, value] : context.symbolTable->externFunctionMap)
		{
			CheckExternalFunctions(value);
		}

		context.scopeUtils.scopeQueue.pop_back();
		if (context.scopeUtils.scopeQueue.size() != 0) AddError("PackageChecker:Check Not all scopes popped, possible compiler error");
	}

	void CheckGlobalVal(Stmnt* global)
	{
		nodeChecker.CheckDefinition(global);
	}

	void CheckState(const StringView& name, StateSymbol& stateSymbol)
	{
		Stmnt* state = stateSymbol.state;
		if (!state)
		{
			AddError("State was not defined for name: " + name);
			return;
		}

		CheckGenericDeclarations(GetGenerics(stateSymbol.state));
		nodeChecker.AddScope();
		auto& stateRef = state->state;
		for (Stmnt* member : *stateRef.members)
		{
			nodeChecker.CheckDefinition(member);
		}
		nodeChecker.PopScope();
	}

	void CheckConstructors(eastl::hash_set<Stmnt*, MethodHash, MethodEqual>& constructors)
	{
		for (Stmnt* constructor : constructors)
		{
			context.currentContext = constructor;
			auto& decl = constructor->constructor.decl;
			CheckFunctionDecl(decl, constructor);
		}
	}

	void CheckMethods(eastl::hash_set<Stmnt*, MethodHash, MethodEqual>& methods)
	{
		for (Stmnt* method : methods)
		{
			context.currentContext = method;
			auto& decl = method->method.decl;
			CheckGenericDeclarations(GetGenerics(method));
			nodeChecker.CheckType(method->method.returnType, method->start);
			CheckFunctionDecl(decl, method);
		}
	}

	void CheckOperators(eastl::hash_set<Stmnt*, MethodHash, MethodEqual>& operators)
	{
		for (Stmnt* op : operators)
		{
			context.currentContext = op;
			auto& decl = op->stateOperator.decl;
			nodeChecker.CheckType(op->stateOperator.returnType, op->start);
			CheckFunctionDecl(decl, op);
		}
	}

	void CheckDestructor(Stmnt* destructor)
	{
		context.currentContext = destructor;
		if (!destructor) return; // Destructor not required
		CheckFunctionDecl(destructor->destructor.decl, destructor);
	}

	void CheckFunction(Stmnt* function)
	{
		context.currentContext = function;
		auto& decl = function->function.decl;
		CheckGenericDeclarations(GetGenerics(function));
		nodeChecker.CheckType(function->function.returnType, function->start);
		CheckFunctionDecl(decl, function);
	}

	void CheckExternalFunctions(Stmnt* function)
	{

	}

	void CheckGenericDeclarations(Stmnt* generics)
	{
		if (!generics) return;
		for (Expr* defaultValue : *generics->generics.defaultValues)
		{
			if (defaultValue)
			{
				nodeChecker.CheckExpr(defaultValue);
			}
		}
	}

	inline void CheckFunctionDecl(Stmnt* functionDecl, Stmnt* of)
	{
		nodeChecker.AddScope();
		eastl::vector<Stmnt*>* params = functionDecl->functionDecl.parameters;
		if (params)
		{
			for (Stmnt* param : *params)
			{
				nodeChecker.CheckDefinition(param);
			}
		}
		nodeChecker.PopScope();
	}
};