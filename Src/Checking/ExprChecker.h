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
		case GenericsExpr:
			CheckExpr(expr->genericsExpr.expr, node, expr);
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
				eastl::vector<Token*>* genericNames = functionStmnt->state.generics ? functionStmnt->state.generics->generics.names : nullptr;
				for (Stmnt* con : stateSymbol->constructors)
				{
					Stmnt* conDecl = con->constructor.decl;
					if (CheckValidFunctionCallParams(conDecl, params, genericNames)) return;
				}

				AddError(expr->start, "ExprChecker:CheckFunctionCallExpr No constructor found with matching paramters");
				return;
			}
			case StmntID::FunctionStmnt:
			{
				eastl::vector<Token*>* genericNames = functionStmnt->function.generics ? functionStmnt->function.generics->generics.names : nullptr;
				if (!CheckValidFunctionCallParams(functionStmnt->function.decl, params, genericNames))
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Invalid parameters passed for call signature for function");
				}
				return;
			}
			case StmntID::Method:
			{
				eastl::vector<Token*> genericNames = eastl::vector<Token*>();
				if (functionStmnt->method.generics)
				{
					for (Token* name : *functionStmnt->method.generics->generics.names) genericNames.push_back(name);
				}
				Stmnt* state = globalTable->FindLocalOrImportedState(functionStmnt->method.stateName, symbolTable);
				if (state && state->state.generics)
				{
					for (Token* name : *state->state.generics->generics.names) genericNames.push_back(name);
				}

				// Add call expression as first parameter
				eastl::vector<Expr*> methodParams = eastl::vector<Expr*>();
				methodParams.push_back(GetStateParamForMethodCall(function));
				for (Expr* param : *params) methodParams.push_back(param);

				if (!CheckValidFunctionCallParams(functionStmnt->method.decl, &methodParams, &genericNames))
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
		if (param->typeID == ExprID::GenericsExpr)
		{
			param = param->genericsExpr.expr;
		}

		return param->selectorExpr.on;
	}

	bool CheckValidFunctionCallParams(Stmnt* funcDecl, eastl::vector<Expr*>* params, eastl::vector<Token*>* genericNames)
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
			if (!IsGenericType(defType, genericNames) && !utils.IsAssignable(defType, utils.InferType(exprParam)))
				return false;
		}

		return true;
	}

	bool IsGenericType(Type* type, eastl::vector<Token*>* genericNames)
	{
		if (!genericNames || type->typeID != TypeID::NamedType) return false;

		for (Token* name : *genericNames)
		{
			if (type->namedType.typeName->val == name->val) return true;
		}

		return false;
	}

	eastl::vector<Type*> BuildTypeArrFromParams(eastl::vector<Stmnt*>* funcParams, Expr* callExpr, Stmnt* generics)
	{
		eastl::vector<Type*> types = eastl::vector<Type*>();

		return types;
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
};