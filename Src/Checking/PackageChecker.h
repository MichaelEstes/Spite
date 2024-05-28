#pragma once
#include "../Intermediate/GlobalTable.h"
#include "EASTL/deque.h"
#include "TypeChecker.h"
#include "ExprChecker.h"

struct PackageChecker
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>> scopeQueue;
	eastl::hash_map<StringView, Stmnt*, StringViewHash>* globalScope;
	Stmnt* currentContext = nullptr;
	TypeChecker typeChecker;
	ExprChecker exprChecker;

	PackageChecker(GlobalTable* globalTable, SymbolTable* symbolTable)
		: typeChecker(globalTable, symbolTable, scopeQueue), exprChecker(globalTable, symbolTable, scopeQueue)
	{
		this->globalTable = globalTable;
		this->symbolTable = symbolTable;
	}

	void Check()
	{
		AddScope();
		globalScope = &scopeQueue.back();
		for (auto& [key, value] : symbolTable->globalValMap)
		{
			CheckGlobalVal(value);
		}

		for (auto& [key, value] : symbolTable->stateMap)
		{
			AddScope();
			CheckState(key, value.state);
			CheckConstructors(value.constructors);
			CheckMethods(value.methods);
			CheckOperators(value.operators);
			CheckDestructor(value.destructor);
			PopScope();
		}

		for (auto& [key, value] : symbolTable->functionMap)
		{
			CheckFunction(value);
		}

		for (Stmnt* node : symbolTable->onCompiles)
		{

		}

		scopeQueue.pop_back();
		if (scopeQueue.size() != 0) AddError("PackageChecker:Check Not all scopes popped, possible compiler error");
	}

	void AddScope()
	{
		scopeQueue.emplace_back();
	}

	void PopScope()
	{
		scopeQueue.pop_back();
		if (scopeQueue.size() == 0)
		{
			Logger::FatalError("PackageChecker::PopScope Global scope removed, possible compiler error");
		}
	}

	void CheckGlobalVal(Stmnt* global)
	{
		CheckDefinition(global);
	}

	void CheckState(const StringView& name, Stmnt* state)
	{
		if (!state)
		{
			AddError("State was not defined for name: " + name);
			return;
		}

		currentContext = state;
		auto& stateRef = state->state;
		for (Stmnt* member : *stateRef.members)
		{
			CheckDefinition(member);
		}
	}

	void CheckConstructors(eastl::hash_set<Stmnt*, MethodHash, MethodEqual>& constructors)
	{
		for (Stmnt* constructor : constructors)
		{
			currentContext = constructor;
			auto& decl = constructor->constructor.decl;
			CheckFunctionDecl(decl, constructor);
		}
	}

	void CheckMethods(eastl::hash_set<Stmnt*, MethodHash, MethodEqual>& methods)
	{
		for (Stmnt* method : methods)
		{
			currentContext = method;
			auto& decl = method->method.decl;
			CheckFunctionDecl(decl, method);
		}
	}

	void CheckOperators(eastl::hash_set<Stmnt*, MethodHash, MethodEqual>& operators)
	{
		for (Stmnt* op : operators)
		{
			currentContext = op;
			auto& decl = op->stateOperator.decl;
			CheckFunctionDecl(decl, op);
		}
	}

	void CheckDestructor(Stmnt* destructor)
	{
		currentContext = nullptr;
		if (!destructor) return; // Destructor not required
		CheckBody(destructor->destructor.body);
	}

	void CheckFunction(Stmnt* function)
	{
		currentContext = function;
		auto& decl = function->function.decl;
		CheckFunctionDecl(decl, function);
	}

	inline void CheckFunctionDecl(Stmnt* functionDecl, Stmnt* of)
	{
		auto& params = functionDecl->functionDecl.parameters;
		auto& body = functionDecl->functionDecl.body;
		CheckFuncBody(body, params);
	}

	inline void CheckBody(Body& body)
	{
		AddScope();
		CheckNode(body.body);
		PopScope();
	}

	inline void CheckFuncBody(Body& body, eastl::vector<Stmnt*>* params = nullptr)
	{
		AddScope();
		if (params) for (Stmnt* node : *params) CheckDefinition(node);
		CheckNode(body.body);
		PopScope();
	}

	void CheckNode(Stmnt* node)
	{
		switch (node->nodeID)
		{
		case ExpressionStmnt:
			exprChecker.CheckExpr(node->expressionStmnt.expression, node);
			break;
		case Definition:
			CheckDefinition(node);
			break;
		case InlineDefinition:
			CheckInlineDefinition(node);
			break;
		case Conditional:
		{
			auto& conditional = node->conditional;
			typeChecker.CheckConditionalType(node);
			CheckBody(conditional.body);
			break;
		}
		case AssignmentStmnt:
		{
			typeChecker.CheckAssignmentStmnt(node);
			exprChecker.CheckAssignmentStmnt(node);
			break;
		}
		case IfStmnt:
		{
			auto& ifStmnt = node->ifStmnt;
			CheckNode(ifStmnt.condition);
			for (Stmnt* elif : *ifStmnt.elifs)
			{
				CheckNode(elif);
			}

			if (ifStmnt.elseCondition) CheckBody(ifStmnt.elseCondition);
			break;
		}
		case ForStmnt:
		{
			auto& forStmnt = node->forStmnt;
			typeChecker.CheckForType(node);
			AddScope();
			CheckDefinition(forStmnt.iterated.declaration);
			CheckBody(forStmnt.body);
			PopScope();
			break;
		}
		case WhileStmnt:
		{
			CheckNode(node->whileStmnt.conditional);
			break;
		}
		case SwitchStmnt:
		{
			typeChecker.CheckSwitchType(node);

			auto& switchStmnt = node->switchStmnt;
			for (Stmnt* caseStmnt : *switchStmnt.cases) {
				CheckNode(caseStmnt);
			}

			if (switchStmnt.defaultCase) CheckBody(switchStmnt.defaultCase);
			break;
		}
		case DeleteStmnt:
		{
			auto& deleteStmnt = node->deleteStmnt;
			break;
		}
		case DeferStmnt:
		{
			auto& deferStmnt = node->deferStmnt;
			if (deferStmnt.deferIf) CheckNode(deferStmnt.conditional);
			else CheckBody(deferStmnt.body);
			break;
		}
		case ContinueStmnt:
		{
			break;
		}
		case BreakStmnt:
			break;
		case ReturnStmnt:
		{
			typeChecker.CheckReturnType(node);
			exprChecker.CheckExpr(node->returnStmnt.expr, node);
			break;
		}
		case Block:
		{
			for (Stmnt* n : *node->block.inner) CheckNode(n);
			break;
		}
		default:
			break;
		}
	}

	void CheckDefinition(Stmnt* node)
	{
		auto& definition = node->definition;
		typeChecker.CheckDefinitionType(node);
		if (definition.assignment) exprChecker.CheckExpr(definition.assignment, node);

		StringView& name = definition.name->val;
		eastl::hash_map<StringView, Stmnt*, StringViewHash>& back = scopeQueue.back();
		if (back.find(name) != back.end()) AddError(node->start, "Re-definition of variable name: " + name);
		else back[name] = node;
	}

	void CheckInlineDefinition(Stmnt* node)
	{
		auto& inlineDefinition = node->inlineDefinition;
		Type* type = inlineDefinition.type;
		Expr* expr = inlineDefinition.assignment;
		typeChecker.CheckAnonType(node, type, expr);
		exprChecker.CheckExpr(expr, node);

		if (type->typeID == TypeID::ExplicitType)
		{
			eastl::hash_map<StringView, Stmnt*, StringViewHash>& back = scopeQueue.back();
			auto& explicitType = type->explicitType;
			for (Stmnt* decl : *explicitType.declarations)
			{
				StringView& name = decl->definition.name->val;
				if (back.find(name) != back.end()) AddError(decl->start, "Re-definition of variable name: " + name);
				else back[name] = decl;
			}
		}
		else
		{
			AddError(node->start, "Unable to create a valid inline definition");
		}
	}
};