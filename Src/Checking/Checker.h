#pragma once
#include "../Intermediate/SymbolTable.h"
#include "EASTL/deque.h"
#include "TypeChecker.h"
#include "ExprChecker.h"

struct Checker
{
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<InplaceString, Node*, InplaceStringHash>> scopeQueue;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash>* globalScope;
	Node* currentContext = nullptr;
	TypeChecker typeChecker;
	ExprChecker exprChecker;

	Checker(SymbolTable* symbolTable) 
		: typeChecker(symbolTable, scopeQueue), exprChecker(symbolTable, scopeQueue) {
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

		for (Node* node : symbolTable->onCompiles)
		{

		}

		scopeQueue.pop_back();
		if (scopeQueue.size() != 0) AddError("Checker:Check Not all scopes popped, possible compiler error");
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
			Logger::FatalError("Checker::PopScope Global scope removed, possible compiler error");
		}
	}

	void CheckGlobalVal(Node* global)
	{
		CheckDefinition(global);
	}

	void CheckState(const InplaceString& name, Node* state)
	{
		if (!state)
		{
			AddError("State was not defined for name: " + name);
			return;
		}

		currentContext = state;
		auto& stateRef = state->state;
		for (Node* member : *stateRef.members)
		{
			CheckDefinition(member);
		}
	}

	void CheckConstructors(eastl::vector<Node*>& constructors)
	{
		for (Node* constructor : constructors)
		{
			currentContext = constructor;
			auto& decl = constructor->constructor.decl;
			CheckFunctionDecl(decl, constructor);
		}
	}

	void CheckMethods(eastl::vector<Node*>& methods)
	{
		for (Node* method : methods)
		{
			currentContext = method;
			auto& decl = method->method.decl;
			CheckFunctionDecl(decl, method);
		}
	}

	void CheckOperators(eastl::vector<Node*>& operators)
	{
		for (Node* op : operators)
		{
			currentContext = op;
			auto& decl = op->stateOperator.decl;
			CheckFunctionDecl(decl, op);
		}
	}

	void CheckDestructor(Node* destructor)
	{
		currentContext = nullptr;
		if (!destructor) return; // Destructor not required
		CheckBody(destructor->destructor.body);
	}

	void CheckFunction(Node* function)
	{
		currentContext = function;
		auto& decl = function->function.decl;
		CheckFunctionDecl(decl, function);
	}

	inline void CheckFunctionDecl(Node* functionDecl, Node* of)
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

	inline void CheckFuncBody(Body& body, eastl::vector<Node*>* params = nullptr)
	{
		AddScope();
		if (params) for (Node* node : *params) CheckDefinition(node);
		CheckNode(body.body);
		PopScope();
	}

	void CheckNode(Node* node)
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
			exprChecker.CheckAssignmentStmnt(node);
			typeChecker.CheckAssignmentStmnt(node);
			break;
		}
		case IfStmnt:
		{
			auto& ifStmnt = node->ifStmnt;
			CheckNode(ifStmnt.condition);
			for (Node* elif : *ifStmnt.elifs)
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
			for (Node* caseStmnt : *switchStmnt.cases) {
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
			if (!node->returnStmnt.voidReturn) exprChecker.CheckExpr(node->returnStmnt.expr, node);
			typeChecker.CheckReturnType(node);
			break;
		}
		case Block:
		{
			for (Node* n : *node->block.inner) CheckNode(n);
			break;
		}
		default:
			break;
		}
	}

	void CheckDefinition(Node* node)
	{
		auto& definition = node->definition;
		if (definition.assignment) exprChecker.CheckExpr(definition.assignment, node);
		typeChecker.CheckDefinitionType(node);

		InplaceString& name = definition.name->val;
		eastl::hash_map<InplaceString, Node*, InplaceStringHash>& back = scopeQueue.back();
		if (back.find(name) != back.end()) AddError(node->start, "Re-definition of variable name: " + name);
		else back[name] = node;
	}

	void CheckInlineDefinition(Node* node)
	{
		auto& inlineDefinition = node->inlineDefinition;
		Type* type = inlineDefinition.type;
		Expr* expr = inlineDefinition.assignment;
		exprChecker.CheckExpr(expr, node);
		typeChecker.CheckAnonType(node, type, expr);

		if (type->typeID == TypeID::ExplicitType)
		{
			eastl::hash_map<InplaceString, Node*, InplaceStringHash>& back = scopeQueue.back();
			auto& explicitType = type->explicitType;
			for (Node* decl : *explicitType.declarations)
			{
				InplaceString& name = decl->definition.name->val;
				if (back.find(name) != back.end()) AddError(decl->start, "Re-definition of variable name: " + name);
				else back[name] = decl;
			}
		}
		else
		{
			AddError(node->start, "Unabled to create a valid inline definition");
		}
	}
};