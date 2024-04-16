#pragma once
#include "../Intermediate/Syntax.h"
#include "EASTL/deque.h"
#include "TypeChecker.h"

struct Checker
{
	Syntax& syntax;
	eastl::deque<eastl::hash_map<InplaceString, Node*, InplaceStringHash>> scopeQueue;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash>* globalScope;
	Node* currentContext = nullptr;
	TypeChecker typeChecker;

	Checker(Syntax& syntax) : syntax(syntax), typeChecker(syntax, scopeQueue, currentContext) {}

	void Check()
	{
		AddScope();
		globalScope = &scopeQueue.back();
		for (auto& [key, value] : syntax.symbolTable->globalValMap)
		{
			CheckGlobalVal(value);
		}

		for (auto& [key, value] : syntax.symbolTable->stateMap)
		{
			AddScope();
			CheckState(key, value.state);
			CheckConstructors(value.constructors);
			CheckMethods(value.methods);
			CheckOperators(value.operators);
			CheckDestructor(value.destructor);
			PopScope();
		}

		for (auto& [key, value] : syntax.symbolTable->functionMap)
		{
			CheckFunction(value);
		}

		for (Node* node : syntax.symbolTable->onCompiles)
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
		CheckBody(destructor->destructor.body, destructor);
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
		CheckBody(body, of, params);
	}

	inline void CheckBody(Body& body, Node* of, Node* param)
	{
		AddScope();
		if (param) CheckDefinition(param);
		CheckNode(body.body, of);
		PopScope();
	}

	inline void CheckBody(Body& body, Node* of, eastl::vector<Node*>* params = nullptr)
	{
		AddScope();
		if (params) for (Node* node : *params) CheckDefinition(node);
		CheckNode(body.body, of);
		PopScope();
	}

	void CheckNode(Node* node, Node* of)
	{
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
		case Conditional:
		{
			auto& conditional = node->conditional;
			Type* inferred = typeChecker.InferType(conditional.condition);
			if (!typeChecker.IsBoolLike(inferred))
			{
				AddError(node->start, "Conditional expression doesn't evaluate to a conditional value");
			}
			CheckBody(conditional.body, node);
			break;
		}
		case AssignmentStmnt:
		{
			auto& assignment = node->assignmentStmnt;
			Type* to = typeChecker.InferType(assignment.assignTo);
			Type* from = typeChecker.InferType(assignment.assignment);
			if (*to != *from)
			{
				AddError(node->start, "Invalid type evaluation for assignment expression: " + ToString(to));
			}
			break;
		}
		case IfStmnt:
		{
			auto& ifStmnt = node->ifStmnt;
			CheckNode(ifStmnt.condition, node);
			for (Node* elif : *ifStmnt.elifs)
			{
				CheckNode(elif, node);
			}

			if (ifStmnt.elseCondition) CheckBody(ifStmnt.elseCondition, node);
			break;
		}
		case ForStmnt:
		{
			auto& forStmnt = node->forStmnt;
			if (!forStmnt.isDeclaration)
			{
				Token* identifier = forStmnt.iterated.identifier;
				Node* decl = syntax.CreateNode(identifier, NodeID::Definition);
				decl->definition.assignment = nullptr;
				decl->definition.name = identifier;
				Type* type = typeChecker.InferType(forStmnt.toIterate);
				if (forStmnt.rangeFor)
				{
					if (!typeChecker.IsInt(type))
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

			CheckBody(forStmnt.body, node, forStmnt.iterated.declaration);
			break;
		}
		case WhileStmnt:
		{
			CheckNode(node->whileStmnt.conditional, node);
			break;
		}
		case SwitchStmnt:
		{
			auto& switchStmnt = node->switchStmnt;
			if (!typeChecker.IsInt(typeChecker.InferType(switchStmnt.switchOn)))
			{
				AddError(switchStmnt.switchOn->start, "Switch expressions must evaluate to an int type");
			}

			for (Node* caseStmnt : *switchStmnt.cases) {
				CheckNode(caseStmnt, node);
			}

			if (switchStmnt.defaultCase) CheckBody(switchStmnt.defaultCase, node);
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
			if (deferStmnt.deferIf) CheckNode(deferStmnt.conditional, node);
			else CheckBody(deferStmnt.body, node);
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
			if (typeChecker.InferType(node->returnStmnt.expr)->typeID == TypeID::InvalidType)
			{
				AddError(node->start, "Return expressions doesn't evaluate to a valid type");
			}
			break;
		}
		case Block:
		{
			for (Node* n : *node->block.inner) CheckNode(n, of);
			break;
		}
		default:
			break;
		}
	}

	void CheckExpr(Expr* expr)
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
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			break;
		case NewExpr:
			break;
		case FixedExpr:
			break;
		case AnonTypeExpr:
			break;
		case AsExpr:
			break;
		case DereferenceExpr:
			break;
		case ReferenceExpr:
			break;
		case BinaryExpr:
			break;
		case UnaryExpr:
			break;
		case GroupedExpr:
			break;
		case GenericsExpr:
			break;
		case FunctionTypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		default:
			break;
		}
	}

	void CheckDefinition(Node* node)
	{
		auto& definition = node->definition;
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
		typeChecker.CheckAnonType(node->start, type, expr);

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