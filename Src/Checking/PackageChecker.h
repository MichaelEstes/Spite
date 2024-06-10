#pragma once
#include "../Intermediate/GlobalTable.h"
#include "EASTL/deque.h"
#include "TypeChecker.h"
#include "ExprChecker.h"
#include "DeferredChecker.h"
#include "CheckerContext.h"


struct PackageChecker
{
	CheckerContext context;

	TypeChecker typeChecker;
	ExprChecker exprChecker;

	PackageChecker(GlobalTable* globalTable, SymbolTable* symbolTable, DeferredContainer& deferred)
		: context(globalTable, symbolTable, deferred), typeChecker(context), exprChecker(context) {}

	void Check()
	{
		AddScope();
		for (auto& [key, value] : context.symbolTable->globalValMap)
		{
			CheckGlobalVal(value);
		}

		for (auto& [key, value] : context.symbolTable->stateMap)
		{
			AddScope();
			// Keep in state in context for method checking
			context.currentStateContext = value.state;
			CheckState(key, value.state);
			CheckConstructors(value.constructors);
			CheckMethods(value.methods);
			CheckOperators(value.operators);
			CheckDestructor(value.destructor);
			PopScope();
		}
		context.currentStateContext = nullptr;

		for (auto& [key, value] : context.symbolTable->functionMap)
		{
			CheckFunction(value);
		}

		for (Stmnt* node : context.symbolTable->onCompiles)
		{

		}

		context.scopeQueue.pop_back();
		if (context.scopeQueue.size() != 0) AddError("PackageChecker:Check Not all scopes popped, possible compiler error");
	}

	void AddScope()
	{
		context.scopeQueue.emplace_back();
	}

	void PopScope()
	{
		context.scopeQueue.pop_back();
		if (context.scopeQueue.size() == 0)
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

		context.currentContext = state;
		auto& stateRef = state->state;
		for (Stmnt* member : *stateRef.members)
		{
			CheckDefinition(member);
			CheckType(member->definition.type, member->start);
		}
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
			CheckFunctionDecl(decl, method);
			CheckType(method->method.returnType, method->start);
		}
	}

	void CheckOperators(eastl::hash_set<Stmnt*, MethodHash, MethodEqual>& operators)
	{
		for (Stmnt* op : operators)
		{
			context.currentContext = op;
			auto& decl = op->stateOperator.decl;
			CheckFunctionDecl(decl, op);
			CheckType(op->stateOperator.returnType, op->start);
		}
	}

	void CheckDestructor(Stmnt* destructor)
	{
		context.currentContext = nullptr;
		if (!destructor) return; // Destructor not required
		CheckBody(destructor->destructor.body);
	}

	void CheckFunction(Stmnt* function)
	{
		context.currentContext = function;
		auto& decl = function->function.decl;
		CheckFunctionDecl(decl, function);
		CheckType(function->function.returnType, function->start);
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
				CheckType(node->definition.type, node->start);
			}
		}
		CheckStmnt(body.body);
		PopScope();
	}

	void CheckStmnt(Stmnt* node)
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
		case AnonFunction:
			CheckType(node->anonFunction.returnType, node->start);
			CheckFunctionDecl(node->anonFunction.decl, node);
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
			CheckExpr(node->assignmentStmnt.assignTo);
			CheckExpr(node->assignmentStmnt.assignment);
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
			typeChecker.CheckForType(node);
			AddScope();
			CheckStmnt(forStmnt.iterated.declaration);
			CheckBody(forStmnt.body);
			PopScope();
			break;
		}
		case WhileStmnt:
		{
			CheckStmnt(node->whileStmnt.conditional);
			break;
		}
		case SwitchStmnt:
		{
			typeChecker.CheckSwitchType(node);

			auto& switchStmnt = node->switchStmnt;
			for (Stmnt* caseStmnt : *switchStmnt.cases) {
				CheckStmnt(caseStmnt);
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
			typeChecker.CheckReturnType(node);
			CheckExpr(node->returnStmnt.expr);
			break;
		}
		case Block:
		{
			for (Stmnt* n : *node->block.inner) CheckStmnt(n);
			break;
		}
		case CompileStmnt:
			CheckType(node->compileStmnt.returnType, node->start);
			CheckBody(node->compileStmnt.body);
			break;
		case CompileDebugStmnt:
			CheckBody(node->compileDebugStmnt.body);
			break;
		default:
			break;
		}
	}

	void CheckExpr(Expr* expr)
	{
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
			break;
		case FunctionCallExpr:
			CheckExpr(expr->functionCallExpr.function);
			exprChecker.CheckFunctionCallExpr(expr);
			break;
		case NewExpr:
			CheckExpr(expr->newExpr.primaryExpr);
			exprChecker.CheckNew(expr);
			break;
		case FixedExpr:
			CheckExpr(expr->fixedExpr.atExpr);
			exprChecker.CheckFixed(expr);
			break;
		case AnonTypeExpr:
		{
			for (Expr* e : *expr->anonTypeExpr.values) CheckExpr(e);
			break;
		}
		case AsExpr:
			CheckExpr(expr->asExpr.of);
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
			CheckExpr(expr->templateExpr.expr);
			for (Expr* templArg : *expr->templateExpr.templateArgs) CheckExpr(templArg);
			exprChecker.CheckGenerics(expr);
			break;
		}
		case TypeExpr:
			CheckType(expr->typeExpr.type, expr->start);
			break;
		case FunctionTypeDeclExpr:
			CheckStmnt(expr->functionTypeDeclExpr.anonFunction);
			break;
		case CompileExpr:
			CheckStmnt(expr->compileExpr.compile);
			break;
		default:
			break;
		}
	}

	void CheckType(Type* type, Token* start)
	{
		switch (type->typeID)
		{
		case InvalidType:
		case UnknownType:
		case ImplicitType:
			AddError(start, "Invalid type found");
			break;
		case NamedType:
		{
			typeChecker.CheckNamedType(type);
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
		case PointerType:
			CheckType(type->pointerType.type, type->pointerType.ptr);
			break;
		case ValueType:
			CheckType(type->valueType.type, type->valueType.valueOp);
			break;
		case ArrayType:
			CheckType(type->arrayType.type, type->arrayType.arr);
			break;
		case TemplatedType:
		{
			CheckType(type->templatedType.type, start);
			for (Expr* templ : *type->templatedType.templates->templateExpr.templateArgs)
			{
				CheckExpr(templ);
			}
			break;
		}
		case FunctionType:
		{
			CheckType(type->functionType.returnType, start);

			for (Type* param : *type->functionType.paramTypes)
			{
				CheckType(param, start);
			}
			break;
		}
		default:
			break;
		}
	}

	void CheckDefinition(Stmnt* node, bool scopeDefinition = true)
	{
		auto& definition = node->definition;
		typeChecker.CheckDefinitionType(node);
		if (definition.assignment) CheckExpr(definition.assignment);
		CheckType(node->definition.type, node->start);

		if (scopeDefinition)
		{
			StringView& name = definition.name->val;
			eastl::hash_map<StringView, Stmnt*, StringViewHash>& back = context.scopeQueue.back();
			if (back.find(name) != back.end()) AddError(node->start, "Re-definition of variable name: " + name);
			else back[name] = node;
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
			eastl::hash_map<StringView, Stmnt*, StringViewHash>& back = context.scopeQueue.back();
			auto& explicitType = type->explicitType;
			for (Stmnt* decl : *explicitType.declarations)
			{
				StringView& name = decl->definition.name->val;
				if (back.find(name) != back.end()) AddError(decl->start, "Re-definition of variable name: " + name);
				else back[name] = decl;
				CheckType(decl->definition.type, decl->start);
			}
		}
		else
		{
			AddError(node->start, "Unable to create a valid inline definition");
		}
	}
};