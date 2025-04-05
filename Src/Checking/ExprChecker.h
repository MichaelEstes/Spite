#pragma once

#include "../Syntax/SymbolTable.h"
#include "../Syntax/GlobalTable.h"
#include "TypeInference.h"
#include "DeferredChecker.h"
#include "CheckerContext.h"

struct ExprChecker
{
	CheckerContext& context;

	TypeInferer inferer;
	DeferredContainer& deferred;

	ExprChecker(CheckerContext& context, DeferredContainer& deferred) :
		context(context), deferred(deferred), inferer(context)
	{}

	bool IsGenericOfCurrentContext(Expr* expr)
	{
		return context.globalTable->IsGenericOfStmnt(expr, context.currentContext, context.symbolTable);
	}

	bool TemplatesContainForwardedGeneric(eastl::vector<Expr*>* templates)
	{
		for (Expr* expr : *templates)
		{
			if (IsGenericOfCurrentContext(expr)) return true;
		}

		return false;
	}

	Stmnt* InferGenericForDefinition(Stmnt* generics, Stmnt* def, eastl::vector<Expr*>* templateArgs)
	{
		if (def->nodeID != StmntID::Definition)
		{
			AddError(def->start, "ExprChecker:InferGenericForDefinition Expected definition statment to infer generics");
			return def;
		}

		def->definition.type = InferGenericType(generics, def->definition.type, templateArgs);
		if (def->definition.assignment)
		{
			def->definition.assignment = InferGenericExpr(generics, def->definition.assignment, templateArgs);
		}

		return def;
	}

	Type* InferGenericType(Stmnt* generics, Type* type, eastl::vector<Expr*>* templateArgs)
	{
		if (!type) return type;

		switch (type->typeID)
		{
		case NamedType:
		{
			StringView& ident = type->namedType.typeName->val;

			for (size_t i = 0; i < generics->generics.names->size(); i++)
			{
				StringView& genericName = generics->generics.names->at(i)->val;
				if (genericName == ident)
				{
					Expr* arg = templateArgs->at(i);
					if (arg->typeID != ExprID::TypeExpr)
					{
						AddError(type->namedType.typeName, "ExprChecker:InferGenericType Expected type expression as template");
						return type;
					}

					return arg->typeExpr.type;
				}
			}
			break;
		}
		case ExplicitType:
		{
			for (Stmnt*& decl : *type->explicitType.declarations)
			{
				decl = InferGenericForDefinition(generics, decl, templateArgs);
			}
			break;
		}
		case PointerType:
			type->pointerType.type = InferGenericType(generics, type->pointerType.type, templateArgs);
			break;
		case ValueType:
			type->valueType.type = InferGenericType(generics, type->valueType.type, templateArgs);
			break;
		case ArrayType:
			type->arrayType.type = InferGenericType(generics, type->arrayType.type, templateArgs);
			type->arrayType.size = InferGenericExpr(generics, type->arrayType.size, templateArgs);
			break;
		case TemplatedType:
			type->templatedType.type = InferGenericType(generics, type->templatedType.type, templateArgs);
			type->templatedType.templates = InferGenericExpr(generics, type->templatedType.templates, templateArgs);
			break;
		case FunctionType:
		{
			type->functionType.returnType = InferGenericType(generics, type->functionType.returnType, templateArgs);
			for (Type*& param : *type->functionType.paramTypes)
			{
				param = InferGenericType(generics, param, templateArgs);
			}
			break;
		}
		case UnionType:
		{
			for (Stmnt*& decl : *type->unionType.declarations)
			{
				decl = InferGenericForDefinition(generics, decl, templateArgs);
			}
			break;
		}
		case AnonymousType:
		{
			for (Type*& param : *type->anonType.types)
			{
				param = InferGenericType(generics, param, templateArgs);
			}
			break;
		}
		default:
			break;
		}

		return type;
	}

	Expr* InferGenericExpr(Stmnt* generics, Expr* expr, eastl::vector<Expr*>* templateArgs)
	{
		if (!expr) return expr;

		switch (expr->typeID)
		{
		case IdentifierExpr:
		{
			StringView& ident = expr->identifierExpr.identifier->val;

			for (size_t i = 0; i < generics->generics.names->size(); i++)
			{
				StringView& genericName = generics->generics.names->at(i)->val;
				if (genericName == ident)
				{
					return templateArgs->at(i);
				}
			}

			break;
		}
		case SelectorExpr:
			expr->selectorExpr.on = InferGenericExpr(generics, expr->selectorExpr.on, templateArgs);
			expr->selectorExpr.select = InferGenericExpr(generics, expr->selectorExpr.select, templateArgs);
			break;
		case IndexExpr:
			expr->indexExpr.of = InferGenericExpr(generics, expr->indexExpr.of, templateArgs);
			expr->indexExpr.index = InferGenericExpr(generics, expr->indexExpr.index, templateArgs);
			break;
		case FunctionCallExpr:
		{
			expr->functionCallExpr.function = InferGenericExpr(generics, expr->functionCallExpr.function, templateArgs);
			for (Expr*& param : *expr->functionCallExpr.params)
			{
				param = InferGenericExpr(generics, param, templateArgs);
			}
			break;
		}
		case NewExpr:
			expr->newExpr.primaryExpr = InferGenericExpr(generics, expr->newExpr.primaryExpr, templateArgs);
			expr->newExpr.atExpr = InferGenericExpr(generics, expr->newExpr.atExpr, templateArgs);
			break;
		case FixedExpr:
			expr->fixedExpr.atExpr = InferGenericExpr(generics, expr->fixedExpr.atExpr, templateArgs);
			break;
		case TypeLiteralExpr:
		{
			for (Expr*& value : *expr->typeLiteralExpr.values)
			{
				value = InferGenericExpr(generics, value, templateArgs);
			}
			break;
		}
		case ExplicitTypeExpr:
		{
			for (Stmnt*& value : *expr->explicitTypeExpr.values)
			{
				value = InferGenericForDefinition(generics, value, templateArgs);
			}
			break;
		}
		case AsExpr:
			expr->asExpr.of = InferGenericExpr(generics, expr->asExpr.of, templateArgs);
			expr->asExpr.to = InferGenericType(generics, expr->asExpr.to, templateArgs);
			break;
		case DereferenceExpr:
			expr->dereferenceExpr.of = InferGenericExpr(generics, expr->dereferenceExpr.of, templateArgs);
			break;
		case ReferenceExpr:
			expr->referenceExpr.of = InferGenericExpr(generics, expr->referenceExpr.of, templateArgs);
			break;
		case BinaryExpr:
			expr->binaryExpr.left = InferGenericExpr(generics, expr->binaryExpr.left, templateArgs);
			expr->binaryExpr.right = InferGenericExpr(generics, expr->binaryExpr.right, templateArgs);
			break;
		case UnaryExpr:
			expr->unaryExpr.expr = InferGenericExpr(generics, expr->unaryExpr.expr, templateArgs);
			break;
		case GroupedExpr:
			expr->groupedExpr.expr = InferGenericExpr(generics, expr->groupedExpr.expr, templateArgs);
			break;
		case TemplateExpr:
		{
			expr->templateExpr.expr = InferGenericExpr(generics, expr->templateExpr.expr, templateArgs);
			for (Expr*& arg : *expr->templateExpr.templateArgs)
			{
				arg = InferGenericExpr(generics, arg, templateArgs);
			}
			break;
		}

		case TypeExpr:
			expr->typeExpr.type = InferGenericType(generics, expr->typeExpr.type, templateArgs);
			break;
		case FunctionTypeDeclExpr:
		{
			Stmnt* anonFunction = expr->functionTypeDeclExpr.anonFunction;
			Stmnt* funcDecl = anonFunction->anonFunction.decl;
			anonFunction->anonFunction.returnType = InferGenericType(generics, anonFunction->anonFunction.returnType, templateArgs);
			for (Stmnt*& param : *funcDecl->functionDecl.parameters)
			{
				param = InferGenericForDefinition(generics, param, templateArgs);
			}
			break;
		}
		case SizeOfExpr:
			expr->sizeOfExpr.expr = InferGenericExpr(generics, expr->sizeOfExpr.expr, templateArgs);
			break;
		case AlignOfExpr:
			expr->alignOfExpr.expr = InferGenericExpr(generics, expr->alignOfExpr.expr, templateArgs);
			break;
		case OffsetOfExpr:
			expr->offsetOfExpr.type = InferGenericExpr(generics, expr->offsetOfExpr.type, templateArgs);
			expr->offsetOfExpr.expr = InferGenericExpr(generics, expr->offsetOfExpr.expr, templateArgs);
			break;
		case TypeOfExpr:
			expr->typeOfExpr.expr = InferGenericExpr(generics, expr->typeOfExpr.expr, templateArgs);
			break;
		default:
			break;
		}

		return expr;
	}

	void InferDefaultTemplateArgs(Stmnt* generics, eastl::vector<Expr*>* templateArgs,
		intmax_t startIndex, Token* start)
	{
		for (size_t i = startIndex; i < templateArgs->size(); i++)
		{
			Expr*& arg = templateArgs->at(i);
			arg = InferGenericExpr(generics, arg, templateArgs);
		}
	}

	void AddTemplatesToExpand(Stmnt* stmnt, eastl::vector<Expr*>* templateArgs, Token* start,
		Expr* ofExpr = nullptr, Type* ofType = nullptr)
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
			else if (ofType && context.globalTable->IsGenericOfStmnt(ofType, context.currentContext, context.symbolTable))
			{
				Token* genericTok = context.globalTable->GetBaseType(ofType)->namedType.typeName;
				DeferredTemplateForwarded toDefer = DeferredTemplateForwarded();
				toDefer.genericName = genericTok;
				toDefer.templatesToForward = templateArgs;
				deferred.deferredForwardedTemplates[context.currentContext].push_back(toDefer);
				return;
			}
			else if (ofType && IsAny(ofType)) return;

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
				Expr* defaultValue = genericsNode->generics.defaultValues->at(i);
				templateArgs->push_back(context.symbolTable->CloneExpr(defaultValue));
			}

			InferDefaultTemplateArgs(genericsNode, templateArgs, argSize, start);
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

			if (currGenerics != genericsNode) deferred.deferredTemplates[currGenerics].push_back(toDefer);
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
		AddTemplatesToExpand(state, templateArgs, templateExpr->start, nullptr, type->templatedType.type);
	}

	void CheckGenerics(Expr* expr)
	{
		auto& templateExpr = expr->templateExpr;
		eastl::vector<Expr*>* templateArgs = templateExpr.templateArgs;
		Expr* ofExpr = templateExpr.expr;

		Stmnt* stmnt = inferer.GetDeclarationStmntForExpr(ofExpr);
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
		Stmnt* functionStmnt = inferer.GetDeclarationStmntForExpr(function);

		if (functionStmnt)
		{
			switch (functionStmnt->nodeID)
			{
			// Constructor being called
			case StmntID::StateStmnt:
			{
				functionCall.callKind = FunctionCallKind::ConstructorCall;
				StateSymbol* stateSymbol = context.globalTable->FindScopedStateSymbol(functionStmnt->state.name, context.symbolTable);
				if (!stateSymbol)
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Unable to resolve state symbol, missing import");
					return;
				}

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
					if (CheckValidFunctionCallParams(con, conDecl->functionDecl.parameters, &conParams))
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
				if (!CheckValidFunctionCallParams(functionStmnt, functionStmnt->function.decl->functionDecl.parameters,
					params))
				{
					CheckValidFunctionCallParams(functionStmnt, functionStmnt->function.decl->functionDecl.parameters,
						params);
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
					Type* type = inferer.InferType(caller);
					// Methods can be called from the . syntax for pointers since 'this' param is always a reference
					if (type->typeID == TypeID::PointerType) type = type->pointerType.type;
					thisIdent.typeExpr.type = type;
					methodParams.push_back(&thisIdent);
					functionCall.callKind = FunctionCallKind::MemberMethodCall;
				}
				functionCall.functionStmnt = functionStmnt;

				for (Expr* param : *params) methodParams.push_back(param);

				if (!CheckValidFunctionCallParams(functionStmnt, functionStmnt->method.decl->functionDecl.parameters,
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

				if (!CheckValidFunctionCallParams(functionStmnt, functionStmnt->externFunction.parameters, params))
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Invalid parameters passed for call signature for external function declaration");
				}
				return;
			}
			case StmntID::Definition:
			{
				CheckFunctionTypeCall(functionStmnt->definition.type, expr);
				return;
			}
			default:
				AddError(expr->start, "ExprChecker:CheckFunctionCallExpr No callable statement found");
				return;
			}
		}
		else
		{
			Type* functionType = inferer.InferType(function);
			CheckFunctionTypeCall(functionType, expr);
		}
	}

	void CheckFunctionTypeCall(Type* functionType, Expr* expr)
	{
		auto& functionCall = expr->functionCallExpr;
		Expr* function = functionCall.function;
		eastl::vector<Expr*>* params = functionCall.params;
		size_t paramCount = params->size();

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
					if (CheckValidFunctionCallParams(con, conDecl->functionDecl.parameters, &strConParams))
					{
						return;
					}
				}

				AddError(function->start, "ExprChecker:CheckFunctionCallExpr No string constructor overload found");
				return;
			}

			if (paramCount == 1)
			{
				if (!inferer.IsAssignable(functionType, inferer.InferType(params->at(0))))
				{
					AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Non primitive parameter passed into primitive constructor");
					return;
				}
			}
			else
			{
				AddError(expr->start, "ExprChecker:CheckFunctionCallExpr Primitive constructors can have at most one parameter");
			}

			break;
		}
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
				if (!inferer.IsAssignable(func.paramTypes->at(i), inferer.InferType(params->at(i))))
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

	bool CheckValidFunctionCallParams(Stmnt* calledFor, eastl::vector<Stmnt*>* funcParams,
		eastl::vector<Expr*>* params)
	{
		size_t paramCount = params->size();
		size_t requiredParamCount = RequiredFunctionParamCount(calledFor);
		if (requiredParamCount > paramCount) return false;
		if (paramCount > funcParams->size()) return false;

		for (size_t i = 0; i < paramCount; i++)
		{
			Expr* exprParam = params->at(i);
			Stmnt* funcParam = funcParams->at(i);
			Type* defType = funcParam->definition.type;
			if (!inferer.IsAssignable(defType, inferer.InferType(exprParam), calledFor))
				return false;
		}

		return true;
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