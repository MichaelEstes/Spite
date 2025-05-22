#pragma once

#include "../Syntax/Syntax.h"


Expr* InferGenericExpr(Stmnt* generics, Expr* expr, eastl::vector<Expr*>* templateArgs, bool* wasInferred = nullptr);
Type* InferGenericType(Stmnt* generics, Type* type, eastl::vector<Expr*>* templateArgs, bool* wasInferred = nullptr);

Stmnt* InferGenericForDefinition(Stmnt* generics, Stmnt* def, eastl::vector<Expr*>* templateArgs, bool* wasInferred)
{
	if (def->nodeID != StmntID::Definition)
	{
		AddError(def->start, "ExprChecker:InferGenericForDefinition Expected definition statment to infer generics");
		return def;
	}

	def->definition.type = InferGenericType(generics, def->definition.type, templateArgs, wasInferred);
	if (def->definition.assignment)
	{
		def->definition.assignment = InferGenericExpr(generics, def->definition.assignment, templateArgs, wasInferred);
	}

	return def;
}

Type* InferGenericType(Stmnt* generics, Type* type, eastl::vector<Expr*>* templateArgs, bool* wasInferred)
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

				if (wasInferred) *wasInferred = true;
				return arg->typeExpr.type;
			}
		}
		break;
	}
	case ExplicitType:
	{
		for (Stmnt*& decl : *type->explicitType.declarations)
		{
			decl = InferGenericForDefinition(generics, decl, templateArgs, wasInferred);
		}
		break;
	}
	case PointerType:
		type->pointerType.type = InferGenericType(generics, type->pointerType.type, templateArgs, wasInferred);
		break;
	case ValueType:
		type->valueType.type = InferGenericType(generics, type->valueType.type, templateArgs, wasInferred);
		break;
	case RefType:
		type->refType.type = InferGenericType(generics, type->refType.type, templateArgs, wasInferred);
		break;
	case ArrayType:
		type->arrayType.type = InferGenericType(generics, type->arrayType.type, templateArgs, wasInferred);
		type->arrayType.size = InferGenericExpr(generics, type->arrayType.size, templateArgs, wasInferred);
		break;
	case TemplatedType:
		type->templatedType.type = InferGenericType(generics, type->templatedType.type, templateArgs, wasInferred);
		type->templatedType.templates = InferGenericExpr(generics, type->templatedType.templates, templateArgs, wasInferred);
		break;
	case FunctionType:
	{
		type->functionType.returnType = InferGenericType(generics, type->functionType.returnType, templateArgs, wasInferred);
		for (Type*& param : *type->functionType.paramTypes)
		{
			param = InferGenericType(generics, param, templateArgs, wasInferred);
		}
		break;
	}
	case UnionType:
	{
		for (Stmnt*& decl : *type->unionType.declarations)
		{
			decl = InferGenericForDefinition(generics, decl, templateArgs, wasInferred);
		}
		break;
	}
	case AnonymousType:
	{
		for (Type*& param : *type->anonType.types)
		{
			param = InferGenericType(generics, param, templateArgs, wasInferred);
		}
		break;
	}
	default:
		break;
	}

	return type;
}

Expr* InferGenericExpr(Stmnt* generics, Expr* expr, eastl::vector<Expr*>* templateArgs, bool* wasInferred)
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
				if (wasInferred) *wasInferred = true;
				return templateArgs->at(i);
			}
		}
		break;
	}
	case SelectorExpr:
		expr->selectorExpr.on = InferGenericExpr(generics, expr->selectorExpr.on, templateArgs, wasInferred);
		expr->selectorExpr.select = InferGenericExpr(generics, expr->selectorExpr.select, templateArgs, wasInferred);
		break;
	case IndexExpr:
		expr->indexExpr.of = InferGenericExpr(generics, expr->indexExpr.of, templateArgs, wasInferred);
		expr->indexExpr.index = InferGenericExpr(generics, expr->indexExpr.index, templateArgs, wasInferred);
		break;
	case FunctionCallExpr:
	{
		expr->functionCallExpr.function = InferGenericExpr(generics, expr->functionCallExpr.function, templateArgs, wasInferred);
		for (Expr*& param : *expr->functionCallExpr.params)
		{
			param = InferGenericExpr(generics, param, templateArgs, wasInferred);
		}
		break;
	}
	case NewExpr:
		expr->newExpr.primaryExpr = InferGenericExpr(generics, expr->newExpr.primaryExpr, templateArgs, wasInferred);
		expr->newExpr.atExpr = InferGenericExpr(generics, expr->newExpr.atExpr, templateArgs, wasInferred);
		break;
	case FixedExpr:
		expr->fixedExpr.atExpr = InferGenericExpr(generics, expr->fixedExpr.atExpr, templateArgs, wasInferred);
		break;
	case TypeLiteralExpr:
	{
		if (expr->typeLiteralExpr.typed)
		{
			expr->typeLiteralExpr.typed = InferGenericExpr(generics, expr->typeLiteralExpr.typed, templateArgs, wasInferred);
		}
		for (Expr*& value : *expr->typeLiteralExpr.values)
		{
			value = InferGenericExpr(generics, value, templateArgs, wasInferred);
		}
		break;
	}
	case ExplicitTypeExpr:
	{
		for (Stmnt*& value : *expr->explicitTypeExpr.values)
		{
			value = InferGenericForDefinition(generics, value, templateArgs, wasInferred);
		}
		break;
	}
	case AsExpr:
		expr->asExpr.of = InferGenericExpr(generics, expr->asExpr.of, templateArgs, wasInferred);
		expr->asExpr.to = InferGenericType(generics, expr->asExpr.to, templateArgs, wasInferred);
		break;
	case DereferenceExpr:
		expr->dereferenceExpr.of = InferGenericExpr(generics, expr->dereferenceExpr.of, templateArgs, wasInferred);
		break;
	case ReferenceExpr:
		expr->referenceExpr.of = InferGenericExpr(generics, expr->referenceExpr.of, templateArgs, wasInferred);
		break;
	case BinaryExpr:
		expr->binaryExpr.left = InferGenericExpr(generics, expr->binaryExpr.left, templateArgs, wasInferred);
		expr->binaryExpr.right = InferGenericExpr(generics, expr->binaryExpr.right, templateArgs, wasInferred);
		break;
	case UnaryExpr:
		expr->unaryExpr.expr = InferGenericExpr(generics, expr->unaryExpr.expr, templateArgs, wasInferred);
		break;
	case GroupedExpr:
		expr->groupedExpr.expr = InferGenericExpr(generics, expr->groupedExpr.expr, templateArgs, wasInferred);
		break;
	case TemplateExpr:
	{
		expr->templateExpr.expr = InferGenericExpr(generics, expr->templateExpr.expr, templateArgs, wasInferred);
		for (Expr*& arg : *expr->templateExpr.templateArgs)
		{
			arg = InferGenericExpr(generics, arg, templateArgs, wasInferred);
		}
		break;
	}
	case TypeExpr:
		expr->typeExpr.type = InferGenericType(generics, expr->typeExpr.type, templateArgs, wasInferred);
		break;
	case FunctionTypeDeclExpr:
	{
		Stmnt* anonFunction = expr->functionTypeDeclExpr.anonFunction;
		Stmnt* funcDecl = anonFunction->anonFunction.decl;
		anonFunction->anonFunction.returnType = InferGenericType(generics, anonFunction->anonFunction.returnType, templateArgs, wasInferred);
		for (Stmnt*& param : *funcDecl->functionDecl.parameters)
		{
			param = InferGenericForDefinition(generics, param, templateArgs, wasInferred);
		}
		break;
	}
	case SizeOfExpr:
		expr->sizeOfExpr.expr = InferGenericExpr(generics, expr->sizeOfExpr.expr, templateArgs, wasInferred);
		break;
	case AlignOfExpr:
		expr->alignOfExpr.expr = InferGenericExpr(generics, expr->alignOfExpr.expr, templateArgs, wasInferred);
		break;
	case OffsetOfExpr:
		expr->offsetOfExpr.type = InferGenericExpr(generics, expr->offsetOfExpr.type, templateArgs, wasInferred);
		expr->offsetOfExpr.expr = InferGenericExpr(generics, expr->offsetOfExpr.expr, templateArgs, wasInferred);
		break;
	case TypeOfExpr:
		expr->typeOfExpr.expr = InferGenericExpr(generics, expr->typeOfExpr.expr, templateArgs, wasInferred);
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