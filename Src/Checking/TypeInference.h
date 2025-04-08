#pragma once
#include "../Syntax/Syntax.h"
#include "../Syntax/GlobalTable.h"
#include "../Syntax/ScopeUtils.h"
#include "CheckerContext.h"

static Type boolType = Type(1, UniqueType::Bool, false);
static Token runtimePackage = "_";
static Token runtimeType = "_Type";

struct TypeInferer
{
	GlobalTable*& globalTable;
	SymbolTable*& symbolTable;
	ScopeUtils& scopeUtils;
	Stmnt*& context;

	TypeInferer(CheckerContext& checkerContext)
		: globalTable(checkerContext.globalTable), symbolTable(checkerContext.symbolTable),
		scopeUtils(checkerContext.scopeUtils), context(checkerContext.currentContext)
	{
	}

	eastl::vector<Stmnt*>* GetTypeDeclarations(Type* type)
	{
		if (type->typeID == TypeID::UnionType || type->typeID == TypeID::ExplicitType)
		{
			return type->explicitType.declarations;
		}

		return nullptr;
	}

	Stmnt* GetDeclarationStmntForExpr(Expr* expr, Token* package = nullptr)
	{
		switch (expr->typeID)
		{
		case IdentifierExpr:
		{
			Token* ident = expr->identifierExpr.identifier;
			Stmnt* stmnt = nullptr;
			if (package)
			{
				stmnt = globalTable->FindStatementForPackage(package, ident);
			}
			else
			{
				stmnt = scopeUtils.FindForName(ident);
			}
			if (!stmnt) return stmnt;

			switch (stmnt->nodeID)
			{
			case Definition:
			{
				Stmnt* state = globalTable->FindStateForType(stmnt->definition.type, symbolTable);
				if (state) return state;
				return stmnt;
			}
			case FunctionStmnt:
			case StateStmnt:
			case ExternFunctionDecl:
			case EnumStmnt:
				return stmnt;
			default:
				return nullptr;
			}
		}
		case SelectorExpr:
		{
			if (scopeUtils.IsPackageExpr(expr))
			{
				Token* package = expr->selectorExpr.on->identifierExpr.identifier;
				return GetDeclarationStmntForExpr(expr->selectorExpr.select, package);
			}
			else
			{
				Stmnt* stmnt = GetDeclarationStmntForExpr(expr->selectorExpr.on);
				Token* ident = expr->selectorExpr.select->identifierExpr.identifier;
				if (stmnt && stmnt->nodeID == StmntID::StateStmnt)
				{
					return globalTable->FindStateMemberOrMethodStmnt(stmnt, ident, symbolTable);
				}
				else 
				{
					Type* type;
					if (stmnt && stmnt->nodeID == StmntID::Definition) type = stmnt->definition.type;
					else type = InferType(expr->selectorExpr.on);

					eastl::vector<Stmnt*>* decls = GetTypeDeclarations(type);
					if (decls)
					{
						for (Stmnt* decl : *decls)
						{
							if (decl->definition.name->val == ident->val)
							{
								Stmnt* state = globalTable->FindStateForType(decl->definition.type, symbolTable);
								if (state) return state;
								return decl;
							}
						}
					}
					else if (type->typeID == TypeID::TemplatedType)
					{
						Stmnt* state = globalTable->FindStateForType(type->templatedType.type, symbolTable);
						if (state)
						{
							return globalTable->FindStateMemberOrMethodStmnt(state, ident, symbolTable);
						}
					}
				}

				return nullptr;
			}
		}
		case TemplateExpr:
			return GetDeclarationStmntForExpr(expr->templateExpr.expr);
		case GroupedExpr:
			return GetDeclarationStmntForExpr(expr->groupedExpr.expr);
		case TypeExpr:
			return globalTable->FindStateForType(expr->typeExpr.type, symbolTable);
		case TypeOfExpr:
			return globalTable->FindStateForType(CreateTypeOfType(), symbolTable);
		case FunctionCallExpr:
		case IndexExpr:
		case BinaryExpr:
		case UnaryExpr:
		case DereferenceExpr:
		case ReferenceExpr:
		{
			Type* inferred = InferType(expr);
			return globalTable->FindStateForType(inferred, symbolTable);
		}
		default:
			break;
		}

		return nullptr;
	}

	inline Type* GetGenericsType(Expr* expr, Type* of)
	{
		if (of->typeID == TypeID::FunctionType)
		{
			Stmnt* func = GetDeclarationStmntForExpr(expr->templateExpr.expr);
			auto& funcType = of->functionType;
			Type* templatedFuncType = symbolTable->CreateTypePtr(TypeID::TemplatedType);
			templatedFuncType->templatedType.templates = expr;
			funcType.returnType = ExpandTypeTemplates(funcType.returnType, func, templatedFuncType);
			for (size_t i = 0; i < funcType.paramTypes->size(); i++)
			{
				Type*& param = funcType.paramTypes->at(i);
				param = ExpandTypeTemplates(param, func, templatedFuncType);
			}

			return of;
		}

		Type* type = symbolTable->CreateTypePtr(TypeID::TemplatedType);
		type->templatedType.templates = symbolTable->CreateExpr(expr->start, ExprID::TemplateExpr);
		type->templatedType.templates->templateExpr.expr = nullptr;
		type->templatedType.templates->templateExpr.templateArgs = expr->templateExpr.templateArgs;
		type->templatedType.type = of;
		return type;
	}

	inline Type* FillFunctionTypeParams(eastl::vector<Stmnt*>* parameters, Type* type)
	{
		type->functionType.paramTypes = symbolTable->CreateVectorPtr<Type>();
		for (Stmnt* stmnt : *parameters)
		{
			type->functionType.paramTypes->push_back(stmnt->definition.type);
		}

		return type;
	}

	inline Type* FunctionToFunctionType(Stmnt* stmnt)
	{
		Type* type = symbolTable->CreateTypePtr(TypeID::FunctionType);
		eastl::vector<Stmnt*>* parameters = nullptr;

		switch (stmnt->nodeID)
		{
		case FunctionStmnt:
		{
			type->functionType.returnType = stmnt->function.returnType;
			parameters = stmnt->function.decl->functionDecl.parameters;
			break;
		}
		case Method:
		{
			type->functionType.returnType = stmnt->method.returnType;
			parameters = stmnt->method.decl->functionDecl.parameters;
			break;
		}
		case ExternFunctionDecl:
		{
			type->functionType.returnType = stmnt->externFunction.returnType;
			parameters = stmnt->externFunction.parameters;
		}
		default:
			break;
		}

		if (!parameters)
		{
			AddError(stmnt->start, "TypeInferer:FunctionToFunctionType Unable to find function declaration for statment: " + ToString(stmnt));
			return nullptr;
		}
		return FillFunctionTypeParams(parameters, type);
	}

	inline Type* GetIdentType(Expr* expr, Token* name)
	{
		if (globalTable->IsGenericOfStmnt(expr, context, symbolTable))
		{
			return symbolTable->CreateTypePtr(TypeID::AnyType);
		}

		Stmnt* stmnt = scopeUtils.FindForName(name);
		if (!stmnt)
		{
			if (globalTable->IsPackage(name->val))
			{
				Type* importedType = symbolTable->CreateTypePtr(TypeID::ImportedType);
				importedType->importedType.packageName = expr->identifierExpr.identifier;
				importedType->importedType.typeName = nullptr;
				return importedType;
			}
			else
			{
				AddError(expr->start, "TypeInferer:GetIdentType Unable to get node for name: " + name->val);
				return nullptr;
			}
		}

		switch (stmnt->nodeID)
		{
		case StmntID::Definition:
			return stmnt->definition.type;
		case StmntID::FunctionStmnt:
		case StmntID::ExternFunctionDecl:
			return FunctionToFunctionType(stmnt);
		case StmntID::EnumStmnt:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::ImportedType);
			type->importedType.packageName = stmnt->package;
			type->importedType.typeName = stmnt->enumStmnt.name;
			return type;
		}
		case StmntID::StateStmnt:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::ImportedType);
			type->importedType.packageName = stmnt->package;
			type->importedType.typeName = stmnt->state.name;
			return type;
		}
		}

		AddError("TypeInferer:GetIdentType unable to find type for name: " + name->val);
		return nullptr;
	}

	Type* GetImportedTypeForSelector(Expr* of, Type* type)
	{
		Token* package = type->importedType.packageName;
		Token* name = of->selectorExpr.select->identifierExpr.identifier;
		Stmnt* stmnt = globalTable->FindStatementForPackage(package, name);
		if (!stmnt)
		{
			AddError(of->start, "TypeInferer:GetImportedTypeForSelector No statement found for expression");
			return type;
		}

		switch (stmnt->nodeID)
		{
		case Definition:
			return stmnt->definition.type;
		case FunctionStmnt:
			return FunctionToFunctionType(stmnt);
		case EnumStmnt:
			type->importedType.typeName = stmnt->enumStmnt.name;
			return type;
		case StateStmnt:
			type->importedType.typeName = stmnt->state.name;
			return type;
		default:
			break;
		}

		AddError(of->start, "TypeInferer:GetImportedTypeForSelector Unable to infer type for selector expression");
		return type;
	}

	Type* DereferenceType(Type* type)
	{
		switch (type->typeID)
		{
		case PointerType:
			return DereferenceType(type->pointerType.type);
		case ValueType:
			return DereferenceType(type->valueType.type);
		default:
			break;
		}

		return type;
	}

	Expr* CreateExprFromTemplates(Expr* toExpand, eastl::vector<Token*>* genericNames,
		eastl::vector<Expr*>* templateArgs)
	{
		Token* name = GetTokenForTemplate(toExpand);
		if (name)
		{
			for (size_t i = 0; i < templateArgs->size(); i++)
			{
				Token* genericName = genericNames->at(i);
				if (genericName->val == name->val)
				{
					return templateArgs->at(i);
				}
			}

		}

		return toExpand;
	}

	Type* CreateTypeFromTemplates(Type* toExpand, eastl::vector<Token*>* genericNames,
		eastl::vector<Expr*>* templateArgs)
	{
		Type* expanded = symbolTable->CreateTypePtr(TypeID::InvalidType);
		*expanded = *toExpand;

		switch (expanded->typeID)
		{
		case NamedType:
		{
			Token* name = expanded->namedType.typeName;
			for (size_t i = 0; i < templateArgs->size(); i++)
			{
				Token* genericName = genericNames->at(i);
				if (genericName->val == name->val)
				{
					Expr* templ = templateArgs->at(i);
					if (templ->typeID == ExprID::TypeExpr)
					{
						expanded = templ->typeExpr.type;
						break;
					}
					else
					{
						// Error?
						break;
					}
				}
			}
			break;
		}
		case ExplicitType:
		{
			eastl::vector<Stmnt*>* decls = symbolTable->CreateVectorPtr<Stmnt>();
			for (Stmnt* decl : *expanded->explicitType.declarations)
			{
				Stmnt* clonedDecl = symbolTable->CreateStmnt(decl->start, decl->nodeID, decl->package, decl->scope);
				*clonedDecl = *decl;
				clonedDecl->definition.type = CreateTypeFromTemplates(clonedDecl->definition.type,
					genericNames, templateArgs);
				decls->push_back(clonedDecl);
			}
			expanded->explicitType.declarations = decls;
			break;
		}
		case UnionType:
		{
			eastl::vector<Stmnt*>* decls = symbolTable->CreateVectorPtr<Stmnt>();
			for (Stmnt* decl : *expanded->unionType.declarations)
			{
				Stmnt* clonedDecl = symbolTable->CreateStmnt(decl->start, decl->nodeID, decl->package, decl->scope);
				*clonedDecl = *decl;
				clonedDecl->definition.type = CreateTypeFromTemplates(clonedDecl->definition.type,
					genericNames, templateArgs);
				decls->push_back(clonedDecl);
			}
			expanded->unionType.declarations = decls;
			break;
		}
		case PointerType:
			expanded->pointerType.type = CreateTypeFromTemplates(expanded->pointerType.type,
				genericNames, templateArgs);
			break;
		case ValueType:
			expanded->valueType.type = CreateTypeFromTemplates(expanded->valueType.type,
				genericNames, templateArgs);
			break;
		case ArrayType:
			expanded->arrayType.type = CreateTypeFromTemplates(expanded->arrayType.type,
				genericNames, templateArgs);
			if (expanded->arrayType.size)
				expanded->arrayType.size = CreateExprFromTemplates(expanded->arrayType.size, genericNames, templateArgs);
			break;
		case TemplatedType:
		{
			expanded->templatedType.type = CreateTypeFromTemplates(expanded->templatedType.type,
				genericNames, templateArgs);
			eastl::vector<Expr*>* newArgs = symbolTable->CreateVectorPtr<Expr>();
			for (Expr* expr : *expanded->templatedType.templates->templateExpr.templateArgs)
			{
				newArgs->push_back(CreateExprFromTemplates(expr, genericNames, templateArgs));
			}
			expanded->templatedType.templates->templateExpr.templateArgs = newArgs;
			break;
		}
		case FunctionType:
		{
			eastl::vector<Type*>* paramTypes = symbolTable->CreateVectorPtr<Type>();
			for (Type* param : *expanded->functionType.paramTypes)
			{
				paramTypes->push_back(CreateTypeFromTemplates(param, genericNames, templateArgs));
			}
			expanded->functionType.paramTypes = paramTypes;
			expanded->functionType.returnType = CreateTypeFromTemplates(expanded->functionType.returnType,
				genericNames, templateArgs);
			break;
		}
		case AnonymousType:
		{
			eastl::vector<Type*>* paramTypes = symbolTable->CreateVectorPtr<Type>();
			for (Type* param : *expanded->anonType.types)
			{
				paramTypes->push_back(CreateTypeFromTemplates(param, genericNames, templateArgs));
			}
			expanded->anonType.types = paramTypes;
			break;
		}
		default:
			break;
		}

		return expanded;
	}

	Type* ExpandTypeTemplates(Type* toExpand, Stmnt* stmnt, Type* expandFrom)
	{
		Type* derefExpandFrom = DereferenceType(expandFrom);
		if (derefExpandFrom->typeID != TypeID::TemplatedType) return toExpand;

		Stmnt* stmntGenerics = GetGenerics(stmnt);
		if (!stmntGenerics)
		{
			AddError(derefExpandFrom->templatedType.templates->start,
				"TypeInference:ExpandTypeTemplates Templated epression found for statement without generics");
			return toExpand;
		}

		eastl::vector<Token*>* genericNames = stmntGenerics->generics.names;
		eastl::vector<Expr*>* templateArgs = derefExpandFrom->templatedType.templates->templateExpr.templateArgs;

		if (templateArgs->size() > genericNames->size())
		{
			AddError(derefExpandFrom->templatedType.templates->start,
				"TypeInference:ExpandTypeTemplates More template arguments provided than generic names for statement");
			return toExpand;
		}
		return CreateTypeFromTemplates(symbolTable->CloneType(toExpand), genericNames, templateArgs);
	}

	inline Type* GetSelectorType(Expr* of, Type* type)
	{
		auto& selector = of->selectorExpr;
		StringView& name = selector.select->identifierExpr.identifier->val;

		if (type->typeID == TypeID::ImportedType && !type->importedType.typeName)
		{
			return GetImportedTypeForSelector(of, type);
		}
		else if (type->typeID == TypeID::ExplicitType)
		{
			Stmnt* explicitMember = FindTypeMember(type->explicitType.declarations, name);
			if (!explicitMember)
			{
				AddError(of->start, "TypeInferer:GetSelectorType No member found for explicit type");
				return nullptr;
			}
			return explicitMember->definition.type;
		}
		else if (type->typeID == TypeID::UnionType)
		{
			Stmnt* unionMember = FindTypeMember(type->unionType.declarations, name);
			if (!unionMember)
			{
				AddError(of->start, "TypeInferer:GetSelectorType No member found for union type");
				return nullptr;
			}
			return unionMember->definition.type;
		}
		else if (IsAny(type))
		{
			return type;
		}

		Stmnt* state = globalTable->FindStateForType(type, symbolTable);
		if (!state)
		{
			Stmnt* enumStmnt = globalTable->FindEnumForType(type, symbolTable);
			if (enumStmnt && HasEnumMember(enumStmnt, name))
			{
				return enumStmnt->enumStmnt.type;
			}

			if (globalTable->IsGenericOfStmnt(type, context, symbolTable))
			{
				return symbolTable->CreateTypePtr(TypeID::AnyType);
			}

			AddError(of->start, "TypeInferer:GetSelectorType No state found for type: " + ToString(type));
			return nullptr;
		}

		Stmnt* member = FindStateMember(state, name);
		if (member)
		{
			if (state->state.generics)
				return ExpandTypeTemplates(member->definition.type, state, type);
			return member->definition.type;
		}
		else
		{
			StateSymbol* stateSymbol = globalTable->FindScopedStateSymbol(state->state.name, symbolTable);
			Stmnt* method = FindStateMethod(stateSymbol, name);
			if (!method)
			{
				if (scopeUtils.IsPackageExpr(of) && type->typeID == TypeID::ImportedType)
				{
					Token* package = scopeUtils.GetPackageFromExpr(of);
					if (package->val != type->importedType.packageName->val)
					{
						type->importedType.packageName = package;
						type->importedType.typeName = nullptr;
						return GetSelectorType(of, type);
					}
					return GetImportedTypeForSelector(of, type);
				}
				AddError(of->start, "Unable to find member or method for type: " + ToString(type));
				return nullptr;
			}

			Type* funcType = symbolTable->CloneType(FunctionToFunctionType(method));
			return ExpandTypeTemplates(funcType, state, type);
		}
	}

	inline Type* GetIndexType(Expr* of, Type* type)
	{
		return GetIndexTypeAccessArray(of, type);
	}

	inline Type* GetIndexTypeAccessArray(Expr* of, Type* type)
	{
		if (!of->indexExpr.index)
		{
			AddError(of->start, "TypeInferer:GetIndexTypeAccessArray No expression for indexing array");
			return type;
		}

		switch (type->typeID)
		{
		case ImportedType:
		case NamedType:
		case TemplatedType:
		{
			Type* typeOfIndex = InferType(of->indexExpr.index);
			Type* indexType = GetStateOperatorType(of->start, UniqueType::Array, type, typeOfIndex);
			return indexType;
		}
		case PointerType:
			return type;
		case ValueType:
			return GetIndexTypeAccessArray(of, type->valueType.type);
		case ArrayType:
			return type->arrayType.type;
		case PrimitiveType:
		{
			if (type->primitiveType.type == UniqueType::String)
			{
				Type* typeOfIndex = InferType(of->indexExpr.index);
				Type* indexType = FindOperatorOverloadReturnType(globalTable->stringSymbol->state,
					UniqueType::Array, typeOfIndex);
				return indexType;
			}
			break;
		}
		case AnyType:
			return type;
		}

		AddError(of->start, "TypeInferer:GetIndexTypeAccessArray Not a valid type to access index of: " + ToString(type));
		return nullptr;
	}

	inline Type* GetFunctionCallType(Expr* of, Type* type)
	{
		auto& function = of->functionCallExpr;

		if (!type) return nullptr;
		switch (type->typeID)
		{
		case FunctionType:
			return type->functionType.returnType;
		case TemplatedType:
		{
			if (of->functionCallExpr.function->typeID == ExprID::TemplateExpr) return type;
			break;
		}
		default:
			return type;
		}

		return nullptr;
	}

	Type* EvalType(Expr* expr)
	{
		Type* type = nullptr;
		switch (expr->typeID)
		{
		case PrimitiveExpr:
			type = symbolTable->CreatePrimitive(expr->primitiveExpr.primitive->uniqueType);
			break;
		case TypeExpr:
			type = expr->typeExpr.type;
			break;
		case TemplateExpr:
			type = GetGenericsType(expr, InferType(expr->templateExpr.expr));
			break;
		case IdentifierExpr:
			type = GetIdentType(expr, expr->identifierExpr.identifier);
			break;
		case SelectorExpr:
			type = GetSelectorType(expr, InferType(expr->selectorExpr.on));
			break;
		case IndexExpr:
			type = GetIndexType(expr, InferType(expr->indexExpr.of));
			break;
		case FunctionCallExpr:
			type = GetFunctionCallType(expr, InferType(expr->functionCallExpr.function));
			break;
		default:
			break;
		}

		if (!type)
		{
			AddError(expr->start, "TypeInferer:EvalType unable to create type for expression: " + ToString(expr));
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		}

		if (type->typeID == TypeID::ValueType) type = type->valueType.type;
		return type;
	}

	Type* ConvertGenericsToAny(Type* type, Stmnt* stmnt)
	{
		if (globalTable->IsGenericOfStmnt(type, stmnt, symbolTable))
		{
			return symbolTable->CreateTypePtr(TypeID::AnyType);
		}

		Type* converted = symbolTable->CreateTypePtr(TypeID::InvalidType);
		*converted = *type;

		switch (type->typeID)
		{
		case ExplicitType:
		{
			eastl::vector<Stmnt*>* decls = symbolTable->CreateVectorPtr<Stmnt>();
			for (Stmnt* decl : *converted->explicitType.declarations)
			{
				Stmnt* clonedDecl = symbolTable->CreateStmnt(decl->start, decl->nodeID, decl->package, decl->scope);
				*clonedDecl = *decl;
				clonedDecl->definition.type = ConvertGenericsToAny(clonedDecl->definition.type, stmnt);
				decls->push_back(clonedDecl);
			}
			converted->explicitType.declarations = decls;
			break;
		}
		case PointerType:
			converted->pointerType.type = ConvertGenericsToAny(converted->pointerType.type, stmnt);
			break;
		case ValueType:
			converted->valueType.type = ConvertGenericsToAny(converted->valueType.type, stmnt);
			break;
		case ArrayType:
			converted->arrayType.type = ConvertGenericsToAny(converted->arrayType.type, stmnt);
			break;
		case TemplatedType:
		{
			converted->templatedType.type = ConvertGenericsToAny(converted->templatedType.type, stmnt);
			break;
		}
		case FunctionType:
		{
			eastl::vector<Type*>* paramTypes = symbolTable->CreateVectorPtr<Type>();
			for (Type* param : *converted->functionType.paramTypes)
			{
				paramTypes->push_back(ConvertGenericsToAny(param, stmnt));
			}
			converted->functionType.paramTypes = paramTypes;
			converted->functionType.returnType = ConvertGenericsToAny(converted->functionType.returnType, stmnt);
			break;
		}
		case AnonymousType:
		{
			eastl::vector<Type*>* paramTypes = symbolTable->CreateVectorPtr<Type>();
			for (Type* param : *converted->anonType.types)
			{
				paramTypes->push_back(ConvertGenericsToAny(param, stmnt));
			}
			converted->anonType.types = paramTypes;
			break;
		}
		default:
			break;

		}

		return converted;
	}

	Type* FindOperatorOverloadReturnType(Stmnt* stateStmnt, UniqueType op, Type* rhs = nullptr)
	{
		StateSymbol* state = globalTable->FindScopedStateSymbol(stateStmnt->state.name, symbolTable);
		if (state)
		{
			for (Stmnt* opNode : state->operators)
			{
				auto& stateOp = opNode->stateOperator;
				if (stateOp.op->uniqueType == op)
				{
					if (!rhs) return stateOp.returnType;
					else if (stateOp.decl->functionDecl.parameters->size() > 1 &&
						IsAssignable(stateOp.decl->functionDecl.parameters->at(1)->definition.type, rhs, stateStmnt))
						return stateOp.returnType;
				}
			}
		}

		return nullptr;
	}

	Type* GetStateOperatorType(Token* token, UniqueType op, Type* type, Type* rhs = nullptr)
	{

		Stmnt* node = globalTable->FindStateForType(type, symbolTable);
		if (node)
		{
			Type* returnType = FindOperatorOverloadReturnType(node, op, rhs);
			if (returnType) return ConvertGenericsToAny(ExpandTypeTemplates(returnType, node, type), node);

			AddError(token, "TypeInferer:GetStateOperatorType No operator found for named type: " + ToString(type));
		}
		else
		{
			if (globalTable->IsGenericOfStmnt(type, context, symbolTable) ||
				globalTable->IsGenericOfStmnt(rhs, context, symbolTable))
			{
				return symbolTable->CreateTypePtr(TypeID::AnyType);
			}
			AddError(token, "TypeInferer:GetStateOperatorType State not found for named type: " + ToString(type));
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	inline Type* GetPrimitiveOperatorType(Token* op, Type* left, Type* right)
	{
		if (IsBooleanOperator(op)) return &boolType;

		auto& lPrim = left->primitiveType;
		auto& rPrim = right->primitiveType;
		switch (lPrim.type)
		{
		case UniqueType::Void:
			return left;
		case UniqueType::Bool:
			return left;
		case UniqueType::Byte:
		case UniqueType::Int:
		case UniqueType::Int16:
		case UniqueType::Int32:
		case UniqueType::Int64:
		case UniqueType::Int128:
		{
			if (IsFloat(right)) return right;
			else if (IsString(right)) return right;
			else if (!rPrim.isSigned) return left;
			else if (lPrim.size > rPrim.size) return left;
			else return right;
		}
		case UniqueType::Ubyte:
		case UniqueType::Uint:
		case UniqueType::Uint16:
		case UniqueType::Uint32:
		case UniqueType::Uint64:
		case UniqueType::Uint128:
		{
			if (IsFloat(right)) return right;
			else if (IsString(right)) return right;
			else if (rPrim.isSigned) return right;
			else if (lPrim.size > rPrim.size) return left;
			else return right;
		}
		case UniqueType::Float:
		case UniqueType::Float32:
		case UniqueType::Float64:
		{
			if (IsInt(right)) return left;
			else if (IsString(right)) return right;
			else if (lPrim.size > rPrim.size) return left;
			else return right;
		}
		case UniqueType::String:
			return left;
		default:
			break;
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	Type* GetOperatorType(Token* op, Type* left, Type* right)
	{
		if (left->typeID == TypeID::ValueType)
			return GetOperatorType(op, left->valueType.type, right);

		if (right->typeID == TypeID::ValueType)
			return GetOperatorType(op, left, right->valueType.type);

		if (IsAny(left)) return left;
		if (IsAny(right)) return right;

		switch (left->typeID)
		{
		case PrimitiveType:
		{
			if (right->typeID == TypeID::PrimitiveType) return GetPrimitiveOperatorType(op, left, right);
			else if (IsIntLike(right)) return right;
			else
			{
				Stmnt* enumStmnt = globalTable->FindEnumForType(right, symbolTable);
				if (enumStmnt)
				{
					return GetPrimitiveOperatorType(op, left, enumStmnt->enumStmnt.type);
				}
				AddError(op, "TypeInferer:GetOperatorType Expected right hand side to be a primitive for operator");
			}

			break;
		}
		case ImportedType:
		case NamedType:
		{
			Stmnt* enumStmnt = globalTable->FindEnumForType(left, symbolTable);
			if (enumStmnt)
			{
				return GetOperatorType(op, enumStmnt->enumStmnt.type, right);
			}

			return GetStateOperatorType(op, op->uniqueType, left, right);
		}
		case UnionType:
		case ExplicitType:
		case ImplicitType:
			AddError(op, "Binary operators are not valid with explicit, implicit or union types");
			break;
		case PointerType:
		{
			if (IsBooleanOperator(op)) return &boolType;
			if (IsInt(right) || IsIntLike(right))
			{
				return left;
			}
			break;
		}
		break;
		case ValueType:
			return GetOperatorType(op, left->valueType.type, right);
		case ArrayType:
			break;
		case TemplatedType:
			return GetOperatorType(op, left->templatedType.type, right);
		case FunctionType:
			if (IsBooleanOperator(op)) return &boolType;
			AddError(op, "You can't multiply functions");
			break;
		default:
			break;
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	Type* GetUnaryType(Token* op, Type* type)
	{
		switch (type->typeID)
		{
		case PrimitiveType:
			if (op->uniqueType == UniqueType::Not) return symbolTable->CreatePrimitive(UniqueType::Bool);
			return type;
		case ImportedType:
		case NamedType:
			return GetStateOperatorType(op, op->uniqueType, type);
		case PointerType:
			if (op->uniqueType == UniqueType::Not) return symbolTable->CreatePrimitive(UniqueType::Bool);
			return type;
		case ValueType:
			return GetUnaryType(op, type->valueType.type);
		case TemplatedType:
			return GetUnaryType(op, type->templatedType.type);
		case AnyType:
			return type;
		default:
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		}
	}

	Type* CreateTypeOfType()
	{
		Type* typeOfType = symbolTable->CreateTypePtr(TypeID::ImportedType);
		typeOfType->importedType.packageName = &runtimePackage;
		typeOfType->importedType.typeName = &runtimeType;

		Type* typeOfTypePtr = symbolTable->CreateTypePtr(TypeID::PointerType);
		typeOfTypePtr->pointerType.type = typeOfType;
		return typeOfTypePtr;
	}

	Type* InferType(Expr* of)
	{
		switch (of->typeID)
		{
		case InvalidExpr:
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		case PrimitiveExpr:
		case IdentifierExpr:
		case SelectorExpr:
		case IndexExpr:
		case FunctionCallExpr:
		case TypeExpr:
		case TemplateExpr:
			return EvalType(of);
		case LiteralExpr:
		{
			auto& literal = of->literalExpr;
			UniqueType uniqueType;
			switch (literal.val->uniqueType)
			{
			case IntLiteral:
				uniqueType = UniqueType::Int;
				break;
			case FloatLiteral:
				uniqueType = UniqueType::Float;
				break;
			case HexLiteral:
				uniqueType = UniqueType::Int;
				break;
			case ByteLiteral:
				uniqueType = UniqueType::Byte;
				break;
			case StringLiteral:
				uniqueType = UniqueType::String;
				break;
			case TrueLiteral:
			case FalseLiteral:
				uniqueType = UniqueType::Bool;
				break;
			default:
				uniqueType = UniqueType::Void;
				break;
			}

			return symbolTable->CreatePrimitive(uniqueType);
		}
		case NewExpr:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::PointerType);
			type->pointerType.type = InferType(of->newExpr.primaryExpr);
			return type;
		}
		case FixedExpr:
		{
			Type* fixedArrType = InferType(of->fixedExpr.atExpr);
			if (fixedArrType->typeID != TypeID::ArrayType)
			{
				AddError(of->start, "TypeInferer:InferType fixed expressions must evaluate to a fixed sized array type");
				return symbolTable->CreateTypePtr(TypeID::InvalidType);
			}

			Type* fixedType = symbolTable->CreateTypePtr(TypeID::PointerType);
			fixedType->pointerType.type = fixedArrType->arrayType.type;
			return fixedType;
		}
		case TypeLiteralExpr:
		{
			if (!of->typeLiteralExpr.array)
			{
				if (of->typeLiteralExpr.typed)
				{
					return InferType(of->typeLiteralExpr.typed);
				}

				Type* anonType = symbolTable->CreateTypePtr(TypeID::AnonymousType);
				anonType->anonType.types = symbolTable->CreateVectorPtr<Type>();

				for (Expr* value : *of->typeLiteralExpr.values)
				{
					anonType->anonType.types->push_back(InferType(value));
				}

				return anonType;
			}
			else
			{
				size_t size = of->typeLiteralExpr.values->size();
				if (size > 0)
				{
					Type* arrType = symbolTable->CreateTypePtr(TypeID::ArrayType);
					if (of->typeLiteralExpr.typed)
					{
						arrType->arrayType.type = InferType(of->typeLiteralExpr.typed);
					}
					else
					{
						arrType->arrayType.type = InferType(of->typeLiteralExpr.values->at(0));
					}

					arrType->arrayType.size = symbolTable->CreateIntLiteralExpr(size, of->start);
					return arrType;
				}
			}

			break;
		}
		case ExplicitTypeExpr:
		{
			Type* explicitType = symbolTable->CreateTypePtr(TypeID::ExplicitType);
			explicitType->explicitType.declarations = symbolTable->CreateVectorPtr<Stmnt>();
			for (Stmnt* param : *of->explicitTypeExpr.values)
			{
				auto& def = param->definition;
				Stmnt* decl = symbolTable->CreateStmnt(param->start, StmntID::Definition, param->package, param->scope);
				decl->definition.assignment = nullptr;
				decl->definition.name = def.name;
				if (def.type->typeID == TypeID::UnknownType)
				{
					decl->definition.type = InferType(def.assignment);
				}
				else
				{
					decl->definition.type = def.type;
				}

				explicitType->explicitType.declarations->push_back(decl);
			}
			return explicitType;
		}
		case AsExpr:
			return of->asExpr.to;
		case DereferenceExpr:
		{
			Type* type = InferType(of->dereferenceExpr.of);
			if (type->typeID == TypeID::PointerType) return type->pointerType.type;
			return type;
		}
		case ReferenceExpr:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::PointerType);
			type->pointerType.type = InferType(of->referenceExpr.of);
			return type;
		}
		case BinaryExpr:
		{
			Type* left = InferType(of->binaryExpr.left);
			Type* right = InferType(of->binaryExpr.right);
			return GetOperatorType(of->binaryExpr.op, left, right);
		}
		case UnaryExpr:
		{
			Type* type = InferType(of->unaryExpr.expr);
			return GetUnaryType(of->unaryExpr.op, type);
		}
		case GroupedExpr:
			return InferType(of->groupedExpr.expr);
		case FunctionTypeDeclExpr:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::FunctionType);
			auto& anonFunction = of->functionTypeDeclExpr.anonFunction->anonFunction;
			type->functionType.returnType = anonFunction.returnType;
			type->functionType.paramTypes = symbolTable->CreateVectorPtr<Type>();
			for (Stmnt* param : *anonFunction.decl->functionDecl.parameters)
			{
				type->functionType.paramTypes->push_back(param->definition.type);
			}
			return type;
		}
		case CompileExpr:
			return of->compileExpr.compile->compileStmnt.returnType;
		case SizeOfExpr:
		case AlignOfExpr:
		case OffsetOfExpr:
			return symbolTable->CreatePrimitive(UniqueType::Int);
		case TypeOfExpr:
			return CreateTypeOfType();
		default:
			break;
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}

	bool IsArrayStateType(Type* type)
	{
		return globalTable->FindStateForType(type, symbolTable) ==
			globalTable->GetArrayState();
	}

	bool IsStringStateType(Type* type)
	{
		return globalTable->FindStateForType(type, symbolTable) ==
			globalTable->GetStringState();
	}

	bool IsTypeGenericOf(Stmnt* stmnt, Type* type)
	{
		if (!stmnt) return globalTable->IsGenericOfStmnt(type, context, symbolTable);
		if (!type || type->typeID != TypeID::NamedType) return false;

		return globalTable->IsGenericOfStmnt(type, stmnt, symbolTable);
	}

	bool IsExprGenericOf(Stmnt* stmnt, Expr* expr)
	{
		if (!stmnt) return globalTable->IsGenericOfStmnt(expr, context, symbolTable);
		if (!expr) return false;

		return globalTable->IsGenericOfStmnt(expr, stmnt, symbolTable);
	}

	inline bool IsComplexType(Type* type)
	{
		TypeID id = type->typeID;
		return id == TypeID::NamedType || id == TypeID::ImportedType || id == TypeID::ExplicitType ||
			id == TypeID::AnonymousType;
	}

	eastl::vector<Type*> UnwrapComplexType(Type* type)
	{
		if (type->typeID == TypeID::AnonymousType) return *type->anonType.types;
		eastl::vector<Type*> types = eastl::vector<Type*>();

		eastl::vector<Stmnt*>* decls = nullptr;
		if (type->typeID == TypeID::NamedType || type->typeID == TypeID::ImportedType)
		{
			Stmnt* state = globalTable->FindStateForType(type, symbolTable);
			if (!state)
			{
				AddError("CheckerUtils:UnwrapComplexType Unable to find state for: " + ToString(type));
				return types;
			}
			decls = state->state.members;
		}
		else
		{
			decls = type->explicitType.declarations;
		}

		for (Stmnt* decl : *decls) types.push_back(decl->definition.type);
		return types;
	}

	bool IsAssignable(Type* left, Type* right, Stmnt* stmntContext = nullptr)
	{
		if (!left || !right) return false;

		if (*left == *right) return true;

		if (IsAny(left) || IsAny(right)) return true;

		if (left->typeID == TypeID::ValueType)
			return IsAssignable(left->valueType.type, right, stmntContext);

		if (right->typeID == TypeID::ValueType)
			return IsAssignable(left, right->valueType.type, stmntContext);

		if (left->typeID == TypeID::NamedType || left->typeID == TypeID::ImportedType)
		{
			Stmnt* enumStmnt = globalTable->FindEnumForType(left, symbolTable);
			if (enumStmnt)
			{
				return IsAssignable(enumStmnt->enumStmnt.type, right, stmntContext);
			}
		}

		if (right->typeID == TypeID::NamedType || right->typeID == TypeID::ImportedType)
		{
			Stmnt* enumStmnt = globalTable->FindEnumForType(right, symbolTable);
			if (enumStmnt)
			{
				return IsAssignable(left, enumStmnt->enumStmnt.type, stmntContext);
			}
		}

		if (left->typeID == TypeID::UnionType)
		{
			for (Stmnt* decl : *left->unionType.declarations)
			{
				if (IsAssignable(decl->definition.type, right, stmntContext)) return true;
			}

			return false;
		}

		if (right->typeID == TypeID::UnionType)
		{
			for (Stmnt* decl : *right->unionType.declarations)
			{
				if (IsAssignable(left, decl->definition.type, stmntContext)) return true;
			}

			return false;
		}

		if (left->typeID == TypeID::PrimitiveType && right->typeID == TypeID::PrimitiveType)
		{
			// Void can only be assigned to void which would be caught in the type equality check above
			if (left->primitiveType.type == UniqueType::Void ||
				right->primitiveType.type == UniqueType::Void) return false;

			bool isStringL = IsString(left);
			bool isStringR = IsString(right);
			return isStringL == isStringR;
		}

		if (IsVoidPtr(left))
		{
			return right->typeID == TypeID::PointerType || right->typeID == TypeID::FunctionType;
		}

		if (IsVoidPtr(right))
		{
			return left->typeID == TypeID::PointerType || left->typeID == TypeID::FunctionType;
		}

		if (left->typeID == TypeID::PointerType && right->typeID == TypeID::PointerType)
		{
			Type* leftPointeeType = left->pointerType.type;
			Type* rightPointeeType = right->pointerType.type;

			return IsAssignable(leftPointeeType, rightPointeeType, stmntContext);
		}

		if (left->typeID == TypeID::ArrayType && right->typeID == TypeID::ArrayType)
		{
			return IsAssignable(left->arrayType.type, right->arrayType.type, stmntContext);
		}

		if (IsArrayStateType(left))
		{
			return right->typeID == TypeID::ArrayType || IsArrayStateType(right);
		}

		if (IsStringStateType(left))
		{
			return IsString(right) || IsStringStateType(right);
		}

		if (left->typeID == TypeID::TemplatedType && right->typeID == TypeID::TemplatedType)
		{
			if (!IsAssignable(left->templatedType.type, right->templatedType.type, stmntContext))
				return false;

			eastl::vector<Expr*>* leftTemplateArgs = left->templatedType.templates->templateExpr.templateArgs;
			eastl::vector<Expr*>* rightTemplateArgs = right->templatedType.templates->templateExpr.templateArgs;

			if (leftTemplateArgs->size() != rightTemplateArgs->size()) return false;

			Stmnt* state = globalTable->FindStateForType(left, symbolTable);

			for (size_t i = 0; i < leftTemplateArgs->size(); i++)
			{
				Expr* lTempl = leftTemplateArgs->at(i);
				Expr* rTempl = rightTemplateArgs->at(i);
				if (IsExprGenericOf(state, lTempl) || IsExprGenericOf(state, rTempl) ||
					IsExprGenericOf(stmntContext, lTempl) || IsExprGenericOf(stmntContext, rTempl))
					continue;

				if (!IsAssignable(InferType(lTempl), InferType(rTempl))) return false;
			}

			return true;
		}

		if (IsTypeGenericOf(stmntContext, left) || IsTypeGenericOf(stmntContext, right)) return true;

		if (IsComplexType(left) && IsComplexType(right))
		{
			eastl::vector<Type*> leftTypes = UnwrapComplexType(left);
			eastl::vector<Type*> rightTypes = UnwrapComplexType(right);

			if (rightTypes.size() != leftTypes.size()) return false;

			for (size_t i = 0; i < leftTypes.size(); i++)
			{
				Type* lType = leftTypes.at(i);
				Type* rType = rightTypes.at(i);
				if (!IsAssignable(lType, rType, stmntContext)) return false;
			}

			return true;
		}

		// Dangerous because casting information can be lost, but not sure how to check for 
		// generic types being used as function type return/params otherwise
		if (left->typeID == TypeID::FunctionType && right->typeID == TypeID::FunctionType)
		{
			auto& l = left->functionType;
			auto& r = right->functionType;
			if (!IsAssignable(l.returnType, r.returnType, stmntContext)) return false;
			if (l.paramTypes->size() != r.paramTypes->size()) return false;
			for (int i = 0; i < l.paramTypes->size(); i++)
			{
				if (!IsAssignable(l.paramTypes->at(i), r.paramTypes->at(i), stmntContext)) return false;
			}
			return true;
		}

		return false;
	}
};
