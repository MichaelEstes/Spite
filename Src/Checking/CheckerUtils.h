#pragma once
#pragma once

#include <EASTL/deque.h>
#include "../Intermediate/Syntax.h"
#include "../Intermediate/GlobalTable.h"


struct CheckerUtils
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>>& scopeQueue;

	CheckerUtils(GlobalTable* globalTable, SymbolTable* symbolTable,
		eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>>& scopeQueue)
		: globalTable(globalTable), symbolTable(symbolTable), scopeQueue(scopeQueue) {}

	Stmnt* GetDeclarationStmntForExpr(Expr* expr)
	{

		switch (expr->typeID)
		{
		case IdentifierExpr:
		{
			StringView& ident = expr->identifierExpr.identifier->val;
			Stmnt* stmnt = FindNodeForName(ident);
			if (!stmnt) return stmnt;

			switch (stmnt->nodeID)
			{
			case Definition:
				return GetStmntForType(stmnt->definition.type, expr->start);
			case FunctionStmnt:
			case StateStmnt:
				return stmnt;
			default:
				AddError(expr->start, "CheckerUtils:GetDeclarationStmntForExpr Found invalid statement for identifier : " + ident);
				return stmnt;

			}
		}
		case SelectorExpr:
		{
			Stmnt* stmnt = GetDeclarationStmntForExpr(expr->selectorExpr.on);
			if (stmnt && stmnt->nodeID == StmntID::StateStmnt)
			{
				return FindStateMemberOrMethodStmnt(stmnt, expr->selectorExpr.select->identifierExpr.identifier);
			}
			else if (IsPackageExpr(expr))
			{
				StringView& package = expr->selectorExpr.on->identifierExpr.identifier->val;
				StringView& name = expr->selectorExpr.on->identifierExpr.identifier->val;
				//Package is being selected, find in global table
				return globalTable->FindStateOrFunction(package, name);
			}
			else
			{
				AddError(expr->start, "CheckerUtils: No declaration found for selector expression");
				return nullptr;
			}
		}
		default:
			break;
		}

		AddError(expr->start, "CheckerUtils: Cannot find statement for expression");
		return nullptr;
	}

	Stmnt* GetStmntForType(Type* type, Token* start)
	{
		switch (type->typeID)
		{
		case NamedType:
			return symbolTable->FindState(type->namedType.typeName->val);
		case ImportedType:
			return globalTable->FindState(type->importedType.packageName->val, type->importedType.typeName->val);
		case PointerType:
			return GetStmntForType(type->pointerType.type, start);
		case ValueType:
			return GetStmntForType(type->valueType.type, start);
		case ArrayType:
			return GetStmntForType(type->arrayType.type, start);
		case GenericsType:
			return GetStmntForType(type->arrayType.type, start);
		default:
			AddError(start, "CheckerUtils:GetStmntForType Invalid type to find statement for");
			return nullptr;
		}
	}

	inline bool IsPackageExpr(Expr* expr)
	{
		return expr->typeID == ExprID::SelectorExpr && expr->selectorExpr.on->typeID == ExprID::IdentifierExpr &&
			globalTable->IsPackage(expr->selectorExpr.on->identifierExpr.identifier->val);
	}

	inline Type* GetOuterReturnType(Stmnt* node)
	{
		Stmnt* outer = GetOuterScope(node);
		if (outer) return GetReturnType(outer);
		return nullptr;
	}

	inline Type* GetReturnType(Stmnt* node)
	{
		switch (node->nodeID)
		{
		case StmntID::FunctionStmnt:
			return node->function.returnType;
		case StmntID::Method:
			return node->method.returnType;
		case StmntID::StateOperator:
			return node->stateOperator.returnType;
		case StmntID::AnonFunction:
			return node->anonFunction.returnType;
		case StmntID::CompileStmnt:
			return node->compileStmnt.returnType;
		default:
			break;
		}

		return nullptr;
	}

	inline bool IsOuterScope(Stmnt* node)
	{
		StmntID nodeID = node->nodeID;
		return nodeID == StmntID::FunctionStmnt || nodeID == StmntID::Method ||
			nodeID == StmntID::StateOperator || nodeID == StmntID::AnonFunction ||
			nodeID == StmntID::CompileStmnt;
	}

	Stmnt* GetOuterScope(Stmnt* node)
	{
		while (node && !IsOuterScope(node)) node = node->scope;
		return node;
	}

	inline Type* GetGenericsType(Expr* expr, Type* of)
	{
		if (of->typeID == TypeID::FunctionType)
		{
			Stmnt* func = GetDeclarationStmntForExpr(expr->genericsExpr.expr);
			if (!func)
			{
				AddError(expr->start, "CheckerUtils:GetGenericsType No function found for expression");
				return nullptr;
			}
			Stmnt* generics = GetGenerics(func);
			if (!generics)
			{
				AddError(expr->start, "CheckerUtils:GetGenericsType Template expression used on non-templated function");
				return nullptr;
			}

			auto& funcType = of->functionType;

			eastl::vector<Expr*>* templateArgs = expr->genericsExpr.templateArgs;
			if (templateArgs->size() != generics->generics.names->size())
			{
				AddError(expr->start, "CheckerUtils:GetGenericsType Invalid number of template arguments for generic type");
				return nullptr;
			}
			for (size_t i = 0; i < generics->generics.names->size(); i++)
			{
				Token* name = generics->generics.names->at(i);
				if (funcType.returnType->typeID == TypeID::NamedType &&
					funcType.returnType->namedType.typeName->val == name->val)
				{
					funcType.returnType = InferType(templateArgs->at(i));
				}

				for (size_t j = 0; j < funcType.paramTypes->size(); j++)
				{
					Type*& param = funcType.paramTypes->at(i);
					if (param->typeID == TypeID::NamedType &&
						param->namedType.typeName->val == name->val)
					{
						param = InferType(templateArgs->at(i));
					}
				}

			}
			
			return of;
		}

		Type* type = symbolTable->CreateTypePtr(TypeID::GenericsType);
		type->genericsType.generics = symbolTable->CreateExpr(expr->start, ExprID::GenericsExpr);
		type->genericsType.generics->genericsExpr.expr = nullptr;
		type->genericsType.generics->genericsExpr.open = expr->genericsExpr.open;
		type->genericsType.generics->genericsExpr.close = expr->genericsExpr.close;
		type->genericsType.generics->genericsExpr.templateArgs = expr->genericsExpr.templateArgs;
		type->genericsType.type = of;
		return type;
	}

	inline Type* FunctionToFunctionType(Stmnt* node)
	{
		Type* type = symbolTable->CreateTypePtr(TypeID::FunctionType);
		Stmnt* decl = nullptr;

		switch (node->nodeID)
		{
		case FunctionStmnt:
		{
			type->functionType.returnType = node->function.returnType;
			decl = node->function.decl;
			break;
		}
		case Method:
		{
			type->functionType.returnType = node->method.returnType;
			decl = node->method.decl;
			break;
		}
		default:
			break;
		}

		if (!decl)
		{
			AddError(node->start, "PackageChecker:FunctionToFunctionType Unable to find function declaration for statment: " + ToString(node));
			return nullptr;
		}
		return FillFunctionTypeParams(decl, type);
	}

	inline Type* FillFunctionTypeParams(Stmnt* decl, Type* type)
	{
		type->functionType.paramTypes = symbolTable->CreateVectorPtr<Type*>();
		for (Stmnt* node : *decl->functionDecl.parameters)
		{
			type->functionType.paramTypes->push_back(node->definition.type);
		}

		return type;
	}

	inline Stmnt* FindInScope(StringView& val)
	{
		for (auto it = scopeQueue.rbegin(); it != scopeQueue.rend(); it++)
		{
			if (auto entry = it->find(val); entry != it->end())
			{
				return entry->second;
			}
		}

		return nullptr;
	}

	inline Stmnt* FindStateMemberOrMethodStmnt(Stmnt* state, Token* name)
	{
		Stmnt* stmnt = FindStateMember(state, name->val);
		if (stmnt)
		{
			return GetStmntForType(stmnt->definition.type, name);
		}

		StateSymbol* stateSymbol = symbolTable->FindStateSymbol(state->state.name->val);
		stmnt = FindStateMethod(stateSymbol, name->val);
		return stmnt;
	}

	inline Stmnt* FindStateMethod(StateSymbol* of, StringView& val)
	{
		auto& methods = of->methods;
		for (Stmnt* node : methods)
		{
			if (node->method.name->val == val) return node;
		}

		return nullptr;
	}

	inline Stmnt* FindTypeMember(eastl::vector<Stmnt*>* members, StringView& val)
	{
		for (Stmnt* node : *members)
		{
			if (node->definition.name->val == val) return node;
		}

		return nullptr;
	}

	inline Stmnt* FindStateMember(Stmnt* of, StringView& val)
	{
		return FindTypeMember(of->state.members, val);
	}

	inline Stmnt* FindNodeForName(StringView& val)
	{
		Stmnt* node = FindInScope(val);
		if (node) return node;

		
		node = symbolTable->FindStateOrFunction(val);
		if (node) return node;

		for (Stmnt* import : symbolTable->imports)
		{
			StringView& package = import->importStmnt.packageName->val;
			node = globalTable->FindStateOrFunction(package, val);
			if (node) return node;
		}

		return nullptr;
	}

	inline Type* GetIdentType(Expr* expr, StringView& name)
	{
		Stmnt* node = FindNodeForName(name);
		if (!node)
		{
			if (globalTable->IsPackage(name))
			{
				Type* importedType = symbolTable->CreateTypePtr(TypeID::ImportedType);
				importedType->importedType.packageName = expr->identifierExpr.identifier;
				return importedType;
			}
			else
			{
				AddError(expr->start, "PackageChecker:GetIdentType Unable to get node for name: " + name);
				return nullptr;
			}
		}

		switch (node->nodeID)
		{
		case StmntID::Definition:
			return node->definition.type;
		case StmntID::FunctionStmnt:
			return FunctionToFunctionType(node);
		case StmntID::StateStmnt:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::NamedType);
			type->namedType.typeName = node->state.name;
			return type;
		}
		}

		AddError("PackageChecker:GetIdentType unable to find type for name: " + name);
		return nullptr;
	}

	inline Type* GetSelectorType(Expr* of, Type* type)
	{
		auto& selector = of->selectorExpr;
		StringView& name = selector.select->identifierExpr.identifier->val;

		if (type->typeID == TypeID::ExplicitType)
		{
			Stmnt* explicitMember = FindTypeMember(type->explicitType.declarations, name);
			return explicitMember->definition.type;
		}

		if (type->typeID != TypeID::NamedType)
		{
			AddError(of->start, "PackageChecker:GetSelectorType Expected a named type from selector");
			return nullptr;
		}

		StateSymbol* stateSymbol = symbolTable->FindStateSymbol(type->namedType.typeName->val);
		if (!stateSymbol || !stateSymbol->state)
		{
			AddError(of->start, "PackageChecker:GetSelectorType No state found for named type: " + ToString(type));
			return nullptr;
		}

		Stmnt* member = FindStateMember(stateSymbol->state, name);
		if (member)
		{
			return member->definition.type;
		}
		else
		{
			Stmnt* method = FindStateMethod(stateSymbol, name);
			if (!method)
			{
				AddError(of->start, "Unable to find member or method for type: " + ToString(type));
				return nullptr;
			}

			return FunctionToFunctionType(method);
		}
	}

	inline bool IsNameOfPrimitive(StringView& name)
	{
		// Early out if string is longer than any primitive names
		if (name.count > 8) return false;

		TokenTree<eastl::string, TokenType, UniqueType>::TokenNode* node = tokenTypeLookup.Find(name);
		if (!node) return false;
		else return node->type == TokenType::Primitive;
	}

	inline bool IsNameOfType(StringView& name)
	{
		StateSymbol* state = symbolTable->FindStateSymbol(name);
		if (state) return true;
		else return IsNameOfPrimitive(name);
	}

	inline bool IsImportedType(Expr* expr)
	{
		auto& selector = expr->selectorExpr;
		Expr* on = selector.on;
		Expr* select = selector.select;
		return on->typeID == ExprID::IdentifierExpr && select->typeID == ExprID::IdentifierExpr &&
			globalTable->FindStateSymbol(on->identifierExpr.identifier->val, select->identifierExpr.identifier->val);
	}

	bool IsExprOfType(Expr* expr)
	{
		switch (expr->typeID)
		{
		case IdentifierExpr:
			return IsNameOfType(expr->identifierExpr.identifier->val);
		case PrimitiveExpr:
			return true;
		case SelectorExpr:
			return IsImportedType(expr);
		case IndexExpr:
			return IsExprOfType(expr->indexExpr.of);
		case FunctionCallExpr:
			return IsExprOfType(expr->functionCallExpr.function);
		case GenericsExpr:
			return IsExprOfType(expr->genericsExpr.expr);
		case TypeExpr:
			return true;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		default:
			break;
		}

		return false;
	}

	inline Type* GetIndexType(Expr* of, Type* type)
	{
		if (IsExprOfType(of))
		{
			return GetIndexTypeCreateArray(of, type);
		}
		else
		{
			return GetIndexTypeAccessArray(of, type);
		}
	}

	inline Type* GetIndexTypeCreateArray(Expr* of, Type* type)
	{
		Type* arrType = symbolTable->CreateTypePtr(TypeID::ArrayType);
		arrType->arrayType.type = type;
		return arrType;
	}

	inline Type* GetIndexTypeAccessArray(Expr* of, Type* type)
	{
		switch (type->typeID)
		{
		case NamedType:
			// Find index operator for type
			break;
		case PointerType:
			// Figure out rules for indexing pointer
			return type;
		case ValueType:
			return GetIndexTypeAccessArray(of, type->valueType.type);
		case ArrayType:
			return type->arrayType.type;
		}

		AddError(of->start, "PackageChecker:GetIndexTypeAccessArray Not a valid type to access index of: " + ToString(type));
		return nullptr;
	}

	inline Type* GetFunctionCallType(Expr* of, Type* type)
	{
		auto& function = of->functionCallExpr;

		if (!type) return nullptr;
		switch (type->typeID)
		{
		case PrimitiveType:
			return type;
		case NamedType:
			// Constructor, needs more validation
			return type;
		case FunctionType:
			return type->functionType.returnType;
		case GenericsType:
		{
			if (of->functionCallExpr.function->typeID == ExprID::GenericsExpr) return type;
		}
		default:
			break;
		}

		AddError(of->start, "PackageChecker:GetFunctionCallType unable to create type for expression: " + ToString(of));
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
		case GenericsExpr:
			type = GetGenericsType(expr, EvalType(expr->genericsExpr.expr));
			break;
		case IdentifierExpr:
			type = GetIdentType(expr, expr->identifierExpr.identifier->val);
			break;
		case SelectorExpr:
			type = GetSelectorType(expr, EvalType(expr->selectorExpr.on));
			break;
		case IndexExpr:
			type = GetIndexType(expr, EvalType(expr->indexExpr.of));
			break;
		case FunctionCallExpr:
			type = GetFunctionCallType(expr, EvalType(expr->functionCallExpr.function));
			break;
		default:
			break;
		}

		if (!type)
		{
			AddError(expr->start, "PackageChecker:EvalType unable to create type for expression: " + ToString(expr));
			return type;
		}

		if (type->typeID == TypeID::ValueType) type = type->valueType.type;
		return type;
	}

	bool IsFloat(Type* primitive)
	{
		switch (primitive->primitiveType.type)
		{
		case UniqueType::Float:
		case UniqueType::Float32:
		case UniqueType::Float64:
			return true;
		default:
			return false;
		}
	}

	bool IsInt(Type* primitive)
	{
		switch (primitive->primitiveType.type)
		{
		case UniqueType::Byte:
		case UniqueType::Int:
		case UniqueType::Int16:
		case UniqueType::Int32:
		case UniqueType::Int64:
		case UniqueType::Int128:
		case UniqueType::Ubyte:
		case UniqueType::Uint:
		case UniqueType::Uint16:
		case UniqueType::Uint32:
		case UniqueType::Uint64:
		case UniqueType::Uint128:
			return true;
		default:
			return false;
		}
	}

	bool IsString(Type* primitive)
	{
		switch (primitive->primitiveType.type)
		{
		case UniqueType::String:
			return true;
		default:
			return false;
		}
	}

	bool IsBoolLike(Type* type)
	{
		TypeID id = type->typeID;
		// TODO Support bool checks on state
		return id == TypeID::PrimitiveType || id == TypeID::PointerType;
	}

	inline Stmnt* GetGenerics(Stmnt* node)
	{
		switch (node->nodeID)
		{
		case FunctionStmnt:
			return node->function.generics;
		case StateStmnt:
			return node->state.generics;
		case Method:
			return node->method.generics;
		case StateOperator:
			return node->stateOperator.generics;
		default:
			return nullptr;
		}
	}

	bool IsGenericType(Type* namedType, Stmnt* node)
	{
		Stmnt* outer = GetOuterScope(node);
		if (outer)
		{
			Stmnt* generics = GetGenerics(outer);
			if (generics)
			{
				StringView& name = namedType->namedType.typeName->val;
				for (Token* gen : *generics->generics.names)
				{
					if (gen->val == name) return true;
				}
			}
		}

		return false;
	}

	Type* GetStateOperatorType(Token* token, UniqueType op, Type* namedType, Type* rhs = nullptr)
	{
		Stmnt* node = GetStmntForType(namedType, token);
		StateSymbol* state = symbolTable->FindStateSymbol(namedType->namedType.typeName->val);
		if (state)
		{
			for (Stmnt* opNode : state->operators)
			{
				auto& stateOp = opNode->stateOperator;
				if (stateOp.op->uniqueType == op)
				{
					if (!rhs) return stateOp.returnType;
					else if (*stateOp.decl->functionDecl.parameters->at(0)->definition.type == *rhs)
						return stateOp.returnType;
				}
			}
		}
		else if (IsGenericType(namedType, node))
		{
			return namedType;
		}
		else
		{
			AddError(token, "PackageChecker:GetStateOperatorType State not found for named type: " + ToString(namedType));
		}

		AddError(token, "No operator found for state");
		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}


	inline Type* GetPrimitiveOperatorType(Type* left, Type* right)
	{
		auto& lPrim = left->primitiveType;
		auto& rPrim = right->primitiveType;
		switch (lPrim.type)
		{
		case UniqueType::Void:
			break;
		case UniqueType::Bool:
			break;
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
		switch (left->typeID)
		{
		case PrimitiveType:
		{
			if (right->typeID == TypeID::PrimitiveType)
			{
				return GetPrimitiveOperatorType(left, right);
			}

			break;
		}
		case NamedType:
			return GetStateOperatorType(op, op->uniqueType, left, right);
		case ExplicitType:
		case ImplicitType:
			AddError(op, "Binary operators are not valid with explicit and implicit types");
			break;
		case PointerType:
		{
			if (IsInt(right))
			{
				return left;
			}
			// AddError pointers are ints, can't operate with anything other than ints
			// To think about, if both operands are pointer should it treat them as the underlying types?
			break;
		}
		break;
		case ValueType:
			return GetOperatorType(op, left->valueType.type, right);
		case ArrayType:
			break;
		case GenericsType:
			break;
		case FunctionType:
			AddError(op, "You can't multiply functions");
			break;
		case ImportedType:
			// TODO Add imported type checking
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
		case NamedType:
			return GetStateOperatorType(op, op->uniqueType, type);
		case ImportedType:
			// TODO Support imported types
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		case PointerType:
			return GetUnaryType(op, type->pointerType.type);
		case ValueType:
			return GetUnaryType(op, type->valueType.type);
		case GenericsType:
			return GetUnaryType(op, type->genericsType.type);
		default:
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		}
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
		case GenericsExpr:
			return EvalType(of);
		case LiteralExpr:
		{
			auto& literal = of->literalExpr;
			UniqueType uniqueType;
			switch (literal.type)
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
			Type* fixedType = InferType(of->fixedExpr.atExpr);
			if (fixedType->typeID == TypeID::PointerType) fixedType = fixedType->pointerType.type;
			if (fixedType->typeID != TypeID::ArrayType)
			{
				// AddError, fixed types must evaluate to array types
				return symbolTable->CreateTypePtr(TypeID::InvalidType);
			}

			Type* baseType = nullptr;
			Type* type = nullptr;
			do
			{
				fixedType = fixedType->arrayType.type;
				Type* pointerType = symbolTable->CreateTypePtr(TypeID::PointerType);
				if (!type)
				{
					type = pointerType;
					baseType = type;
				}
				else
				{
					type->pointerType.type = pointerType;
					type = type->pointerType.type;
				}
			} while (fixedType->typeID == TypeID::ArrayType);

			type->pointerType.type = fixedType;
			return baseType;
		}
		case AnonTypeExpr:
		{
			// AddError AnonType can not be implicitly assigned, loss of member handle name
			// Maybe in the future
			// anon := { 1, 2.0, 'str' }
			// anon[0] = 1 
			// anon[2] = 'str'
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
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
			type->functionType.paramTypes = symbolTable->CreateVectorPtr<Type*>();
			for (Stmnt* param : *anonFunction.decl->functionDecl.parameters)
			{
				type->functionType.paramTypes->push_back(param->definition.type);
			}
			return type;
		}
		case CompileExpr:
			return of->compileExpr.compile->compileStmnt.returnType;
		default:
			break;
		}

		return symbolTable->CreateTypePtr(TypeID::InvalidType);
	}
};