#pragma once

#include "../Intermediate/SymbolTable.h"
#include <EASTL/deque.h>


struct TypeChecker
{
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<InplaceString, Node*, InplaceStringHash>>& scopeQueue;

	TypeChecker(SymbolTable* symbolTable,
		eastl::deque<eastl::hash_map<InplaceString, Node*, InplaceStringHash>>& scopeQueue)
		: symbolTable(symbolTable), scopeQueue(scopeQueue) {}

	void CheckDefinitionType(Node* node)
	{
		auto& definition = node->definition;
		Type* type = definition.type;
		if (type->typeID == TypeID::UnknownType)
		{
			Type* inferredType = InferType(definition.assignment, node);
			if (!inferredType)
			{
				AddError(definition.assignment->start, "Checker:CheckDefinition Unable to infer type of implicit definition for expression: " + ToString(definition.assignment));
			}
			definition.type = InferType(definition.assignment, node);
		}
		else if (definition.assignment)
		{
			if (type->typeID == TypeID::ExplicitType)
			{
				CheckAnonType(node, type, definition.assignment);
			}
			else
			{
				Type* inferredType = InferType(definition.assignment, node);
				if (!inferredType)
				{
					AddError(definition.assignment->start, "Checker:CheckDefinition Unable to infer type of definition for expression: " + ToString(definition.assignment));
				}
				else if (*definition.type != *inferredType)
				{
					AddError(node->start, "Expression evaluates to type:" + ToString(inferredType) + " which doesn't evaluate to type " + ToString(definition.type));
					return;
				}
			}
		}
	}

	void CheckAnonType(Node* node, Type* type, Expr* expr)
	{
		if (expr->typeID != ExprID::AnonTypeExpr)
		{
			AddError(node->start, "Can only assign anonymous type expressions to inline types");
			return;
		}

		auto& anonExpr = expr->anonTypeExpr;
		if (type->typeID == TypeID::ImplicitType)
		{
			eastl::vector<Token*>* identifiers = type->implicitType.identifiers;
			if (anonExpr.values->size() != identifiers->size())
			{
				AddError(node->start, "Incorrect number of anonymous type expression compared to implicit type values");
				return;
			}

			type->typeID = TypeID::ExplicitType;
			type->explicitType.declarations = symbolTable->CreateVectorPtr<Node*>();
			for (int i = 0; i < identifiers->size(); i++)
			{
				Expr* itemExpr = anonExpr.values->at(i);
				Token* token = identifiers->at(i);
				Node* decl = symbolTable->CreateNode(token, NodeID::Definition, node);
				decl->definition.assignment = nullptr;
				decl->definition.name = token;
				decl->definition.type = InferType(itemExpr, node);
				type->explicitType.declarations->push_back(decl);
			}
		}
		else if (type->typeID == TypeID::ExplicitType)
		{
			eastl::vector<Node*>* decls = type->explicitType.declarations;
			if (anonExpr.values->size() != decls->size())
			{
				AddError(node->start, "Incorrect number of anonymous type expression compared to explicit type declarations");
				return;
			}

			for (int i = 0; i < decls->size(); i++)
			{
				Expr* itemExpr = anonExpr.values->at(i);
				Node* decl = decls->at(i);

				Type* inferredType = InferType(itemExpr, node);
				if (*decl->definition.type != *inferredType)
				{
					AddError(node->start, "Anonymous expression doesn't evaluate to type " + ToString(decl->definition.type));
					return;
				}
			}
		}
		else
		{
			AddError(node->start, "Inline definition type can only be an implicit or explicit type");
		}
	}

	inline void CheckAssignmentStmnt(Node* node)
	{
		auto& assignment = node->assignmentStmnt;
		Type* to = InferType(assignment.assignTo, node);
		Type* from = InferType(assignment.assignment, node);
		if (*to != *from)
		{
			AddError(node->start, "Invalid type evaluation for assignment expression: " + ToString(to));
		}
	}

	inline void CheckConditionalType(Node* node)
	{
		auto& conditional = node->conditional;
		Type* inferred = InferType(conditional.condition, node);
		if (!IsBoolLike(inferred))
		{
			AddError(node->start, "Conditional expression doesn't evaluate to a conditional value");
		}
	}

	inline void CheckForType(Node* node)
	{
		auto& forStmnt = node->forStmnt;
		if (!forStmnt.isDeclaration)
		{
			Token* identifier = forStmnt.iterated.identifier;
			Node* decl = symbolTable->CreateNode(identifier, NodeID::Definition, node);
			decl->definition.assignment = nullptr;
			decl->definition.name = identifier;
			Type* type = InferType(forStmnt.toIterate, node);
			if (forStmnt.rangeFor)
			{
				if (!IsInt(type))
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
	}

	inline void CheckSwitchType(Node* node)
	{
		auto& switchStmnt = node->switchStmnt;
		if (!IsInt(InferType(switchStmnt.switchOn, node)))
		{
			AddError(switchStmnt.switchOn->start, "Switch expressions must evaluate to an int type");
		}
	}

	inline void CheckReturnType(Node* node)
	{
		if (node->returnStmnt.voidReturn) return;

		Type* returnType = GetOuterReturnType(node);
		if (!returnType)
		{
			AddError(node->start, "TypeChecker:CheckReturnType Unable to get return type node");
			return;
		}

		Type* inferred = InferType(node->returnStmnt.expr, node);
		if (*inferred != *returnType)
		{
			AddError(node->start, "TypeChecker:CheckerReturnType Expected return type: " + ToString(returnType) +
				", return expression evaluated to: " + ToString(inferred));
		}
	}

	inline Type* GetOuterReturnType(Node* node)
	{
		Node* outer = GetOuterScope(node);
		if (outer) return GetReturnType(outer);
		return nullptr;
	}

	inline Type* GetReturnType(Node* node)
	{
		switch (node->nodeID)
		{
		case NodeID::FunctionStmnt:
			return node->function.returnType;
		case NodeID::Method:
			return node->method.returnType;
		case NodeID::StateOperator:
			return node->stateOperator.returnType;
		case NodeID::AnonFunction:
			return node->anonFunction.returnType;
		case NodeID::CompileStmnt:
			return node->compileStmnt.returnType;
		default:
			break;
		}

		return nullptr;
	}

	inline bool IsOuterScope(Node* node)
	{
		NodeID nodeID = node->nodeID;
		return nodeID == NodeID::FunctionStmnt || nodeID == NodeID::Method ||
			nodeID == NodeID::StateOperator || nodeID == NodeID::AnonFunction ||
			nodeID == NodeID::CompileStmnt;
	}

	Node* GetOuterScope(Node* node)
	{
		while (node && !IsOuterScope(node)) node = node->scope;
		return node;
	}

	inline Type* GenericsExprToType(Expr* expr, Type* of)
	{
		Type* type = symbolTable->CreateTypePtr(TypeID::GenericsType);
		type->genericsType.generics = symbolTable->CreateExpr(expr->start, ExprID::GenericsExpr);
		type->genericsType.generics->genericsExpr.expr = nullptr;
		type->genericsType.generics->genericsExpr.open = expr->genericsExpr.open;
		type->genericsType.generics->genericsExpr.close = expr->genericsExpr.close;
		type->genericsType.generics->genericsExpr.types = expr->genericsExpr.types;
		type->genericsType.type = of;
		return type;
	}

	inline Type* FunctionToFunctionType(Node* node)
	{
		Type* type = symbolTable->CreateTypePtr(TypeID::FunctionType);
		Node* decl = nullptr;

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
			AddError(node->start, "Checker:FunctionToFunctionType Unable to find function declaration for node: " + ToString(node));
			return nullptr;
		}
		return FillFunctionTypeParams(decl, type);
	}

	inline Type* FillFunctionTypeParams(Node* decl, Type* type)
	{
		type->functionType.paramTypes = symbolTable->CreateVectorPtr<Type*>();
		for (Node* node : *decl->functionDecl.parameters)
		{
			type->functionType.paramTypes->push_back(node->definition.type);
		}

		return type;
	}

	inline Node* FindInScope(InplaceString& val)
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

	Node* GetNodeForName(InplaceString& val)
	{
		Node* node = FindInScope(val);
		if (node) return node;
		StateSymbol* state = symbolTable->GetStateForName(val);
		if (state) return state->state;
		else return symbolTable->GetFunctionForName(val);
	}


	inline Type* GetTypeFromName(Expr* expr, InplaceString& name)
	{
		Node* node = GetNodeForName(name);
		if (!node)
		{
			AddError(expr->start, "Checker:GetTypeFromName Unable to get node for name: " + name);
			return nullptr;
		}

		switch (node->nodeID)
		{
		case NodeID::Definition:
			return node->definition.type;
		case NodeID::FunctionStmnt:
			return FunctionToFunctionType(node);
		case NodeID::StateStmnt:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::NamedType);
			type->namedType.typeName = node->state.name;
			return type;
		}
		}

		AddError("Checker:GetTypeFromName unable to find type for name: " + name);
		return nullptr;
	}

	inline Type* GetSelectorType(Expr* of, Type* type)
	{
		auto& selector = of->selectorExpr;
		InplaceString& name = selector.select->identifierExpr.identifier->val;

		if (type->typeID == TypeID::ExplicitType)
		{
			Node* explicitMember = symbolTable->FindTypeMember(type->explicitType.declarations, name);
			return explicitMember->definition.type;
		}

		if (type->typeID != TypeID::NamedType)
		{
			AddError(of->start, "Checker:GetSelectorType Expected a named type from selector");
			return nullptr;
		}

		StateSymbol* stateSymbol = symbolTable->GetStateForName(type->namedType.typeName->val);
		if (!stateSymbol || !stateSymbol->state)
		{
			AddError(of->start, "Checker:GetSelectorType No state found for named type: " + ToString(type));
			return nullptr;
		}

		Node* member = symbolTable->FindStateMember(stateSymbol->state, name);
		if (member)
		{
			return member->definition.type;
		}
		else
		{
			Node* method = symbolTable->FindStateMethod(stateSymbol, name);
			if (!method)
			{
				AddError(of->start, "Unable to find member or method for type: " + ToString(type));
				return nullptr;
			}

			return FunctionToFunctionType(method);
		}
	}

	inline bool IsNameOfPrimitive(InplaceString& name)
	{
		// Early out if string is longer than any primitive names
		if (name.count > 8) return false;

		TokenTree<eastl::string, TokenType, UniqueType>::TokenNode* node = tokenTypeLookup.Find(name);
		if (!node) return false;
		else return node->type == TokenType::Primitive;
	}

	inline bool IsNameOfType(InplaceString& name)
	{
		StateSymbol* state = symbolTable->GetStateForName(name);
		if (state) return true;
		else return IsNameOfPrimitive(name);
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
			// Check if Imported type
			break;
		case IndexExpr:
			return IsExprOfType(expr->indexExpr.of);
		case FunctionCallExpr:
			return IsExprOfType(expr->functionCallExpr.function);
		case GenericsExpr:
			return IsExprOfType(expr->genericsExpr.expr);
		case FunctionTypeExpr:
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

		AddError(of->start, "Checker:GetIndexTypeAccessArray Not a valid type to access index of: " + ToString(type));
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

		AddError(of->start, "Checker:GetFunctionCallType unable to create type for expression: " + ToString(of));
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
		case FunctionTypeExpr:
			type = expr->functionTypeExpr.functionType;
			break;
		case GenericsExpr:
			type = GenericsExprToType(expr, EvalType(expr->genericsExpr.expr));
			break;
		case IdentifierExpr:
			type = GetTypeFromName(expr, expr->identifierExpr.identifier->val);
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
			AddError(expr->start, "Checker:EvalType unable to create type for expression: " + ToString(expr));
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

	inline Node* GetGenerics(Node* node)
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

	bool IsGenericType(Type* namedType, Node* node)
	{
		Node* outer = GetOuterScope(node);
		if (outer)
		{
			Node* generics = GetGenerics(outer);
			if (generics)
			{
				InplaceString& name = namedType->namedType.typeName->val;
				for (Token* gen : *generics->generics.names)
				{
					if (gen->val == name) return true;
				}
			}
		}

		return false;
	}

	Type* GetStateOperatorType(Node* node, Token* token, UniqueType op, Type* namedType, Type* rhs = nullptr)
	{
		StateSymbol* state = symbolTable->GetStateForName(namedType->namedType.typeName->val);
		if (state)
		{
			for (Node* opNode : state->operators)
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
			AddError(token, "Checker:GetStateOperatorType State not found for named type: " + ToString(namedType));
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

	Type* GetOperatorType(Node* node, Token* op, Type* left, Type* right)
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
			return GetStateOperatorType(node, op, op->uniqueType, left, right);
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
			return GetOperatorType(node, op, left->valueType.type, right);
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

	Type* GetUnaryType(Node* node, Token* op, Type* type)
	{
		switch (type->typeID)
		{
		case PrimitiveType:
			if (op->uniqueType == UniqueType::Not) return symbolTable->CreatePrimitive(UniqueType::Bool);
			return type;
		case NamedType:
			return GetStateOperatorType(node, op, op->uniqueType, type);
		case ImportedType:
			// TODO Support imported types
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		case PointerType:
			return GetUnaryType(node, op, type->pointerType.type);
		case ValueType:
			return GetUnaryType(node, op, type->valueType.type);
		case GenericsType:
			return GetUnaryType(node, op, type->genericsType.type);
		default:
			return symbolTable->CreateTypePtr(TypeID::InvalidType);
		}
	}

	Type* InferType(Expr* of, Node* node)
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
		case FunctionTypeExpr:
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
			type->pointerType.type = InferType(of->newExpr.primaryExpr, node);
			return type;
		}
		case FixedExpr:
		{
			Type* fixedType = InferType(of->fixedExpr.atExpr, node);
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
			Type* type = InferType(of->dereferenceExpr.of, node);
			if (type->typeID == TypeID::PointerType) return type->pointerType.type;
			return type;
		}
		case ReferenceExpr:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::PointerType);
			type->pointerType.type = InferType(of->referenceExpr.of, node);
			return type;
		}
		case BinaryExpr:
		{
			Type* left = InferType(of->binaryExpr.left, node);
			Type* right = InferType(of->binaryExpr.right, node);
			return GetOperatorType(node, of->binaryExpr.op, left, right);
		}
		case UnaryExpr:
		{
			Type* type = InferType(of->unaryExpr.expr, node);
			return GetUnaryType(node, of->unaryExpr.op, type);
		}
		case GroupedExpr:
			return InferType(of->groupedExpr.expr, node);
		case FunctionTypeDeclExpr:
		{
			Type* type = symbolTable->CreateTypePtr(TypeID::FunctionType);
			auto& anonFunction = of->functionTypeDeclExpr.anonFunction->anonFunction;
			type->functionType.returnType = anonFunction.returnType;
			type->functionType.paramTypes = symbolTable->CreateVectorPtr<Type*>();
			for (Node* param : *anonFunction.decl->functionDecl.parameters)
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