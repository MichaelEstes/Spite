#pragma once
#include "Syntax.h"
#include "EASTL/deque.h"

struct Checker
{
	enum DefinitionContext
	{
		GlobalDef,
		FuncDef,
		MethodDef
	};

	Syntax& syntax;
	eastl::deque<eastl::hash_map<InplaceString, Node*, InplaceStringHash>> scopeQueue;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash>* globalScope;
	DefinitionContext defContext;

	Checker(Syntax& syntax) : syntax(syntax) {}

	void Check()
	{
		AddScope();
		globalScope = &scopeQueue.back();
		defContext = DefinitionContext::GlobalDef;
		for (auto& [key, value] : syntax.symbolTable->globalValMap)
		{
			CheckGlobalVal(value);
		}

		defContext = DefinitionContext::MethodDef;
		for (auto& [key, value] : syntax.symbolTable->stateMap)
		{
			CheckState(value.state);
			CheckConstructors(value.constructors);
			CheckMethods(value.methods);
			CheckOperators(value.operators);
			CheckDestructor(value.destructor);
		}

		defContext = DefinitionContext::FuncDef;
		for (auto& [key, value] : syntax.symbolTable->functionMap)
		{
			CheckFunction(value);
		}

		for (Node* node : syntax.symbolTable->onCompiles)
		{

		}
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

	void CheckState(Node* state)
	{
		if (!state)
		{
			// Add Error, state was not defined
			return;
		}


	}

	void CheckConstructors(eastl::vector<Node*>& constructors)
	{
		for (Node* constructor : constructors)
		{
			auto& decl = constructor->constructor.decl;
			CheckFunctionDecl(decl);
		}
	}

	void CheckMethods(eastl::vector<Node*>& methods)
	{
		for (Node* method : methods)
		{
			auto& decl = method->method.decl;
			CheckFunctionDecl(decl);
		}
	}

	void CheckOperators(eastl::vector<Node*>& operators)
	{
		for (Node* op : operators)
		{
			auto& decl = op->stateOperator.decl;
			CheckFunctionDecl(decl);
		}
	}

	void CheckDestructor(Node* destructor)
	{
		if (!destructor) return; // Destructor not required
		CheckBody(destructor->destructor.body);
	}

	void CheckFunction(Node* function)
	{
		auto& decl = function->function.decl;
		CheckFunctionDecl(decl);
	}

	inline void CheckFunctionDecl(Node* functionDecl)
	{
		auto& params = functionDecl->functionDecl.parameters;
		auto& body = functionDecl->functionDecl.body;
		CheckBody(body);
	}

	inline void CheckBody(Body& body)
	{
		if (body.statement) AddScope();
		CheckNode(body.body);
		if (body.statement) PopScope();
	}

	void CheckNode(Node* node)
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
			break;
		case Conditional:
			break;
		case AssignmentStmnt:
			break;
		case IfStmnt:
			break;
		case ForStmnt:
			break;
		case WhileStmnt:
			break;
		case SwitchStmnt:
			break;
		case DeleteStmnt:
			break;
		case DeferStmnt:
			break;
		case ContinueStmnt:
			break;
		case BreakStmnt:
			break;
		case ReturnStmnt:
			break;
		case Block:
		{
			AddScope();
			for (Node* n : *node->block.inner) CheckNode(n);
			PopScope();
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
		Type* type = definition.type;
		if (type->typeID == TypeID::UnknownType)
		{
			definition.type = InferType(definition.assignment);
		}

		InplaceString& name = definition.name->val;
		eastl::hash_map<InplaceString, Node*, InplaceStringHash>& back = scopeQueue.back();
		back[name] = node;
	}

	inline StateSymbol* GetStateForName(InplaceString& val)
	{
		if (auto entry = syntax.symbolTable->stateMap.find(val); entry != syntax.symbolTable->stateMap.end())
		{
			return &entry->second;
		}
		return nullptr;
	}

	Node* FindInScope(InplaceString& val)
	{
		for (auto it = scopeQueue.rbegin(); it != scopeQueue.rend(); it++)
		{
			if (auto entry = it->find(val); entry != it->end())
			{
				return entry->second;
			}
		}
	}

	Node* GetNodeForName(InplaceString& val)
	{
		Node* node = FindInScope(val);
		if (node) return node;
		else return GetStateForName(val)->state;
	}

	inline bool IsNotBaseType(Type* type)
	{
		TypeID typeId = type->typeID;
		return typeId == TypeID::GenericsType || typeId == TypeID::ArrayType ||
			typeId == TypeID::ValueType || typeId == TypeID::PointerType;
	}

	Type* GetBaseType(Type* type)
	{
		Type* baseType = type;
		while (IsNotBaseType(baseType))
		{
			switch (baseType->typeID)
			{
			case PointerType:
				baseType = baseType->pointerType.type;
				break;
			case ValueType:
				baseType = baseType->valueType.type;
				break;
			case ArrayType:
				baseType = baseType->arrayType.type;
				break;
			case GenericsType:
				baseType = baseType->genericsType.type;
				break;
			default:
				break;
			}
		}

		return baseType;
	}

	Node* FindStateMember(Node* of, InplaceString& val)
	{
		eastl::vector<Node*>* members = of->state.members;
		for (Node* node : *members)
		{
			if (node->definition.name->val == val) return node;
		}

		return nullptr;
	}

	Node* FindStateMethod(StateSymbol* of, InplaceString& val)
	{
		eastl::vector<Node*>& methods = of->methods;
		for (Node* node : methods)
		{
			if (node->method.name->val == val) return node;
		}

		return nullptr;
	}

	Type* GetInnerType(Type* of, eastl::vector<Token*>& idents)
	{
		StateSymbol* stateSymbol = GetStateForName(of->namedType.typeName->val);
		Node* state = stateSymbol->state;
		if (state)
		{
			for (int i = idents.size() - 1; i >= 0; i--)
			{
				InplaceString& val = idents.at(i)->val;
				Node* node = FindStateMember(state, val);
				if (node)
				{
					Type* type = node->definition.type;
					if (i == 0) return type;

					stateSymbol = GetStateForName(type->namedType.typeName->val);
					state = stateSymbol->state;
					if (!state)
					{
						// AddError missing selector state
					}
				}
				else
				{
					Node* method = FindStateMethod(stateSymbol, val);
					if (method && i == 0)
					{
						return method->method.returnType;
					}
					else
					{
						// AddError no member for state
					}
				}
			}
		}
		else
		{
			// AddError missing state
		}

		return syntax.CreateTypePtr(TypeID::InvalidType);
	}

	Type* GetStateOperatorType(Token* op, Type* namedType, Type* rhs = nullptr)
	{
		StateSymbol* state = GetStateForName(namedType->namedType.typeName->val);
		if (state)
		{
			for (Node* opNode : state->operators)
			{
				auto& stateOp = opNode->stateOperator;
				if (stateOp.op->uniqueType == op->uniqueType)
				{
					if (!rhs) return stateOp.returnType;
					else if (*stateOp.decl->functionDecl.parameters->at(0)->definition.type == *rhs)
						return stateOp.returnType;
				}
			}
		}
		else
		{
			// AddError state not found for named type
		}

		return syntax.CreateTypePtr(TypeID::InvalidType);
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

		return syntax.CreateTypePtr(TypeID::InvalidType);
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
			return GetStateOperatorType(op, left, right);
		case ExplicitType:
		case ImplicitType:
			// Add Error, Binary operators not valid with explicit and implicit types
			break;
		case PointerType:
		{
			if (IsInt(right))
			{
				return left;
			}
			// Add Error, pointers are ints, can't operate with anything other than ints
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
			// Add error, can't multiply functions
			break;
		case ImportedType:
			// TODO Add imported type checking
			break;
		default:
			break;
		}

		return syntax.CreateTypePtr(TypeID::InvalidType);
	}

	Type* GetUnaryType(Token* op, Type* type)
	{
		switch (type->typeID)
		{
		case PrimitiveType:
			if (op->uniqueType == UniqueType::Not) return syntax.CreatePrimitive(UniqueType::Bool);
			return type;
		case NamedType:
			return GetStateOperatorType(op, type);
		case ImportedType:
			// TODO Support imported types
			return syntax.CreateTypePtr(TypeID::InvalidType);
		case PointerType:
			return GetUnaryType(op, type->pointerType.type);
		case ValueType:
			return GetUnaryType(op, type->valueType.type);
		case GenericsType:
			return GetUnaryType(op, type->genericsType.type);
		default:
			return syntax.CreateTypePtr(TypeID::InvalidType);
		}
	}

	Type* InferType(Expr* of)
	{
		switch (of->typeID)
		{
		case InvalidExpr:
		{
			return syntax.CreateTypePtr(TypeID::InvalidType);
		}
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

			return syntax.CreatePrimitive(uniqueType);
		}
		case IdentifierExpr:
		{
			Type* type = syntax.CreateTypePtr(TypeID::InvalidType);
			InplaceString& val = of->identifierExpr.identifier->val;
			Node* node = GetNodeForName(val);
			if (!node) return type;
			else if (node->nodeID == NodeID::Definition) *type = *node->definition.type;
			else if (node->nodeID == NodeID::StateStmnt)
			{
				type->typeID = TypeID::NamedType;
				type->namedType.typeName = node->state.name;
			}

			return type;
		}
		case PrimitiveExpr:
		{
			return syntax.CreatePrimitive(of->primitiveExpr.primitive->uniqueType);
		}
		case SelectorExpr:
		{
			Type* type = syntax.CreateTypePtr(TypeID::InvalidType);
			Expr* firstSelector = of;
			eastl::vector<Token*> idents = eastl::vector<Token*>();
			while (firstSelector->selectorExpr.on->typeID == ExprID::SelectorExpr)
			{
				idents.push_back(firstSelector->selectorExpr.select->identifierExpr.identifier);
				firstSelector = firstSelector->selectorExpr.on;
			}
			idents.push_back(firstSelector->selectorExpr.select->identifierExpr.identifier);

			auto& selector = of->selectorExpr;
			InplaceString& firstName = firstSelector->selectorExpr.on->identifierExpr.identifier->val;
			Node* node = GetNodeForName(firstName);
			if (!node)
			{
				if (selector.on->typeID == ExprID::IdentifierExpr && selector.select->typeID == ExprID::IdentifierExpr)
				{
					type->typeID = TypeID::ImportedType;
					type->importedType.packageName = selector.on->identifierExpr.identifier;
					type->importedType.typeName = selector.on->identifierExpr.identifier;
				}
				else break;
			}
			else if (node->nodeID == NodeID::Definition)
			{
				Type* baseType = GetBaseType(node->definition.type);
				switch (baseType->typeID)
				{
				case NamedType:
					type = GetInnerType(baseType, idents);
					break;
				case ImportedType:
					break;
				default:
					break;
				}
			}
			else if (node->nodeID == NodeID::StateStmnt)
			{
				type->typeID = TypeID::NamedType;
				type->namedType.typeName = node->state.name;
			}

			return type;
		}
		case IndexExpr:
		{
			Type* type = syntax.CreateTypePtr(TypeID::ArrayType);
			type->arrayType.type = InferType(of->indexExpr.of);
			return type;
		}
		case FunctionCallExpr:
		{
			// TODO infer return types of functions
			Type* type = InferType(of->functionCallExpr.function);
			return type;
		}
		case NewExpr:
		{
			Type* type = syntax.CreateTypePtr(TypeID::PointerType);
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
				return syntax.CreateTypePtr(TypeID::InvalidType);
			}

			Type* baseType = nullptr;
			Type* type = nullptr;
			do
			{
				fixedType = fixedType->arrayType.type;
				Type* pointerType = syntax.CreateTypePtr(TypeID::PointerType);
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
			return syntax.CreateTypePtr(TypeID::InvalidType);
		}
		case AsExpr:
			return of->asExpr.to;
		case DereferenceExpr:
			break;
		case ReferenceExpr:
		{
			Type* type = syntax.CreateTypePtr(TypeID::PointerType);
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
		case GenericsExpr:
		{
			Type* type = syntax.CreateTypePtr(TypeID::GenericsType);
			type->genericsType.generics->genericsExpr.types = of->genericsExpr.types;
			type->genericsType.type = InferType(of->genericsExpr.expr);
			return type;
		}
		case FunctionTypeExpr:
			return of->functionTypeExpr.functionType;
		case FunctionTypeDeclExpr:
		{
			Type* type = syntax.CreateTypePtr(TypeID::FunctionType);
			type->functionType.returnType = of->functionTypeDeclExpr.returnType;
			type->functionType.paramTypes = syntax.CreateVectorPtr<Type*>();
			for (Node* param : *of->functionTypeDeclExpr.functionDecl->functionDecl.parameters)
			{
				type->functionType.paramTypes->push_back(param->definition.type);
			}
			return type;
		}
		case CompileExpr:
			return of->compileExpr.returnType;
		default:
			break;
		}
		return syntax.CreateTypePtr(TypeID::InvalidType);
	}

};