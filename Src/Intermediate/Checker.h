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
	Node* currentContext = nullptr;

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
			AddScope();
			CheckState(key, value.state);
			CheckConstructors(value.constructors);
			CheckMethods(value.methods);
			CheckOperators(value.operators);
			CheckDestructor(value.destructor);
			PopScope();
		}

		defContext = DefinitionContext::FuncDef;
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

	void AddError(const eastl::string& err)
	{
		Logger::FatalError(err);
		//Logger::AddError(token->pos, token->index, err);
	}

	void AddError(Token* token, const eastl::string& err)
	{
		Logger::FatalErrorAt(err, token->pos);
		//Logger::AddError(token->pos, token->index, err);
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
			Type* inferred = InferType(conditional.condition);
			if (!IsBoolLike(inferred))
			{
				AddError(node->start, "Conditional expression doesn't evaluate to a conditional value");
			}
			CheckBody(conditional.body, node);
			break;
		}
		case AssignmentStmnt:
		{
			auto& assignment = node->assignmentStmnt;
			Type* to = InferType(assignment.assignTo);
			Type* from = InferType(assignment.assignment);
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
				Type* type = InferType(forStmnt.toIterate);
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
			if (!IsInt(InferType(switchStmnt.switchOn)))
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
			if (InferType(node->returnStmnt.expr)->typeID == TypeID::InvalidType)
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
		Type* type = definition.type;
		if (type->typeID == TypeID::UnknownType)
		{
			definition.type = InferType(definition.assignment);
		}
		else if (definition.assignment)
		{
			if (type->typeID == TypeID::ExplicitType)
			{
				CheckAnonType(node->start, type, definition.assignment);
			}
			else
			{
				Type* inferredType = InferType(definition.assignment);
				if (*definition.type != *inferredType)
				{
					AddError(node->start, "Expression doesn't evaluate to type " + ToString(definition.type));
					return;
				}
			}
		}

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
		CheckAnonType(node->start, type, expr);

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

	void CheckAnonType(Token* start, Type* type, Expr* expr)
	{
		if (expr->typeID != ExprID::AnonTypeExpr)
		{
			AddError(start, "Can only assign anonymous type expressions to inline types");
			return;
		}

		auto& anonExpr = expr->anonTypeExpr;
		if (type->typeID == TypeID::ImplicitType)
		{
			eastl::vector<Token*>* identifiers = type->implicitType.identifiers;
			if (anonExpr.values->size() != identifiers->size())
			{
				AddError(start, "Incorrect number of anonymous type expression compared to implicit type values");
				return;
			}

			type->typeID = TypeID::ExplicitType;
			type->explicitType.declarations = syntax.CreateVectorPtr<Node*>();
			for (int i = 0; i < identifiers->size(); i++)
			{
				Expr* itemExpr = anonExpr.values->at(i);
				Token* token = identifiers->at(i);
				Node* decl = syntax.CreateNode(token, NodeID::Definition);
				decl->definition.assignment = nullptr;
				decl->definition.name = token;
				decl->definition.type = InferType(itemExpr);
				type->explicitType.declarations->push_back(decl);
			}
		}
		else if (type->typeID == TypeID::ExplicitType)
		{
			eastl::vector<Node*>* decls = type->explicitType.declarations;
			if (anonExpr.values->size() != decls->size())
			{
				AddError(start, "Incorrect number of anonymous type expression compared to explicit type declarations");
				return;
			}

			for (int i = 0; i < decls->size(); i++)
			{
				Expr* itemExpr = anonExpr.values->at(i);
				Node* decl = decls->at(i);

				Type* inferredType = InferType(itemExpr);
				if (*decl->definition.type != *inferredType)
				{
					AddError(start, "Anonymous expression doesn't evaluate to type " + ToString(decl->definition.type));
					return;
				}
			}
		}
		else
		{
			AddError(start, "Inline definition type can only be an implicit or explicit type");
		}
	}

	inline StateSymbol* GetStateForName(InplaceString& val)
	{
		if (auto entry = syntax.symbolTable->stateMap.find(val); entry != syntax.symbolTable->stateMap.end())
		{
			return &entry->second;
		}
		return nullptr;
	}

	inline Node* GetFunctionForName(InplaceString& val)
	{
		if (auto entry = syntax.symbolTable->functionMap.find(val); entry != syntax.symbolTable->functionMap.end())
		{
			return entry->second;
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
		StateSymbol* state = GetStateForName(val);
		if (state) return state->state;
		else return GetFunctionForName(val);
	}

	Type* GetBaseType(Type* type)
	{
		Type* baseType = type;
		while (baseType->typeID == TypeID::PointerType || baseType->typeID == TypeID::ValueType)
		{
			switch (baseType->typeID)
			{
			case PointerType:
				baseType = baseType->pointerType.type;
				break;
			case ValueType:
				baseType = baseType->valueType.type;
				break;
				/*case ArrayType:
					baseType = baseType->arrayType.type;
					break;
				case GenericsType:
					baseType = baseType->genericsType.type;
					break;*/
			default:
				break;
			}
		}

		return baseType;
	}

	inline Node* FindTypeMember(eastl::vector<Node*>* members, InplaceString& val)
	{
		for (Node* node : *members)
		{
			if (node->definition.name->val == val) return node;
		}

		return nullptr;
	}

	inline Node* FindStateMember(Node* of, InplaceString& val)
	{
		return FindTypeMember(of->state.members, val);
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

	Type* GetInnerType(Type* of, eastl::vector<Token*>& idents, int index)
	{
		Node* node = nullptr;
		Token* curr = idents.at(index);
		if (of->typeID == TypeID::ExplicitType)
		{
			auto& expl = of->explicitType;
			node = FindTypeMember(expl.declarations, curr->val);
		}
		else if (of->typeID == TypeID::NamedType)
		{
			StateSymbol* stateSymbol = GetStateForName(of->namedType.typeName->val);
			if (stateSymbol && stateSymbol->state)
			{
				node = FindStateMember(stateSymbol->state, curr->val);
				if (!node)
				{
					node = FindStateMethod(stateSymbol, curr->val);
					if (node && index == 0) return node->method.returnType;
					else
					{
						AddError(curr, "No state member or method found selector");
						return syntax.CreateTypePtr(TypeID::InvalidType);
					}
				}
			}
		}

		if (node && node->nodeID == NodeID::Definition)
		{
			Type* type = node->definition.type;
			if (index == 0) return type;
			else return GetInnerType(type, idents, index - 1);
		}

		AddError(curr, "Unable to determine an inner type for selector statement");
		return syntax.CreateTypePtr(TypeID::InvalidType);
	}

	Node* GetGenerics(Node* node)
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

	bool IsGenericType(Type* namedType)
	{
		if (currentContext)
		{
			Node* generics = GetGenerics(currentContext);
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

	Type* GetStateOperatorType(Token* token, UniqueType op, Type* namedType, Type* rhs = nullptr)
	{
		StateSymbol* state = GetStateForName(namedType->namedType.typeName->val);
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
		else if (IsGenericType(namedType))
		{
			return namedType;
		}
		else
		{
			AddError(token, "Checker:GetStateOperatorType State not found for named type: " + ToString(namedType));
		}

		AddError(token, "No operator found for state");
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

	bool IsBoolLike(Type* type)
	{
		TypeID id = type->typeID;
		// TODO Support bool checks on state
		return id == TypeID::PrimitiveType || id == TypeID::PointerType;
	}

	bool IsStateType(Type* type)
	{
		switch (type->typeID)
		{
		case ImportedType:
		case NamedType:
			return true;
		case PointerType:
			return IsStateType(type->pointerType.type);
		case ValueType:
			return IsStateType(type->valueType.type);
		case GenericsType:
			return IsStateType(type->genericsType.type);
		default:
			return false;
		}
	}

	bool IsTypeExpr(Expr* expr)
	{
		switch (expr->typeID)
		{
		case NewExpr:
		case PrimitiveExpr:
		case FixedExpr:
		case AnonTypeExpr:
		case FunctionTypeExpr:
			return true;
		case IdentifierExpr:
			return GetStateForName(expr->identifierExpr.identifier->val);
		case SelectorExpr:
			//Todo check if imported type
			break;
		case IndexExpr:
			return IsTypeExpr(expr->indexExpr.of);
		default:
			break;
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
			return GetStateOperatorType(op, op->uniqueType, type);
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
			InplaceString& val = of->identifierExpr.identifier->val;
			Node* node = GetNodeForName(val);
			if (!node) break;
			else if (node->nodeID == NodeID::Definition) return node->definition.type;
			else if (node->nodeID == NodeID::StateStmnt)
			{
				Type* type = syntax.CreateTypePtr(TypeID::NamedType);
				type->namedType.typeName = node->state.name;
				return type;
			}
			else if (node->nodeID == NodeID::FunctionStmnt) return node->function.returnType;

			break;
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
					//TODO check imported types and function return types
				}
				else break;
			}
			else if (node->nodeID == NodeID::Definition)
			{
				Type* baseType = GetBaseType(node->definition.type);
				switch (baseType->typeID)
				{
				case ExplicitType:
				case ImportedType:
				case NamedType:
					type = GetInnerType(baseType, idents, idents.size() - 1);
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
			// Index expression can be used for creating an array eg. myVal: []int = int[12];
			// Or getting a value at an index eg. myInt := myVal[4];
			// Check if it's creating an array by checking if the indexed expression is a valid type to create
			// an array with eg. int[expr] (primitive) MyState[expr] (state)
			if (IsTypeExpr(of->indexExpr.of))
			{
				// Type expression, create array type
				Type* type = syntax.CreateTypePtr(TypeID::ArrayType);
				type->arrayType.type = InferType(of->indexExpr.of);
				return type;
			}
			else
			{
				//Not a type expression, try and evaluated indexed type
				Expr* curr = of;
				int indexDimensions = 0;
				while (curr->typeID == ExprID::IndexExpr)
				{
					curr = curr->indexExpr.of;
					indexDimensions += 1;
				}

				Type* type = GetBaseType(InferType(curr));
				if (type->typeID == TypeID::ArrayType)
				{
					for (int i = 0; i < indexDimensions; i++)
					{
						if (type->typeID != TypeID::ArrayType)
						{
							AddError(curr->start, "Index used on non-array type");
							return syntax.CreateTypePtr(TypeID::InvalidType);
						}

						type = type->arrayType.type;
					}
				}
				else if (IsStateType(type))
				{
					Type* indexType = GetStateOperatorType(of->start, UniqueType::Array, type);
					if (indexType) return indexType;
				}

				return type;
			}
			break;
		}
		case FunctionCallExpr:
		{
			Type* type = InferType(of->functionCallExpr.function);
			if (type->typeID == TypeID::FunctionType) return type->functionType.returnType;
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
		{
			Type* type = InferType(of->dereferenceExpr.of);
			if (type->typeID == TypeID::PointerType) return type->pointerType.type;
			return type;
		}
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
			type->genericsType.generics = syntax.CreateExpr(of->start, ExprID::GenericsExpr);
			type->genericsType.generics->genericsExpr.expr = nullptr;
			type->genericsType.generics->genericsExpr.open = of->genericsExpr.open;
			type->genericsType.generics->genericsExpr.close = of->genericsExpr.close;
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