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
			Type* inferredType = InferType(definition.assignment);
			if (!inferredType)
			{
				AddError(definition.assignment->start, "Checker:CheckDefinition Unable to infer type of implicit definition for expression: " + ToString(definition.assignment));
			}
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
				if (!inferredType)
				{
					AddError(definition.assignment->start, "Checker:CheckDefinition Unable to infer type of definition for expression: " + ToString(definition.assignment));
				}
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

	inline Node* FindInScope(InplaceString& val)
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

	inline Type* FunctionToFunctionType(Node* node)
	{
		Type* type = syntax.CreateTypePtr(TypeID::FunctionType);
		Node* decl = nullptr;

		switch (node->nodeID)
		{
		case FunctionStmnt:
		{
			type->functionType.returnType = node->function.returnType;
			decl = node->function.decl;
		}
		case Method:
		{
			type->functionType.returnType = node->method.returnType;
			decl = node->method.decl;
		}
		default:
			break;
		}
		
		return FillFunctionTypeParams(decl, type);
	}

	inline Type* FillFunctionTypeParams(Node* decl, Type* type)
	{
		type->functionType.paramTypes = syntax.CreateVectorPtr<Type*>();
		for (Node* node : *decl->functionDecl.parameters)
		{
			type->functionType.paramTypes->push_back(node->definition.type);
		}

		return type;
	}

	inline Type* GetTypeFromName(InplaceString& name)
	{
		Node* node = GetNodeForName(name);

		switch (node->nodeID)
		{
		case Definition:
			return node->definition.type;
		case FunctionStmnt:
			return FunctionToFunctionType(node);
		case StateStmnt:
		{
			Type* type = syntax.CreateTypePtr(TypeID::NamedType);
			type->namedType.typeName = node->state.name;
			return type;
		}
		}

		return nullptr;
	}

	inline Type* GetSelectorType(Expr* of, Type* type)
	{
		auto& selector = of->selectorExpr;
		InplaceString& name = selector.select->identifierExpr.identifier->val;

		if (type->typeID == TypeID::ExplicitType)
		{
			Node* explicitMember = FindTypeMember(type->explicitType.declarations, name);
			return explicitMember->definition.type;
		}

		if (type->typeID != TypeID::NamedType)
		{
			AddError(of->start, "Checker:GetSelectorType Expected a named type from selector");
			return nullptr;
		}

		StateSymbol* stateSymbol = GetStateForName(type->namedType.typeName->val);
		if (!stateSymbol || !stateSymbol->state)
		{
			AddError(of->start, "Checker:GetSelectorType No state found for named type: " + ToString(type));
			return nullptr;
		}

		Node* member = FindStateMember(stateSymbol->state, name);
		if (member)
		{
			return member->definition.type;
		}
		else
		{
			Node* method = FindStateMethod(stateSymbol, name);
			if (!method)
			{
				AddError(of->start, "Unable to find member or method for type: " + ToString(type));
				return nullptr;
			}

			return FunctionToFunctionType(method);
		}
	}

	inline Type* GetIndexType(Expr* of, Type* type)
	{
		if (!type)
		{
			AddError(of->start, "Checker:GetIndexType undefined type for expression: " + ToString(of));
		}
		auto& index = of->indexExpr;

		switch (type->typeID)
		{
		case PrimitiveType:
		{
			Type* arrType = syntax.CreateTypePtr(TypeID::ArrayType);
			arrType->arrayType.type = type;
			return arrType;
		}
		case NamedType:
			// Find index operator for type
			// Check if creating an array or accessing an index of array
			break;
		case ExplicitType:
			break;
		case ImplicitType:
			break;
		case PointerType:
			break;
		case ValueType:
			break;
		case ArrayType:
			return type->arrayType.type;
		case GenericsType:
			break;
		case FunctionType:
			break;
		case ImportedType:
			break;
		default:
			break;
		}

		return nullptr;
	}

	inline Type* GetFunctionCallType(Expr* of, Type* type)
	{
		auto& function = of->functionCallExpr;

		if (!type) return nullptr;
		switch (type->typeID)
		{
		case NamedType:
			// Constructor
			return type;
		case FunctionType:
			break;
		default:
			break;
		}

		return nullptr;
	}

	Type* EvalType(Expr* expr)
	{
		switch (expr->typeID)
		{
		case PrimitiveExpr:
			return syntax.CreatePrimitive(expr->primitiveExpr.primitive->uniqueType);
		case IdentifierExpr:
			return GetTypeFromName(expr->identifierExpr.identifier->val);
		case SelectorExpr:
			return GetSelectorType(expr, EvalType(expr->selectorExpr.on));
		case IndexExpr:
			return GetIndexType(expr, EvalType(expr->indexExpr.of));
		case FunctionCallExpr:
			return GetFunctionCallType(expr, EvalType(expr->functionCallExpr.function));
		default:
			break;
		}

		return nullptr;
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
			return syntax.CreateTypePtr(TypeID::InvalidType);
		case PrimitiveExpr:
		case IdentifierExpr:
		case SelectorExpr:
		case IndexExpr:
		case FunctionCallExpr:
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

			return syntax.CreatePrimitive(uniqueType);
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