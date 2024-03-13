#pragma once
#include "Syntax.h"

struct Checker
{
	Syntax& syntax;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash> globalDefinitionMap;
	eastl::hash_map<InplaceString, Node*, InplaceStringHash> localDefinitionMap;

	Checker(Syntax& syntax) : syntax(syntax) {}

	void Check()
	{
		for (Node* node : syntax.nodes)
		{
			CheckNode(node, true);
		}
	}

	void CheckNode(Node* node, bool global)
	{
		switch (node->nodeID)
		{
		case InvalidNode:
		case CommentStmnt:
		case UsingStmnt:
		case PackageStmnt:
			break;

		case ExpressionStmnt:
			CheckExpr(node->expressionStmnt.expression);
			break;
		case Definition:
			CheckDefinition(node, global);
			break;
		case InlineDefinition:
			break;
		case FunctionStmnt:
		{
			CheckNode(node->function.decl, true);
			break;
		}
		case FunctionDecl:
		{
			auto& body = node->functionDecl.body;
			if (!body.statement) CheckNode(node->functionDecl.body.body, true);
			break;
		}
		case StateStmnt:
			break;
		case GenericsDecl:
			break;
		case WhereStmnt:
			break;
		case Method:
			break;
		case StateOperator:
			break;
		case Destructor:
			break;
		case Constructor:
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
		case CompileStmnt:
			break;
		case CompileDebugStmnt:
			break;
		case Block:
		{
			localDefinitionMap.clear();
			for (Node* n : *node->block.inner) CheckNode(n, false);
			break;
		}
		default:
			break;
		}
	}

	void CheckExpr(Expr* expr)
	{

	}

	void CheckDefinition(Node* node, bool global)
	{
		auto& definition = node->definition;
		Type* type = definition.type;
		if (type->typeID == TypeID::UnknownType)
		{
			definition.type = InferType(definition.assignment);
		}

		InplaceString& name = definition.name->val;
		if (global) globalDefinitionMap[name] = node;
		else localDefinitionMap[name] = node;
	}

	inline Node* GetStateNodeForName(InplaceString& val)
	{
		if (auto entry = syntax.symbolTable->stateMap.find(val); entry != syntax.symbolTable->stateMap.end())
		{
			return entry->second.state;
		}
		return nullptr;
	}

	Node* GetNodeForName(InplaceString& val)
	{
		if (auto entry = globalDefinitionMap.find(val); entry != globalDefinitionMap.end())
		{
			return entry->second;
		}
		else if (auto entry = localDefinitionMap.find(val); entry != localDefinitionMap.end())
		{
			return entry->second;
		}
		else
		{
			return GetStateNodeForName(val);
		}
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

	Type* GetInnerType(Type* of, eastl::vector<Token*>& idents)
	{
		Node* state = GetStateNodeForName(of->namedType.typeName->val);
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

					state = GetStateNodeForName(type->namedType.typeName->val);
					if (!state)
					{
						// AddError missing selector state
					}
				}
				else
				{
					// AddError no member for state
				}
			}
		}
		else
		{
			// AddError missing state
		}

		return nullptr;
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
					*type = *GetInnerType(baseType, idents);
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
			break;
		case NewExpr:
		{
			Type* type = syntax.CreateTypePtr(TypeID::PointerType);
			type->pointerType.valuePtr = false;
			type->pointerType.type = syntax.CreateTypePtr(TypeID::InvalidType);
			type->pointerType.type = InferType(of->newExpr.primaryExpr);
			return type;
		}
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
			return InferType(of->groupedExpr.expr);
		case GenericsExpr:
			break;
		case FunctionTypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			return of->compileExpr.returnType;
		default:
			break;
		}
		return syntax.CreateTypePtr(TypeID::InvalidType);
	}

};