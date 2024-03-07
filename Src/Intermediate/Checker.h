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
			break;
		case FunctionDecl:
			break;
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
		if (type->typeID == TypeID::ImplicitType)
		{
			InferType(definition.assignment, definition.type);
		}

		InplaceString& name = definition.name->val;
		if (global) globalDefinitionMap[name] = node;
		else localDefinitionMap[name] = node;
	}

	void InferType(Expr* of, Type*& type)
	{
		switch (of->typeID)
		{
		case InvalidExpr:
			type->typeID = TypeID::InvalidType;
			break;
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

			type = syntax.CreatePrimitive(uniqueType);
			break;
		}
		case IdentifierExpr:
		{
			InplaceString& val = of->identfierExpr.identifier->val;
			if (globalDefinitionMap.find(val) != globalDefinitionMap.end())
			{

			}
			else if (localDefinitionMap.find(val) != localDefinitionMap.end())
			{

			}
			break;
		}
		case PrimitiveExpr:
			type = syntax.CreatePrimitive(of->primitiveExpr.primitive->uniqueType);
			break;
		case SelectorExpr:
			break;
		case IndexExpr:
		{
			type->typeID = TypeID::ArrayType;
			InferType(of->indexExpr.of, type->arrayType.type);
			break;
		}
		case FunctionCallExpr:
			break;
		case NewExpr:
		{
			type->typeID = TypeID::PointerType;
			type->pointerType.valuePtr = false;
			type->pointerType.type = syntax.CreateTypePtr(TypeID::InvalidType);
			InferType(of->newExpr.primaryExpr, type->pointerType.type);
			break;
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

			break;
		case GenericsExpr:
			break;
		case FunctionTypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			*type = *of->compileExpr.returnType;
			break;
		default:
			break;
		}
	}

};