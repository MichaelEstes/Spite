#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"
#include "Expr.h"
#include "Type.h"

typedef size_t NodeIndex;
typedef size_t TokenIndex;
typedef size_t ScopeIndex;

enum NodeID
{
	Unknown = 0,
	ExpressionStmnt,
	UsingStmnt,
	PackageStmnt,
	Definition,
	Function,
	Conditional,
	IfStmnt,
	ForStmnt,
	WhileStmnt,
	SwitchStmnt,
	TernaryStmnt,
	DeleteStmnt,
	DeferStmnt,
	ReturnStmnt,
	OnCompileStmnt,
	StateStmnt,
	WhereStmnt,
	Block,
	CommentStmnt,
};

struct Body
{
	bool exprFunction;
	union
	{
		Node* block;
		Expr* expr;
	};
};

struct Node
{
	TokenIndex start;
	TokenIndex end;
	ScopeIndex scope;
	NodeID nodeID;
	NodeIndex index;

	union
	{
		struct
		{
			Expr* expression;
		} expressionStmnt;

		struct
		{
			TokenIndex packageName;
			TokenIndex alias;
		} using_;

		struct
		{
			TokenIndex name;
		} package;

		struct
		{
			Type type;
			TokenIndex name;
			Expr* assignment;
		} definition;

		struct
		{
			Type returnType;
			TokenIndex name;
			Expr* generics;
			eastl::vector<Node*>* parameters;
			Body body;
		} function;

		struct
		{
			Expr* condition;
			Body body;
		} conditional;

		struct 
		{
			Node* condition;
			eastl::vector<Node*>* elifs;
			Node* elseCondition;
		} ifStmnt;

		struct 
		{
		} forStmnt;

		struct 
		{
		} whileStmnt;

		struct 
		{
		} switchStmnt;

		struct 
		{
		} ternaryStmnt;

		struct 
		{
		} deleteStmnt;

		struct 
		{
		} deferStmnt;

		struct 
		{
		} returnStmnt;

		struct 
		{
		} onCompileStmnt;

		struct 
		{
		} whereStmnt;

		struct
		{
			eastl::vector<Node*>* inner;
		} block;

		struct
		{
			NodeIndex name;
			NodeIndex type;
			eastl::vector<NodeIndex> memberFunctions;
		} state;
	};

	Node()
	{
		start = 0;
		end = 0;
		scope = 0;
		nodeID = NodeID::Unknown;
		index = 0;
	}

	Node(NodeID nodeID, TokenIndex start, ScopeIndex scope)
	{
		this->nodeID = nodeID;
		this->start = start;
		this->scope = scope;
	}

	Node(const Node& copy)
	{
		*this = copy;
	}

	Node& operator=(const Node& copy)
	{
		start = copy.start;
		end = copy.end;
		scope = copy.scope;
		nodeID = copy.nodeID;
		index = copy.index;

		switch (nodeID)
		{
		case Unknown:
		case CommentStmnt:
			break;
		case ExpressionStmnt:
			expressionStmnt = copy.expressionStmnt;
			break;
		case UsingStmnt:
			using_ = copy.using_;
			break;
		case PackageStmnt:
			package = copy.package;
			break;
		case Definition:
			definition = copy.definition;
			break;
		case Function:
			function = copy.function;
			break;
		case Conditional:
			conditional = copy.conditional;
			break;
		case IfStmnt:
			ifStmnt = copy.ifStmnt;
			break;
		case ForStmnt:
			forStmnt = copy.forStmnt;
			break;
		case WhileStmnt:
			whileStmnt = copy.whileStmnt;
			break;
		case SwitchStmnt:
			switchStmnt = copy.switchStmnt;
			break;
		case TernaryStmnt:
			ternaryStmnt = copy.ternaryStmnt;
			break;
		case DeleteStmnt:
			deleteStmnt = copy.deleteStmnt;
			break;
		case DeferStmnt:
			deferStmnt = copy.deferStmnt;
			break;
		case ReturnStmnt:
			returnStmnt = copy.returnStmnt;
			break;
		case OnCompileStmnt:
			onCompileStmnt = copy.onCompileStmnt;
			break;
		case WhereStmnt:
			whereStmnt = copy.whereStmnt;
			break;
		case StateStmnt:
			state = copy.state;
			break;
		case Block:
			block = copy.block;
			break;
		default:
			break;
		}
		return *this;
	}

	~Node() {};
};