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
	Using_,
	Package_,
	Definition,
	Function,
	State_,
	Block,
	Comment_,
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
			bool exprFunction;
			union
			{
				Node* block;
				Expr* expr;
			} body;
		} function;

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
		case Comment_:
			break;
		case ExpressionStmnt:
			expressionStmnt = copy.expressionStmnt;
			break;
		case Using_:
			using_ = copy.using_;
			break;
		case Package_:
			package = copy.package;
			break;
		case Definition:
			definition = copy.definition;
			break;
		case Function:
			function = copy.function;
			break;
		case State_:
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