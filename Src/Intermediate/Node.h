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
	InvalidNode = 0,
	CommentStmnt,
	ExpressionStmnt,
	UsingStmnt,
	PackageStmnt,
	Definition,
	InlineDefinition,
	Function,
	FunctionDecl,
	StateStmnt,
	InsetStmnt,
	GenericsDecl,
	WhereStmnt,
	Method,
	StateOperator,
	Destructor,
	Constructor,
	Conditional,
	AssignmentStmnt,
	IfStmnt,
	ForStmnt,
	WhileStmnt,
	SwitchStmnt,
	TernaryStmnt,
	DeleteStmnt,
	DeferStmnt,
	ContinueStmnt,
	BreakStmnt,
	ReturnStmnt,
	CompileStmnt,
	CompileDebugStmnt,
	Block,
};

enum InsetID
{
	SizeInset,
	NullInset,
	SerializedInset,
	NoAlignInset,
};

struct Body
{
	bool statement;
	Node* body;

	Body()
	{
		statement = false;
		body = nullptr;
	}

	Body(const Body& copy)
	{
		statement = copy.statement;
		body = copy.body;
	}

	operator void* () const
	{
		return (void*)body;
	}
};

struct Node
{
	TokenIndex start;
	TokenIndex end;
	ScopeIndex scope;
	NodeID nodeID;

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
			TokenIndex op;
			Expr* assignment;
		} definition;

		struct
		{
			Type type;
			TokenIndex op;
			Expr* assignment;
		} inlineDefinition;

		struct
		{
			Type returnType;
			TokenIndex name;
			Node* generics;
			Node* decl;
		} function;

		struct
		{
			eastl::vector<Node*>* parameters;
			Body body;
		} functionDecl;

		struct
		{
			TokenIndex name;
			Node* generics;
			eastl::vector<Node*>* members;
		} state;

		struct
		{
			InsetID type;
		} insetStmnt;

		struct
		{
			eastl::vector<TokenIndex>* names;
			Node* whereStmnt;
		} generics;

		struct
		{
			Node* decl;
		} whereStmnt;

		struct
		{
			Type returnType;
			TokenIndex stateName;
			TokenIndex name;
			Node* generics;
			Node* decl;
		} method;

		struct
		{
			Type returnType;
			TokenIndex stateName;
			Node* generics;
			TokenIndex op;
			Node* decl;
		} stateOperator;

		struct
		{
			TokenIndex stateName;
			TokenIndex del;
			Body body;
		} destructor;

		struct
		{
			TokenIndex stateName;
			Node* decl;
		} constructor;

		struct
		{
			Expr* condition;
			Body body;
		} conditional;

		struct
		{
			Expr* assignTo;
			TokenIndex op;
			Expr* assignment;
		} assignmentStmnt;

		struct
		{
			Node* condition;
			eastl::vector<Node*>* elifs;
			Body elseCondition;
		} ifStmnt;

		struct
		{
			bool rangeFor;
			bool isDeclaration;
			union
			{
				Node* declaration;
				TokenIndex identifier;
			}iterated;
			TokenIndex iterator;
			Expr* toIterate;
			Body body;
		} forStmnt;

		struct
		{
			Node* conditional;
		} whileStmnt;

		struct
		{
			Expr* switchOn;
			eastl::vector<Node*>* cases;
			Body defaultCase;
		} switchStmnt;

		struct
		{
		} ternaryStmnt;

		struct
		{
			Expr* primaryExpr;
			bool arrDelete;
		} deleteStmnt;

		struct
		{
			bool deferIf;
			union
			{
				Node* conditional;
				Body body;
			};
		} deferStmnt;

		struct
		{
			TokenIndex token;
		} continueStmnt;

		struct
		{
			TokenIndex token;
		} breakStmnt;

		struct
		{
			bool voidReturn;
			Expr* expr;
		} returnStmnt;

		struct
		{
			Expr* compileExpr;
		} compileStmnt;

		struct
		{
			Body body;
		} compileDebugStmnt;

		struct
		{
			eastl::vector<Node*>* inner;
		} block;
	};

	Node()
	{
		start = 0;
		end = 0;
		scope = 0;
		nodeID = NodeID::InvalidNode;
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

		switch (nodeID)
		{
		case InvalidNode:
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
		case InlineDefinition:
			inlineDefinition = copy.inlineDefinition;
			break;
		case Function:
			function = copy.function;
			break;
		case FunctionDecl:
			functionDecl = copy.functionDecl;
			break;
		case StateStmnt:
			state = copy.state;
			break;
		case InsetStmnt:
			insetStmnt = copy.insetStmnt;
			break;
		case GenericsDecl:
			generics = copy.generics;
			break;
		case WhereStmnt:
			whereStmnt = copy.whereStmnt;
			break;
		case Method:
			method = copy.method;
			break;
		case StateOperator:
			stateOperator = copy.stateOperator;
			break;
		case Destructor:
			destructor = copy.destructor;
			break;
		case Constructor:
			constructor = copy.constructor;
			break;
		case Conditional:
			conditional = copy.conditional;
			break;
		case AssignmentStmnt:
			assignmentStmnt = copy.assignmentStmnt;
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
		case ContinueStmnt:
			continueStmnt = copy.continueStmnt;
			break;
		case BreakStmnt:
			breakStmnt = copy.breakStmnt;
			break;
		case ReturnStmnt:
			returnStmnt = copy.returnStmnt;
			break;
		case CompileStmnt:
			compileStmnt = copy.compileStmnt;
			break;
		case CompileDebugStmnt:
			compileDebugStmnt = copy.compileDebugStmnt;
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