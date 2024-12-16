#pragma once

#include "EASTL/unordered_set.h"

#include "../Tokens/Token.h"
#include "Expr.h"
#include "Type.h"
#include "../Containers/Flags.h"

struct ExprArrHash;
struct ExprArrEqual;

enum StmntID
{
	InvalidStmnt = 0,
	CommentStmnt,
	ExpressionStmnt,
	ImportStmnt,
	Definition,
	InlineDefinition,
	LinkDecl,
	ExternFunctionDecl,
	FunctionStmnt,
	AnonFunction,
	FunctionDecl,
	StateStmnt,
	GenericsDecl,
	WhereStmnt,
	Method,
	StateOperator,
	Destructor,
	Constructor,
	EnumStmnt,
	Conditional,
	AssignmentStmnt,
	IfStmnt,
	ForStmnt,
	WhileStmnt,
	SwitchStmnt,
	DeleteStmnt,
	DeferStmnt,
	ContinueStmnt,
	BreakStmnt,
	ReturnStmnt,
	CompileStmnt,
	CompileDebugStmnt,
	Block,
	LogStmnt,
	AssertStmnt,
};

enum InsetID
{
	SizeInset,
	SOAInset,
	SerializedInset,
	NoAlignInset,
	InvalidInset
};

struct Body
{
	bool statement;
	Stmnt* body;

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

struct Stmnt
{
	Token* start;
	Token* end;
	StmntID nodeID;

	Token* package;
	Stmnt* scope;

	union
	{
		struct
		{
			Expr* expression;
		} expressionStmnt;

		struct
		{
			Token* packageName;
			Token* alias;
		} importStmnt;

		struct
		{
			Token* name;
			Type* type;
			Token* op;
			Expr* assignment;
		} definition;

		struct
		{
			Type* type;
			Token* op;
			Expr* assignment;
		} inlineDefinition;

		struct
		{
			Token* platform;
			Token* lib;
		} linkDecl;

		struct
		{
			Type* returnType;
			Token* externName;
			Token* callName;
			eastl::vector<Stmnt*>* parameters;
			eastl::vector<Stmnt*>* links;
		} externFunction;

		struct
		{
			Type* returnType;
			Token* name;
			Stmnt* generics;
			Stmnt* decl;
		} function;

		struct
		{
			Type* returnType;
			Stmnt* decl;
		} anonFunction;

		struct
		{
			eastl::vector<Stmnt*>* parameters;
			Body body;
		} functionDecl;

		struct
		{
			Token* name;
			Stmnt* generics;
			eastl::vector<Stmnt*>* members;
			Flags<>* insetFlags;
		} state;

		struct
		{
			eastl::vector<Token*>* names;
			Stmnt* whereStmnt;
			eastl::hash_set<eastl::vector<Expr*>*, ExprArrHash, ExprArrEqual>* templatesToExpand;
		} generics;

		struct
		{
			Stmnt* decl;
		} whereStmnt;

		struct
		{
			Type* returnType;
			Token* stateName;
			Token* name;
			Stmnt* generics;
			Stmnt* decl;
		} method;

		struct
		{
			Type* returnType;
			Token* stateName;
			Token* op;
			Stmnt* decl;
		} stateOperator;

		struct
		{
			Token* stateName;
			Token* del;
			Stmnt* decl;
		} destructor;

		struct
		{
			Token* stateName;
			Stmnt* decl;
		} constructor;

		struct
		{
			Token* name;
			Type* type;
			eastl::vector<Token*>* names;
			eastl::vector<Expr*>* valueExprs;
			eastl::vector<intmax_t>* values;
		} enumStmnt;

		struct
		{
			Expr* condition;
			Body body;
		} conditional;

		struct
		{
			Expr* assignTo;
			Token* op;
			Expr* assignment;
		} assignmentStmnt;

		struct
		{
			Stmnt* condition;
			eastl::vector<Stmnt*>* elifs;
			Body elseCondition;
		} ifStmnt;

		struct
		{
			bool rangeFor;
			bool isDeclaration;
			union
			{
				Stmnt* declaration;
				Token* identifier;
			} iterated;
			Token* iterator;
			Expr* toIterate;
			Body body;
		} forStmnt;

		struct
		{
			Stmnt* conditional;
		} whileStmnt;

		struct
		{
			Expr* switchOn;
			eastl::vector<Stmnt*>* cases;
			Body defaultCase;
		} switchStmnt;

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
				Stmnt* conditional;
				Body body;
			};
		} deferStmnt;

		struct
		{
			Token* token;
		} continueStmnt;

		struct
		{
			Token* token;
		} breakStmnt;

		struct
		{
			Expr* expr;
		} returnStmnt;

		struct
		{
			Type* returnType;
			Body body;
		} compileStmnt;

		struct
		{
			Body body;
		} compileDebugStmnt;

		struct
		{
			eastl::vector<Stmnt*>* inner;
		} block;

		struct
		{
			eastl::vector<Expr*>* exprs;
		} logStmnt;

		struct
		{
			Expr* expr;
			Expr* message;
		} assertStmnt;
	};

	Stmnt()
	{
		nodeID = StmntID::InvalidStmnt;
		start = nullptr;
		end = nullptr;
		package = nullptr;
		scope = nullptr;
	}

	Stmnt(StmntID nodeID, Token* start, Token* package, Stmnt* scope)
	{
		this->nodeID = nodeID;
		this->start = start;
		this->end = nullptr;
		this->package = package;
		this->scope = scope;
	}

	Stmnt(const Stmnt& copy)
	{
		*this = copy;
	}

	Stmnt& operator=(const Stmnt& copy)
	{
		start = copy.start;
		end = copy.end;
		nodeID = copy.nodeID;
		package = copy.package;
		scope = copy.scope;

		switch (nodeID)
		{
		case InvalidStmnt:
		case CommentStmnt:
			break;
		case ExpressionStmnt:
			expressionStmnt = copy.expressionStmnt;
			break;
		case ImportStmnt:
			importStmnt = copy.importStmnt;
			break;
		case Definition:
			definition = copy.definition;
			break;
		case InlineDefinition:
			inlineDefinition = copy.inlineDefinition;
			break;
		case LinkDecl:
			linkDecl = copy.linkDecl;
			break;
		case ExternFunctionDecl:
			externFunction = copy.externFunction;
			break;
		case FunctionStmnt:
			function = copy.function;
			break;
		case AnonFunction:
			anonFunction = copy.anonFunction;
			break;
		case FunctionDecl:
			functionDecl = copy.functionDecl;
			break;
		case StateStmnt:
			state = copy.state;
			break;
		case EnumStmnt:
			enumStmnt = copy.enumStmnt;
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
		case LogStmnt:
			logStmnt = copy.logStmnt;
			break;
		case AssertStmnt:
			assertStmnt = copy.assertStmnt;
			break;
		default:
			break;
		}
		return *this;
	}
};