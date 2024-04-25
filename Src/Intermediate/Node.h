#pragma once
#include "../Tokens/Token.h"
#include "Expr.h"
#include "Type.h"
#include "../Containers/Flags.h"

enum NodeID
{
	InvalidNode = 0,
	CommentStmnt,
	ExpressionStmnt,
	UsingStmnt,
	PackageStmnt,
	Definition,
	InlineDefinition,
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
	Token* start;
	Token* end;
	NodeID nodeID;

	Node* scope;

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
		} using_;

		struct
		{
			Token* name;
		} package;

		struct
		{
			Type* type;
			Token* name;
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
			Type* returnType;
			Token* name;
			Node* generics;
			Node* decl;
		} function;

		struct
		{
			Type* returnType;
			Node* decl;
		} anonFunction;

		struct
		{
			eastl::vector<Node*>* parameters;
			Body body;
		} functionDecl;

		struct
		{
			Token* name;
			Node* generics;
			eastl::vector<Node*>* members;
			Flags<>* insetFlags;
		} state;

		struct
		{
			eastl::vector<Token*>* names;
			Node* whereStmnt;
			size_t count;
			eastl::vector<Type*>* types;
		} generics;

		struct
		{
			Node* decl;
		} whereStmnt;

		struct
		{
			Type* returnType;
			Token* stateName;
			Token* name;
			Node* generics;
			Node* decl;
		} method;

		struct
		{
			Type* returnType;
			Token* stateName;
			Node* generics;
			Token* op;
			Node* decl;
		} stateOperator;

		struct
		{
			Token* stateName;
			Token* del;
			Body body;
		} destructor;

		struct
		{
			Token* stateName;
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
			Token* op;
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
				Token* identifier;
			} iterated;
			Token* iterator;
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
			Token* token;
		} continueStmnt;

		struct
		{
			Token* token;
		} breakStmnt;

		struct
		{
			bool voidReturn;
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
			eastl::vector<Node*>* inner;
		} block;
	};

	Node()
	{
		start = nullptr;
		end = nullptr;
		nodeID = NodeID::InvalidNode;
	}

	Node(NodeID nodeID, Token* start, Node* scope)
	{
		this->nodeID = nodeID;
		this->start = start;
		this->end = nullptr;
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
		nodeID = copy.nodeID;
		scope = copy.scope;

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
		default:
			break;
		}
		return *this;
	}

	~Node() {};
};

bool operator==(const Type& left, const Type& right)
{
	if (left.typeID == TypeID::ValueType) return *left.valueType.type == right;
	if (right.typeID == TypeID::ValueType) return *right.valueType.type == left;
	if (left.typeID != right.typeID) return false;

	switch (left.typeID)
	{
	case PrimitiveType:
	{
		auto& l = left.primitiveType;
		auto& r = right.primitiveType;
		return l.isSigned == r.isSigned && l.size == r.size && l.type == r.type;
	}
	case NamedType:
		return left.namedType.typeName->val == right.namedType.typeName->val;
	case ExplicitType:
	{
		if (left.explicitType.declarations->size() != right.explicitType.declarations->size()) return false;
		for (int i = 0; i < left.explicitType.declarations->size(); i++)
		{
			if (*left.explicitType.declarations->at(i)->definition.type !=
				*right.explicitType.declarations->at(i)->definition.type) return false;
		}
		return true;
	}
	case ImplicitType:
		// Implicit Types cannot be compared
		return false;
	case PointerType:
	{
		auto& l = left.pointerType;
		auto& r = right.pointerType;
		return *l.type == *r.type;
	}
	case ValueType:
		// Should never get here, value types are just a signal to copy a type's memory into a new location
		// Essentially the opposite of a reference type, since everything defaults to a reference
		return false;
	case ArrayType:
		return *left.arrayType.type == *right.arrayType.type;
	case GenericsType:
	{
		auto& l = left.genericsType;
		auto& r = right.genericsType;
		eastl::vector<Type*>* lTypes = l.generics->genericsExpr.types;
		eastl::vector<Type*>* rTypes = r.generics->genericsExpr.types;
		if (lTypes->size() != rTypes->size()) return false;
		for (int i = 0; i < lTypes->size(); i++)
		{
			if (*lTypes->at(i) != *rTypes->at(i)) return false;
		}

		return *l.type == *r.type;
	}
	case FunctionType:
	{
		auto& l = left.functionType;
		auto& r = right.functionType;
		if (*l.returnType != *r.returnType) return false;
		if (l.paramTypes->size() != r.paramTypes->size()) return false;
		for (int i = 0; i < l.paramTypes->size(); i++)
		{
			if (*l.paramTypes->at(i) != *r.paramTypes->at(i)) return false;
		}
		return true;
	}
	case ImportedType:
		return left.importedType.packageName->val == right.importedType.packageName->val &&
			left.importedType.typeName->val == right.importedType.typeName->val;
	default:
		break;
	}

	return false;
}

bool operator!=(const Type& left, const Type& right)
{
	return !(left == right);
}