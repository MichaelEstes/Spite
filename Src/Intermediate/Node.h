#pragma once

#include "EASTL/hash_map.h"

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
			eastl::hash_map<size_t, eastl::vector<Expr*>*>* templates;
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

bool operator==(const Expr& left, const Expr& right);
bool operator!=(const Expr& left, const Expr& right);

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
		eastl::vector<Expr*>* lTempl = l.generics->genericsExpr.templates;
		eastl::vector<Expr*>* rTempl = r.generics->genericsExpr.templates;
		if (lTempl->size() != rTempl->size()) return false;
		for (int i = 0; i < lTempl->size(); i++)
		{
			if (*lTempl->at(i) != *rTempl->at(i)) return false;
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

bool operator==(const Expr& left, const Expr& right)
{
	if (left.typeID != right.typeID) return false;

	switch (left.typeID)
	{
	case InvalidExpr:
		//Maybe this should return false?
		return true;
	case LiteralExpr:
		return left.literalExpr.val->val == right.literalExpr.val->val &&
			left.literalExpr.type == right.literalExpr.type;
	case IdentifierExpr:
		return left.identifierExpr.identifier->val == right.identifierExpr.identifier->val;
	case PrimitiveExpr:
		return left.primitiveExpr.primitive == right.primitiveExpr.primitive;
	case SelectorExpr:
		return *left.selectorExpr.on == *right.selectorExpr.on &&
			*left.selectorExpr.select == *right.selectorExpr.select;
	case IndexExpr:
		return *left.indexExpr.of == *right.indexExpr.of &&
			*left.indexExpr.index == *right.indexExpr.index;
	case FunctionCallExpr:
	{
		if (left.functionCallExpr.params->size() != right.functionCallExpr.params->size()) return false;

		for (size_t i = 0; i < left.functionCallExpr.params->size(); i++)
		{
			if (*left.functionCallExpr.params->at(i) != *right.functionCallExpr.params->at(i)) return false;
		}

		return *left.functionCallExpr.function == *right.functionCallExpr.function;
	}
	case NewExpr:
		return *left.newExpr.primaryExpr == *right.newExpr.primaryExpr;
	case FixedExpr:
		return *left.fixedExpr.atExpr == *right.fixedExpr.atExpr;
	case AnonTypeExpr:
	{
		//Not sure what good comparing anon types is
		if (left.anonTypeExpr.values->size() != left.anonTypeExpr.values->size()) return false;
		return true;
	}
	case AsExpr:
		return *left.asExpr.to == *right.asExpr.to &&
			*left.asExpr.of == *right.asExpr.of;
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
	case TypeExpr:
		break;
	case FunctionTypeDeclExpr:
		break;
	case CompileExpr:
		break;
	default:
		break;
	}

	return false;
}

bool operator!=(const Expr& left, const Expr& right)
{
	return !(left == right);
}

inline size_t HashType(const Type* type);
inline size_t HashExpr(const Expr* expr);

inline size_t HashType(const Type* type)
{
	StringViewHash inplaceStrHasher;
	switch (type->typeID)
	{
	case PrimitiveType:
		return type->primitiveType.type;
	case NamedType:
	{
		size_t hash = 0;
		auto& namedType = type->namedType;
		hash += inplaceStrHasher(namedType.typeName->val);
		return hash;
	}
	case ExplicitType:
	{
		size_t hash = 0;
		for (Node* node : *type->explicitType.declarations)
		{
			Type* defType = node->definition.type;
			hash += HashType(defType);
		}
		return hash;
	}
	case PointerType:
	{
		size_t hash = '*';
		return hash + HashType(type->pointerType.type);
	}
	case ValueType:
	{
		size_t hash = '~';
		return hash + HashType(type->valueType.type);
	}
	case ArrayType:
	{
		size_t hash = '[' + ']';
		return hash + HashType(type->arrayType.type);
	}
	case GenericsType:
	{
		size_t hash = 0;
		auto& genericType = type->genericsType;
		for (Expr* templ : *genericType.generics->genericsExpr.templates)
		{
			hash += HashExpr(templ);
		}
		return hash + HashType(genericType.type);
	}
	case FunctionType:
	{
		size_t hash = 0;
		auto& functionType = type->functionType;
		hash += HashType(functionType.returnType);
		for (Type* param : *functionType.paramTypes)
		{
			hash += HashType(param);
		}
		return hash;
	}
	case ImportedType:
	{
		size_t hash = 0;
		auto& importedType = type->importedType;
		hash += inplaceStrHasher(importedType.packageName->val);
		hash += inplaceStrHasher(importedType.typeName->val);
		return hash;
	}
	default:
		break;
	}

	Logger::FatalError("SymbolTable:TypeHash Unable to create hash for Type");
	return 0;
}

inline size_t HashExpr(const Expr* expr)
{
	StringViewHash inplaceStrHasher;
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
	case TypeExpr:
	{
		return HashType(expr->typeExpr.type);
	}
	case FunctionTypeDeclExpr:
		break;
	case CompileExpr:
		break;
	default:
		break;
	}

	Logger::FatalError("SymbolTable:TypeHash Unable to create hash for Type");
	return 0;
}

eastl::string ToString(Expr* expr);
eastl::string ToString(Type* type);
eastl::string ToString(Body& body);

eastl::string ToString(Node* node)
{
	switch (node->nodeID)
	{
	case InvalidNode:
		return "INVALID";
	case CommentStmnt:
		return node->start->ToString();
	case ExpressionStmnt:
		return ToString(node->expressionStmnt.expression);
	case UsingStmnt:
		return node->start->ToString() + " " +
			node->using_.packageName->ToString() +
			(node->using_.alias ? " as " + node->using_.alias->ToString() : "");
	case PackageStmnt:
		return node->start->ToString() + " " +
			node->package.name->ToString();
	case Definition:
		return node->definition.name->ToString() + " : " +
			ToString(node->definition.type) +
			(node->definition.assignment != nullptr
				? " " + node->definition.op->ToString() + " " + ToString(node->definition.assignment) : "");
	case InlineDefinition:
		return ToString(node->inlineDefinition.type) +
			" " + node->inlineDefinition.op->ToString() + " " +
			ToString(node->inlineDefinition.assignment);
	case FunctionStmnt:
		return ToString(node->function.returnType) + " " +
			node->function.name->ToString() +
			(node->function.generics != nullptr ? ToString(node->function.generics) : "") +
			ToString(node->function.decl);
	case AnonFunction:
		return ToString(node->anonFunction.returnType) +
			ToString(node->anonFunction.decl);
	case Method:
		return ToString(node->method.returnType) + " " +
			node->method.stateName->ToString() + "::" +
			node->method.name->ToString() +
			(node->method.generics != nullptr ? ToString(node->method.generics) : "") +
			ToString(node->method.decl);
	case StateOperator:
		return ToString(node->stateOperator.returnType) + " " +
			node->stateOperator.stateName->ToString() + "::operator" +
			(node->stateOperator.generics != nullptr ? ToString(node->stateOperator.generics) : "") + "::" +
			node->stateOperator.op->ToString() +
			ToString(node->stateOperator.decl);
	case Destructor:
		return node->destructor.stateName->ToString() + "::" +
			node->destructor.del->ToString() +
			ToString(node->destructor.body);
	case Constructor:
		return node->destructor.stateName->ToString() + "::" +
			ToString(node->constructor.decl);
	case FunctionDecl:
	{
		eastl::string params = "(";
		if (node->functionDecl.parameters != nullptr)
		{
			for (Node* param : *node->functionDecl.parameters)
			{
				params += ToString(param) + ", ";
			}
		}
		params += ")";
		return params + ToString(node->functionDecl.body);
	}
	case Conditional:
		return "(" + ToString(node->conditional.condition) + ")" +
			ToString(node->conditional.body);
	case AssignmentStmnt:
		return ToString(node->assignmentStmnt.assignTo) +
			" " + node->assignmentStmnt.op->ToString() + " " +
			ToString(node->assignmentStmnt.assignment);
	case IfStmnt:
	{
		eastl::string elseifs = "";
		for (Node* elseif : *node->ifStmnt.elifs)
		{
			elseifs += "else if " + ToString(elseif);
		}

		return node->start->ToString() + " " +
			ToString(node->ifStmnt.condition) +
			elseifs +
			(node->ifStmnt.elseCondition ? "else " + ToString(node->ifStmnt.elseCondition) : "");
	}
	case ForStmnt:
		return node->start->ToString() + " (" +
			(node->forStmnt.isDeclaration
				? ToString(node->forStmnt.iterated.declaration)
				: node->forStmnt.iterated.identifier->ToString()) +
			" " + node->forStmnt.iterator->ToString() + " " +
			ToString(node->forStmnt.toIterate) + ")" +
			ToString(node->forStmnt.body);
	case WhileStmnt:
		return node->start->ToString() + " "
			+ ToString(node->whileStmnt.conditional);
	case SwitchStmnt:
	{
		eastl::string cases = "";
		for (Node* node : *node->switchStmnt.cases)
		{
			cases += "case " + ToString(node);
		}

		return node->start->ToString() + " (" +
			ToString(node->switchStmnt.switchOn) + " )\n" +
			cases +
			"default" + ToString(node->switchStmnt.defaultCase) + "\n";
	}
	case DeleteStmnt:
		return node->start->ToString() +
			(node->deleteStmnt.arrDelete ? "[]" : "") + " " +
			ToString(node->deleteStmnt.primaryExpr);
	case DeferStmnt:
		return node->start->ToString() + " " +
			(node->deferStmnt.deferIf
				? ToString(node->deferStmnt.conditional)
				: ToString(node->deferStmnt.body));
	case ContinueStmnt:
		return node->continueStmnt.token->ToString();
	case BreakStmnt:
		return node->breakStmnt.token->ToString();
	case ReturnStmnt:
		return node->start->ToString() + " " +
			(!node->returnStmnt.voidReturn ? ToString(node->returnStmnt.expr) : "");
	case WhereStmnt:
	{
		return node->start->ToString() + ToString(node->whereStmnt.decl);
	}
	case StateStmnt:
	{
		eastl::string insets = "";
		Flags<>* flags = node->state.insetFlags;
		if ((*flags)[SizeInset]) insets += "[size]\n";
		if ((*flags)[SOAInset]) insets += "[soa]\n";
		if ((*flags)[SerializedInset]) insets += "[serialized]\n";
		if ((*flags)[NoAlignInset]) insets += "[noalign]\n";

		eastl::string members = "";
		for (Node* member : *node->state.members)
		{
			members += ToString(member) + ",\n";
		}

		return node->start->ToString() + " " +
			node->state.name->ToString() +
			(node->state.generics ? ToString(node->state.generics) : "") +
			"\n{\n" + insets + members + "}\n";
	}
	case GenericsDecl:
	{
		eastl::string names = "";
		for (Token* name : *node->generics.names)
		{
			names += name->ToString() + ", ";
		}

		return "<" + names +
			(node->generics.whereStmnt ? " : " + ToString(node->generics.whereStmnt) : "") + ">";
	}
	case CompileStmnt:
		return node->start->ToString() + " " +
			ToString(node->compileStmnt.returnType) + " " +
			ToString(node->compileStmnt.body);
	case CompileDebugStmnt:
		return node->start->ToString() + " " + ToString(node->compileDebugStmnt.body);
	case Block:
	{
		eastl::string stmnts = "";
		for (Node* stmnt : *node->block.inner)
		{
			stmnts += ToString(stmnt) + "\n";
		}
		return "\n{\n" + stmnts + "}\n";
	}
	default:
		return "";
	}
}

eastl::string ToString(Body& body)
{
	if (!body) return "";
	if (body.statement) return " " + ToString(body.body) + "\n";
	else return ToString(body.body);
}


eastl::string ToString(Expr* expr)
{
	switch (expr->typeID)
	{
	case LiteralExpr:
		return expr->literalExpr.val->ToString();
	case IdentifierExpr:
		return expr->identifierExpr.identifier->ToString();
	case PrimitiveExpr:
		return expr->primitiveExpr.primitive->ToString();
	case SelectorExpr:
		return ToString(expr->selectorExpr.on) + "." + ToString(expr->selectorExpr.select);
	case IndexExpr:
		return ToString(expr->indexExpr.of) +
			expr->indexExpr.lBrack->ToString() +
			ToString(expr->indexExpr.index) +
			expr->indexExpr.rBrack->ToString();
		break;
	case FunctionCallExpr:
	{
		eastl::string params = "";
		if (expr->functionCallExpr.params != nullptr)
		{
			for (Expr* expr : *expr->functionCallExpr.params)
			{
				params += ToString(expr) + ", ";
			}
		}

		return ToString(expr->functionCallExpr.function) +
			expr->functionCallExpr.lParen->ToString() +
			params +
			expr->functionCallExpr.rParen->ToString();
	}
	break;
	case NewExpr:
		return expr->newExpr.newIndex->ToString() + " " +
			ToString(expr->newExpr.primaryExpr) +
			(expr->newExpr.atExpr != nullptr ? " at " + ToString(expr->newExpr.atExpr) : "");
	case FixedExpr:
		return expr->fixedExpr.fixed->ToString() + " " +
			ToString(expr->fixedExpr.atExpr);
	case AnonTypeExpr:
	{
		eastl::string values = "";
		for (Expr* expr : *expr->anonTypeExpr.values)
		{
			values += ToString(expr) + ", ";
		}

		return "{" + values + "}";
	}
	case AsExpr:
		return ToString(expr->asExpr.of) + " as " +
			ToString(expr->asExpr.to);
	case DereferenceExpr:
		return ToString(expr->dereferenceExpr.of) +
			expr->dereferenceExpr.op->ToString();
	case ReferenceExpr:
		return ToString(expr->referenceExpr.of) +
			expr->referenceExpr.op->ToString();
	case BinaryExpr:
		return ToString(expr->binaryExpr.left) +
			expr->binaryExpr.op->ToString() +
			ToString(expr->binaryExpr.right);
	case UnaryExpr:
		return expr->unaryExpr.op->ToString() +
			ToString(expr->unaryExpr.expr);
	case GroupedExpr:
		return expr->groupedExpr.lParen->ToString() +
			ToString(expr->groupedExpr.expr) +
			expr->groupedExpr.rParen->ToString();

	case GenericsExpr:
	{
		eastl::string templates = "";

		for (Expr* templ : *expr->genericsExpr.templates)
		{
			templates += ToString(templ) + ", ";
		}

		return (expr->genericsExpr.expr != nullptr ? ToString(expr->genericsExpr.expr) : "") +
			expr->genericsExpr.open->ToString() +
			templates +
			expr->genericsExpr.close->ToString();
	}
	case TypeExpr:
		return ToString(expr->typeExpr.type);
	case FunctionTypeDeclExpr:
		return ToString(expr->functionTypeDeclExpr.anonFunction);
	case CompileExpr:
		return ToString(expr->compileExpr.compile);
	default:
		return "";
	}
}

eastl::string PrimitiveToString(UniqueType type)
{
	switch (type)
	{
	case UniqueType::Void:
		return "void";
	case UniqueType::Bool:
		return "bool";
	case UniqueType::Byte:
		return "byte";
	case UniqueType::Ubyte:
		return "ubyte";
	case UniqueType::Int:
		return "int";
	case UniqueType::Int16:
		return "int16";
	case UniqueType::Int32:
		return "int32";
	case UniqueType::Int64:
		return "int64";
	case UniqueType::Int128:
		return "int128";
	case UniqueType::Uint:
		return "uint";
	case UniqueType::Uint16:
		return "uint16";
	case UniqueType::Uint32:
		return "uint32";
	case UniqueType::Uint64:
		return "uint64";
	case UniqueType::Uint128:
		return "uint128";
	case UniqueType::Float:
		return "float";
	case UniqueType::Float32:
		return "float32";
	case UniqueType::Float64:
		return "float64";
	case UniqueType::String:
		return "string";
	default:
		break;
	}

	return "INVALID";
}

eastl::string ToString(Type* type)
{
	switch (type->typeID)
	{
	case UnknownType:
		return "implicit";
	case PrimitiveType:
		return PrimitiveToString(type->primitiveType.type);
	case NamedType:
		return type->namedType.typeName->ToString();
	case ExplicitType:
	{
		eastl::string types = "";

		for (Node* node : *type->explicitType.declarations)
		{
			types += ToString(node) + ", ";
		}

		return "{ " + types + "}";
	}
	case ImplicitType:
	{
		eastl::string types = "";

		for (Token* index : *type->implicitType.identifiers)
		{
			types += index->ToString() + ", ";
		}

		return "{ " + types + "}";
	}
	case PointerType:
		return "*" + ToString(type->pointerType.type);
	case ValueType:
		return "~" + ToString(type->valueType.type);
	case ArrayType:
		return "[]" + ToString(type->arrayType.type);
	case GenericsType:
		return ToString(type->genericsType.type) + ToString(type->genericsType.generics);
	case FunctionType:
	{
		eastl::string params = "";

		for (Type* type : *type->functionType.paramTypes)
		{
			params += ToString(type) + ", ";
		}

		return "::" + ToString(type->functionType.returnType) + "(" + params + ")";
	}
	case ImportedType:
		return type->importedType.packageName->ToString() +
			"." +
			type->importedType.typeName->ToString();
	default:
		return "";
	}
}