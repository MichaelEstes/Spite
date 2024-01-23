#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"
#include "../Log/Errors.h"
#include "../Log/Logger.h"
#include "../Utils/Utils.h"
#include "../Containers/Arena.h"

typedef size_t NodeIndex;
typedef size_t TokenIndex;
typedef size_t ScopeIndex;

struct Node;

enum TypeID
{
	InvalidType,
	UnknownType,
	PrimitiveType,
	NamedType,
	ExplicitType,
	ImplicitType,
	PointerType,
	ArrayType,
	FunctionType,
	ImportedType,
};

struct Type
{
	TypeID typeID;

	union
	{
		struct
		{
			size_t size;
			TokenIndex name;
			bool isSigned;
		} primitiveType;

		struct
		{
			TokenIndex typeName;
		} namedType;

		struct
		{
		} explicitType;

		struct
		{
		} implicitType;

		struct
		{
			bool raw;
			TokenIndex ptr;
			Type* type;
		} pointerType;

		struct
		{
			TokenIndex arr;
			Type* type;
		} arrayType;

		struct
		{
			Type* returnType;
			// Param types
			// Block?
		} functionType;

		struct
		{
			TokenIndex packageName;
			TokenIndex typeName;
		} importedType;

	};

	Type()
	{
		typeID = TypeID::InvalidType;
	}

	Type(TypeID typeID)
	{
		this->typeID = typeID;
	}

	Type(const Type& copy)
	{
		*this = copy;
	}

	Type& operator=(const Type& copy)
	{
		typeID = copy.typeID;

		switch (typeID)
		{
		case PrimitiveType:
			primitiveType = copy.primitiveType;
			break;
		case NamedType:
			namedType = copy.namedType;
			break;
		case ExplicitType:
			explicitType = copy.explicitType;
			break;
		case ImplicitType:
			implicitType = copy.implicitType;
			break;
		case PointerType:
			pointerType = copy.pointerType;
			break;
		case ArrayType:
			arrayType = copy.arrayType;
			break;
		case FunctionType:
			functionType = copy.functionType;
			break;
		case ImportedType:
			importedType = copy.importedType;
			break;
		default:
			break;
		}
		return *this;
	}

	eastl::string ToString(Tokens& tokens)
	{
		switch (typeID)
		{
		case UnknownType:
			return "implicit";
		case PrimitiveType:
			return tokens.At(primitiveType.name)->ToString();
		case NamedType:
			return tokens.At(namedType.typeName)->ToString();
		case ExplicitType:
			return "";
		case ImplicitType:
			return "";
		case PointerType:
			return tokens.At(pointerType.ptr)->ToString() + pointerType.type->ToString(tokens);
		case ArrayType:
			return tokens.At(arrayType.arr)->ToString() + arrayType.type->ToString(tokens);
		case FunctionType:
			return functionType.returnType->ToString(tokens);
		case ImportedType:
			return tokens.At(importedType.packageName)->ToString() +
				"." +
				tokens.At(importedType.typeName)->ToString();
		default:
			return "";
		}
	}
};


enum ExprID
{
	InvalidExpr,
	LiteralExpr,
	IdentifierExpr,
	SelectorExpr,
	IndexExpr,
	FunctionCallExpr,
	NewExpr,
	AnonTypeExpr,
	BinaryExpr,
	UnaryExpr,
	GroupedExpr,
};

struct Expr
{
	ExprID typeID;
	TokenIndex start;

	union {
		struct
		{
			TokenIndex val;
			UniqueType type;
		} literalExpr;

		struct
		{
			TokenIndex identifier;
		} identfierExpr;

		struct
		{
			Expr* on;
			Expr* select;
		} selectorExpr;

		struct
		{
			Expr* of;
			Expr* index;
			TokenIndex lBrack;
			TokenIndex rBrack;
		} indexExpr;

		struct
		{
			Expr* function;
			eastl::vector<Expr*>* params;
			TokenIndex lParen;
			TokenIndex rParen;
		} functionCallExpr;

		struct
		{
			TokenIndex newIndex;
			Expr* primaryExpr;
			Expr* atExpr;
		} newExpr;

		struct
		{
			eastl::vector<Node*> definitions;
		} anonTypeExpr;

		struct
		{
			Expr* left;
			Expr* right;
			TokenIndex op;
			UniqueType opType;
		} binaryExpr;

		struct
		{
			Expr* expr;
			TokenIndex op;
			UniqueType opType;
		} unaryExpr;

		struct
		{
			Expr* expr;
			TokenIndex lParen;
			TokenIndex rParen;
		} groupedExpr;
	};

	Expr(ExprID typeID, TokenIndex start)
	{
		this->typeID = typeID;
		this->start = start;
	}

	Expr()
	{
		typeID = ExprID::InvalidExpr;
	}

	Expr(const Expr& copy)
	{
		*this = copy;
	}

	Expr(TokenIndex start, Token* op, Expr* left, Expr* right)
	{
		typeID = ExprID::BinaryExpr;
		this->start = start;
		this->binaryExpr.op = op->index;
		this->binaryExpr.opType = op->uniqueType;
		this->binaryExpr.left = left;
		this->binaryExpr.right = right;
	}

	Expr& operator=(const Expr& copy)
	{
		typeID = copy.typeID;
		start = copy.start;

		switch (typeID)
		{
		case LiteralExpr:
			literalExpr = copy.literalExpr;
			break;
		case IdentifierExpr:
			identfierExpr = copy.identfierExpr;
			break;
		case SelectorExpr:
			selectorExpr = copy.selectorExpr;
			break;
		case IndexExpr:
			indexExpr = copy.indexExpr;
			break;
		case FunctionCallExpr:
			functionCallExpr = copy.functionCallExpr;
			break;
		case NewExpr:
			newExpr = copy.newExpr;
			break;
		case AnonTypeExpr:
			anonTypeExpr = copy.anonTypeExpr;
			break;
		case BinaryExpr:
			binaryExpr = copy.binaryExpr;
			break;
		case UnaryExpr:
			unaryExpr = copy.unaryExpr;
			break;
		case GroupedExpr:
			groupedExpr = copy.groupedExpr;
			break;
		default:
			break;
		}
		return *this;
	}

	eastl::string ToString(Tokens& tokens)
	{
		switch (typeID)
		{
		case LiteralExpr:
			return tokens.At(literalExpr.val)->ToString();
		case IdentifierExpr:
			return tokens.At(identfierExpr.identifier)->ToString();
		case SelectorExpr:
			return selectorExpr.on->ToString(tokens) + "." + selectorExpr.select->ToString(tokens);
		case IndexExpr:
			return indexExpr.of->ToString(tokens) +
				tokens.At(indexExpr.lBrack)->ToString() +
				indexExpr.index->ToString(tokens) +
				tokens.At(indexExpr.rBrack)->ToString();
			break;
		case FunctionCallExpr:
		{
			eastl::string params = "";
			if (functionCallExpr.params != nullptr)
			{
				for (Expr* expr : *functionCallExpr.params)
				{
					params += expr->ToString(tokens) + ",";
				}
			}

			return functionCallExpr.function->ToString(tokens) +
				tokens.At(functionCallExpr.lParen)->ToString() +
				params +
				tokens.At(functionCallExpr.rParen)->ToString();
		}
		break;
		case NewExpr:
			return tokens.At(newExpr.newIndex)->ToString() + " " +
				newExpr.primaryExpr->ToString(tokens) +
				(newExpr.atExpr != nullptr ? " at " + newExpr.atExpr->ToString(tokens) : "");
			break;
		case AnonTypeExpr:
			return "";
		case BinaryExpr:
			return binaryExpr.left->ToString(tokens) +
				tokens.At(binaryExpr.op)->ToString() +
				binaryExpr.right->ToString(tokens);
		case UnaryExpr:
			return tokens.At(unaryExpr.op)->ToString() +
				unaryExpr.expr->ToString(tokens);
		case GroupedExpr:
			return tokens.At(groupedExpr.lParen)->ToString() +
				groupedExpr.expr->ToString(tokens) +
				tokens.At(groupedExpr.rParen)->ToString();
		default:
			return "";
		}
	}

	~Expr() {};
};

enum NodeID
{
	Unknown = 0,
	ExpressionStmnt,
	Using_,
	Package_,
	Definition,
	Function,
	State_,
	Generics,
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
			Node* generics;
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
			eastl::vector<Type>* types;
		} generics;

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
		case Generics:
			generics = copy.generics;
			break;
		case Block:
			block = copy.block;
			break;
		default:
			break;
		}
		return *this;
	}

	eastl::string ToString(Tokens& tokens)
	{
		switch (nodeID)
		{
		case Unknown:
			return "unknown";
		case Comment_:
			return tokens.At(start)->ToString();
		case ExpressionStmnt:
			return expressionStmnt.expression->ToString(tokens);
		case Using_:
			return tokens.At(start)->ToString() + " " +
				tokens.At(using_.packageName)->ToString() +
				(using_.alias != -1 ? " as " + tokens.At(using_.alias)->ToString() : "") +
				tokens.At(end)->ToString();
		case Package_:
			return tokens.At(start)->ToString() + " " +
				tokens.At(package.name)->ToString() +
				tokens.At(end)->ToString();
		case Definition:
			return tokens.At(definition.name)->ToString() + " : " +
				definition.type.ToString(tokens) + " : " +
				(definition.assignment != nullptr ? definition.assignment->ToString(tokens) : "") +
				tokens.At(end)->ToString();
		case Function:
		{
			eastl::string params = "(";
			if (function.parameters != nullptr)
			{
				for (Node* param : *function.parameters)
				{
					params += param->ToString(tokens) + ", ";
				}
			}
			params += ")";
			return function.returnType.ToString(tokens) + " " +
				tokens.At(function.name)->ToString() +
				(function.generics != nullptr ? function.generics->ToString(tokens) : "") +
				params;
		}
		case State_:
			return "";
		case Generics:
		{
			eastl::string types = "";

			for (Type type : *generics.types)
			{
				types += type.ToString(tokens) + ", ";
			}

			return tokens.At(start)->ToString() +
				types +
				tokens.At(end)->ToString();
		}
		return "";
		case Block:
			return "";
		default:
			return "";
		}
	}

	~Node() {};
};

struct Scope
{
	NodeIndex scopeOf;
	ScopeIndex parent;
	ScopeIndex index;
	eastl::vector<NodeIndex> nodes;

	Scope()
	{
		scopeOf = 0;
		parent = 0;
		index = 0;
		nodes = eastl::vector<NodeIndex>();
	}

	Scope(const Scope& copy)
	{
		scopeOf = copy.scopeOf;
		parent = copy.parent;
		index = copy.index;
		nodes = copy.nodes;
	}

	void AddNode(Node node)
	{
		nodes.push_back(node.index);
	}
};

struct Syntax
{
	Tokens& tokens;
	Scope currScope;
	Token* curr;
	Errors errors;

	eastl::vector<Node> nodes;
	eastl::vector<Scope> scopes;
	eastl::vector<Node> imports;
	Node package;
	Arena* arena;

	Syntax(Tokens& tokensRef) : tokens(tokensRef)
	{
		curr = nullptr;
		nodes = eastl::vector<Node>();
		scopes = eastl::vector<Scope>();
		imports = eastl::vector<Node>();
		currScope = Scope();
		scopes.push_back(currScope);
	}

	~Syntax()
	{
		delete arena;
	}

	void Print()
	{
		if (package.nodeID == NodeID::Package_)
		{
			Logger::Info(package.ToString(tokens));
		}

		for (Node node : imports)
		{
			Logger::Info(node.ToString(tokens));
		}

		for (Node node : nodes)
		{
			Logger::Info(node.ToString(tokens));
		}
	}

	void AddNode(Node& node)
	{
		node.index = nodes.size();
		currScope.AddNode(node);
		nodes.emplace_back(node);
	}

	Node CreateNode(Token* start, NodeID nodeID)
	{
		Node node = Node(nodeID, start->index, currScope.index);
		return node;
	}

	Expr* CreateExpr(TokenIndex start, ExprID exprID)
	{
		return arena->Emplace<Expr>(exprID, start);
	}

	Expr* CopyExpr(Expr* expr)
	{
		Expr* copy = CreateExpr(expr->start, expr->typeID);
		*copy = *expr;
		return copy;
	}

	void StartScope()
	{
		Scope scope = Scope();
		scope.index = scopes.size();
		scope.parent = currScope.index;
		scope = scopes.emplace_back(scope);
		currScope = scope;
	}

	void EndScope()
	{
		currScope = scopes.at(currScope.parent);
	}

	inline bool IsGlobalScope()
	{
		return currScope.index == 0 && currScope.parent == 0;
	}

	inline bool IsEOF()
	{
		return curr->type == TokenType::EndOfFile;
	}

	void BuildSyntax()
	{
		arena = new Arena((tokens.tokens.size() / 4) * sizeof(Node));
		curr = tokens.First();

		ParseComments();
		ParsePackage();

		while (!IsEOF())
		{
			ParseNext();
		}

		Print();
	}

	inline bool Expect(UniqueType type, const eastl::string& errMsg = "")
	{
		return ToExpect(curr->uniqueType, type, errMsg);
	}

	inline bool Expect(TokenType type, const eastl::string& errMsg = "")
	{
		return ToExpect(curr->type, type, errMsg);
	}

	inline bool ThenExpect(UniqueType type, const eastl::string& errMsg = "")
	{
		Advance();
		return ToExpect(curr->uniqueType, type, errMsg);
	}

	inline bool ThenExpect(TokenType type, const eastl::string& errMsg = "")
	{
		Advance();
		return ToExpect(curr->type, type, errMsg);
	}

	inline bool ToExpect(size_t toCheck, size_t checkAginst, const eastl::string& errMsg)
	{
		bool expected = toCheck == checkAginst;
		if (!expected && errMsg != "") AddError(curr, errMsg);
		return toCheck == checkAginst;
	}

	inline bool ExpectSemicolon()
	{
		return Expect(UniqueType::Semicolon, errors.missingSemicolon);
	}

	inline bool ThenExpectSemicolon()
	{
		return ThenExpect(UniqueType::Semicolon, errors.missingSemicolon);
	}

	inline void Advance()
	{
		curr = tokens.Next(curr);
		ParseComments();
	}

	inline Token* Peek()
	{
		return tokens.Next(curr);
	}

	void ParseComments()
	{
		while (Expect(TokenType::Comment))
		{
			Node node = CreateNode(curr, NodeID::Comment_);
			AddNode(node);
			curr = tokens.Next(curr);
		}
	}

	inline void AddError(Token* token, const eastl::string& msg)
	{
		Logger::AddMessage(LogLevel::ERROR, token->pos, token->index, msg);
	}

	void ParsePackage(bool setInTree = true)
	{
		Token* start = curr;
		if (Expect(UniqueType::Package, errors.missingPackage) &&
			ThenExpect(TokenType::Identifier, errors.missingPackageName))
		{
			Node node = CreateNode(start, NodeID::Package_);
			node.package.name = curr->index;
			if (ThenExpectSemicolon())
			{
				node.end = curr->index;
				if (setInTree) package = node;
				Advance();
			}
		}
	}

	void ParseNext()
	{
		bool advance = true;
		switch (curr->uniqueType)
		{
		case UniqueType::Any:
			if (Expect(TokenType::Comment)) ParseComments();
			advance = false;
			break;
		case UniqueType::Package:
			AddError(curr, errors.multiplePackages);
			ParsePackage(false);
			advance = false;
			break;
		case UniqueType::Using:
			ParseUsing();
			advance = false;
			break;
		case UniqueType::Name:
		{
			Node assignment = ParseAssignmentStatement();
			if (assignment.nodeID != NodeID::Unknown)
			{
				AddNode(assignment);
				return;
			}
		}
		default:
		{
			Token* start = curr;
			Type type = ParseDeclarationType();
			if (type.typeID == TypeID::InvalidType) break;
			Node func = ParseFunction(type, start);
			if (func.nodeID != NodeID::Unknown) AddNode(func);
			return;
		}
		}

		if (advance) Advance();
	}

	void ParseUsing()
	{
		bool addNode = IsGlobalScope();
		if (!addNode) AddError(curr, errors.usingOutsideOfGlobal);

		Token* start = curr;
		if (Expect(UniqueType::Using) &&
			ThenExpect(TokenType::Identifier, errors.missingUsingName))
		{
			Node node = CreateNode(start, NodeID::Using_);
			node.using_.packageName = curr->index;
			Advance();

			if (Expect(UniqueType::As)
				&& ThenExpect(TokenType::Identifier, errors.expectedUsingAlias))
			{
				node.using_.alias = curr->index;
			}
			else
			{
				node.using_.alias = -1;
			}

			if (ThenExpectSemicolon())
			{
				node.end = curr->index;
				if (addNode) imports.push_back(node);
				Advance();
			}
		}
	}

	Node ParseFunction(Type& returnType, Token* start)
	{
		if (Expect(TokenType::Identifier, errors.expectedFunctionName))
		{
			Node function = CreateNode(start, NodeID::Function);
			Token* name = curr;
			Advance();

			if (Expect(UniqueType::Less))
			{
				Node* generics = ParseGenerics();
				function.function.generics = generics;
			}
			else
			{
				function.function.generics = nullptr;
			}

			if (Expect(UniqueType::DoubleColon))
			{
				//Parse method
			}
			else if (Expect(UniqueType::Lparen, errors.expectedFunction))
			{
				function.function.returnType = returnType;
				function.function.name = name->index;
				Advance();
				eastl::vector<Node*>* parameters = ParseParametersList();
				function.function.parameters = parameters;
				if (Expect(UniqueType::Rparen, errors.expectedFunctionClosure)) Advance();

				if (Expect(UniqueType::Lbrace))
				{
					function.function.exprFunction = false;
					//Node* block = ParseBlock(function);
					return function;
				}
				else if (Expect(UniqueType::FatArrow))
				{
					function.function.exprFunction = true;
					function.function.body.expr = ParseExpr();
					return function;
				}
				else
				{
					AddError(curr, errors.expectedBlockStart);
				}

			}
		}

		return Node();
	}

	Node* ParseGenerics()
	{
		Node* generics = arena->Emplace<Node>(CreateNode(curr, NodeID::Generics));
		eastl::vector<Type>* genericTypes = arena->Emplace<eastl::vector<Type>>();
		generics->generics.types = genericTypes;

		Advance();

		if (Expect(UniqueType::Greater))
		{
			Advance();
			AddError(curr, errors.emptyGenerics);
			return generics;
		}

		while (!Expect(UniqueType::Greater) && !IsEOF())
		{
			Type type = ParseDeclarationType();
			if (type.typeID != TypeID::InvalidType) genericTypes->push_back(type);
			if (Expect(UniqueType::Comma)) Advance();
		}

		if (Expect(UniqueType::Greater, errors.expectedGenericsClosure))
		{
			generics->end = curr->index;
			Advance();
		}

		return generics;
	}

	Node* ParseBlock(Node& of)
	{
		Node* block = arena->Emplace<Node>(CreateNode(curr, NodeID::Block));
		if (!Expect(UniqueType::Lbrace, errors.expectedBlockStart)) return nullptr;
		Advance();

		StartScope();

		while (!Expect(UniqueType::Rbrace) && !IsEOF())
		{
			Node* node = ParseBlockStatment();
			//block->block.inner->push_back(ParseBlockStatment());
		}

		if (Expect(UniqueType::Rbrace, errors.expectedBlockEnd))
		{
			block->end = curr->index;
			Advance();
		}

		EndScope();

		return block;
	}

	Node* ParseBlockStatment()
	{
		switch (curr->uniqueType)
		{
		default:
			break;
		}
		return nullptr;
	}

	eastl::vector<Node*>* ParseParametersList()
	{
		eastl::vector<Node*>* parameters = arena->Emplace<eastl::vector<Node*>>();

		if (Expect(UniqueType::Rparen)) return parameters;

		Node first = ParseDeclaration();
		if (first.nodeID != NodeID::Unknown) parameters->push_back(arena->Emplace<Node>(first));
		while (Expect(UniqueType::Comma) && !IsEOF())
		{
			Node decl = ParseDeclaration();
			if (decl.nodeID != NodeID::Unknown) parameters->push_back(arena->Emplace<Node>(decl));
		}

		if (Expect(UniqueType::Assign))
		{
			ParseAssignment(parameters->back());
		}

		else if (Expect(TokenType::Identifier))
		{
			Node assign = ParseAssignmentStatement();
			if (assign.nodeID != NodeID::Unknown) parameters->push_back(arena->Emplace<Node>(assign));
		}

		while (Expect(UniqueType::Comma) && !IsEOF())
		{
			Node def = ParseAssignmentStatement();
			if (def.nodeID != NodeID::Unknown) parameters->push_back(arena->Emplace<Node>(def));
		}

		return parameters;
	}

	void ParseIf()
	{
	}

	void ParseFor()
	{
	}

	void ParseSwitch()
	{
	}

	void ParseWhile()
	{
	}

	Node ParseAssignmentStatement()
	{
		Token* start = curr;
		switch (Peek()->uniqueType)
		{
		case UniqueType::ImplicitAssign:
		case UniqueType::Colon:
		{
			Node def = ParseDefinition();
			return def;
		}
		default:
			//Advance();
			//AddError(curr, errors.invalidTokenAfterIdentifier);
			return Node();
		}
	}

	Node ParseDefinition()
	{
		Token* start = curr;
		Expect(TokenType::Identifier, errors.identifierExpected);
		switch (Peek()->uniqueType)
		{
		case UniqueType::ImplicitAssign:
			return ParseImplicitAssignment();
		case UniqueType::Colon:
			return ParseExplicitAssignment();
		default:
			AddError(start, errors.expectedDefinition);
			return Node();
		}
	}

	Node ParseImplicitAssignment()
	{
		Token* start = curr;
		if (Expect(TokenType::Identifier) &&
			ThenExpect(UniqueType::ImplicitAssign))
		{
			Advance();
			Type type = Type(TypeID::UnknownType);
			Expr* expr = ParseAssignmentType();
			Node node = CreateNode(start, NodeID::Definition);
			node.definition.name = start->index;
			node.definition.type = type;
			node.definition.assignment = expr;
			if (ExpectSemicolon())
			{
				node.end = curr->index;
				Advance();
			}
			return node;
		}

		return Node();
	}

	Node ParseExplicitAssignment()
	{
		Node node = ParseDeclaration();

		if (ParseAssignment(&node)) return node;

		return Node();
	}

	bool ParseAssignment(Node* decl)
	{
		if (Expect(UniqueType::Assign, errors.expectedAssignment))
		{
			Advance();
			Expr* expr = ParseAssignmentType();
			decl->definition.assignment = expr;
			decl->end = curr->index;

			Advance();
			return true;
		}

		return false;
	}

	Node ParseDeclaration()
	{
		Token* start = curr;
		if (Expect(TokenType::Identifier, errors.identifierExpected) &&
			ThenExpect(UniqueType::Colon, errors.expectedColon))
		{
			Advance();
			Type type = ParseDeclarationType();
			Node node = CreateNode(start, NodeID::Definition);
			node.definition.name = start->index;
			node.definition.type = type;
			node.definition.assignment = nullptr;

			return node;
		}

		return Node();
	}

	Type ParseDeclarationType()
	{
		Type type = Type();

		switch (curr->type)
		{
		case Primitive:
			type = CreatePrimitive();
			Advance();
			break;

		case Identifier:
		{
			TokenIndex name = curr->index;
			if (Peek()->uniqueType == UniqueType::Period)
			{
				Advance();
				if (ThenExpect(TokenType::Identifier, errors.expectedImportedType))
				{
					type.typeID = TypeID::ImportedType;
					type.importedType.packageName = name;
					type.importedType.typeName = curr->index;
					Advance();
				}
			}
			else
			{
				type.typeID = TypeID::NamedType;
				type.namedType.typeName = name;
				Advance();
			}
		}
		break;

		default:
			switch (curr->uniqueType)
			{
			case UniqueType::Lbrace:
				break;

			case UniqueType::Multiply:
			case UniqueType::Rawpointer:
				type = ParsePointerType();
				break;

			case UniqueType::Array:
				break;

			case UniqueType::Period:
				break;

			default:
				AddError(curr, errors.expectedType);
				break;
			}
			break;
		}

		return type;
	}

	Type ParsePointerType()
	{
		Type ptrType = Type(TypeID::PointerType);
		ptrType.pointerType.raw = Expect(UniqueType::Rawpointer);
		ptrType.pointerType.ptr = curr->index;
		Advance();
		ptrType.pointerType.type = arena->Emplace<Type>(ParseDeclarationType());
		return ptrType;
	}

	Expr* ParseAssignmentType()
	{
		switch (curr->uniqueType)
		{
		case UniqueType::New:
			return ParseNewExpr();

		default:
			return ParseExpr();
		}
	}

	Expr* ParseNewExpr()
	{
		TokenIndex newIndex = curr->index;
		Expr* newExpr = CreateExpr(newIndex, ExprID::NewExpr);
		newExpr->newExpr.newIndex = newIndex;
		Advance();
		newExpr->newExpr.primaryExpr = ParsePrimaryExpr();
		if (Expect(UniqueType::At))
		{
			Advance();
			newExpr->newExpr.atExpr = ParsePrimaryExpr();
		}
		else
		{
			newExpr->newExpr.atExpr = nullptr;
		}

		return newExpr;
	}

	Expr* ParseAnonymousType()
	{
		Token* rBrace = curr;
		Advance();

		eastl::vector<Node*> params = eastl::vector<Node*>();
		while (curr->uniqueType != UniqueType::Lbrace && !IsEOF())
		{
			Node* def = arena->Emplace<Node>(ParseDefinition());
			if (def->nodeID == NodeID::Definition) params.push_back(def);
			if (Expect(UniqueType::Comma)) Advance();
		}

		if (params.size() == 0)
		{
			AddError(rBrace, errors.emptyAnonymousType);
			return nullptr;
		}

		Expr* anonTypeExpr = CreateExpr(rBrace->index, ExprID::AnonTypeExpr);

		return anonTypeExpr;
	}

	Expr* ParseExpr()
	{
		return ParseBinaryExpr();
	}

	Expr* ParseBinaryExpr(Expr* expr = nullptr, int currPrecedence = 1)
	{
		if (!expr)
		{
			expr = ParseUnaryExpr();
		}

		while (!Expect(UniqueType::Semicolon))
		{
			Token* op = curr;
			int opPrecedence = GetOperatorPrecedence();

			if (opPrecedence < currPrecedence) return expr;

			Advance();
			Expr* right = ParseBinaryExpr(nullptr, opPrecedence + 1);
			::new(expr) Expr(expr->start, op, CopyExpr(expr), right);
		}

		return expr;
	}

	Expr* ParseUnaryExpr()
	{
		if (IsUnaryOperator())
		{
			Token* op = curr;
			Advance();
			Expr* expr = ParseUnaryExpr();
			Expr* unary = CreateExpr(op->index, ExprID::UnaryExpr);
			unary->unaryExpr.op = op->index;
			unary->unaryExpr.expr = expr;
			return unary;
		}

		return ParsePrimaryExpr();
	}

	Expr* ParsePrimaryExpr(Expr* expr = nullptr)
	{
		if (!expr)
		{
			expr = ParseOperand();
		}

		while (!Expect(UniqueType::Semicolon))
		{
			switch (curr->uniqueType)
			{
			case UniqueType::Period:
				expr = ParseSelector(expr);
				break;

			case UniqueType::Lbrack:
				expr = ParseIndex(expr);
				break;

			case UniqueType::Lparen:
				expr = ParseFunctionCall(expr);
				break;

			case UniqueType::As:

				break;

			default:
				return expr;
				break;
			}
		}

		return expr;
	}

	Expr* ParseOperand()
	{
		switch (curr->uniqueType)
		{
		case Name:
			return ParseIdentifierExpr();

		case IntLiteral:
		case FloatLiteral:
		case HexLiteral:
		case StringLiteral:
			return ParseLiteralExpr();

		case Lparen:
			return ParseGroupedExpr();

		default:
			AddError(curr, errors.missingOperand);
			return CreateExpr(curr->index, ExprID::InvalidExpr);
		}
	}

	Expr* ParseIdentifierExpr()
	{
		Expr* ident = CreateExpr(curr->index, ExprID::IdentifierExpr);
		ident->identfierExpr.identifier = curr->index;
		Advance();
		return ident;
	}

	Expr* ParseLiteralExpr()
	{
		Expr* primLit = CreateExpr(curr->index, ExprID::LiteralExpr);
		primLit->literalExpr.type = curr->uniqueType;
		primLit->literalExpr.val = curr->index;
		Advance();
		return primLit;
	}

	Expr* ParseGroupedExpr()
	{
		Token* lParen = curr;
		Expr* groupExpr = CreateExpr(curr->index, ExprID::GroupedExpr);
		groupExpr->groupedExpr.lParen = lParen->index;
		Advance();
		Expr* innerExpr = ParseExpr();
		groupExpr->groupedExpr.expr = innerExpr;
		if (Expect(UniqueType::Rparen, errors.unclosedGroupExpression))
		{
			groupExpr->groupedExpr.rParen = curr->index;
			Advance();
		}
		return groupExpr;
	}

	Expr* ParseSelector(Expr* on)
	{
		Expr* selector = CreateExpr(curr->index, ExprID::SelectorExpr);
		selector->selectorExpr.on = CopyExpr(on);
		if (ThenExpect(TokenType::Identifier, errors.identifierExpected))
		{
			selector->selectorExpr.select = ParseIdentifierExpr();
		}
		return selector;
	}

	Expr* ParseIndex(Expr* of)
	{
		Token* lBrack = curr;
		Expr* indexExpr = CreateExpr(lBrack->index, ExprID::IndexExpr);
		indexExpr->indexExpr.of = CopyExpr(of);
		indexExpr->indexExpr.lBrack = lBrack->index;
		Advance();
		if (curr->uniqueType == UniqueType::Rbrack)
		{
			indexExpr->indexExpr.rBrack = curr->index;
			AddError(curr, errors.emptyIndex);
		}
		else
		{
			Expr* expr = ParseExpr();
			indexExpr->indexExpr.index = expr;
			if (Expect(UniqueType::Rbrack, errors.unclosedIndex))
			{
				indexExpr->indexExpr.rBrack = curr->index;
				Advance();
			}
		}

		return indexExpr;
	}

	Expr* ParseFunctionCall(Expr* of)
	{
		Expr* funcCall = CreateExpr(of->start, ExprID::FunctionCallExpr);
		Token* lParen = curr;
		funcCall->functionCallExpr.function = CopyExpr(of);
		funcCall->functionCallExpr.lParen = lParen->index;
		Advance();
		if (!Expect(UniqueType::Rparen))
		{
			funcCall->functionCallExpr.params = ParseExprList();
		}
		else
		{
			funcCall->functionCallExpr.params = nullptr;
		}

		if (Expect(UniqueType::Rparen, errors.unclosedFunctionCall))
		{
			funcCall->functionCallExpr.rParen = curr->index;
			Advance();
		}

		return funcCall;
	}

	eastl::vector<Expr*>* ParseExprList()
	{
		eastl::vector<Expr*>* exprs = arena->Emplace<eastl::vector<Expr*>>();

		exprs->push_back(ParseExpr());
		while (Expect(UniqueType::Comma) && !IsEOF())
		{
			Advance();
			exprs->push_back(ParseExpr());
		}

		return exprs;
	}

	Expr* ParseFunctionParam()
	{
		if (curr->uniqueType == UniqueType::Rbrace)
		{
			return ParseAnonymousType();
		}
		else
		{
			return ParseExpr();
		}
	}

	size_t GetTargetArchBitWidth()
	{
		return 64;
	}

	bool IsUnaryOperator()
	{
		UniqueType uniqueType = curr->uniqueType;
		return curr->type == TokenType::Operator &&
			(uniqueType == UniqueType::Subtract ||
				uniqueType == UniqueType::Not ||
				uniqueType == UniqueType::Xor);
	}

	bool IsBinaryOperator()
	{
		UniqueType uniqueType = curr->uniqueType;
		return curr->type == TokenType::Operator &&
			(uniqueType == UniqueType::Add ||
				uniqueType == UniqueType::Subtract ||
				uniqueType == UniqueType::Multiply ||
				uniqueType == UniqueType::Divide ||
				uniqueType == UniqueType::Modulo ||
				uniqueType == UniqueType::And ||
				uniqueType == UniqueType::Or ||
				uniqueType == UniqueType::Xor ||
				uniqueType == UniqueType::Shiftl ||
				uniqueType == UniqueType::Shiftr ||
				uniqueType == UniqueType::AndNot ||
				uniqueType == UniqueType::LogicAnd ||
				uniqueType == UniqueType::LogicOr ||
				uniqueType == UniqueType::Equal ||
				uniqueType == UniqueType::Less ||
				uniqueType == UniqueType::Greater ||
				uniqueType == UniqueType::NotEql ||
				uniqueType == UniqueType::LessEqual ||
				uniqueType == UniqueType::GreaterEqual);
	}

	int GetOperatorPrecedence()
	{
		switch (curr->uniqueType) {
		case UniqueType::LogicOr:
			return 1;
		case UniqueType::LogicAnd:
			return 2;
		case UniqueType::Equal:
		case UniqueType::NotEql:
		case UniqueType::Less:
		case UniqueType::Greater:
		case UniqueType::LessEqual:
		case UniqueType::GreaterEqual:
			return 3;
		case UniqueType::Add:
		case UniqueType::Subtract:
		case UniqueType::Or:
		case UniqueType::Xor:
			return 4;
		case UniqueType::Multiply:
		case UniqueType::Divide:
		case UniqueType::Modulo:
		case UniqueType::And:
		case UniqueType::AndNot:
		case UniqueType::Shiftl:
		case UniqueType::Shiftr:
			return 5;
		default:
			AddError(curr, errors.expectedBinaryOperator);
			return 0;
		}
	}

	Type CreatePrimitive()
	{
		Token* start = curr;
		Type type = Type(TypeID::PrimitiveType);
		type.primitiveType.name = curr->index;
		switch (curr->uniqueType)
		{
		case UniqueType::Void:
			type.primitiveType.size = 0;
			type.primitiveType.isSigned = false;
			break;
		case UniqueType::Bool:
			type.primitiveType.size = 8;
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Byte:
			type.primitiveType.size = 8;
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Ubyte:
			type.primitiveType.size = 8;
			type.primitiveType.isSigned = false;
			break;
		case UniqueType::Int:
			type.primitiveType.size = GetTargetArchBitWidth();
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Int16:
			type.primitiveType.size = 16;
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Int32:
			type.primitiveType.size = 32;
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Int64:
			type.primitiveType.size = 64;
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Int128:
			type.primitiveType.size = 128;
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Uint:
			type.primitiveType.size = GetTargetArchBitWidth();
			type.primitiveType.isSigned = false;
			break;
		case UniqueType::Uint16:
			type.primitiveType.size = 16;
			type.primitiveType.isSigned = false;
			break;
		case UniqueType::Uint32:
			type.primitiveType.size = 32;
			type.primitiveType.isSigned = false;
			break;
		case UniqueType::Uint64:
			type.primitiveType.size = 64;
			type.primitiveType.isSigned = false;
			break;
		case UniqueType::Uint128:
			type.primitiveType.size = 128;
			type.primitiveType.isSigned = false;
			break;
		case UniqueType::Float:
			type.primitiveType.size = GetTargetArchBitWidth();
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Float32:
			type.primitiveType.size = 32;
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::Float64:
			type.primitiveType.size = 64;
			type.primitiveType.isSigned = true;
			break;
		case UniqueType::String:
			type.primitiveType.size = GetTargetArchBitWidth() * 2;
			type.primitiveType.isSigned = false;
			break;
		default:
			break;
		}

		return type;
	}
};
