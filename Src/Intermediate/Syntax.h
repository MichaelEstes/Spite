#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"
#include "../Log/Errors.h"
#include "../Log/Logger.h"
#include "../Utils/Utils.h"
#include "../Containers/Arena.h"

#include "Node.h"
#include "Type.h"
#include "Expr.h"

typedef size_t NodeIndex;
typedef size_t TokenIndex;
typedef size_t ScopeIndex;

eastl::string ToString(Expr* expr, Tokens& tokens);
eastl::string ToString(Type* type, Tokens& tokens);
eastl::string ToString(Body& body, Tokens& tokens);

eastl::string ToString(Node& node, Tokens& tokens)
{
	switch (node.nodeID)
	{
	case InvalidNode:
		return "unknown";
	case CommentStmnt:
		return tokens.At(node.start)->ToString();
	case ExpressionStmnt:
		return ToString(node.expressionStmnt.expression, tokens);
	case UsingStmnt:
		return tokens.At(node.start)->ToString() + " " +
			tokens.At(node.using_.packageName)->ToString() +
			(node.using_.alias != -1 ? " as " + tokens.At(node.using_.alias)->ToString() : "") +
			tokens.At(node.end)->ToString();
	case PackageStmnt:
		return tokens.At(node.start)->ToString() + " " +
			tokens.At(node.package.name)->ToString() +
			tokens.At(node.end)->ToString();
	case Definition:
		return tokens.At(node.definition.name)->ToString() + " : " +
			ToString(&node.definition.type, tokens) +
			(node.definition.assignment != nullptr
				? +" = " + ToString(node.definition.assignment, tokens) + tokens.At(node.end)->ToString() : "");
	case Function:
	{
		eastl::string params = "(";
		if (node.function.parameters != nullptr)
		{
			for (Node* param : *node.function.parameters)
			{
				params += ToString(*param, tokens) + ", ";
			}
		}
		params += ")";
		return ToString(&node.function.returnType, tokens) + " " +
			tokens.At(node.function.name)->ToString() +
			(node.function.generics != nullptr ? ToString(node.function.generics, tokens) : "") +
			params +
			ToString(node.function.body, tokens);
	}
	case Conditional:
		return "(" + ToString(node.conditional.condition, tokens) + ")" +
			ToString(node.conditional.body, tokens);
	case IfStmnt:
	{
		eastl::string elseifs = "";
		for (Node* elseif : *node.ifStmnt.elifs)
		{
			elseifs += "else if " + ToString(*elseif, tokens);
		}

		return tokens.At(node.start)->ToString() + " " +
			ToString(*node.ifStmnt.condition, tokens) +
			elseifs +
			(node.ifStmnt.elseCondition ? "else " + ToString(node.ifStmnt.elseCondition, tokens) : "");
	}
	case ForStmnt:
		return tokens.At(node.start)->ToString() + " (" +
			(node.forStmnt.isDeclaration
				? ToString(*node.forStmnt.iterated.declaration, tokens)
				: tokens.At(node.forStmnt.iterated.identifier)->ToString()) +
			" " + tokens.At(node.forStmnt.iterator)->ToString() + " " +
			ToString(node.forStmnt.toIterate, tokens) + ")" +
			ToString(node.forStmnt.body, tokens);
	case WhileStmnt:
		return tokens.At(node.start)->ToString() + " "
			+ ToString(*node.whileStmnt.conditional, tokens);
	case SwitchStmnt:
	{
		eastl::string cases = "";
		for (Node* node : *node.switchStmnt.cases)
		{
			cases += "case " + ToString(*node, tokens);
		}

		return tokens.At(node.start)->ToString() + " (" +
			ToString(node.switchStmnt.switchOn, tokens) + " )\n" +
			cases +
			"default" + ToString(node.switchStmnt.defaultCase, tokens);
	}
	return "";
	case TernaryStmnt:
		return "";
	case DeleteStmnt:
		return tokens.At(node.start)->ToString() +
			(node.deleteStmnt.arrDelete ? "[]" : "") + " " +
			ToString(node.deleteStmnt.primaryExpr, tokens);
	case DeferStmnt:
		return "";
	case ReturnStmnt:
		return tokens.At(node.start)->ToString() + " " +
			(!node.returnStmnt.voidReturn ? ToString(node.returnStmnt.expr, tokens) : "");
	case OnCompileStmnt:
		return "";
	case WhereStmnt:
		return "";
	case StateStmnt:
		return "";
	case Block:
	{
		eastl::string stmnts = "";
		for (Node* stmnt : *node.block.inner)
		{
			stmnts += ToString(*stmnt, tokens) + "\n";
		}
		return "\n{\n" + stmnts + "}\n";
	}
	default:
		return "";
	}
}

eastl::string ToString(Body& body, Tokens& tokens)
{
	if (!body) return "";
	if (body.exprFunction)
	{
		return " " + ToString(body.expr, tokens) + "\n";
	}
	else
	{
		return ToString(*body.block, tokens);
	}
}


eastl::string ToString(Expr* expr, Tokens& tokens)
{
	switch (expr->typeID)
	{
	case LiteralExpr:
		return tokens.At(expr->literalExpr.val)->ToString();
	case IdentifierExpr:
		return tokens.At(expr->identfierExpr.identifier)->ToString();
	case PrimitiveExpr:
		return tokens.At(expr->primitiveExpr.primitive)->ToString();
	case SelectorExpr:
		return ToString(expr->selectorExpr.on, tokens) + "." + ToString(expr->selectorExpr.select, tokens);
	case IndexExpr:
		return ToString(expr->indexExpr.of, tokens) +
			tokens.At(expr->indexExpr.lBrack)->ToString() +
			ToString(expr->indexExpr.index, tokens) +
			tokens.At(expr->indexExpr.rBrack)->ToString();
		break;
	case FunctionCallExpr:
	{
		eastl::string params = "";
		if (expr->functionCallExpr.params != nullptr)
		{
			for (Expr* expr : *expr->functionCallExpr.params)
			{
				params += ToString(expr, tokens) + ",";
			}
		}

		return ToString(expr->functionCallExpr.function, tokens) +
			tokens.At(expr->functionCallExpr.lParen)->ToString() +
			params +
			tokens.At(expr->functionCallExpr.rParen)->ToString();
	}
	break;
	case NewExpr:
		return tokens.At(expr->newExpr.newIndex)->ToString() + " " +
			ToString(expr->newExpr.primaryExpr, tokens) +
			(expr->newExpr.atExpr != nullptr ? " at " + ToString(expr->newExpr.atExpr, tokens) : "");
	case FixedExpr:
		return tokens.At(expr->fixedExpr.fixed)->ToString() + " " +
			ToString(expr->fixedExpr.atExpr, tokens);
	case AnonTypeExpr:
		return "";
	case BinaryExpr:
		return ToString(expr->binaryExpr.left, tokens) +
			tokens.At(expr->binaryExpr.op)->ToString() +
			ToString(expr->binaryExpr.right, tokens);
	case UnaryExpr:
		return tokens.At(expr->unaryExpr.op)->ToString() +
			ToString(expr->unaryExpr.expr, tokens);
	case GroupedExpr:
		return tokens.At(expr->groupedExpr.lParen)->ToString() +
			ToString(expr->groupedExpr.expr, tokens) +
			tokens.At(expr->groupedExpr.rParen)->ToString();

	case GenericsExpr:
	{
		eastl::string types = "";

		for (Type type : *expr->genericsExpr.types)
		{
			types += ToString(&type, tokens) + ", ";
		}

		return (expr->genericsExpr.expr != nullptr ? ToString(expr->genericsExpr.expr, tokens) : "") +
			tokens.At(expr->genericsExpr.open)->ToString() +
			types +
			tokens.At(expr->genericsExpr.close)->ToString();
	}
	return "";
	default:
		return "";
	}
}

eastl::string ToString(Type* type, Tokens& tokens)
{
	switch (type->typeID)
	{
	case UnknownType:
		return "implicit";
	case PrimitiveType:
		return tokens.At(type->primitiveType.name)->ToString();
	case NamedType:
		return tokens.At(type->namedType.typeName)->ToString();
	case ExplicitType:
	{
		eastl::string types = "";

		for (Node* node : *type->explicitType.declarations)
		{
			types += ToString(*node, tokens) + ", ";
		}

		return "{ " + types + "}";
	}
	case ImplicitType:
	{
		eastl::string types = "";

		for (TokenIndex index : *type->implicitType.identifiers)
		{
			types += tokens.At(index)->ToString() + ", ";
		}

		return "{ " + types + "}";
	}
	case PointerType:
		return tokens.At(type->pointerType.ptr)->ToString() + ToString(type->pointerType.type, tokens);
	case ArrayType:
		return tokens.At(type->arrayType.arr)->ToString() + ToString(type->arrayType.type, tokens);
	case GenericsType:
		return ToString(type->genericsType.type, tokens) + ToString(type->genericsType.generics, tokens);
	case FunctionType:
		return ToString(type->functionType.returnType, tokens);
	case ImportedType:
		return tokens.At(type->importedType.packageName)->ToString() +
			"." +
			tokens.At(type->importedType.typeName)->ToString();
	default:
		return "";
	}
}

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
		if (package.nodeID == NodeID::PackageStmnt)
		{
			Logger::Info(ToString(package, tokens));
		}

		for (Node node : imports)
		{
			Logger::Info(ToString(node, tokens));
		}

		for (Node node : nodes)
		{
			Logger::Info(ToString(node, tokens));
		}
	}

	void AddNode(Node& node)
	{
		node.index = nodes.size();
		currScope.AddNode(node);
		nodes.emplace_back(node);
	}

	inline Node CreateNode(Token* start, NodeID nodeID)
	{
		return Node(nodeID, start->index, currScope.index);
	}

	inline Node* CreateNodePtr(Token* start, NodeID nodeID)
	{
		return arena->Emplace<Node>(nodeID, start->index, currScope.index);
	}

	inline Node* CreateNodePtr(Node node)
	{
		return arena->Emplace<Node>(node);
	}

	template<typename T>
	inline eastl::vector<T>* CreateVectorPtr()
	{
		return arena->Emplace<eastl::vector<T>>();
	}

	inline Expr* CreateExpr(TokenIndex start, ExprID exprID)
	{
		return arena->Emplace<Expr>(exprID, start);
	}

	inline Expr* CopyExpr(Expr* expr)
	{
		Expr* copy = CreateExpr(expr->start, expr->typeID);
		*copy = *expr;
		return copy;
	}

	inline void StartScope()
	{
		Scope scope = Scope();
		scope.index = scopes.size();
		scope.parent = currScope.index;
		scope = scopes.emplace_back(scope);
		currScope = scope;
	}

	inline void EndScope()
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
			Node node = CreateNode(curr, NodeID::CommentStmnt);
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
			Node node = CreateNode(start, NodeID::PackageStmnt);
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
			if (assignment.nodeID != NodeID::InvalidNode)
			{
				AddNode(assignment);
				return;
			}
		}
		default:
		{
			Token* start = curr;
			Type type = ParseDeclarationType(false);
			if (type.typeID == TypeID::InvalidType) break;
			Node func = ParseFunction(type, start);
			if (func.nodeID != NodeID::InvalidNode) AddNode(func);
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
			Node node = CreateNode(start, NodeID::UsingStmnt);
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

			function.function.generics = ParseGenerics();

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
					function.function.body.exprFunction = false;
					function.function.body.block = ParseBlock();
					return function;
				}
				else if (Expect(UniqueType::FatArrow))
				{
					function.function.body.exprFunction = true;
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

	Expr* ParseGenerics()
	{
		if (Expect(UniqueType::Less))
		{
			return ParseGenericsExpr();
		}
		else
		{
			return nullptr;
		}
	}

	Body ParseBody()
	{
		Body body = Body();

		if (Expect(UniqueType::Lbrace))
		{
			body.exprFunction = false;
			body.block = ParseBlock();
		}
		else
		{
			body.exprFunction = true;
			body.expr = ParseBodyExpr();
		}

		return body;
	}

	Expr* ParseBodyExpr()
	{
		StartScope();
		Expr* expr = ParseExpr();
		if (Expect(UniqueType::Semicolon)) Advance();
		EndScope();

		return expr;
	}

	Node* ParseBlock()
	{
		Node* block = CreateNodePtr(curr, NodeID::Block);
		block->block.inner = CreateVectorPtr<Node*>();
		if (!Expect(UniqueType::Lbrace, errors.expectedBlockStart))
		{
			block->nodeID = NodeID::InvalidNode;
			return block;
		}
		Advance();

		StartScope();

		while (!Expect(UniqueType::Rbrace) && !IsEOF())
		{
			Node* stmnt = ParseBlockStatment();
			if (stmnt != nullptr) block->block.inner->push_back(stmnt);
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
		case UniqueType::Name:
			return ParseIdentifierStmnt();
		case UniqueType::If:
			return ParseIf();
		case UniqueType::For:
			return ParseFor();
		case UniqueType::Switch:
			return ParseSwitch();
		case UniqueType::While:
			return ParseWhile();
		case UniqueType::Delete:
			return ParseDelete();
		case UniqueType::Return:
			return ParseReturn();
		default:
			Advance();
			break;
		}

		return nullptr;
	}

	Node* ParseIdentifierStmnt()
	{
		Token* next = Peek();
		if (next->uniqueType == UniqueType::Colon || next->uniqueType == UniqueType::ImplicitAssign)
		{
			return CreateNodePtr(ParseAssignmentStatement());
		}

		return ParseExprStmnt();
	}

	Node* ParseExprStmnt()
	{
		Node* node = CreateNodePtr(CreateNode(curr, NodeID::ExpressionStmnt));
		node->expressionStmnt.expression = ParseExpr();
		node->end = curr->index;
		return node;
	}

	Node* ParseIf()
	{
		Node* node = CreateNodePtr(CreateNode(curr, NodeID::IfStmnt));
		Advance();
		node->ifStmnt.condition = ParseConditional();
		node->ifStmnt.elifs = CreateVectorPtr<Node*>();
		node->ifStmnt.elseCondition = Body();
		while (Expect(UniqueType::Else) && Peek()->uniqueType == UniqueType::If)
		{
			Advance();
			Advance();
			node->ifStmnt.elifs->push_back(ParseConditional());
		}

		if (Expect(UniqueType::Else))
		{
			Advance();
			node->ifStmnt.elseCondition = ParseBody();
		}

		node->end = curr->index;
		return node;
	}

	Node* ParseConditional()
	{
		Node* node = CreateNodePtr(CreateNode(curr, NodeID::Conditional));
		if (Expect(UniqueType::Lparen, errors.expectedConditionOpen))
		{
			Advance();
			Expr* condition = ParseExpr();
			if (condition->typeID != ExprID::InvalidExpr &&
				Expect(UniqueType::Rparen, errors.expectedConditionClose))
			{
				Advance();
				node->conditional.condition = condition;
				node->conditional.body = ParseBody();
				node->end = curr->index;
				return node;
			}
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseFor()
	{
		Node* node = CreateNodePtr(CreateNode(curr, NodeID::ForStmnt));
		Advance();
		if (Expect(UniqueType::Lparen, errors.expectedForOpen))
		{
			Advance();
			if (Expect(TokenType::Identifier, errors.expectedForIdent))
			{
				if (Peek()->uniqueType == UniqueType::Colon)
				{
					node->forStmnt.isDeclaration = true;
					node->forStmnt.iterated.declaration = CreateNodePtr(ParseDeclaration());
				}
				else
				{
					node->forStmnt.isDeclaration = false;
					node->forStmnt.iterated.identifier = curr->index;
					Advance();
				}

				if (Expect(UniqueType::In)) node->forStmnt.rangeFor = false;
				else if (Expect(UniqueType::To)) node->forStmnt.rangeFor = true;
				else
				{
					AddError(curr, errors.expectedForIterator);
					node->nodeID = NodeID::InvalidNode;
					return node;
				}

				node->forStmnt.iterator = curr->index;
				Advance();

				Expr* expr = ParseExpr();
				if (expr->typeID != ExprID::InvalidExpr)
				{
					node->forStmnt.toIterate = expr;
					if (Expect(UniqueType::Rparen, errors.expectedForClose))
					{
						Advance();
						node->forStmnt.body = ParseBody();
						node->end = curr->index;
						return node;
					}
				}
			}
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseSwitch()
	{
		Node* node = CreateNodePtr(curr, NodeID::SwitchStmnt);
		Advance();

		if (Expect(UniqueType::Lparen, errors.expectedSwitchOpen))
		{
			Advance();
			Expr* switchOn = ParseExpr();
			if (switchOn->typeID != ExprID::InvalidExpr)
			{
				node->switchStmnt.switchOn = switchOn;

				if (Expect(UniqueType::Rparen, errors.expectedSwitchClose))
				{
					Advance();

					if (Expect(UniqueType::Lbrace, errors.expectedSwitchBlockOpen))
					{
						Advance();

						node->switchStmnt.cases = CreateVectorPtr<Node*>();
						while (Expect(UniqueType::Case))
						{
							Advance();
							node->switchStmnt.cases->push_back(ParseConditional());
						}

						if (Expect(UniqueType::Default, errors.expectedSwitchDefault))
						{
							Advance();
							node->switchStmnt.defaultCase = ParseBody();

							if (Expect(UniqueType::Rbrace, errors.expectedSwitchBlockClose))
							{
								node->end = curr->index;
								Advance();
								return node;
							}
						}
					}
				}
			}
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseWhile()
	{
		Node* node = CreateNodePtr(curr, NodeID::WhileStmnt);
		Advance();
		Node* conditional = ParseConditional();
		if (conditional->nodeID != NodeID::InvalidNode)
		{
			node->whileStmnt.conditional = conditional;
			node->end = curr->index;
			return node;
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseDelete()
	{
		Node* node = CreateNodePtr(curr, NodeID::DeleteStmnt);
		Advance();
		if (Expect(UniqueType::Array))
		{
			node->deleteStmnt.arrDelete = true;
			Advance();
		}
		else node->deleteStmnt.arrDelete = false;

		Expr* primaryExpr = ParsePrimaryExpr();
		if (primaryExpr->typeID != ExprID::InvalidExpr)
		{
			node->deleteStmnt.primaryExpr = primaryExpr;
			if (ExpectSemicolon())
			{
				node->end = curr->index;
				return node;
			}
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseReturn()
	{
		Node* node = CreateNodePtr(CreateNode(curr, NodeID::ReturnStmnt));
		Advance();
		if (Expect(UniqueType::Semicolon))
		{
			node->returnStmnt.voidReturn = true;
			node->returnStmnt.expr = nullptr;
		}
		else
		{
			node->returnStmnt.voidReturn = false;
			node->returnStmnt.expr = ParseExpr();
		}

		if (ExpectSemicolon())
		{
			node->end = curr->index;
			Advance();
		}

		return node;
	}

	eastl::vector<Node*>* ParseParametersList()
	{
		eastl::vector<Node*>* parameters = CreateVectorPtr<Node*>();

		if (Expect(UniqueType::Rparen)) return parameters;

		Node first = ParseDeclaration();
		if (first.nodeID != NodeID::InvalidNode) parameters->push_back(CreateNodePtr(first));
		while (Expect(UniqueType::Comma) && !IsEOF())
		{
			Node decl = ParseDeclaration();
			if (decl.nodeID != NodeID::InvalidNode) parameters->push_back(CreateNodePtr(decl));
		}

		if (Expect(UniqueType::Assign))
		{
			ParseAssignment(parameters->back());
		}

		else if (Expect(TokenType::Identifier))
		{
			Node assign = ParseAssignmentStatement();
			if (assign.nodeID != NodeID::InvalidNode) parameters->push_back(CreateNodePtr(assign));
		}

		while (Expect(UniqueType::Comma) && !IsEOF())
		{
			Node def = ParseAssignmentStatement();
			if (def.nodeID != NodeID::InvalidNode) parameters->push_back(CreateNodePtr(def));
		}

		return parameters;
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
			node.end = curr->index;
			if (ExpectSemicolon())
			{
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

	Type ParseDeclarationType(bool allowImplicitType = true)
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
				type = ParseInlineType(allowImplicitType);
				break;

			case UniqueType::Multiply:
			case UniqueType::Rawpointer:
				type = ParsePointerType();
				break;

			case UniqueType::Array:
				type = ParseArrayType();
				break;

			default:
				AddError(curr, errors.expectedType);
				break;
			}
			break;
		}

		if (type.typeID != TypeID::InvalidType && Expect(UniqueType::Less))
		{
			type = ParseGenericsType(type);
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

	Type ParseArrayType()
	{
		Type arrType = Type(TypeID::ArrayType);
		arrType.arrayType.arr = curr->index;
		Advance();
		arrType.arrayType.type = arena->Emplace<Type>(ParseDeclarationType());
		return arrType;
	}

	Type ParseInlineType(bool allowImplicitType)
	{
		Token* start = curr;
		Advance();
		if (allowImplicitType)
		{
			Token* next = Peek();
			bool implicit = Expect(TokenType::Identifier) && next->uniqueType == UniqueType::Comma;
			if (Expect(UniqueType::Rbrace))
			{
				AddError(curr, errors.emptyInlineType);
				return Type();
			}

			return implicit ? ParseImplicitType() : ParseExplicitType();
		}

		return ParseExplicitType();
	}

	Type ParseImplicitType()
	{
		Type type = Type(TypeID::ImplicitType);
		eastl::vector<TokenIndex>* idents = CreateVectorPtr<NodeIndex>();
		type.implicitType.identifiers = idents;
		while (!Expect(UniqueType::Rbrace) && !IsEOF())
		{
			if (Expect(TokenType::Identifier, errors.implictTypeNotIdent))
			{
				idents->push_back(curr->index);
				Advance();
				if (Expect(UniqueType::Comma)) Advance();
			}
			else
			{
				return Type();
			}
		}


		if (Expect(UniqueType::Rbrace, errors.inlineTypeNoClosure))
		{
			if (idents->size() == 1)
			{
				AddError(curr, errors.onlyOneInlineType);
				return Type();
			}
			Advance();
			return type;
		}

		return Type();
	}

	Type ParseExplicitType()
	{
		Type type = Type(TypeID::ExplicitType);
		eastl::vector<Node*>* decls = CreateVectorPtr<Node*>();
		type.explicitType.declarations = decls;
		while (!Expect(UniqueType::Rbrace) && !IsEOF())
		{
			Node* node = CreateNodePtr(ParseDeclaration());
			if (node->nodeID != NodeID::InvalidNode)
			{
				decls->push_back(node);
				if (Expect(UniqueType::Comma)) Advance();
			}
			else
			{
				return Type();
			}
		}

		if (Expect(UniqueType::Rbrace, errors.inlineTypeNoClosure))
		{
			if (decls->size() == 1)
			{
				AddError(curr, errors.onlyOneInlineType);
				return Type();
			}
			Advance();
			return type;
		}

		return Type();
	}

	Type ParseGenericsType(Type& type)
	{
		Type genericsType = Type(TypeID::GenericsType);
		genericsType.genericsType.generics = ParseGenericsExpr();
		genericsType.genericsType.type = arena->Emplace<Type>(type);

		return genericsType;
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
			Node* def = CreateNodePtr(ParseDefinition());
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

			case UniqueType::Less:
				if (TestGenericsExpr())
				{
					expr = ParseGenericsExpr(expr);
				}
				else
				{
					return expr;
				}
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
		case UniqueType::Name:
			return ParseIdentifierExpr();

		case UniqueType::IntLiteral:
		case UniqueType::FloatLiteral:
		case UniqueType::HexLiteral:
		case UniqueType::StringLiteral:
			return ParseLiteralExpr();

		case UniqueType::Lparen:
			return ParseGroupedExpr();

		case UniqueType::Fixed:
			return ParseFixedExpr();

		default:
			switch (curr->type)
			{
			case TokenType::Primitive:
				return ParsePrimitiveExpr();

			default:
				AddError(curr, errors.missingOperand);
				return CreateExpr(curr->index, ExprID::InvalidExpr);
			}
		}
	}

	Expr* ParseIdentifierExpr()
	{
		Expr* ident = CreateExpr(curr->index, ExprID::IdentifierExpr);
		ident->identfierExpr.identifier = curr->index;
		Advance();
		return ident;
	}

	Expr* ParsePrimitiveExpr()
	{
		Expr* prim = CreateExpr(curr->index, ExprID::PrimitiveExpr);
		prim->primitiveExpr.primitive = curr->index;
		Advance();
		return prim;
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

	Expr* ParseFixedExpr()
	{
		Expr* fixed = CreateExpr(curr->index, ExprID::FixedExpr);
		fixed->fixedExpr.fixed = curr->index;
		Advance();
		fixed->fixedExpr.atExpr = ParseOperand();
		return fixed;
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

	bool TestGenericsExpr()
	{
		Token* start = curr;

		while (!Expect(UniqueType::Greater) && !Expect(UniqueType::Semicolon) && !IsEOF())
		{
			Advance();
		}

		if (Expect(UniqueType::Greater) && Peek()->uniqueType == UniqueType::Lparen)
		{
			curr = start;
			return true;
		}

		curr = start;
		return false;
	}

	Expr* ParseGenericsExpr(Expr* expr = nullptr)
	{
		Expr* generics = CreateExpr(curr->index, ExprID::GenericsExpr);
		eastl::vector<Type>* genericTypes = CreateVectorPtr<Type>();
		generics->genericsExpr.expr = (expr != nullptr ? CopyExpr(expr) : expr);
		generics->genericsExpr.open = curr->index;
		generics->genericsExpr.types = genericTypes;

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
			if (type.typeID != TypeID::InvalidType)
			{
				genericTypes->push_back(type);
				if (Expect(UniqueType::Comma)) Advance();
			}
			else
			{
				generics->typeID = ExprID::InvalidExpr;
				return generics;
			}
		}

		if (Expect(UniqueType::Greater, errors.expectedGenericsClosure))
		{
			generics->genericsExpr.close = curr->index;
			Advance();
		}
		else
		{
			generics->typeID = ExprID::InvalidExpr;
		}

		return generics;
	}

	eastl::vector<Expr*>* ParseExprList()
	{
		eastl::vector<Expr*>* exprs = CreateVectorPtr<Expr*>();

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
