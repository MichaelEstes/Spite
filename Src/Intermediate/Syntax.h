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
				? " " + tokens.At(node.definition.op)->ToString() + " " + ToString(node.definition.assignment, tokens) + tokens.At(node.end)->ToString() : "");
	case InlineDefinition:
		return ToString(&node.inlineDefinition.type, tokens) +
			" " + tokens.At(node.inlineDefinition.op)->ToString() + " " +
			ToString(node.inlineDefinition.assignment, tokens);
	case Function:
		return ToString(&node.function.returnType, tokens) + " " +
			tokens.At(node.function.name)->ToString() +
			(node.function.generics != nullptr ? ToString(*node.function.generics, tokens) : "") +
			ToString(*node.function.decl, tokens);
	case Method:
		return ToString(&node.method.returnType, tokens) + " " +
			tokens.At(node.method.stateName)->ToString() + "::" +
			tokens.At(node.method.name)->ToString() +
			(node.method.generics != nullptr ? ToString(*node.method.generics, tokens) : "") +
			ToString(*node.method.decl, tokens);
	case StateOperator:
		return ToString(&node.stateOperator.returnType, tokens) + " " +
			tokens.At(node.stateOperator.stateName)->ToString() + "::operator" +
			(node.stateOperator.generics != nullptr ? ToString(*node.stateOperator.generics, tokens) : "") + "::" +
			tokens.At(node.stateOperator.op)->ToString() +
			ToString(*node.stateOperator.decl, tokens);
	case Destructor:
		return tokens.At(node.destructor.stateName)->ToString() + "::" +
			tokens.At(node.destructor.del)->ToString() +
			ToString(node.destructor.body, tokens);
	case Constructor:
		return tokens.At(node.destructor.stateName)->ToString() + "::" +
			ToString(*node.constructor.decl, tokens);
	case FunctionDecl:
	{
		eastl::string params = "(";
		if (node.functionDecl.parameters != nullptr)
		{
			for (Node* param : *node.functionDecl.parameters)
			{
				params += ToString(*param, tokens) + ", ";
			}
		}
		params += ")";
		return params + ToString(node.functionDecl.body, tokens);
	}
	case Conditional:
		return "(" + ToString(node.conditional.condition, tokens) + ")" +
			ToString(node.conditional.body, tokens);
	case AssignmentStmnt:
		return ToString(node.assignmentStmnt.assignTo, tokens) +
			" " + tokens.At(node.assignmentStmnt.op)->ToString() + " " +
			ToString(node.assignmentStmnt.assignment, tokens) +
			tokens.At(node.end)->ToString();
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
			"default" + ToString(node.switchStmnt.defaultCase, tokens) + "\n";
	}
	return "";
	case TernaryStmnt:
		return "";
	case DeleteStmnt:
		return tokens.At(node.start)->ToString() +
			(node.deleteStmnt.arrDelete ? "[]" : "") + " " +
			ToString(node.deleteStmnt.primaryExpr, tokens);
	case DeferStmnt:
		return tokens.At(node.start)->ToString() + " " +
			(node.deferStmnt.deferIf
				? ToString(*node.deferStmnt.conditional, tokens)
				: ToString(node.deferStmnt.body, tokens));
	case ContinueStmnt:
		return tokens.At(node.continueStmnt.token)->ToString();
	case BreakStmnt:
		return tokens.At(node.breakStmnt.token)->ToString();
	case ReturnStmnt:
		return tokens.At(node.start)->ToString() + " " +
			(!node.returnStmnt.voidReturn ? ToString(node.returnStmnt.expr, tokens) : "");
	case OnCompileStmnt:
		return "";
	case WhereStmnt:
	{
		return tokens.At(node.start)->ToString() + ToString(*node.whereStmnt.decl, tokens);
	}
	case StateStmnt:
	{
		eastl::string members = "";
		for (Node* member : *node.state.members)
		{
			members += ToString(*member, tokens) + ",\n";
		}

		return tokens.At(node.start)->ToString() + " " +
			tokens.At(node.state.name)->ToString() +
			(node.state.generics ? ToString(*node.state.generics, tokens) : "") +
			"\n{\n" + members + "}\n";
	}
	case InsetStmnt:
	{
		eastl::string inset = "";
		switch (node.insetStmnt.type)
		{
		case SizeInset:
			inset = "size";
			break;
		case NullInset:
			inset = "null";
			break;
		case SerializedInset:
			inset = "serialized";
			break;
		case NoAlignInset:
			inset = "noalign";
			break;
		default:
			break;
		}

		return tokens.At(node.start)->ToString() + inset + tokens.At(node.end)->ToString();
	}
	case GenericsDecl:
	{
		eastl::string names = "";
		for (NodeIndex name : *node.generics.names)
		{
			names += tokens.At(name)->ToString() + ", ";
		}

		return tokens.At(node.start)->ToString() + names +
			(node.generics.whereStmnt ? " : " + ToString(*node.generics.whereStmnt, tokens) : "") +
			tokens.At(node.end)->ToString();
	}
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
	if (body.statement)
	{
		return " " + ToString(*body.body, tokens) + "\n";
	}
	else
	{
		return ToString(*body.body, tokens);
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
				params += ToString(expr, tokens) + ", ";
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
	{
		eastl::string values = "";
		for (Expr* expr : *expr->anonTypeExpr.values)
		{
			values += ToString(expr, tokens) + ", ";
		}

		return "{" + values + "}";
	}
	case AsExpr:
		return ToString(expr->asExpr.of, tokens) + " as " +
			ToString(expr->asExpr.to, tokens);
	case DereferenceExpr:
		return ToString(expr->dereferenceExpr.of, tokens) +
			tokens.At(expr->dereferenceExpr.op)->ToString();
	case ReferenceExpr:
		return ToString(expr->referenceExpr.of, tokens) +
			tokens.At(expr->referenceExpr.op)->ToString();
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
	case FunctionTypeExpr:
		return (expr->functionTypeExpr.of ? ToString(expr->functionTypeExpr.of, tokens) : "") +
			ToString(expr->functionTypeExpr.functionType, tokens);
	case FunctionTypeDeclExpr:
		return (expr->functionTypeDeclExpr.of ? ToString(expr->functionTypeDeclExpr.of, tokens) : "") +
			ToString(expr->functionTypeDeclExpr.returnType, tokens) +
			ToString(*expr->functionTypeDeclExpr.functionDecl, tokens);
	return "";
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

eastl::string ToString(Type* type, Tokens& tokens)
{
	switch (type->typeID)
	{
	case UnknownType:
		return "implicit";
	case PrimitiveType:
		return PrimitiveToString(type->primitiveType.type);
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
	{
		eastl::string params = "";

		for (Type* type : *type->functionType.paramTypes)
		{
			params += ToString(type, tokens) + ", ";
		}

		return "::" + ToString(type->functionType.returnType, tokens) + "(" + params + ")";
	}
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

	void AddNode(Node& node)
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

	inline Type* CreateTypePtr(TypeID typeID)
	{
		return arena->Emplace<Type>(typeID);
	}

	inline Type* CreateTypePtr(Type type)
	{
		return arena->Emplace<Type>(type);
	}

	inline Expr* CreateExpr(TokenIndex start, ExprID exprID)
	{
		return arena->Emplace<Expr>(exprID, start);
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

	/*const size_t breakCount = 100000;
	size_t lastTokenIndex = -1;
	size_t sameCount = 0;*/
	inline bool IsEOF()
	{
		/*if (curr->index != lastTokenIndex)
		{
			lastTokenIndex = curr->index;
			sameCount = 0;
		}
		else
		{
			sameCount += 1;
			if (sameCount > breakCount) Logger::FatalErrorAt(errors.fatalError, curr->pos);
		}*/
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
		Logger::AddError(token->pos, token->index, msg);
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
			return;
		case UniqueType::Package:
			AddError(curr, errors.multiplePackages);
			ParsePackage(false);
			return;
		case UniqueType::Using:
			ParseUsing();
			return;
		case UniqueType::State:
			ParseState();
			return;
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
			Type type;
			if (curr->uniqueType == UniqueType::Name)
			{
				UniqueType next = Peek()->uniqueType;
				if (next == UniqueType::DoubleColon ||
					next == UniqueType::Lparen ||
					next == UniqueType::Less) type = CreateVoidType();
				else type = ParseDeclarationType(false);
			}
			else type = ParseDeclarationType(false);

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

	void ParseState()
	{
		Node node = CreateNode(curr, NodeID::StateStmnt);
		Advance();
		if (Expect(TokenType::Identifier, errors.expectedStateName))
		{
			node.state.name = curr->index;
			node.state.members = CreateVectorPtr<Node*>();
			Advance();
			node.state.generics = ParseGenerics();

			if (Expect(UniqueType::Lbrace, errors.expectedStateOpen))
			{
				Advance();
				if (Expect(UniqueType::Rbrace))
				{
					AddError(curr, errors.emptyState);
					Advance();
					return;
				}

				while (!Expect(UniqueType::Rbrace) && !IsEOF())
				{
					Node* member = ParseStateMember();
					if (member->nodeID != NodeID::InvalidNode) node.state.members->push_back(member);
					if (Expect(UniqueType::Comma)) Advance();
				}

				if (Expect(UniqueType::Rbrace, errors.expectedStateClose))
				{
					node.end = curr->index;
					Advance();
					AddNode(node);
				}
			}
		}
	}

	Node* ParseStateMember()
	{
		if (Expect(UniqueType::Lbrack)) return ParseStateInset();

		return ParseDeclOrDef();
	}

	Node* ParseDeclOrDef()
	{
		Node* node = CreateNodePtr(ParseDeclaration());
		if (Expect(UniqueType::Assign)) ParseAssignment(node);

		return node;
	}

	Node* ParseStateInset()
	{
		Node* node = CreateNodePtr(curr, NodeID::InsetStmnt);
		Advance();

		if (Expect(TokenType::Identifier, errors.expectedInsetName))
		{
			if (curr->val == "size") node->insetStmnt.type = InsetID::SizeInset;
			else if (curr->val == "null") node->insetStmnt.type = InsetID::NullInset;
			else if (curr->val == "serialized") node->insetStmnt.type = InsetID::SerializedInset;
			else if (curr->val == "noalign") node->insetStmnt.type = InsetID::NoAlignInset;
			else
			{
				AddError(curr, errors.expectedInsetName);
				Advance();
				if (Expect(UniqueType::Rbrack)) Advance();
				node->nodeID = NodeID::InvalidNode;
				return node;
			}

			Advance();
			if (Expect(UniqueType::Rbrack, errors.expectedInsetClose))
			{
				node->end = curr->index;
				Advance();
				return node;
			}
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseFunctionDecl()
	{
		Node* node = CreateNodePtr(curr, NodeID::FunctionDecl);
		Advance();
		eastl::vector<Node*>* parameters = ParseParametersList();
		node->functionDecl.parameters = parameters;
		if (Expect(UniqueType::Rparen, errors.expectedFunctionParamsClose)) Advance();

		if (Expect(UniqueType::Lbrace))
		{
			node->functionDecl.body.statement = false;
			node->functionDecl.body.body = ParseBlock();
			node->end = node->functionDecl.body.body->end;
			return node;
		}
		else if (Expect(UniqueType::FatArrow))
		{
			Advance();
			node->functionDecl.body.statement = true;
			node->functionDecl.body.body = ParseBlockStatment();
			node->end = node->functionDecl.body.body->end;
			return node;
		}
		else
		{
			AddError(curr, errors.expectedBlockStart);
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node ParseFunction(Type& returnType, Token* start)
	{
		if (Expect(TokenType::Identifier, errors.expectedFunctionName))
		{
			Node function = CreateNode(start, NodeID::Function);
			Token* name = curr;
			Advance();

			if (Expect(UniqueType::DoubleColon))
			{
				return ParseStateFunction(returnType, start, name);
			}
			else
			{
				function.function.generics = ParseGenerics();
				if (Expect(UniqueType::Lparen, errors.expectedFunctionParamsOpen))
				{
					function.function.returnType = returnType;
					function.function.name = name->index;
					function.function.decl = ParseFunctionDecl();
					function.end = function.function.decl->end;
					return function;
				}
			}
		}

		return Node();
	}

	Node ParseStateFunction(Type& returnType, Token* start, Token* name)
	{
		Advance();
		switch (curr->uniqueType)
		{
		case UniqueType::Name:
		{
			Node method = CreateNode(start, NodeID::Method);
			method.method.returnType = returnType;
			method.method.stateName = name->index;
			method.method.name = curr->index;
			Advance();
			method.method.generics = ParseGenerics();
			if (Expect(UniqueType::Lparen, errors.expectedFunctionParamsOpen))
			{
				method.method.decl = ParseFunctionDecl();
				method.end = method.method.decl->end;
				return method;
			}
			break;
		}
		case UniqueType::OperatorOverload:
		{
			Node op = CreateNode(start, NodeID::StateOperator);
			op.stateOperator.returnType = returnType;
			op.stateOperator.stateName = name->index;
			Advance();
			op.stateOperator.generics = ParseGenerics();
			if (Expect(UniqueType::DoubleColon, errors.operatorDoubleColon))
			{
				Advance();
				if (curr->type == TokenType::Operator)
				{
					op.stateOperator.op = curr->index;
					Advance();
					if (Expect(UniqueType::Lparen, errors.expectedFunctionParamsOpen))
					{
						op.stateOperator.decl = ParseFunctionDecl();
						op.end = op.stateOperator.decl->end;
						return op;
					}
				}
				else AddError(curr, errors.invalidOperator);
			}
			break;
		}
		case UniqueType::Delete:
		{
			Node del = CreateNode(start, NodeID::Destructor);
			del.destructor.stateName = name->index;
			del.destructor.del = curr->index;
			Advance();
			if (Expect(UniqueType::FatArrow)) Advance();
			del.destructor.body = ParseBody();
			return del;
		}
		case UniqueType::Lparen:
		{
			Node con = CreateNode(start, NodeID::Constructor);
			con.constructor.stateName = name->index;
			con.constructor.decl = ParseFunctionDecl();
			return con;
		}
		default:
			break;
		}

		return Node();
	}

	Node* ParseGenerics()
	{
		if (Expect(UniqueType::Less))
		{
			return ParseGenericDecl();
		}
		else
		{
			return nullptr;
		}
	}

	Node* ParseGenericDecl()
	{
		Node* node = CreateNodePtr(curr, NodeID::GenericsDecl);
		node->generics.names = CreateVectorPtr<TokenIndex>();
		node->generics.whereStmnt = nullptr;
		Advance();

		if (Expect(UniqueType::Greater))
		{
			AddError(curr, errors.emptyGenerics);
			node->nodeID = NodeID::InvalidNode;
			return node;
		}

		while (!Expect(UniqueType::Greater) && !Expect(UniqueType::Colon) && !IsEOF())
		{
			if (Expect(TokenType::Identifier, errors.notGenericIdent))
			{
				node->generics.names->push_back(curr->index);
				Advance();
				if (Expect(UniqueType::Comma)) Advance();
			}
			else
			{
				node->nodeID = NodeID::InvalidNode;
				return node;
			}
		}

		if (Expect(UniqueType::Colon))
		{
			Advance();
			node->generics.whereStmnt = ParseWhere();
		}

		if (Expect(UniqueType::Greater, errors.expectedGenericsClosure))
		{
			node->end = curr->index;
			Advance();
			return node;
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseWhere()
	{
		Node* node = CreateNodePtr(curr, NodeID::WhereStmnt);
		Advance();

		node->whereStmnt.decl = ParseFunctionDecl();

		if (node->whereStmnt.decl->nodeID != NodeID::InvalidNode) return node;

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Body ParseBody()
	{
		Body body = Body();

		if (Expect(UniqueType::Lbrace))
		{
			body.statement = false;
			body.body = ParseBlock();
		}
		else
		{
			body.statement = true;
			body.body = ParseBodyStmnt();
		}

		return body;
	}

	Node* ParseBodyStmnt()
	{
		StartScope();
		Node* stmnt = ParseBlockStatment();
		EndScope();

		return stmnt;
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
		case UniqueType::Lbrace:
			return ParseBlockOrInlineType();
		case UniqueType::If:
			return ParseIf();
		case UniqueType::For:
			return ParseFor();
		case UniqueType::Switch:
			return ParseSwitch();
		case UniqueType::While:
			return ParseWhile();
		case UniqueType::Defer:
			return ParseDefer();
		case UniqueType::Delete:
			return ParseDelete();
		case UniqueType::Continue:
			return ParseContinue();
		case UniqueType::Break:
			return ParseBreak();
		case UniqueType::Return:
			return ParseReturn();

		default:
			Advance();
			break;
		}

		return CreateNodePtr(curr, NodeID::InvalidNode);
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

	Node* ParseBlockOrInlineType()
	{
		Token* start = curr;

		Logger::SetErrorRollback();
		Type type = ParseInlineType(true);
		if (type.typeID != TypeID::InvalidType &&
			(Expect(UniqueType::Assign) || Expect(UniqueType::ImplicitAssign)))
		{
			if ((type.typeID == TypeID::ImplicitType && Expect(UniqueType::Assign)) ||
				(type.typeID == TypeID::ExplicitType && Expect(UniqueType::ImplicitAssign)))
			{
				AddError(curr, errors.inlineTypeWrongAssignmentOp);
				return CreateNodePtr(curr, NodeID::InvalidNode);
			}

			Node* node = CreateNodePtr(start, NodeID::InlineDefinition);
			node->inlineDefinition.type = type;
			node->inlineDefinition.op = curr->index;
			Advance();
			node->inlineDefinition.assignment = ParseExpr();
			if (ExpectSemicolon())
			{
				node->end = curr->index;
				Advance();
				return node;
			}

			node->nodeID = NodeID::InvalidNode;
			return node;
		}

		curr = start;
		Logger::ErrorRollback();
		return ParseBlock();
	}

	Node* ParseExprStmnt()
	{
		Node* node = CreateNodePtr(CreateNode(curr, NodeID::ExpressionStmnt));
		Expr* expr = ParseExpr();
		if (expr->typeID != ExprID::InvalidExpr)
		{
			if (Expect(UniqueType::Semicolon))
			{
				node->expressionStmnt.expression = expr;
				node->end = curr->index;
				Advance();
				return node;
			}
			else if (IsAssignableExpr(expr) && IsAssignmentOperator())
			{
				node->nodeID = NodeID::AssignmentStmnt;
				node->assignmentStmnt.assignTo = expr;
				node->assignmentStmnt.op = curr->index;
				Advance();
				node->assignmentStmnt.assignment = ParseExpr();
				if (ExpectSemicolon())
				{
					node->end = curr->index;
					Advance();
					return node;
				}
			}
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseIf()
	{
		Node* node = CreateNodePtr(CreateNode(curr, NodeID::IfStmnt));
		Advance();

		Node* conditional = ParseConditional();
		if (conditional->nodeID != NodeID::InvalidNode)
		{
			node->ifStmnt.condition = conditional;
			node->ifStmnt.elifs = CreateVectorPtr<Node*>();
			node->ifStmnt.elseCondition = Body();

			while (Expect(UniqueType::Else) && Peek()->uniqueType == UniqueType::If)
			{
				Advance();
				Advance();
				Node* elif = ParseConditional();
				if (elif->nodeID != NodeID::InvalidNode) node->ifStmnt.elifs->push_back(elif);
				else
				{
					node->nodeID = NodeID::InvalidNode;
					return node;
				}
			}

			if (Expect(UniqueType::Else))
			{
				Advance();
				node->ifStmnt.elseCondition = ParseBody();
			}

			node->end = curr->index;
			return node;
		}


		node->nodeID = NodeID::InvalidNode;
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
				node->end = node->conditional.body.body->end;
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
						node->end = node->forStmnt.body.body->end;
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

						if (Expect(UniqueType::Default))
						{
							Advance();
							node->switchStmnt.defaultCase = ParseBody();
						}
						else node->switchStmnt.defaultCase = Body();

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
			node->end = conditional->end;
			return node;
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseDefer()
	{
		Node* node = CreateNodePtr(curr, NodeID::DeferStmnt);
		Advance();

		if (Expect(UniqueType::If))
		{
			node->deferStmnt.deferIf = true;
			Advance();
			Node* conditional = ParseConditional();
			if (conditional->nodeID != NodeID::InvalidNode)
			{
				node->deferStmnt.conditional = conditional;
				node->end = conditional->end;
				return node;
			}
		}
		else
		{
			node->deferStmnt.deferIf = false;
			node->deferStmnt.body = ParseBody();
			node->end = node->deferStmnt.body.body->end;
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
				Advance();
				return node;
			}
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseContinue()
	{
		Node* node = CreateNodePtr(curr, NodeID::ContinueStmnt);
		node->continueStmnt.token = curr->index;
		Advance();
		if (ExpectSemicolon())
		{
			node->end = curr->index;
			Advance();
			return node;
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseBreak()
	{
		Node* node = CreateNodePtr(curr, NodeID::BreakStmnt);
		node->breakStmnt.token = curr->index;
		Advance();
		if (ExpectSemicolon())
		{
			node->end = curr->index;
			Advance();
			return node;
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
			node->end = curr->index;
			Advance();
			return node;
		}
		else
		{
			node->returnStmnt.voidReturn = false;
			node->returnStmnt.expr = ParseExpr();
			if (ExpectSemicolon())
			{
				node->end = curr->index;
				Advance();
				return node;
			}
		}


		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	eastl::vector<Node*>* ParseParametersList(UniqueType end = UniqueType::Rparen)
	{
		eastl::vector<Node*>* parameters = CreateVectorPtr<Node*>();

		if (Expect(UniqueType::Rparen)) return parameters;

		while (!Expect(end) && !IsEOF())
		{
			Node* decl = ParseDeclOrDef();
			if (decl->nodeID != NodeID::InvalidNode) parameters->push_back(decl);
			else Advance();

			if (Expect(UniqueType::Comma)) Advance();
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
			Node node = CreateNode(start, NodeID::Definition);
			node.definition.name = start->index;
			node.definition.op = curr->index;

			Advance();
			Type type = Type(TypeID::UnknownType);
			Expr* expr = ParseAssignmentType();
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
			decl->definition.op = curr->index;
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

			case UniqueType::DoubleColon:
				type = ParseFunctionType();
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
		ptrType.pointerType.type = CreateTypePtr(ParseDeclarationType());
		return ptrType;
	}

	Type ParseArrayType()
	{
		Type arrType = Type(TypeID::ArrayType);
		arrType.arrayType.arr = curr->index;
		Advance();
		arrType.arrayType.type = CreateTypePtr(ParseDeclarationType());
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
		genericsType.genericsType.type = CreateTypePtr(type);

		return genericsType;
	}

	Type ParseFunctionType()
	{
		Type functionType = Type(TypeID::FunctionType);
		functionType.functionType.paramTypes = CreateVectorPtr<Type*>();
		Advance();

		functionType.functionType.returnType = Expect(UniqueType::Lparen)
			? CreateTypePtr(CreateVoidType()) : CreateTypePtr(ParseDeclarationType());

		if (Expect(UniqueType::Lparen, errors.functionTypeOpening))
		{
			Advance();
			while (!Expect(UniqueType::Rparen) && !IsEOF())
			{
				Type type = ParseDeclarationType();
				functionType.functionType.paramTypes->push_back(CreateTypePtr(type));
				if (Expect(UniqueType::Comma)) Advance();
			}

			if (Expect(UniqueType::Rparen, errors.functionTypeClose))
			{
				Advance();
				return functionType;
			}
		}

		return Type();
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
		while (!Expect(UniqueType::Lbrace) && !IsEOF())
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

	inline Expr* CopyExpr(Expr* expr)
	{
		Expr* copy = CreateExpr(expr->start, expr->typeID);
		*copy = *expr;
		return copy;
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
			{
				ExprID exprID = expr->typeID;
				if ((exprID == ExprID::IdentifierExpr || exprID == ExprID::SelectorExpr) && TestGenericsExpr())
				{
					expr = ParseGenericsExpr(expr);
					break;
				}
				else return expr;
			}
			case UniqueType::As:
				expr = ParseAs(expr);
				break;

			case UniqueType::Tilde:
				expr = ParseDereference(expr);
				break;
			case UniqueType::AtOp:
				expr = ParseReference(expr);
				break;

			case UniqueType::DoubleColon:
				expr = ParseFunctionTypeExpr(expr);
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

		case UniqueType::Lbrace:
			return ParseAnonTypeExpr();

		case UniqueType::DoubleColon:
			return ParseFunctionTypeExpr();

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

	Expr* ParseAnonTypeExpr()
	{
		Expr* anon = CreateExpr(curr->index, ExprID::AnonTypeExpr);
		anon->anonTypeExpr.values = CreateVectorPtr<Expr*>();
		Advance();

		if (Expect(UniqueType::Rbrace))
		{
			AddError(curr, errors.emptyInlineType);
			Advance();
			return anon;
		}

		anon->anonTypeExpr.values->push_back(ParseExpr());

		while (Expect(UniqueType::Comma))
		{
			Advance();
			anon->anonTypeExpr.values->push_back(ParseExpr());
		}

		if (Expect(UniqueType::Rbrace, errors.inlineTypeNoClosure)) Advance();

		return anon;
	}

	Expr* ParseFunctionTypeExpr(Expr* on = nullptr)
	{
		Token* start = curr;
		Expr* expr = CreateExpr(start->index, ExprID::FunctionTypeDeclExpr);
		Advance();
		Type* returnType = Expect(UniqueType::Lparen)
			? CreateTypePtr(CreateVoidType()) : CreateTypePtr(ParseDeclarationType());

		if (Expect(UniqueType::Lparen, errors.functionTypeOpening))
		{
			Token* lparen = curr;
			Advance();
			if (Expect(TokenType::Identifier), Peek()->uniqueType == UniqueType::Colon)
			{
				curr = lparen;
				expr->functionTypeDeclExpr.of = on;
				expr->functionTypeDeclExpr.returnType = returnType;
				expr->functionTypeDeclExpr.functionDecl = ParseFunctionDecl();
				return expr;
			}
			else
			{
				curr = start;
				expr->typeID = ExprID::FunctionTypeExpr;
				expr->functionTypeExpr.of = on;
				expr->functionTypeExpr.functionType = CreateTypePtr(ParseFunctionType());
				return expr;
			}
		}

		return CreateExpr(start->index, ExprID::InvalidExpr);
	}

	Expr* ParseSelector(Expr* on)
	{
		Expr* selector = CreateExpr(curr->index, ExprID::SelectorExpr);
		selector->selectorExpr.on = on;
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
		indexExpr->indexExpr.of = of;
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

		Advance();
		while (!Expect(UniqueType::Greater) && !Expect(UniqueType::Semicolon) && !IsEOF())
		{
			if (Expect(TokenType::Operator) || Expect(TokenType::Literal))
			{
				curr = start;
				return false;
			}
			Advance();
		}

		bool generics = Expect(UniqueType::Greater);
		curr = start;
		return generics;
	}

	Expr* ParseGenericsExpr(Expr* expr = nullptr)
	{
		Expr* generics = CreateExpr(curr->index, ExprID::GenericsExpr);
		eastl::vector<Type>* genericTypes = CreateVectorPtr<Type>();
		generics->genericsExpr.expr = expr;
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

	Expr* ParseAs(Expr* of)
	{
		Expr* asExpr = CreateExpr(of->start, ExprID::AsExpr);
		asExpr->asExpr.of = of;
		Advance();
		asExpr->asExpr.to = CreateTypePtr(ParseDeclarationType());

		return asExpr;
	}

	Expr* ParseDereference(Expr* of)
	{
		Expr* expr = CreateExpr(of->start, ExprID::DereferenceExpr);
		expr->dereferenceExpr.of = of;
		expr->dereferenceExpr.op = curr->index;
		Advance();

		return expr;
	}

	Expr* ParseReference(Expr* of)
	{
		Expr* expr = CreateExpr(of->start, ExprID::ReferenceExpr);
		expr->dereferenceExpr.of = of;
		expr->dereferenceExpr.op = curr->index;
		Advance();

		return expr;
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

	bool IsAssignableExpr(Expr* expr)
	{
		ExprID type = expr->typeID;
		return type == ExprID::IdentifierExpr ||
			type == ExprID::SelectorExpr ||
			type == ExprID::IndexExpr;
	}

	bool IsAssignmentOperator()
	{
		UniqueType uniqueType = curr->uniqueType;
		return curr->type == TokenType::Operator &&
			(uniqueType == UniqueType::Assign ||
				uniqueType == UniqueType::AddAssign ||
				uniqueType == UniqueType::SubtractAssign ||
				uniqueType == UniqueType::MultiplyAssign ||
				uniqueType == UniqueType::DivideAssign ||
				uniqueType == UniqueType::ModuloAssign ||
				uniqueType == UniqueType::AndAssign ||
				uniqueType == UniqueType::OrAssign ||
				uniqueType == UniqueType::XorAssign ||
				uniqueType == UniqueType::ShiftlAssign ||
				uniqueType == UniqueType::ShiftrAssign ||
				uniqueType == UniqueType::AndNotAssign);
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
			return 0;
		}
	}

	Type CreateVoidType()
	{
		Type type = Type(TypeID::PrimitiveType);
		type.primitiveType.type = UniqueType::Void;
		type.primitiveType.size = 0;
		type.primitiveType.isSigned = false;

		return type;
	}

	Type CreatePrimitive()
	{
		Token* start = curr;
		Type type = Type(TypeID::PrimitiveType);
		type.primitiveType.type = curr->uniqueType;
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
