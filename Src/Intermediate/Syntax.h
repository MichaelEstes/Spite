#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"

#include "../Tokens/Tokens.h"
#include "../Log/Logger.h"
#include "../Utils/Utils.h"
#include "../Containers/Arena.h"

#include "Node.h"
#include "Type.h"
#include "Expr.h"
#include "SymbolTable.h"

extern int targetArchBitWidth;

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
		return ToString(node->compileStmnt.compileExpr);
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
		eastl::string types = "";

		for (Type* type : *expr->genericsExpr.types)
		{
			types += ToString(type) + ", ";
		}

		return (expr->genericsExpr.expr != nullptr ? ToString(expr->genericsExpr.expr) : "") +
			expr->genericsExpr.open->ToString() +
			types +
			expr->genericsExpr.close->ToString();
	}
	case FunctionTypeExpr:
		return ToString(expr->functionTypeExpr.functionType);
	case FunctionTypeDeclExpr:
		return ToString(expr->functionTypeDeclExpr.returnType) +
			ToString(expr->functionTypeDeclExpr.functionDecl);
	case CompileExpr:
		return expr->start->ToString() + " " +
			ToString(expr->compileExpr.returnType) + " " +
			ToString(*expr->compileExpr.body);
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

struct Scope
{
	NodeIndex scopeOf;
	ScopeIndex parent;
	ScopeIndex index;

	Scope()
	{
		scopeOf = 0;
		parent = 0;
		index = 0;
	}

	Scope(const Scope& copy)
	{
		scopeOf = copy.scopeOf;
		parent = copy.parent;
		index = copy.index;
	}
};

struct Syntax
{
	const InplaceString thisStr = "this";

	Tokens& tokens;
	Scope currScope;
	Token* curr;
	SymbolTable* symbolTable;

	size_t nodeCount;
	eastl::vector<Node*> nodes;
	eastl::vector<Scope> scopes;
	eastl::vector<Node*> imports;
	Node* package;
	Arena* arena;

	Syntax(Tokens& tokensRef) : tokens(tokensRef)
	{
		curr = nullptr;
		nodeCount = 0;
		nodes = eastl::vector<Node*>();
		scopes = eastl::vector<Scope>();
		imports = eastl::vector<Node*>();
		currScope = Scope();
		scopes.push_back(currScope);
	}

	~Syntax()
	{
		delete arena;
	}

	void Print()
	{
		eastl::string toPrint = "";
		if (package->nodeID == NodeID::PackageStmnt)
		{
			toPrint += ToString(package);
			toPrint += '\n';
		}

		for (Node* node : imports)
		{
			toPrint += ToString(node);
			toPrint += '\n';
		}

		for (Node* node : nodes)
		{
			toPrint += ToString(node);
			toPrint += '\n';
		}

		Logger::Info(toPrint);
	}

	inline void AddNode(Node* node)
	{
		nodes.emplace_back(node);
	}

	inline Node* CreateNode(Token* start, NodeID nodeID)
	{
		nodeCount += 1;
		return arena->Emplace<Node>(nodeID, start, currScope.index);
	}

	inline Node* InvalidNode()
	{
		return arena->Emplace<Node>();
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

	inline Expr* CreateExpr(Token* start, ExprID exprID)
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

	inline bool IsEOF()
	{
		return curr->type & TokenType::EndOfFile;
	}

	void BuildSyntax()
	{
		arena = new Arena((tokens.tokens.size() / 2) * sizeof(Node));
		symbolTable = arena->Emplace<SymbolTable>();
		curr = tokens.First();

		ParseComments();
		ParsePackage();
		symbolTable->package = package;

		while (!IsEOF())
		{
			ParseNext();
		}

		Logger::Info("Created " + eastl::to_string(nodeCount) + " Nodes");
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
			Node* node = CreateNode(curr, NodeID::CommentStmnt);
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
		if (Expect(UniqueType::Package, "File must start with a 'package' statement") &&
			ThenExpect(TokenType::Identifier, "Expected an identifier after package token"))
		{
			Node* node = CreateNode(start, NodeID::PackageStmnt);
			node->package.name = curr;
			Advance();
			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			if (setInTree) package = node;
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
			AddError(curr, "File cannot have multiple 'package' statements");
			ParsePackage(false);
			return;
		case UniqueType::Using:
			ParseUsing();
			return;
		case UniqueType::State:
			ParseState();
			return;
		case UniqueType::OnCompile:
			ParseCompile();
			return;
		case UniqueType::OnCompileDebug:
			ParseCompileDebug();
			return;
		case UniqueType::Name:
		{
			Node* assignment = ParseAssignmentStatement();
			if (assignment->nodeID != NodeID::InvalidNode)
			{
				AddNode(assignment);
				symbolTable->AddGlobalVal(assignment);
				return;
			}
		}
		default:
		{
			Token* start = curr;
			Type* type;
			if (curr->uniqueType == UniqueType::Name)
			{
				UniqueType next = Peek()->uniqueType;
				if (next == UniqueType::DoubleColon ||
					next == UniqueType::Lparen ||
					next == UniqueType::Less) type = CreateVoidType();
				else type = ParseDeclarationType(false);
			}
			else type = ParseDeclarationType(false);

			if (type->typeID == TypeID::InvalidType) break;
			Node* func = ParseFunction(type, start);
			if (func->nodeID != NodeID::InvalidNode) AddNode(func);
			return;
		}
		}

		if (advance) Advance();
	}

	void ParseUsing()
	{
		bool addNode = IsGlobalScope();
		if (!addNode) AddError(curr, "Cannot import packages outside of the global scope");

		Token* start = curr;
		if (Expect(UniqueType::Using) &&
			ThenExpect(TokenType::Identifier, "Expected an identifier after 'using' token"))
		{
			Node* node = CreateNode(start, NodeID::UsingStmnt);
			node->using_.packageName = curr;
			Advance();

			if (Expect(UniqueType::As)
				&& ThenExpect(TokenType::Identifier, "Expected an alias identifier after 'as' in using statement"))
			{
				node->using_.alias = curr;
				Advance();
			}
			else
			{
				node->using_.alias = nullptr;
			}

			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			if (addNode) imports.push_back(node);
		}
	}

	void ParseState()
	{
		Node* node = CreateNode(curr, NodeID::StateStmnt);
		Advance();
		if (Expect(TokenType::Identifier, "Expected an identifier after 'state'"))
		{
			node->state.name = curr;
			node->state.members = CreateVectorPtr<Node*>();
			node->state.insetFlags = arena->Emplace<Flags<>>();
			Advance();
			node->state.generics = ParseGenerics();

			if (Expect(UniqueType::Lbrace, "Expected 'state' statement opening ('{')"))
			{
				Advance();
				if (Expect(UniqueType::Rbrace))
				{
					AddError(curr, "Empty 'state' is not allowed ('{}')");
					Advance();
					return;
				}

				while (!Expect(UniqueType::Rbrace) && !IsEOF())
				{
					ParseStateMember(node);
					if (Expect(UniqueType::Comma)) Advance();
				}

				if (Expect(UniqueType::Rbrace, "Expected 'state' statement closure ('}')"))
				{
					node->end = curr;
					Advance();
					AddNode(node);
					symbolTable->AddState(node);
				}
			}
		}
	}

	void ParseStateMember(Node* state)
	{
		if (Expect(UniqueType::Lbrack))
		{
			InsetID inset = ParseStateInset();
			if (inset != InsetID::InvalidInset) state->state.insetFlags->Set(inset);
			return;
		}

		Node* member = ParseDeclOrDef();
		if (member->nodeID != NodeID::InvalidNode) state->state.members->push_back(member);
	}

	Node* ParseDeclOrDef()
	{
		Node* node = ParseDeclaration();
		if (Expect(UniqueType::Assign)) ParseAssignment(node);

		return node;
	}

	InsetID ParseStateInset()
	{
		Advance();

		if (Expect(TokenType::Identifier, "Expected identifier as state inset"))
		{
			InsetID inset = InsetID::InvalidInset;
			if (curr->val == "size") inset = InsetID::SizeInset;
			else if (curr->val == "soa") inset = InsetID::SOAInset;
			else if (curr->val == "serialized") inset = InsetID::SerializedInset;
			else if (curr->val == "noalign") inset = InsetID::NoAlignInset;
			else
			{
				AddError(curr, "Expected valid state inset");
				Advance();
				if (Expect(UniqueType::Rbrack)) Advance();
				return inset;
			}

			Advance();
			if (Expect(UniqueType::Rbrack, "Expected state inset closure (']')"))
			{
				Advance();
				return inset;
			}
		}

		return InsetID::InvalidInset;
	}

	void ParseCompile()
	{
		Node* node = CreateNode(curr, NodeID::CompileStmnt);
		Expr* expr = ParseCompileExpr();
		if (expr->typeID != ExprID::InvalidExpr)
		{
			node->compileStmnt.compileExpr = expr;

			if (Expect(UniqueType::Semicolon))
			{
				Advance();
				node->end = curr;
			}
			else
			{
				//Scott Baio
				node->end = node->compileStmnt.compileExpr->compileExpr.body->body->end;
			}

			symbolTable->AddOnCompile(node);
			AddNode(node);
		}
	}

	void ParseCompileDebug()
	{
		Node* node = CreateNode(curr, NodeID::CompileDebugStmnt);
		Advance();
		node->compileDebugStmnt.body = ParseBody();
		if (node->compileDebugStmnt.body)
		{
			node->end = node->compileDebugStmnt.body.body->end;

			symbolTable->AddOnCompile(node);
			AddNode(node);
		}
	}

	Node* ParseFunctionDecl()
	{
		Node* node = CreateNode(curr, NodeID::FunctionDecl);
		Advance();
		eastl::vector<Node*>* parameters = ParseParametersList();
		node->functionDecl.parameters = parameters;
		if (Expect(UniqueType::Rparen, "Expected function closure (')')")) Advance();

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
			AddError(curr, "Expected function block opening ('{') or ('=>')");
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseFunction(Type* returnType, Token* start)
	{
		if (Expect(TokenType::Identifier, "Expected function name identifier"))
		{
			Node* node = CreateNode(start, NodeID::FunctionStmnt);
			Token* name = curr;
			Advance();

			if (Expect(UniqueType::DoubleColon))
			{
				return ParseStateFunction(returnType, start, name);
			}
			else
			{
				node->function.generics = ParseGenerics();
				if (Expect(UniqueType::Lparen, "Expected function starting with ('(')"))
				{
					node->function.returnType = returnType;
					node->function.name = name;
					node->function.decl = ParseFunctionDecl();
					node->end = node->function.decl->end;
					symbolTable->AddFunction(node);
					return node;
				}
			}
		}

		return InvalidNode();
	}

	void AddUniformCallParam(eastl::vector<Node*>* params, Token* stateName)
	{
		Token* thisName = &thisToken;
		Node* param = CreateNode(thisName, NodeID::Definition);
		param->definition.assignment = nullptr;
		param->definition.name = thisName;
		param->definition.type = CreateTypePtr(TypeID::NamedType);
		param->definition.type->namedType.typeName = stateName;
		params->insert(params->begin(), param);
	}

	Node* ParseStateFunction(Type* returnType, Token* start, Token* name)
	{
		Advance();
		switch (curr->uniqueType)
		{
		case UniqueType::Name:
		{
			Node* node = CreateNode(start, NodeID::Method);
			node->method.returnType = returnType;
			node->method.stateName = name;
			node->method.name = curr;
			Advance();
			node->method.generics = ParseGenerics();
			if (Expect(UniqueType::Lparen, "Expected function starting with ('(')"))
			{
				node->method.decl = ParseFunctionDecl();
				node->end = node->method.decl->end;
				AddUniformCallParam(node->method.decl->functionDecl.parameters, node->method.stateName);
				symbolTable->AddMethod(node);
				return node;
			}
			break;
		}
		case UniqueType::OperatorOverload:
		{
			Node* op = CreateNode(start, NodeID::StateOperator);
			op->stateOperator.returnType = returnType;
			op->stateOperator.stateName = name;
			Advance();
			op->stateOperator.generics = ParseGenerics();
			if (Expect(UniqueType::DoubleColon, "Missing '::' after 'operator' and before the operator to overload"))
			{
				Advance();
				if (curr->type == TokenType::Operator)
				{
					op->stateOperator.op = curr;
					Advance();
					if (Expect(UniqueType::Lparen, "Expected function starting with ('(')"))
					{
						op->stateOperator.decl = ParseFunctionDecl();
						op->end = op->stateOperator.decl->end;
						AddUniformCallParam(op->stateOperator.decl->functionDecl.parameters, op->stateOperator.stateName);
						symbolTable->AddOperator(op);
						return op;
					}
				}
				else AddError(curr, "Expected an overloadable operator");
			}
			break;
		}
		case UniqueType::Delete:
		{
			Node* del = CreateNode(start, NodeID::Destructor);
			del->destructor.stateName = name;
			del->destructor.del = curr;
			Advance();
			if (Expect(UniqueType::FatArrow)) Advance();
			del->destructor.body = ParseBody();
			symbolTable->SetDestructor(del);
			return del;
		}
		case UniqueType::Lparen:
		{
			Node* con = CreateNode(start, NodeID::Constructor);
			con->constructor.stateName = name;
			con->constructor.decl = ParseFunctionDecl();
			AddUniformCallParam(con->constructor.decl->functionDecl.parameters, con->constructor.stateName);
			symbolTable->AddConstructor(con);
			return con;
		}
		default:
			break;
		}

		return InvalidNode();
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
		Node* node = CreateNode(curr, NodeID::GenericsDecl);
		node->generics.names = CreateVectorPtr<Token*>();
		node->generics.whereStmnt = nullptr;
		Advance();

		if (Expect(UniqueType::Greater))
		{
			AddError(curr, "Expected generic types not '<>'");
			node->nodeID = NodeID::InvalidNode;
			return node;
		}

		while (!Expect(UniqueType::Greater) && !Expect(UniqueType::Colon) && !IsEOF())
		{
			if (Expect(TokenType::Identifier, "Only identifiers expected as generic declarations"))
			{
				node->generics.names->push_back(curr);
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

		if (Expect(UniqueType::Greater, "Expected generic closure ('>')"))
		{
			node->end = curr;
			Advance();
			return node;
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseWhere()
	{
		Node* node = CreateNode(curr, NodeID::WhereStmnt);
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
		Node* block = CreateNode(curr, NodeID::Block);
		block->block.inner = CreateVectorPtr<Node*>();
		if (!Expect(UniqueType::Lbrace, "Expected function block opening ('{') or ('=>')"))
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

		if (Expect(UniqueType::Rbrace, "Expected function block closure ('}')"))
		{
			block->end = curr;
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
			if (curr->type == TokenType::Literal) return ParseExprStmnt();
			Advance();
			break;
		}

		return CreateNode(curr, NodeID::InvalidNode);
	}

	Node* ParseIdentifierStmnt()
	{
		Token* next = Peek();
		if (next->uniqueType == UniqueType::Colon || next->uniqueType == UniqueType::ImplicitAssign)
		{
			return ParseAssignmentStatement();
		}

		return ParseExprStmnt();
	}

	Node* ParseBlockOrInlineType()
	{
		Token* start = curr;

		Logger::SetErrorRollback();
		Type* type = ParseInlineType(true);
		if (type->typeID != TypeID::InvalidType &&
			(Expect(UniqueType::Assign) || Expect(UniqueType::ImplicitAssign)))
		{
			if ((type->typeID == TypeID::ImplicitType && Expect(UniqueType::Assign)) ||
				(type->typeID == TypeID::ExplicitType && Expect(UniqueType::ImplicitAssign)))
			{
				AddError(curr, "Explicit types must be assigned with '=' and implicit types must be assigned with ':='");
				return CreateNode(curr, NodeID::InvalidNode);
			}

			Node* node = CreateNode(start, NodeID::InlineDefinition);
			node->inlineDefinition.type = type;
			node->inlineDefinition.op = curr;
			Advance();
			node->inlineDefinition.assignment = ParseExpr();
			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			return node;
		}

		curr = start;
		Logger::ErrorRollback();
		return ParseBlock();
	}

	Node* ParseExprStmnt()
	{
		Node* node = CreateNode(curr, NodeID::ExpressionStmnt);
		Expr* expr = ParseExpr();
		if (expr->typeID != ExprID::InvalidExpr)
		{
			if (Expect(UniqueType::Semicolon)) Advance();
			node->expressionStmnt.expression = expr;
			node->end = curr;

			if (IsAssignableExpr(expr) && IsAssignmentOperator())
			{
				node->nodeID = NodeID::AssignmentStmnt;
				node->assignmentStmnt.assignTo = expr;
				node->assignmentStmnt.op = curr;
				Advance();
				node->assignmentStmnt.assignment = ParseExpr();
				if (Expect(UniqueType::Semicolon)) Advance();
				node->end = curr;
			}

			return node;
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseIf()
	{
		Node* node = CreateNode(curr, NodeID::IfStmnt);
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

			node->end = curr;
			return node;
		}


		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseConditional()
	{
		Node* node = CreateNode(curr, NodeID::Conditional);
		if (Expect(UniqueType::Lparen, "Expected conditional opening ('(')"))
		{
			Advance();
			Expr* condition = ParseExpr();
			if (condition->typeID != ExprID::InvalidExpr &&
				Expect(UniqueType::Rparen, "Expected conditional closure (')')"))
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
		Node* node = CreateNode(curr, NodeID::ForStmnt);
		Advance();
		if (Expect(UniqueType::Lparen, "Expected 'for' statement opening ('(')"))
		{
			Advance();
			if (Expect(TokenType::Identifier, "Expected identifier after '(' in 'for' statement"))
			{
				if (Peek()->uniqueType == UniqueType::Colon)
				{
					node->forStmnt.isDeclaration = true;
					node->forStmnt.iterated.declaration = ParseDeclaration();
				}
				else
				{
					node->forStmnt.isDeclaration = false;
					node->forStmnt.iterated.identifier = curr;
					Advance();
				}

				if (Expect(UniqueType::In)) node->forStmnt.rangeFor = false;
				else if (Expect(UniqueType::To)) node->forStmnt.rangeFor = true;
				else
				{
					AddError(curr, "Expected for iterator '..' or 'in'");
					node->nodeID = NodeID::InvalidNode;
					return node;
				}

				node->forStmnt.iterator = curr;
				Advance();

				Expr* expr = ParseExpr();
				if (expr->typeID != ExprID::InvalidExpr)
				{
					node->forStmnt.toIterate = expr;
					if (Expect(UniqueType::Rparen, "Expected 'for' statement closure (')')"))
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
		Node* node = CreateNode(curr, NodeID::SwitchStmnt);
		Advance();

		if (Expect(UniqueType::Lparen, "Expected 'switch' statement opening ('(')"))
		{
			Advance();
			Expr* switchOn = ParseExpr();
			if (switchOn->typeID != ExprID::InvalidExpr)
			{
				node->switchStmnt.switchOn = switchOn;

				if (Expect(UniqueType::Rparen, "Expected 'switch' statement closure (')')"))
				{
					Advance();

					if (Expect(UniqueType::Lbrace, "Expected 'switch' statement block opening ('{')"))
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

						if (Expect(UniqueType::Rbrace, "Expected 'switch' statement block closure ('}')"))
						{
							node->end = curr;
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
		Node* node = CreateNode(curr, NodeID::WhileStmnt);
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
		Node* node = CreateNode(curr, NodeID::DeferStmnt);
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
		Node* node = CreateNode(curr, NodeID::DeleteStmnt);
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
			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			return node;
		}

		node->nodeID = NodeID::InvalidNode;
		return node;
	}

	Node* ParseContinue()
	{
		Node* node = CreateNode(curr, NodeID::ContinueStmnt);
		node->continueStmnt.token = curr;
		Advance();
		if (Expect(UniqueType::Semicolon)) Advance();
		node->end = curr;
		return node;
	}

	Node* ParseBreak()
	{
		Node* node = CreateNode(curr, NodeID::BreakStmnt);
		node->breakStmnt.token = curr;
		Advance();
		if (Expect(UniqueType::Semicolon)) Advance();
		node->end = curr;
		return node;
	}

	Node* ParseReturn()
	{
		Node* node = CreateNode(curr, NodeID::ReturnStmnt);
		Advance();
		if (Expect(UniqueType::Semicolon))
		{
			node->returnStmnt.voidReturn = true;
			node->returnStmnt.expr = nullptr;
			node->end = curr;
			Advance();
			return node;
		}
		else
		{
			node->returnStmnt.voidReturn = false;
			node->returnStmnt.expr = ParseExpr();
			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			return node;
		}
	}

	eastl::vector<Node*>* ParseParametersList(UniqueType end = UniqueType::Rparen)
	{
		eastl::vector<Node*>* parameters = CreateVectorPtr<Node*>();

		if (Expect(UniqueType::Rparen)) return parameters;

		bool parsingDefs = false;
		while (!Expect(end) && !IsEOF())
		{
			Node* decl = ParseDeclOrDef();
			if (decl->nodeID != NodeID::InvalidNode)
			{
				if (parsingDefs && !decl->definition.assignment) 
					AddError(decl->start, "Cannot have a parameter without a default value after having a parameter with a default value");
				else if (!parsingDefs && decl->definition.assignment) parsingDefs = true;
				parameters->push_back(decl);
			}
			else Advance();

			if (Expect(UniqueType::Comma)) Advance();
		}

		return parameters;
	}

	Node* ParseAssignmentStatement()
	{
		Token* start = curr;
		switch (Peek()->uniqueType)
		{
		case UniqueType::ImplicitAssign:
		case UniqueType::Colon:
		{
			Node* def = ParseDefinition();
			return def;
		}
		default:
			return InvalidNode();
		}
	}

	Node* ParseDefinition()
	{
		Token* start = curr;
		Expect(TokenType::Identifier, "Expected an identifier, possible compiler bug");
		switch (Peek()->uniqueType)
		{
		case UniqueType::ImplicitAssign:
			return ParseImplicitAssignment();
		case UniqueType::Colon:
			return ParseExplicitAssignment();
		default:
			AddError(start, "Expected a variable definition");
			return InvalidNode();
		}
	}

	Node* ParseImplicitAssignment()
	{
		Token* start = curr;
		if (Expect(TokenType::Identifier) &&
			ThenExpect(UniqueType::ImplicitAssign))
		{
			Node* node = CreateNode(start, NodeID::Definition);
			node->definition.name = start;
			node->definition.op = curr;

			Advance();
			Type* type = CreateTypePtr(TypeID::UnknownType);
			Expr* expr = ParseAssignmentType();
			node->definition.type = type;
			node->definition.assignment = expr;
			node->end = curr;
			if (Expect(UniqueType::Semicolon)) Advance();
			return node;
		}

		return InvalidNode();
	}

	Node* ParseExplicitAssignment()
	{
		Node* node = ParseDeclaration();

		if (ParseAssignment(node)) return node;

		return InvalidNode();
	}

	bool ParseAssignment(Node* decl)
	{
		if (Expect(UniqueType::Assign, "Variable declerations must have assignments"))
		{
			decl->definition.op = curr;
			Advance();
			Expr* expr = ParseAssignmentType();
			decl->definition.assignment = expr;

			if (Expect(UniqueType::Semicolon)) Advance();
			decl->end = curr;
			return true;
		}

		return false;
	}

	Node* ParseDeclaration()
	{
		Token* start = curr;
		if (Expect(TokenType::Identifier, "Expected an identifier, possible compiler bug") &&
			ThenExpect(UniqueType::Colon, "Expected a colon (':') after identifier in explicit definition"))
		{
			Advance();
			Type* type = ParseDeclarationType();
			Node* node = CreateNode(start, NodeID::Definition);
			node->definition.name = start;
			node->definition.type = type;
			node->definition.assignment = nullptr;

			return node;
		}

		return InvalidNode();
	}

	Type* ParseDeclarationType(bool allowImplicitType = true)
	{
		Type* type = CreateTypePtr(TypeID::InvalidType);

		switch (curr->type)
		{
		case Primitive:
			type = CreatePrimitive(curr->uniqueType);
			Advance();
			break;

		case Identifier:
		{
			Token* name = curr;
			if (Peek()->uniqueType == UniqueType::Period)
			{
				Advance();
				if (ThenExpect(TokenType::Identifier, "Expected identifier after selector ('.') in declaration type"))
				{
					type->typeID = TypeID::ImportedType;
					type->importedType.packageName = name;
					type->importedType.typeName = curr;
					Advance();
				}
			}
			else
			{
				type->typeID = TypeID::NamedType;
				type->namedType.typeName = name;
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
				type = ParsePointerType();
				break;

			case UniqueType::Tilde:
				type = ParseValueType();
				break;

			case UniqueType::Array:
				type = ParseArrayType();
				break;

			case UniqueType::DoubleColon:
				type = ParseFunctionType();
				break;

			default:
				AddError(curr, "Expected type decleration");
				break;
			}
			break;
		}

		if (type->typeID != TypeID::InvalidType && Expect(UniqueType::Less))
		{
			type = ParseGenericsType(type);
		}

		return type;
	}

	Type* ParsePointerType()
	{
		Type* ptrType = CreateTypePtr(TypeID::PointerType);
		ptrType->pointerType.ptr = curr;
		Advance();
		ptrType->pointerType.type = ParseDeclarationType();
		return ptrType;
	}

	Type* ParseValueType()
	{
		Type* valueType = CreateTypePtr(TypeID::ValueType);
		valueType->valueType.valueOp = curr;
		Advance();
		Type* type = ParseDeclarationType();
		if (type->typeID == TypeID::ValueType)
			AddError(curr, "Cannot have a value type of a value type (ie. ~~MyType)");
		valueType->valueType.type = type;
		return valueType;
	}

	Type* ParseArrayType()
	{
		Type* arrType = CreateTypePtr(TypeID::ArrayType);
		arrType->arrayType.arr = curr;
		Advance();
		arrType->arrayType.type = ParseDeclarationType();
		return arrType;
	}

	Type* ParseInlineType(bool allowImplicitType)
	{
		Token* start = curr;
		Advance();
		if (allowImplicitType)
		{
			Token* next = Peek();
			bool implicit = Expect(TokenType::Identifier) && next->uniqueType == UniqueType::Comma;
			if (Expect(UniqueType::Rbrace))
			{
				AddError(curr, "Empty inline type not allowed ('{}')");
				return CreateTypePtr(TypeID::InvalidType);
			}

			return implicit ? ParseImplicitType() : ParseExplicitType();
		}

		return ParseExplicitType();
	}

	Type* ParseImplicitType()
	{
		Type* type = CreateTypePtr(TypeID::ImplicitType);
		eastl::vector<Token*>* idents = CreateVectorPtr<Token*>();
		type->implicitType.identifiers = idents;
		while (!Expect(UniqueType::Rbrace) && !IsEOF())
		{
			if (Expect(TokenType::Identifier, "Only identifiers are allowed in implicit types"))
			{
				idents->push_back(curr);
				Advance();
				if (Expect(UniqueType::Comma)) Advance();
			}
			else
			{
				type->typeID = TypeID::InvalidType;
				return type;
			}
		}


		if (Expect(UniqueType::Rbrace, "Missing closure for inline type '}'"))
		{
			if (idents->size() == 1)
			{
				AddError(curr, "Inline types with only one parameter are not allowed");
				type->typeID = TypeID::InvalidType;
				return type;
			}
			Advance();
			return type;
		}

		type->typeID = TypeID::InvalidType;
		return type;
	}

	Type* ParseExplicitType()
	{
		Type* type = CreateTypePtr(TypeID::ExplicitType);
		eastl::vector<Node*>* decls = CreateVectorPtr<Node*>();
		type->explicitType.declarations = decls;
		while (!Expect(UniqueType::Rbrace) && !IsEOF())
		{
			Node* node = ParseDeclaration();
			if (node->nodeID != NodeID::InvalidNode)
			{
				decls->push_back(node);
				if (Expect(UniqueType::Comma)) Advance();
			}
			else
			{
				type->typeID = TypeID::InvalidType;
				return type;
			}
		}

		if (Expect(UniqueType::Rbrace, "Missing closure for inline type '}'"))
		{
			if (decls->size() == 1)
			{
				AddError(curr, "Inline types with only one parameter are not allowed");
				type->typeID = TypeID::InvalidType;
				return type;
			}
			Advance();
			return type;
		}

		type->typeID = TypeID::InvalidType;
		return type;
	}

	Type* ParseGenericsType(Type* type)
	{
		Type* genericsType = CreateTypePtr(TypeID::GenericsType);
		genericsType->genericsType.generics = ParseGenericsExpr();
		genericsType->genericsType.type = type;

		return genericsType;
	}

	Type* ParseFunctionType()
	{
		Type* functionType = CreateTypePtr(TypeID::FunctionType);
		functionType->functionType.paramTypes = CreateVectorPtr<Type*>();
		Advance();

		functionType->functionType.returnType = Expect(UniqueType::Lparen)
			? CreateVoidType()
			: ParseDeclarationType();

		if (Expect(UniqueType::Lparen, "Expected function type opening ('(')"))
		{
			Advance();
			while (!Expect(UniqueType::Rparen) && !IsEOF())
			{
				Type* type = ParseDeclarationType();
				functionType->functionType.paramTypes->push_back(type);
				if (Expect(UniqueType::Comma)) Advance();
			}

			if (Expect(UniqueType::Rparen, "Expected function type closure (')')"))
			{
				Advance();
				return functionType;
			}
		}

		functionType->typeID = TypeID::InvalidType;
		return functionType;
	}

	Expr* ParseAssignmentType()
	{
		return ParseExpr();
	}

	Expr* ParseNewExpr()
	{
		Token* newIndex = curr;
		Expr* newExpr = CreateExpr(newIndex, ExprID::NewExpr);
		newExpr->newExpr.newIndex = newIndex;
		Advance();
		newExpr->newExpr.primaryExpr = ParsePrimaryExpr();
		if (Expect(UniqueType::At))
		{
			Advance();
			newExpr->newExpr.atExpr = ParsePrimaryExpr();
		}
		else newExpr->newExpr.atExpr = nullptr;

		return newExpr;
	}

	Expr* ParseCompileExpr()
	{
		Expr* expr = CreateExpr(curr, ExprID::CompileExpr);
		Advance();
		Type* type = ParseDeclarationType();
		if (type->typeID != TypeID::InvalidType)
		{
			expr->compileExpr.returnType = type;
			if (Expect(UniqueType::FatArrow)) Advance();
			expr->compileExpr.body = arena->Emplace<Body>(ParseBody());
			return expr;
		}

		expr->typeID = ExprID::InvalidExpr;
		return expr;
	}

	Expr* ParseAnonymousType()
	{
		Token* rBrace = curr;
		Advance();

		eastl::vector<Node*> params = eastl::vector<Node*>();
		while (!Expect(UniqueType::Lbrace) && !IsEOF())
		{
			Node* def = ParseDefinition();
			if (def->nodeID == NodeID::Definition) params.push_back(def);
			if (Expect(UniqueType::Comma)) Advance();
		}

		if (params.size() == 0)
		{
			AddError(rBrace, "Cannot have an empty anonymous type");
			return nullptr;
		}

		Expr* anonTypeExpr = CreateExpr(rBrace, ExprID::AnonTypeExpr);

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
			Expr* unary = CreateExpr(op, ExprID::UnaryExpr);
			unary->unaryExpr.op = op;
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

		case UniqueType::Lparen:
			return ParseGroupedExpr();

		case UniqueType::Fixed:
			return ParseFixedExpr();

		case UniqueType::Lbrace:
			return ParseAnonTypeExpr();

		case UniqueType::DoubleColon:
			return ParseFunctionTypeExpr();

		case UniqueType::New:
			return ParseNewExpr();

		case UniqueType::OnCompile:
			return ParseCompileExpr();
		default:
			switch (curr->type)
			{
			case TokenType::Literal:
				return ParseLiteralExpr();

			case TokenType::Primitive:
				return ParsePrimitiveExpr();

			default:
				AddError(curr, "No operand found for expression");
				return CreateExpr(curr, ExprID::InvalidExpr);
			}
		}
	}

	Expr* ParseIdentifierExpr()
	{
		Expr* ident = CreateExpr(curr, ExprID::IdentifierExpr);
		ident->identifierExpr.identifier = curr;
		Advance();
		return ident;
	}

	Expr* ParsePrimitiveExpr()
	{
		Expr* prim = CreateExpr(curr, ExprID::PrimitiveExpr);
		prim->primitiveExpr.primitive = curr;
		Advance();
		return prim;
	}

	Expr* ParseLiteralExpr()
	{
		Expr* primLit = CreateExpr(curr, ExprID::LiteralExpr);
		primLit->literalExpr.type = curr->uniqueType;
		primLit->literalExpr.val = curr;
		Advance();
		return primLit;
	}

	Expr* ParseGroupedExpr()
	{
		Token* lParen = curr;
		Expr* groupExpr = CreateExpr(curr, ExprID::GroupedExpr);
		groupExpr->groupedExpr.lParen = lParen;
		Advance();
		Expr* innerExpr = ParseExpr();
		groupExpr->groupedExpr.expr = innerExpr;
		if (Expect(UniqueType::Rparen, "Expected ')' after group expression"))
		{
			groupExpr->groupedExpr.rParen = curr;
			Advance();
		}
		return groupExpr;
	}

	Expr* ParseFixedExpr()
	{
		Expr* fixed = CreateExpr(curr, ExprID::FixedExpr);
		fixed->fixedExpr.fixed = curr;
		Advance();
		fixed->fixedExpr.atExpr = ParsePrimaryExpr();
		return fixed;
	}

	Expr* ParseAnonTypeExpr()
	{
		Expr* anon = CreateExpr(curr, ExprID::AnonTypeExpr);
		anon->anonTypeExpr.values = CreateVectorPtr<Expr*>();
		Advance();

		if (Expect(UniqueType::Rbrace))
		{
			AddError(curr, "Empty inline type not allowed ('{}')");
			Advance();
			return anon;
		}

		anon->anonTypeExpr.values->push_back(ParseExpr());

		while (Expect(UniqueType::Comma))
		{
			Advance();
			anon->anonTypeExpr.values->push_back(ParseExpr());
		}

		if (Expect(UniqueType::Rbrace, "Missing closure for inline type '}'")) Advance();

		return anon;
	}

	Expr* ParseFunctionTypeExpr()
	{
		Token* start = curr;
		Expr* expr = CreateExpr(start, ExprID::FunctionTypeDeclExpr);
		Advance();
		Type* returnType = Expect(UniqueType::Lparen)
			? CreateVoidType()
			: ParseDeclarationType();

		if (Expect(UniqueType::Lparen, "Expected function type opening ('(')"))
		{
			Token* lparen = curr;
			Advance();
			UniqueType peekType = Peek()->uniqueType;
			if ((Expect(TokenType::Identifier) && peekType == UniqueType::Colon) ||
				(Expect(UniqueType::Rparen) && (peekType == UniqueType::Lbrace || peekType == UniqueType::FatArrow)))
			{
				curr = lparen;
				expr->functionTypeDeclExpr.returnType = returnType;
				expr->functionTypeDeclExpr.functionDecl = ParseFunctionDecl();
				return expr;
			}
			else
			{
				curr = start;
				expr->typeID = ExprID::FunctionTypeExpr;
				expr->functionTypeExpr.functionType = ParseFunctionType();
				return expr;
			}
		}

		return CreateExpr(start, ExprID::InvalidExpr);
	}

	Expr* ParseSelector(Expr* on)
	{
		Expr* selector = CreateExpr(curr, ExprID::SelectorExpr);
		selector->selectorExpr.on = on;
		if (ThenExpect(TokenType::Identifier, "Expected an identifier after '.'"))
		{
			selector->selectorExpr.select = ParseIdentifierExpr();
		}
		return selector;
	}

	Expr* ParseIndex(Expr* of)
	{
		Token* lBrack = curr;
		Expr* indexExpr = CreateExpr(lBrack, ExprID::IndexExpr);
		indexExpr->indexExpr.of = of;
		indexExpr->indexExpr.lBrack = lBrack;
		Advance();
		if (curr->uniqueType == UniqueType::Rbrack)
		{
			indexExpr->indexExpr.rBrack = curr;
			AddError(curr, "Unexpected empty index ('[]') in expression");
		}
		else
		{
			Expr* expr = ParseExpr();
			indexExpr->indexExpr.index = expr;
			if (Expect(UniqueType::Rbrack, "Expected ']' after index expression"))
			{
				indexExpr->indexExpr.rBrack = curr;
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
		funcCall->functionCallExpr.lParen = lParen;
		Advance();
		if (!Expect(UniqueType::Rparen))
		{
			funcCall->functionCallExpr.params = ParseExprList();
		}
		else
		{
			funcCall->functionCallExpr.params = nullptr;
		}

		if (Expect(UniqueType::Rparen, "Missing ')' at end of function call"))
		{
			funcCall->functionCallExpr.rParen = curr;
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
		Expr* generics = CreateExpr(curr, ExprID::GenericsExpr);
		eastl::vector<Type*>* genericTypes = CreateVectorPtr<Type*>();
		generics->genericsExpr.expr = expr;
		generics->genericsExpr.open = curr;
		generics->genericsExpr.types = genericTypes;

		Advance();

		if (Expect(UniqueType::Greater))
		{
			Advance();
			AddError(curr, "Expected generic types not '<>'");
			return generics;
		}

		while (!Expect(UniqueType::Greater) && !IsEOF())
		{
			Type* type = ParseDeclarationType();
			if (type->typeID != TypeID::InvalidType)
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

		if (Expect(UniqueType::Greater, "Expected generic closure ('>')"))
		{
			generics->genericsExpr.close = curr;
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
		asExpr->asExpr.to = ParseDeclarationType();

		return asExpr;
	}

	Expr* ParseDereference(Expr* of)
	{
		Expr* expr = CreateExpr(of->start, ExprID::DereferenceExpr);
		expr->dereferenceExpr.of = of;
		expr->dereferenceExpr.op = curr;
		Advance();

		return expr;
	}

	Expr* ParseReference(Expr* of)
	{
		Expr* expr = CreateExpr(of->start, ExprID::ReferenceExpr);
		expr->dereferenceExpr.of = of;
		expr->dereferenceExpr.op = curr;
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

	Type* CreateVoidType()
	{
		Type* type = CreateTypePtr(TypeID::PrimitiveType);
		type->primitiveType.type = UniqueType::Void;
		type->primitiveType.size = 0;
		type->primitiveType.isSigned = false;

		return type;
	}

	Type* CreatePrimitive(UniqueType primType)
	{
		Type* type = CreateTypePtr(TypeID::PrimitiveType);
		type->primitiveType.type = primType;
		switch (primType)
		{
		case UniqueType::Void:
			type->primitiveType.size = 0;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Bool:
			type->primitiveType.size = 1;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Byte:
			type->primitiveType.size = 8;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Ubyte:
			type->primitiveType.size = 8;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Int:
			type->primitiveType.size = targetArchBitWidth;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int16:
			type->primitiveType.size = 16;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int32:
			type->primitiveType.size = 32;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int64:
			type->primitiveType.size = 64;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int128:
			type->primitiveType.size = 128;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Uint:
			type->primitiveType.size = targetArchBitWidth;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint16:
			type->primitiveType.size = 16;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint32:
			type->primitiveType.size = 32;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint64:
			type->primitiveType.size = 64;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint128:
			type->primitiveType.size = 128;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Float:
			type->primitiveType.size = targetArchBitWidth;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Float32:
			type->primitiveType.size = 32;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Float64:
			type->primitiveType.size = 64;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::String:
			type->primitiveType.size = targetArchBitWidth * 2;
			type->primitiveType.isSigned = false;
			break;
		default:
			break;
		}

		return type;
	}
};
