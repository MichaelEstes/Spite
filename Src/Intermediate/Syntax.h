#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"
#include "EASTL/stack.h"

#include "../Tokens/Tokens.h"
#include "../Containers/Arena.h"

#include "Stmnt.h"
#include "Type.h"
#include "Expr.h"
#include "SymbolTable.h"


struct Scope
{
	Stmnt* scopeOf;

	Scope()
	{
		scopeOf = nullptr;
	}

	Scope(Stmnt* scopeOf)
	{
		this->scopeOf = scopeOf;
	}

	Scope(const Scope& copy)
	{
		scopeOf = copy.scopeOf;
	}
};

struct Syntax
{
	const StringView thisStr = "this";

	Tokens& tokens;
	Scope currScope;
	Token* curr;
	SymbolTable* symbolTable;
	size_t nodeCount;

	eastl::vector<Stmnt*> nodes;
	eastl::stack<Scope> scopes;
	Stmnt* package;

	Syntax(Tokens& tokensRef) : tokens(tokensRef)
	{
		curr = nullptr;
		nodes = eastl::vector<Stmnt*>();
		scopes = eastl::stack<Scope>();
		currScope = Scope();
		scopes.push(currScope);
		nodeCount = 0;
	}

	void Print()
	{
		eastl::string toPrint = "";
		if (package->nodeID == StmntID::PackageStmnt)
		{
			toPrint += ToString(package);
			toPrint += '\n';
		}

		for (Stmnt* node : symbolTable->imports)
		{
			toPrint += ToString(node);
			toPrint += '\n';
		}

		for (Stmnt* node : nodes)
		{
			toPrint += ToString(node);
			toPrint += '\n';
		}

		Logger::Info(toPrint);
	}

	inline void AddNode(Stmnt* node)
	{
		nodes.emplace_back(node);
	}

	inline Stmnt* CreateStmnt(Token* start, StmntID nodeID)
	{
		nodeCount += 1;
		return symbolTable->CreateStmnt(start, nodeID, currScope.scopeOf);
	}

	inline Stmnt* InvalidStmnt()
	{
		return symbolTable->InvalidStmnt();
	}

	template<typename T>
	inline eastl::vector<T>* CreateVectorPtr()
	{
		return symbolTable->CreateVectorPtr<T>();
	}

	inline Type* CreateTypePtr(TypeID typeID)
	{
		return symbolTable->CreateTypePtr(typeID);
	}

	inline Expr* CreateExpr(Token* start, ExprID exprID)
	{
		return symbolTable->CreateExpr(start, exprID);
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
		return symbolTable->CreatePrimitive(primType);
	}

	inline void StartScope(Stmnt* of)
	{
		currScope = scopes.emplace(of);
	}

	inline void EndScope()
	{
		if (scopes.size() == 1)
		{
			Logger::FatalError("Syntax:EndScope reached global scope, possible compiler error");
		}

		scopes.pop();
		currScope = scopes.top();
	}

	inline bool IsGlobalScope()
	{
		return currScope.scopeOf == nullptr;
	}

	inline bool IsEOF()
	{
		return curr->type & TokenType::EndOfFile;
	}

	void BuildSyntax()
	{
		symbolTable = new SymbolTable((tokens.tokens.size() / 2) * sizeof(Stmnt));
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
			Stmnt* node = CreateStmnt(curr, StmntID::CommentStmnt);
			AddNode(node);
			curr = tokens.Next(curr);
		}
	}

	void ParsePackage(bool setInTree = true)
	{
		Token* start = curr;
		if (Expect(UniqueType::Package, "File must start with a 'package' statement") &&
			ThenExpect(TokenType::Identifier, "Expected an identifier after package token"))
		{
			Stmnt* node = CreateStmnt(start, StmntID::PackageStmnt);
			node->package.name = curr;
			Advance();
			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			if (setInTree) package = node;
		}
	}

	void ParseNext()
	{
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
		{
			Stmnt* node = ParseCompile();
			if (node->nodeID != StmntID::InvalidStmnt) 
			{
				symbolTable->AddOnCompile(node);
				AddNode(node);
			}
			return;
		}
		case UniqueType::OnCompileDebug:
			ParseCompileDebug();
			return;
		case UniqueType::Name:
		{
			Stmnt* assignment = ParseAssignmentStatement();
			if (assignment->nodeID != StmntID::InvalidStmnt)
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
			Stmnt* func = ParseFunction(type, start);
			if (func->nodeID != StmntID::InvalidStmnt) AddNode(func);
			return;
		}
		}

		AddError(curr, "Syntax:ParseNext Unexpected token : " + curr->ToString());
		Advance();
	}

	void ParseUsing()
	{
		bool addNode = IsGlobalScope();
		if (!addNode) AddError(curr, "Cannot import packages outside of the global scope");

		Token* start = curr;
		if (Expect(UniqueType::Using) &&
			ThenExpect(TokenType::Identifier, "Expected an identifier after 'using' token"))
		{
			Stmnt* node = CreateStmnt(start, StmntID::UsingStmnt);
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
			if (addNode) symbolTable->AddImport(node);
		}
	}

	void ParseState()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::StateStmnt);
		Advance();
		if (Expect(TokenType::Identifier, "Expected an identifier after 'state'"))
		{
			node->state.name = curr;
			node->state.members = CreateVectorPtr<Stmnt*>();
			node->state.insetFlags = symbolTable->arena->Emplace<Flags<>>();
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

	void ParseStateMember(Stmnt* state)
	{
		if (Expect(UniqueType::Lbrack))
		{
			InsetID inset = ParseStateInset();
			if (inset != InsetID::InvalidInset) state->state.insetFlags->Set(inset);
			return;
		}

		Stmnt* member = ParseDeclOrDef();
		if (member->nodeID != StmntID::InvalidStmnt) state->state.members->push_back(member);
	}

	Stmnt* ParseDeclOrDef()
	{
		Stmnt* node = ParseDeclaration();
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

	Stmnt* ParseCompile()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::CompileStmnt);
		Advance();
		Type* type = ParseDeclarationType();
		if (type->typeID != TypeID::InvalidType)
		{
			node->compileStmnt.returnType = type;
			if (Expect(UniqueType::FatArrow)) Advance();
			node->compileStmnt.body = ParseBody(node);

			if (Expect(UniqueType::Semicolon))
			{
				node->end = curr;
				Advance();
			}
			else
			{
				node->end = node->compileStmnt.body.body->end;
			}

			return node;
		}

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	void ParseCompileDebug()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::CompileDebugStmnt);
		Advance();
		node->compileDebugStmnt.body = ParseBody(node);
		if (node->compileDebugStmnt.body)
		{
			node->end = node->compileDebugStmnt.body.body->end;

			symbolTable->AddOnCompile(node);
			AddNode(node);
		}
	}

	Stmnt* ParseFunctionDecl(Stmnt* of)
	{
		Stmnt* node = CreateStmnt(curr, StmntID::FunctionDecl);
		Advance();
		eastl::vector<Stmnt*>* parameters = ParseParametersList();
		node->functionDecl.parameters = parameters;
		if (Expect(UniqueType::Rparen, "Expected function closure (')')")) Advance();

		if (Expect(UniqueType::Lbrace))
		{
			node->functionDecl.body.statement = false;
			node->functionDecl.body.body = ParseBlock(of);
			node->end = node->functionDecl.body.body->end;
			return node;
		}
		else if (Expect(UniqueType::FatArrow))
		{
			Advance();
			node->functionDecl.body.statement = true;
			node->functionDecl.body.body = ParseBodyStmnt(of);
			node->end = node->functionDecl.body.body->end;
			return node;
		}
		else
		{
			AddError(curr, "Expected function block opening ('{') or ('=>')");
		}

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseFunction(Type* returnType, Token* start)
	{
		if (Expect(TokenType::Identifier, "Expected function name identifier"))
		{
			Stmnt* node = CreateStmnt(start, StmntID::FunctionStmnt);
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
					node->function.decl = ParseFunctionDecl(node);
					node->end = node->function.decl->end;
					symbolTable->AddFunction(node);
					return node;
				}
			}
		}

		return InvalidStmnt();
	}

	void AddUniformCallParam(eastl::vector<Stmnt*>* params, Token* stateName)
	{
		Token* thisName = &thisToken;
		Stmnt* param = CreateStmnt(thisName, StmntID::Definition);
		param->definition.assignment = nullptr;
		param->definition.name = thisName;
		param->definition.type = CreateTypePtr(TypeID::NamedType);
		param->definition.type->namedType.typeName = stateName;
		params->insert(params->begin(), param);
	}

	Stmnt* ParseStateFunction(Type* returnType, Token* start, Token* name)
	{
		Advance();
		switch (curr->uniqueType)
		{
		case UniqueType::Name:
		{
			Stmnt* node = CreateStmnt(start, StmntID::Method);
			node->method.returnType = returnType;
			node->method.stateName = name;
			node->method.name = curr;
			Advance();
			node->method.generics = ParseGenerics();
			if (Expect(UniqueType::Lparen, "Expected function starting with ('(')"))
			{
				node->method.decl = ParseFunctionDecl(node);
				node->end = node->method.decl->end;
				AddUniformCallParam(node->method.decl->functionDecl.parameters, node->method.stateName);
				symbolTable->AddMethod(node);
				return node;
			}
			break;
		}
		case UniqueType::OperatorOverload:
		{
			Stmnt* op = CreateStmnt(start, StmntID::StateOperator);
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
						op->stateOperator.decl = ParseFunctionDecl(op);
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
			Stmnt* del = CreateStmnt(start, StmntID::Destructor);
			del->destructor.stateName = name;
			del->destructor.del = curr;
			Advance();
			if (Expect(UniqueType::FatArrow)) Advance();
			del->destructor.body = ParseBody(del);
			symbolTable->SetDestructor(del);
			return del;
		}
		case UniqueType::Lparen:
		{
			Stmnt* con = CreateStmnt(start, StmntID::Constructor);
			con->constructor.stateName = name;
			con->constructor.decl = ParseFunctionDecl(con);
			AddUniformCallParam(con->constructor.decl->functionDecl.parameters, con->constructor.stateName);
			symbolTable->AddConstructor(con);
			return con;
		}
		default:
			break;
		}

		return InvalidStmnt();
	}

	Stmnt* ParseGenerics()
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

	Stmnt* ParseGenericDecl()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::GenericsDecl);
		node->generics.names = CreateVectorPtr<Token*>();
		node->generics.whereStmnt = nullptr;
		Advance();

		if (Expect(UniqueType::Greater))
		{
			AddError(curr, "Expected generic types not '<>'");
			node->nodeID = StmntID::InvalidStmnt;
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
				node->nodeID = StmntID::InvalidStmnt;
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
			node->generics.count = node->generics.names->size();
			node->generics.templatesToExpand = symbolTable->arena->Emplace<eastl::hash_set<eastl::vector<Expr*>*, ExprArrHash>>();
			return node;
		}

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseWhere()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::WhereStmnt);
		Advance();

		node->whereStmnt.decl = ParseFunctionDecl(node);

		if (node->whereStmnt.decl->nodeID != StmntID::InvalidStmnt) return node;

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Body ParseBody(Stmnt* of)
	{
		Body body = Body();

		if (Expect(UniqueType::Lbrace))
		{
			body.statement = false;
			body.body = ParseBlock(of);
		}
		else
		{
			body.statement = true;
			body.body = ParseBodyStmnt(of);
		}

		return body;
	}

	Stmnt* ParseBodyStmnt(Stmnt* of)
	{
		StartScope(of);
		Stmnt* stmnt = ParseBlockStatment();
		EndScope();

		return stmnt;
	}

	Stmnt* ParseBlock(Stmnt* of)
	{
		Stmnt* block = CreateStmnt(curr, StmntID::Block);
		block->block.inner = CreateVectorPtr<Stmnt*>();
		if (!Expect(UniqueType::Lbrace, "Expected function block opening ('{') or ('=>')"))
		{
			block->nodeID = StmntID::InvalidStmnt;
			return block;
		}
		Advance();

		StartScope(of);

		while (!Expect(UniqueType::Rbrace) && !IsEOF())
		{
			Stmnt* stmnt = ParseBlockStatment();
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

	Stmnt* ParseBlockStatment()
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

		//AddError(curr, "Syntax:ParseBlockStatment Unexpected token : " + curr->ToString());
		return CreateStmnt(curr, StmntID::InvalidStmnt);
	}

	Stmnt* ParseIdentifierStmnt()
	{
		Token* next = Peek();
		if (next->uniqueType == UniqueType::Colon || next->uniqueType == UniqueType::ImplicitAssign)
		{
			return ParseAssignmentStatement();
		}

		return ParseExprStmnt();
	}

	Stmnt* ParseBlockOrInlineType()
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
				return CreateStmnt(curr, StmntID::InvalidStmnt);
			}

			Stmnt* node = CreateStmnt(start, StmntID::InlineDefinition);
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
		return ParseBlock(currScope.scopeOf);
	}

	Stmnt* ParseExprStmnt()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::ExpressionStmnt);
		Expr* expr = ParseExpr();
		if (expr->typeID != ExprID::InvalidExpr)
		{
			if (Expect(UniqueType::Semicolon)) Advance();
			node->expressionStmnt.expression = expr;
			node->end = curr;

			if (IsAssignableExpr(expr) && IsAssignmentOperator())
			{
				node->nodeID = StmntID::AssignmentStmnt;
				node->assignmentStmnt.assignTo = expr;
				node->assignmentStmnt.op = curr;
				Advance();
				node->assignmentStmnt.assignment = ParseExpr();
				if (Expect(UniqueType::Semicolon)) Advance();
				node->end = curr;
			}

			return node;
		}

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseIf()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::IfStmnt);
		Advance();

		Stmnt* conditional = ParseConditional(node);
		if (conditional->nodeID != StmntID::InvalidStmnt)
		{
			node->ifStmnt.condition = conditional;
			node->ifStmnt.elifs = CreateVectorPtr<Stmnt*>();
			node->ifStmnt.elseCondition = Body();

			while (Expect(UniqueType::Else) && Peek()->uniqueType == UniqueType::If)
			{
				Advance();
				Advance();
				Stmnt* elif = ParseConditional(node);
				if (elif->nodeID != StmntID::InvalidStmnt) node->ifStmnt.elifs->push_back(elif);
				else
				{
					node->nodeID = StmntID::InvalidStmnt;
					return node;
				}
			}

			if (Expect(UniqueType::Else))
			{
				Advance();
				node->ifStmnt.elseCondition = ParseBody(node);
			}

			node->end = curr;
			return node;
		}


		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseConditional(Stmnt* of)
	{
		Stmnt* node = CreateStmnt(curr, StmntID::Conditional);
		if (Expect(UniqueType::Lparen, "Expected conditional opening ('(')"))
		{
			Advance();
			Expr* condition = ParseExpr();
			if (condition->typeID != ExprID::InvalidExpr &&
				Expect(UniqueType::Rparen, "Expected conditional closure (')')"))
			{
				Advance();
				node->conditional.condition = condition;
				node->conditional.body = ParseBody(of);
				node->end = node->conditional.body.body->end;
				return node;
			}
		}

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseFor()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::ForStmnt);
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
					node->nodeID = StmntID::InvalidStmnt;
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
						node->forStmnt.body = ParseBody(node);
						node->end = node->forStmnt.body.body->end;
						return node;
					}
				}
			}
		}

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseSwitch()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::SwitchStmnt);
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

						node->switchStmnt.cases = CreateVectorPtr<Stmnt*>();
						while (Expect(UniqueType::Case))
						{
							Advance();
							node->switchStmnt.cases->push_back(ParseConditional(node));
						}

						if (Expect(UniqueType::Default))
						{
							Advance();
							node->switchStmnt.defaultCase = ParseBody(node);
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

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseWhile()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::WhileStmnt);
		Advance();
		Stmnt* conditional = ParseConditional(node);
		if (conditional->nodeID != StmntID::InvalidStmnt)
		{
			node->whileStmnt.conditional = conditional;
			node->end = conditional->end;
			return node;
		}

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseDefer()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::DeferStmnt);
		Advance();

		if (Expect(UniqueType::If))
		{
			node->deferStmnt.deferIf = true;
			Advance();
			Stmnt* conditional = ParseConditional(node);
			if (conditional->nodeID != StmntID::InvalidStmnt)
			{
				node->deferStmnt.conditional = conditional;
				node->end = conditional->end;
				return node;
			}
		}
		else
		{
			node->deferStmnt.deferIf = false;
			node->deferStmnt.body = ParseBody(node);
			node->end = node->deferStmnt.body.body->end;
			return node;
		}

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseDelete()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::DeleteStmnt);
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

		node->nodeID = StmntID::InvalidStmnt;
		return node;
	}

	Stmnt* ParseContinue()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::ContinueStmnt);
		node->continueStmnt.token = curr;
		Advance();
		if (Expect(UniqueType::Semicolon)) Advance();
		node->end = curr;
		return node;
	}

	Stmnt* ParseBreak()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::BreakStmnt);
		node->breakStmnt.token = curr;
		Advance();
		if (Expect(UniqueType::Semicolon)) Advance();
		node->end = curr;
		return node;
	}

	Stmnt* ParseReturn()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::ReturnStmnt);
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

	eastl::vector<Stmnt*>* ParseParametersList(UniqueType end = UniqueType::Rparen)
	{
		eastl::vector<Stmnt*>* parameters = CreateVectorPtr<Stmnt*>();

		if (Expect(UniqueType::Rparen)) return parameters;

		bool parsingDefs = false;
		while (!Expect(end) && !IsEOF())
		{
			Stmnt* decl = ParseDeclOrDef();
			if (decl->nodeID != StmntID::InvalidStmnt)
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

	Stmnt* ParseAssignmentStatement()
	{
		Token* start = curr;
		switch (Peek()->uniqueType)
		{
		case UniqueType::ImplicitAssign:
		case UniqueType::Colon:
		{
			Stmnt* def = ParseDefinition();
			return def;
		}
		default:
			return InvalidStmnt();
		}
	}

	Stmnt* ParseDefinition()
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
			return InvalidStmnt();
		}
	}

	Stmnt* ParseImplicitAssignment()
	{
		Token* start = curr;
		if (Expect(TokenType::Identifier) &&
			ThenExpect(UniqueType::ImplicitAssign))
		{
			Stmnt* node = CreateStmnt(start, StmntID::Definition);
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

		return InvalidStmnt();
	}

	Stmnt* ParseExplicitAssignment()
	{
		Stmnt* node = ParseDeclaration();

		if (ParseAssignment(node)) return node;

		return InvalidStmnt();
	}

	bool ParseAssignment(Stmnt* decl)
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

	Stmnt* ParseDeclaration()
	{
		Token* start = curr;
		if (Expect(TokenType::Identifier, "Expected an identifier, possible compiler bug") &&
			ThenExpect(UniqueType::Colon, "Expected a colon (':') after identifier in explicit definition"))
		{
			Advance();
			Type* type = ParseDeclarationType();
			Stmnt* node = CreateStmnt(start, StmntID::Definition);
			node->definition.name = start;
			node->definition.type = type;
			node->definition.assignment = nullptr;

			return node;
		}

		return InvalidStmnt();
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
		eastl::vector<Stmnt*>* decls = CreateVectorPtr<Stmnt*>();
		type->explicitType.declarations = decls;
		while (!Expect(UniqueType::Rbrace) && !IsEOF())
		{
			Stmnt* node = ParseDeclaration();
			if (node->nodeID != StmntID::InvalidStmnt)
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
		Type* type = CreateTypePtr(TypeID::FunctionType);
		type->functionType.paramTypes = CreateVectorPtr<Type*>();
		Advance();

		type->functionType.returnType = Expect(UniqueType::Lparen)
			? CreateVoidType()
			: ParseDeclarationType();

		if (Expect(UniqueType::Lparen, "Expected function type opening ('(')"))
		{
			Advance();
			while (!Expect(UniqueType::Rparen) && !IsEOF())
			{
				Type* declType = ParseDeclarationType();
				type->functionType.paramTypes->push_back(declType);
				if (Expect(UniqueType::Comma)) Advance();
			}

			if (Expect(UniqueType::Rparen, "Expected function type closure (')')"))
			{
				Advance();
				return type;
			}
		}

		type->typeID = TypeID::InvalidType;
		return type;
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
		Stmnt* node = ParseCompile();
		if (node->nodeID != StmntID::InvalidStmnt)
		{
			expr->compileExpr.compile = node;
			return expr;
		}

		expr->typeID = ExprID::InvalidExpr;
		return expr;
	}

	Expr* ParseAnonymousType()
	{
		Token* rBrace = curr;
		Advance();

		eastl::vector<Stmnt*> params = eastl::vector<Stmnt*>();
		while (!Expect(UniqueType::Lbrace) && !IsEOF())
		{
			Stmnt* def = ParseDefinition();
			if (def->nodeID == StmntID::Definition) params.push_back(def);
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
				Stmnt* node = CreateStmnt(start, StmntID::AnonFunction);
				node->anonFunction.returnType = returnType;
				node->anonFunction.decl = ParseFunctionDecl(node);
				expr->functionTypeDeclExpr.anonFunction = node;
				return expr;
			}
			else
			{
				curr = start;
				expr->typeID = ExprID::TypeExpr;
				expr->typeExpr.type = ParseFunctionType();
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

	Expr* ParseTypeOrPrimaryExpr()
	{
		Token* start = curr;
		Logger::SetErrorRollback();
		Type* type = ParseDeclarationType(false);
		if (type->typeID != TypeID::InvalidType)
		{
			Expr* expr = CreateExpr(start, ExprID::TypeExpr);
			expr->typeExpr.type = type;
			return expr;
		}

		curr = start;
		Logger::ErrorRollback();
		return ParsePrimaryExpr();
	}

	Expr* ParseGenericsExpr(Expr* expr = nullptr)
	{
		Expr* generics = CreateExpr(curr, ExprID::GenericsExpr);
		eastl::vector<Expr*>* genericTemplates = CreateVectorPtr<Expr*>();
		generics->genericsExpr.expr = expr;
		generics->genericsExpr.open = curr;
		generics->genericsExpr.templateArgs = genericTemplates;

		Advance();

		if (Expect(UniqueType::Greater))
		{
			Advance();
			AddError(curr, "Expected generic types not '<>'");
			return generics;
		}

		while (!Expect(UniqueType::Greater) && !IsEOF())
		{
			Expr* expr = ParseTypeOrPrimaryExpr();
			if (expr->typeID != ExprID::InvalidExpr)
			{
				genericTemplates->push_back(expr);
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
};
