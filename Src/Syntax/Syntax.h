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
	Tokens& tokens;

	Scope currScope;
	Token* curr;
	SymbolTable* symbolTable;
	size_t nodeCount;
	Token* package;

	eastl::stack<Scope> scopes;

	Syntax(Tokens& tokensRef) : tokens(tokensRef)
	{
		curr = nullptr;
		scopes = eastl::stack<Scope>();
		currScope = Scope();
		scopes.push(currScope);
		nodeCount = 0;
		package = nullptr;
	}

	inline Stmnt* CreateStmnt(Token* start, StmntID nodeID)
	{
		nodeCount += 1;
		return symbolTable->CreateStmnt(start, nodeID, package, currScope.scopeOf);
	}

	inline Stmnt* InvalidStmnt()
	{
		return symbolTable->InvalidStmnt();
	}

	template<typename T>
	inline eastl::vector<T*>* CreateVectorPtr()
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

		while (!IsEOF())
		{
			ParseNext();
		}

		symbolTable->Finalize();
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
		if (!expected && errMsg != "")
		{
			AddError(curr, errMsg);
		}
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
			curr = tokens.Next(curr);
		}
	}

	void ParsePackage(bool setInTree = true)
	{
		Token* start = curr;
		if (Expect(UniqueType::Package, "File must start with a 'package' statement") &&
			ThenExpect(TokenType::Identifier, "Expected an identifier after package token"))
		{
			Token* currPackage = curr;
			Advance();
			if (Expect(UniqueType::Semicolon)) Advance();
			if (setInTree) {
				package = currPackage;
				symbolTable->package = currPackage;
			}
		}
	}

	void ParseNext()
	{
		switch (curr->uniqueType)
		{
		case UniqueType::UniqueUnknown:
			if (Expect(TokenType::Comment)) ParseComments();
			return;
		case UniqueType::Package:
			AddError(curr, "File cannot have multiple 'package' statements");
			ParsePackage(false);
			return;
		case UniqueType::Import:
			ParseImport();
			return;
		case UniqueType::Extern:
			ParseExternBlock();
			return;
		case UniqueType::State:
			ParseState();
			return;
		case UniqueType::Enum:
			ParseEnum();
			return;
		case UniqueType::OnCompile:
		{
			Stmnt* node = ParseCompile();
			if (node->nodeID != StmntID::InvalidStmnt)
			{
				symbolTable->AddOnCompile(node);
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
				symbolTable->AddGlobalVal(assignment);
				return;
			}
		}
		default:
		{
			Token* start = curr;
			Logger::SetErrorRollback();
			Type* type = ParseType(false);
			if (type->typeID == TypeID::InvalidType ||
				Expect(UniqueType::DoubleColon) ||
				Expect(UniqueType::Lparen))
			{
				Logger::ErrorRollback();
				type = CreateVoidType();
				curr = start;
			}

			ParseFunction(type, start);
			return;
		}
		}

		AddError(curr, "Syntax:ParseNext Unexpected token : " + curr->ToString());
		Advance();
	}

	void ParseImport()
	{
		bool addNode = IsGlobalScope();
		if (!addNode) AddError(curr, "Cannot import packages outside of the global scope");

		Token* start = curr;
		if (Expect(UniqueType::Import) &&
			ThenExpect(TokenType::Identifier, "Expected an identifier after 'import'"))
		{
			Stmnt* node = CreateStmnt(start, StmntID::ImportStmnt);
			node->importStmnt.packageName = curr;
			Advance();

			if (Expect(UniqueType::As)
				&& ThenExpect(TokenType::Identifier, "Expected an alias identifier after 'as' in using statement"))
			{
				node->importStmnt.alias = curr;
				Advance();
			}
			else
			{
				node->importStmnt.alias = nullptr;
			}

			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			if (addNode) symbolTable->AddImport(node);
		}
	}

	void ParseExternBlock()
	{
		Token* externTok = curr;
		eastl::vector<Stmnt*>* links = CreateVectorPtr<Stmnt>();
		eastl::vector<Stmnt*> decls;

		Advance();
		if (Expect(UniqueType::Lbrace, "Expected extern block start '{'"))
		{
			Advance();
			while (!Expect(UniqueType::Rbrace) && !IsEOF())
			{
				if (Expect(UniqueType::Link))
				{
					Stmnt* link = ParseLink();
					if (link) links->push_back(link);
				}
				else
				{
					Stmnt* decl = ParseExternDecl();
					if (decl) decls.push_back(decl);
				}
			}

			if (Expect(UniqueType::Rbrace, "Expected extern block closure '}'"))
			{
				Advance();
				for (Stmnt* decl : decls)
				{
					decl->externFunction.links = links;
				}
			}
		}
	}

	Stmnt* ParseLink()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::LinkDecl);
		Advance();

		if (Expect(TokenType::Identifier, "Expected link target platform identifier"))
		{
			node->linkDecl.platform = curr;
			Advance();
			if (Expect(UniqueType::StringLiteral, "Expected link library  string"))
			{
				node->linkDecl.lib = curr;
				Advance();

				if (Expect(UniqueType::Semicolon)) Advance();
				return node;
			}
		}

		return nullptr;
	}

	Stmnt* ParseExternDecl()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::ExternFunctionDecl);

		if (curr->uniqueType == UniqueType::StringLiteral || Peek()->uniqueType == UniqueType::Rparen)
			node->externFunction.returnType = CreateVoidType();
		else node->externFunction.returnType = ParseType(false);

		if (curr->type == TokenType::Identifier || curr->uniqueType == UniqueType::StringLiteral)
		{
			node->externFunction.externName = curr;
			Advance();
		}
		else
		{
			AddError(curr, "Expected identifier or string external function name");
			return nullptr;
		}

		if (Expect(UniqueType::Lparen, "Expected extern function parameter opening '('"))
		{
			Advance();
			node->externFunction.parameters = ParseParametersList();
			if (Expect(UniqueType::Rparen, "Expected extern function parameter closure ')'")) Advance();
			if (Expect(UniqueType::As))
			{
				Advance();
				if (Expect(TokenType::Identifier, "Expected alias identifier after 'as'"))
				{
					node->externFunction.callName = curr;
					Advance();
				}
			}
			else node->externFunction.callName = node->externFunction.externName;

			if (node->externFunction.callName->type != TokenType::Identifier)
				AddError(node->externFunction.callName, "Extern function name must be an identifier if no alias is provided");

			if (Expect(UniqueType::Semicolon)) Advance();
			symbolTable->AddExternFunc(node);
			return node;
		}
	}

	void ParseState()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::StateStmnt);
		Advance();
		if (Expect(TokenType::Identifier, "Expected an identifier after 'state'"))
		{
			node->state.name = curr;
			node->state.members = CreateVectorPtr<Stmnt>();
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
		if (Peek()->uniqueType == UniqueType::ImplicitAssign)
		{
			return ParseImplicitAssignment();
		}

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

	void ParseEnum()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::EnumStmnt);
		Advance();
		if (Expect(TokenType::Identifier, "Expected an identifier after 'enum'"))
		{
			node->enumStmnt.name = curr;
			node->enumStmnt.names = symbolTable->CreateVectorPtr<Token>();
			node->enumStmnt.valueExprs = symbolTable->CreateVectorPtr<Expr>();
			node->enumStmnt.values = symbolTable->CreateVector<intmax_t>();
			Advance();

			if (Expect(UniqueType::Colon))
			{
				Advance();
				node->enumStmnt.type = ParseType(false);
			}
			else
			{
				node->enumStmnt.type = CreatePrimitive(UniqueType::Int);
			}

			if (Expect(UniqueType::Lbrace, "Expected 'enum' statement opening ('{')"))
			{
				Advance();
				if (Expect(UniqueType::Rbrace))
				{
					AddError(curr, "Empty 'enum' is not allowed ('{}')");
					Advance();
					return;
				}

				while (!Expect(UniqueType::Rbrace) && !IsEOF())
				{
					if (Expect(TokenType::Identifier, "Expected enum member identifier"))
					{
						node->enumStmnt.names->push_back(curr);
						Advance();
						if (Expect(UniqueType::Assign))
						{
							Advance();
							node->enumStmnt.valueExprs->push_back(ParseExpr());
						}
						else
						{
							node->enumStmnt.valueExprs->push_back(nullptr);
						}

						if (Expect(UniqueType::Comma)) Advance();
					}
				}

				if (Expect(UniqueType::Rbrace, "Expected 'enum' statement closure ('}')"))
				{
					node->end = curr;
					Advance();
					symbolTable->AddEnum(node);
				}
			}
		}
	}

	Stmnt* ParseCompile()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::CompileStmnt);
		Advance();
		Type* type = ParseType();
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
				if (Expect(UniqueType::Lparen, "Syntax:ParseFunction Expected function starting with ('(')"))
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
		param->definition.type = CreateTypePtr(TypeID::ImportedType);
		param->definition.type->importedType.packageName = package;
		param->definition.type->importedType.typeName = stateName;
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
			if (Expect(UniqueType::Lparen, "Syntax:ParseStateFunction Expected function starting with ('(')"))
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
			if (Expect(UniqueType::DoubleColon, "Missing '::' after 'operator' and before the operator to overload"))
			{
				Advance();
				if (curr->type == TokenType::Operator)
				{
					op->stateOperator.op = curr;
					Advance();
					if (Expect(UniqueType::Lparen, "Syntax:ParseStateFunction Expected function starting with ('(') for operator"))
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
			del->destructor.decl = CreateStmnt(start, StmntID::FunctionDecl);
			del->destructor.decl->functionDecl.parameters = CreateVectorPtr<Stmnt>();
			AddUniformCallParam(del->destructor.decl->functionDecl.parameters, del->destructor.stateName);
			Advance();
			if (Expect(UniqueType::FatArrow)) Advance();
			del->destructor.decl->functionDecl.body = ParseBody(del);
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
		node->generics.names = CreateVectorPtr<Token>();
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
			node->generics.templatesToExpand = symbolTable->arena->Emplace<eastl::hash_set<eastl::vector<Expr*>*, ExprArrHash, ExprArrEqual>>();
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
		block->block.inner = CreateVectorPtr<Stmnt>();
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
			if (Expect(UniqueType::Semicolon)) Advance();
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
		case UniqueType::LogTok:
			return ParseLogStmnt();
		case UniqueType::AssertTok:
			return ParseAssertStmnt();

		default:
			return ParseExprStmnt();
		}
	}

	Stmnt* ParseLogStmnt()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::LogStmnt);
		Advance();
		node->logStmnt.exprs = ParseExprList(UniqueType::Semicolon, false);
		if (Expect(UniqueType::Semicolon)) Advance();
		return node;
	}

	Stmnt* ParseAssertStmnt()
	{
		Stmnt* node = CreateStmnt(curr, StmntID::AssertStmnt);
		Advance();

		Expr* expr = ParseExpr();

		if (expr->typeID != ExprID::InvalidExpr)
		{

			node->assertStmnt.expr = expr;
			if (Expect(UniqueType::Comma))
			{
				Advance();
				Expr* messageExpr = ParseExpr();
				if (expr->typeID == ExprID::InvalidExpr)
					AddError(node->start, "Syntax:ParseAssertStmnt Invalid expression for assert message");
				node->assertStmnt.message = messageExpr;
			}
			else
			{
				node->assertStmnt.message = nullptr;
			}
			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			return node;
		}

		AddError(node->start, "Syntax:ParseAssertStmnt Invalid expression");
		node->nodeID = StmntID::InvalidStmnt;
		return node;
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

			if (IsAssignmentOperator(curr->uniqueType))
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

		AddError(node->start, "Syntax:ParseExprStmnt Invalid expression");
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
			node->ifStmnt.elifs = CreateVectorPtr<Stmnt>();
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

						node->switchStmnt.cases = CreateVectorPtr<Stmnt>();
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
		if (Expect(UniqueType::Semicolon) || Expect(UniqueType::Void))
		{
			node->returnStmnt.expr = CreateExpr(curr, ExprID::TypeExpr);
			node->returnStmnt.expr->typeExpr.type = CreateVoidType();
			node->end = curr;
			Advance();
			if (Expect(UniqueType::Semicolon)) Advance();
			return node;
		}
		else
		{
			node->returnStmnt.expr = ParseExpr();
			if (Expect(UniqueType::Semicolon)) Advance();
			node->end = curr;
			return node;
		}
	}

	eastl::vector<Stmnt*>* ParseParametersList(UniqueType end = UniqueType::Rparen, bool mustAssignAfterFirst = true)
	{
		eastl::vector<Stmnt*>* parameters = CreateVectorPtr<Stmnt>();

		if (Expect(end)) return parameters;

		bool parsingDefs = false;
		while (!Expect(end) && !IsEOF())
		{
			Stmnt* decl = ParseDeclOrDef();
			if (decl->nodeID != StmntID::InvalidStmnt)
			{
				if (mustAssignAfterFirst && parsingDefs && !decl->definition.assignment)
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
		Expect(TokenType::Identifier, "Expected an identifier");
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
		if (Expect(TokenType::Identifier, "Expected an identifier") &&
			ThenExpect(UniqueType::Colon, "Expected a colon (':') after identifier in explicit definition"))
		{
			Advance();
			Type* type = ParseType();
			Stmnt* node = CreateStmnt(start, StmntID::Definition);
			node->definition.name = start;
			node->definition.type = type;
			node->definition.assignment = nullptr;

			return node;
		}

		return InvalidStmnt();
	}

	Type* ParseType(bool allowImplicitType = true)
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

			case UniqueType::Lbrack:
				type = ParseSizedArrayType();
				break;

			case UniqueType::DoubleColon:
				type = ParseFunctionType();
				break;

			case UniqueType::QuestionMark:
				type = ParseUnionType();
				break;

			case UniqueType::Any:
				type = ParseAnyType();
				break;

			default:
				AddError(curr, "Syntax:ParseType Expected type declaration");
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
		Advance();
		ptrType->pointerType.type = ParseType();
		return ptrType;
	}

	Type* ParseValueType()
	{
		Type* valueType = CreateTypePtr(TypeID::ValueType);
		Advance();
		Type* type = ParseType();
		if (type->typeID == TypeID::ValueType)
			AddError(curr, "Cannot have a value type of a value type (ie. ~~MyType)");
		valueType->valueType.type = type;
		return valueType;
	}

	Type* ParseArrayType()
	{
		Type* arrType = CreateTypePtr(TypeID::ArrayType);
		Advance();
		arrType->arrayType.type = ParseType();
		arrType->arrayType.size = nullptr;
		return arrType;
	}

	Type* ParseSizedArrayType()
	{
		Type* arrType = CreateTypePtr(TypeID::ArrayType);
		Advance();
		arrType->arrayType.size = ParseExpr();
		if (Expect(UniqueType::Rbrack, "Expected closing bracket for sized array"))
		{
			Advance();
			arrType->arrayType.type = ParseType();
			return arrType;
		}

		arrType->typeID = TypeID::InvalidType;
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
		eastl::vector<Token*>* idents = CreateVectorPtr<Token>();
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
		eastl::vector<Stmnt*>* decls = CreateVectorPtr<Stmnt>();
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
			Advance();
			return type;
		}

		type->typeID = TypeID::InvalidType;
		return type;
	}

	Type* ParseGenericsType(Type* type)
	{
		Type* templatedType = CreateTypePtr(TypeID::TemplatedType);
		templatedType->templatedType.templates = ParseGenericsExpr();
		templatedType->templatedType.type = type;

		return templatedType;
	}

	Type* ParseFunctionType()
	{
		Type* type = CreateTypePtr(TypeID::FunctionType);
		type->functionType.paramTypes = CreateVectorPtr<Type>();
		Advance();

		type->functionType.returnType = Expect(UniqueType::Lparen)
			? CreateVoidType()
			: ParseType();

		if (Expect(UniqueType::Lparen, "Expected function type opening ('(')"))
		{
			Advance();
			while (!Expect(UniqueType::Rparen) && !IsEOF())
			{
				Type* declType = ParseType();
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

	Type* ParseUnionType()
	{
		Advance();

		if (Expect(UniqueType::Lbrace, "Syntax:ParseUnionType Expected inline type declaration opening for union type ('{')"));
		{
			Advance();
			Type* type = ParseExplicitType();
			if (type->typeID == TypeID::InvalidType) return type;
			
			type->typeID = TypeID::UnionType;
			return type;
		}

		return CreateTypePtr(TypeID::InvalidType);
	}

	Type* ParseAnyType()
	{
		Type* type = CreateTypePtr(TypeID::AnyType);
		Advance();
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
		if (IsUnaryOperator(curr->uniqueType))
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

			case UniqueType::Array:
			case UniqueType::Lbrack:
				expr = ParseIndex(expr);
				break;

			case UniqueType::Lparen:
				expr = ParseFunctionCall(expr);
				break;

			case UniqueType::Less:
			{
				if (TestGenericsExpr())
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
			return ParseTypeLiteralExpr();

		case UniqueType::DoubleColon:
			return ParseFunctionTypeExpr();

		case UniqueType::New:
			return ParseNewExpr();

		case UniqueType::Array:
		case UniqueType::Lbrack:
			return ParseArrayExpr();

		case UniqueType::OnCompile:
			return ParseCompileExpr();

		case UniqueType::SizeOfTok:
			return ParseSizeOfExpr();

		case UniqueType::AlignOfTok:
			return ParseAlignOfExpr();

		default:
			switch (curr->type)
			{
			case TokenType::Literal:
				return ParseLiteralExpr();

			case TokenType::Primitive:
				return ParsePrimitiveExpr();

			default:
				AddError(curr, "Syntax:ParseOperand No operand found for expression");
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
		primLit->literalExpr.val = curr;
		Advance();
		return primLit;
	}

	Expr* ParseSizeOfExpr()
	{
		Expr* sizeOfExpr = CreateExpr(curr, ExprID::SizeOfExpr);
		Advance();
		Expr* expr = ParseTypeOrPrimaryExpr();
		if (expr->typeID == ExprID::InvalidExpr) AddError(sizeOfExpr->start, "Syntax:ParseSizeOfExpr #sizeof must be followed by a type or primary expression");
		sizeOfExpr->sizeOfExpr.expr = expr;
		if (Expect(UniqueType::Semicolon)) Advance();
		return sizeOfExpr;
	}

	Expr* ParseAlignOfExpr()
	{
		Expr* alignOfExpr = CreateExpr(curr, ExprID::AlignOfExpr);
		Advance();
		Expr* expr = ParseTypeOrPrimaryExpr();
		if (expr->typeID == ExprID::InvalidExpr) AddError(alignOfExpr->start, "Syntax:ParseAlignOfExpr #alignof must be followed by a type or primary expression");
		alignOfExpr->alignOfExpr.expr = expr;
		if (Expect(UniqueType::Semicolon)) Advance();
		return alignOfExpr;
	}

	Expr* ParseGroupedExpr()
	{
		Token* lParen = curr;
		Expr* groupExpr = CreateExpr(curr, ExprID::GroupedExpr);
		Advance();
		Expr* innerExpr = ParseExpr();
		groupExpr->groupedExpr.expr = innerExpr;
		if (Expect(UniqueType::Rparen, "Expected ')' after group expression"))
		{
			Advance();
		}
		return groupExpr;
	}

	Expr* ParseFixedExpr()
	{
		Expr* fixed = CreateExpr(curr, ExprID::FixedExpr);
		Advance();
		fixed->fixedExpr.atExpr = ParsePrimaryExpr();
		return fixed;
	}

	Expr* ParseTypeLiteralExpr(UniqueType closure = UniqueType::Rbrace, bool array = false)
	{
		Expr* anon = CreateExpr(curr, ExprID::TypeLiteralExpr);
		Advance();

		if (Expect(closure))
		{
			AddError(curr, "Empty type literals not allowed ('{}')");
			Advance();
			return anon;
		}

		if (!array)
		{
			// Check if explicit type literal
			Token* next = Peek();
			if (next->uniqueType == UniqueType::Colon || next->uniqueType == UniqueType::ImplicitAssign)
			{
				anon->typeID = ExprID::ExplicitTypeExpr;
				anon->explicitTypeExpr.values = ParseParametersList(closure, false);
				if (Expect(closure, "Expected explicit type closure ('}')")) Advance();
				return anon;
			}
		}

		anon->typeLiteralExpr.values = ParseExprList(closure);
		anon->typeLiteralExpr.array = array;
		if (Expect(closure, "Missing closure for type literal (']' or '}')")) Advance();

		return anon;
	}

	Expr* ParseFunctionTypeExpr()
	{
		Token* start = curr;
		Expr* expr = CreateExpr(start, ExprID::FunctionTypeDeclExpr);
		Advance();
		Type* returnType = Expect(UniqueType::Lparen)
			? CreateVoidType()
			: ParseType();

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

	Expr* ParseArrayExpr()
	{
		if (Expect(UniqueType::Array))
		{
			return ParseTypeExpr();
		}

		Token* start = curr;
		Advance();
		Expr* index = ParseExpr();

		if (Expect(UniqueType::Rbrack))
		{
			curr = start;
			return ParseTypeExpr();
		}
		else if (Expect(UniqueType::Comma))
		{
			curr = start;
			return ParseTypeLiteralExpr(UniqueType::Rbrack, true);
		}
		else
		{
			AddError(start, "Syntax:ParseArrayExpr Expected array type declaration or array literal");
			index->typeID = ExprID::InvalidExpr;
			return index;
		}
	}

	Expr* ParseIndex(Expr* of)
	{
		Token* currToken = curr;
		Expr* indexExpr = CreateExpr(currToken, ExprID::IndexExpr);

		Advance();
		if (curr->uniqueType == UniqueType::Rbrack)
		{
			AddError(curr, "Unexpected empty index ('[]') in expression");
			indexExpr->typeID = ExprID::InvalidExpr;
			return indexExpr;
		}
		else
		{
			Expr* expr = ParseExpr();
			indexExpr->indexExpr.index = expr;
			if (Expect(UniqueType::Rbrack, "Expected ']' after index expression"))
			{
				Advance();
			}
			else
			{
				indexExpr->typeID = ExprID::InvalidExpr;
				return indexExpr;
			}
		}

		indexExpr->indexExpr.of = of;
		return indexExpr;
	}

	Expr* ParseFunctionCall(Expr* of)
	{
		Expr* funcCall = CreateExpr(of->start, ExprID::FunctionCallExpr);
		Token* lParen = curr;
		funcCall->functionCallExpr.callKind = FunctionCallKind::UnknownCall;
		funcCall->functionCallExpr.function = of;
		funcCall->functionCallExpr.functionStmnt = nullptr;
		Advance();
		funcCall->functionCallExpr.params = ParseExprList();

		if (Expect(UniqueType::Rparen, "Missing ')' at end of function call"))
		{
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
			if (GetOperatorPrecedence() != 0)
			{
				curr = start;
				return false;
			}
			Advance();
		}

		bool isGenerics = Expect(UniqueType::Greater);
		curr = start;
		return isGenerics;
	}

	Expr* ParseTypeExpr()
	{
		Expr* expr = CreateExpr(curr, ExprID::TypeExpr);
		Type* type = ParseType(false);
		if (type->typeID != TypeID::InvalidType)
		{
			expr->typeExpr.type = type;
			return expr;
		}

		expr->typeID = ExprID::InvalidExpr;
		return expr;
	}

	Expr* ParseTypeOrPrimaryExpr()
	{
		Token* start = curr;
		Logger::SetErrorRollback();

		Expr* expr = ParseTypeExpr();
		if (expr->typeID != ExprID::InvalidExpr)
			return expr;

		curr = start;
		Logger::ErrorRollback();
		return ParsePrimaryExpr();
	}

	Expr* ParseGenericsExpr(Expr* expr = nullptr)
	{
		Expr* generics = CreateExpr(curr, ExprID::TemplateExpr);
		eastl::vector<Expr*>* genericTemplates = CreateVectorPtr<Expr>();
		generics->templateExpr.expr = expr;
		generics->templateExpr.templateArgs = genericTemplates;

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
		asExpr->asExpr.to = ParseType();

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

	eastl::vector<Expr*>* ParseExprList(UniqueType end = UniqueType::Rparen, bool endRequired = true)
	{
		eastl::vector<Expr*>* exprs = CreateVectorPtr<Expr>();

		while (!Expect(end) && !IsEOF())
		{
			exprs->push_back(ParseExpr());
			if (Expect(UniqueType::Comma)) Advance();
			else if (!endRequired) break;
		}

		return exprs;
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
