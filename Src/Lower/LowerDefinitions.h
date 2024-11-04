#include "EASTL/deque.h"
#include "../Syntax/GlobalTable.h"
#include "../Syntax/ScopeUtils.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerContext.h"
#include "../Syntax/TypeInference.h"

extern Config config;

const size_t InvalidRegister = (size_t)-1;

struct ScopeValue
{
	size_t reg = 0;
	SpiteIR::Type* type = nullptr;
};

struct FunctionScope
{
	eastl::hash_map<StringView, ScopeValue, StringViewHash> scopeMap;
	eastl::vector<Expr*> deferred;
	eastl::vector<size_t> toDestroy;
};

struct FunctionContext
{
	SpiteIR::Function* function;
	eastl::deque<FunctionScope> scopeQueue;
	ScopeUtils scopeUtils = ScopeUtils(nullptr, nullptr);
	size_t curr = 0;
	size_t forCount = 0;
	size_t ifCount = 0;
	size_t blockCount = 0;

	void IncrementRegister(SpiteIR::Type* type)
	{
		curr += type->size;
	}

	void Reset(SpiteIR::Function* function, SymbolTable* symbolTable, GlobalTable* globalTable)
	{
		this->function = function;
		scopeQueue.clear();
		scopeUtils.scopeQueue.clear();
		scopeUtils.globalTable = globalTable;
		scopeUtils.symbolTable = symbolTable;
		curr = 0;
		forCount = 0;
		ifCount = 0;
		blockCount = 0;
	}
};

const ScopeValue InvalidScopeValue = { InvalidRegister, nullptr };

struct LowerDefinitions
{
	LowerContext& context;
	FunctionContext funcContext;
	SymbolTable* symbolTable = nullptr;

	Stmnt* currentStmnt = nullptr;
	eastl::vector<Expr*>* currTemplates = nullptr;
	eastl::vector<Token*>* currGenerics = nullptr;
	SpiteIR::Package* currPackage = nullptr;

	SpiteIR::State* arrayState = nullptr;
	SpiteIR::Function* makeArray = nullptr;

	LowerDefinitions(LowerContext& context) : context(context)
	{
		AssignArrayDeclarations(context.ir);
		Assert(arrayState && makeArray);
	}

	void AssignArrayDeclarations(SpiteIR::IR* ir)
	{
		SpiteIR::Package* runtime = ir->runtime;
		arrayState = FindPackageState(runtime, "__array");
		makeArray = FindPackageFunction(runtime, "__make_array");
	}

	SpiteIR::State* FindPackageState(SpiteIR::Package* package, const eastl::string& name)
	{
		if (MapHas(package->states, name))
		{
			return package->states[name];
		}

		return nullptr;
	}

	SpiteIR::Function* FindPackageFunction(SpiteIR::Package* package, const eastl::string& name)
	{
		if (MapHas(package->functions, name))
		{
			return package->functions[name];
		}

		return nullptr;
	}

	void SetCurrentGenerics(Stmnt* stmnt)
	{
		Stmnt* generics = GetGenerics(stmnt);
		if (generics) currGenerics = generics->generics.names;
		else currGenerics = nullptr;
	}

	Expr* ExpandTemplate(Expr* expr)
	{
		Token* exprToken = GetTokenForTemplate(expr);
		if (exprToken)
		{
			for (int i = 0; i < currGenerics->size(); i++)
			{
				Token* token = currGenerics->at(i);
				if (token->val == exprToken->val)
				{
					return currTemplates->at(i);
				}
			}
		}

		return expr;
	}

	eastl::vector<Expr*> ExpandTemplates(eastl::vector<Expr*>* exprs)
	{
		eastl::vector<Expr*> expanded;
		for (Expr* expr : *exprs)
		{
			expanded.push_back(ExpandTemplate(expr));
		}

		return expanded;
	}

	void BuildDefinitions()
	{
		for (SpiteIR::Package* package : context.ir->packages)
		{
			BuildPackageDefinitions(package);
		}
	}

	void BuildPackageDefinitions(SpiteIR::Package* package)
	{
		currPackage = package;
		symbolTable = context.packageToSymbolTableMap[package];
		for (auto& [key, state] : package->states)
		{
			ASTContainer& stateContainer = context.stateASTMap[state];
			currentStmnt = stateContainer.node;
			currTemplates = stateContainer.templates;
			SetCurrentGenerics(stateContainer.node);
			BuildState(state, stateContainer.node);
		}

		for (auto& [key, function] : package->functions)
		{
			ASTContainer& funcContainer = context.functionASTMap[function];
			currentStmnt = funcContainer.node;
			currTemplates = funcContainer.templates;
			SetCurrentGenerics(funcContainer.node);
			BuildFunction(function, funcContainer.node);
		}
	}

	void AddScope()
	{
		funcContext.scopeQueue.emplace_back();
		funcContext.scopeUtils.AddScope();
	}

	void PopScope()
	{
		FunctionScope scope = funcContext.scopeQueue.back();
		funcContext.scopeQueue.pop_back();
		funcContext.scopeUtils.PopScope();
	}

	void AddValueToCurrentScope(const StringView& name, const ScopeValue& value, Stmnt* stmnt)
	{
		funcContext.scopeQueue.back().scopeMap[name] = value;
		funcContext.scopeUtils.AddToTopScope(name, stmnt);
	}

	ScopeValue FindScopeValue(StringView& name)
	{
		for (auto it = funcContext.scopeQueue.rbegin(); it != funcContext.scopeQueue.rend(); it++)
		{
			if (MapHas(it->scopeMap, name))
			{
				return it->scopeMap[name];
			}
		}

		return InvalidScopeValue;
	}

	SpiteIR::Label* GetCurrentLabel()
	{
		return funcContext.function->block->labels.back();
	}

	void BuildState(SpiteIR::State* state, Stmnt* stateStmnt)
	{

	}

	SpiteIR::Type* ToIRType(Type* type)
	{
		SpiteIR::Type* irType = TypeToIRType(context.ir, type, this, currGenerics, currTemplates);
		Assert(irType);
		return irType;
	}

	ScopeValue HandleAutoCast(const ScopeValue& from, SpiteIR::Type* to)
	{
		if (!from.type || !to) return InvalidScopeValue;

		SpiteIR::Type* fromType = from.type;
		if (to->kind == SpiteIR::TypeKind::ReferenceType &&
			fromType->kind != SpiteIR::TypeKind::ReferenceType &&
			fromType->kind != SpiteIR::TypeKind::PointerType)
		{
			return HandleAutoCast(BuildTypeReference(GetCurrentLabel(), from), to);
		}

		if (from.type->kind == SpiteIR::TypeKind::ReferenceType &&
			to->kind != SpiteIR::TypeKind::ReferenceType &&
			to->kind != SpiteIR::TypeKind::PointerType)
		{
			return HandleAutoCast(BuildTypeDereference(GetCurrentLabel(), from), to);
		}

		return from;
	}

	void BuildFunction(SpiteIR::Function* function, Stmnt* funcStmnt)
	{
		Assert(function);

		function->block = context.ir->AllocateBlock();
		funcContext.Reset(function, symbolTable, context.globalTable);

		if (funcStmnt->nodeID == StmntID::StateStmnt)
		{
			//Default state constructor
			SpiteIR::Type* argRefType = function->arguments.front()->value->type;
			SpiteIR::Type* stateType = argRefType->reference.type;
			SpiteIR::Label* label = BuildLabel("entry");
			SpiteIR::Instruction* argAlloc = BuildAllocate(argRefType);
			SpiteIR::Instruction* defaultAlloc = BuildAllocate(stateType);
			ScopeValue defaultVal = BuildDefaultValue(stateType, defaultAlloc->allocate.result, label);
			BuildStorePtr(label, AllocateToOperand(argAlloc), BuildRegisterOperand(defaultVal));
			BuildVoidReturn(label);
			return;
		}

		Stmnt* decl = GetDeclForFunc(funcStmnt);

		if (!function->metadata.externFunc)
		{
			AddScope();
			BuildFunctionArguments(function, funcStmnt);
			BuildEntryLabel(function, funcStmnt, decl);
			SpiteIR::Label* lastLabel = GetCurrentLabel();
			if (!lastLabel->terminator && IsVoidType(function->returnType))
			{
				BuildVoidReturn(lastLabel);
			}
			PopScope();
		}
	}

	void BuildFunctionArguments(SpiteIR::Function* function, Stmnt* funcStmnt)
	{
		Stmnt* decl = GetDeclForFunc(funcStmnt);
		Assert(decl->functionDecl.parameters->size() == function->arguments.size());

		for (size_t i = 0; i < function->arguments.size(); i++)
		{
			SpiteIR::Argument* arg = function->arguments.at(i);
			Stmnt* param = decl->functionDecl.parameters->at(i);

			SpiteIR::Instruction* alloc = BuildAllocate(arg->value->type);
			StringView name = StringView(arg->value->name.c_str());
			AddValueToCurrentScope(name, { alloc->allocate.result, alloc->allocate.type }, param);
		}
	}

	void BuildEntryLabel(SpiteIR::Function* function, Stmnt* funcStmnt, Stmnt* funcDecl)
	{
		auto& decl = funcDecl->functionDecl;
		Body& body = decl.body;
		BuildLabelBody("entry", body);
	}

	SpiteIR::Label* BuildLabel(const eastl::string& name)
	{
		SpiteIR::Label* label = context.ir->AllocateLabel();
		label->name = name;
		funcContext.function->block->labels.push_back(label);

		return label;
	}

	SpiteIR::Label* BuildLabelBody(const eastl::string& name, Body& body)
	{
		SpiteIR::Label* label = BuildLabel(name);
		BuildBody(body);

		return label;
	}

	void BuildBody(Body& body)
	{
		AddScope();
		if (body.body->nodeID == StmntID::Block)
		{
			for (Stmnt* stmnt : *body.body->block.inner)
			{
				BuildStmntForBlock(stmnt);
			}
		}
		else
		{
			BuildStmntForBlock(body.body);
		}
		PopScope();
	}

	void BuildStmntForBlock(Stmnt* stmnt)
	{
		switch (stmnt->nodeID)
		{
		case ExpressionStmnt:
			BuildExpr(stmnt->expressionStmnt.expression, stmnt);
			break;
		case Definition:
			BuildVarDefinition(stmnt);
			break;
		case InlineDefinition:
			BuildInlineDefinition(stmnt);
			break;
		case FunctionStmnt:
			break;
		case AnonFunction:
			break;
		case Conditional:
			break;
		case AssignmentStmnt:
			BuildAssignment(stmnt);
			break;
		case IfStmnt:
			BuildIfStmnt(stmnt);
			break;
		case ForStmnt:
			BuildForStmnt(stmnt);
			break;
		case WhileStmnt:
			break;
		case SwitchStmnt:
			break;
		case DeleteStmnt:
			break;
		case DeferStmnt:
			break;
		case ContinueStmnt:
			break;
		case BreakStmnt:
			break;
		case ReturnStmnt:
			BuildReturn(stmnt);
			break;
		case CompileStmnt:
			break;
		case CompileDebugStmnt:
			break;
		case Block:
			BuildBlock(stmnt);
			break;
		case LogStmnt:
		{
			ScopeValue value = BuildExpr(stmnt->logStmnt.expr, stmnt);
			SpiteIR::Instruction* logInst = BuildLog(GetCurrentLabel(), BuildRegisterOperand(value));
		}
		default:
			Logger::Error("LowerDefinitions:BuildStmnt Invalid Statement");
			break;
		}
	}

	void BuildVarDefinition(Stmnt* stmnt)
	{
		auto& def = stmnt->definition;
		ScopeValue value = BuildExpr(def.assignment, stmnt);
		AddValueToCurrentScope(def.name->val, value, stmnt);
		funcContext.scopeUtils.AddToTopScope(def.name->val, stmnt);
	}

	void BuildInlineDefinition(Stmnt* stmnt)
	{
		Assert(stmnt->inlineDefinition.type->typeID == TypeID::ExplicitType &&
			stmnt->inlineDefinition.assignment);
		auto& def = stmnt->inlineDefinition;
		ScopeValue value = BuildExpr(def.assignment, stmnt);
	}

	void BuildAssignment(Stmnt* stmnt)
	{
		ScopeValue assignTo = BuildExpr(stmnt->assignmentStmnt.assignTo, stmnt);
		ScopeValue assignment = BuildExpr(stmnt->assignmentStmnt.assignment, stmnt);

		if (!assignTo.type || !assignment.type) return;

		if (assignTo.type->kind == SpiteIR::TypeKind::ReferenceType ||
			assignment.type->kind == SpiteIR::TypeKind::ReferenceType)
			BuildReferenceAssignment(assignTo, assignment);
		else
			BuildStore(GetCurrentLabel(), BuildRegisterOperand(assignTo), BuildRegisterOperand(assignment));
	}

	void BuildReferenceAssignment(const ScopeValue& to, const ScopeValue& from)
	{
		SpiteIR::Label* label = GetCurrentLabel();

		// value = ref - Dereference pointer into value
		if (to.type->kind != SpiteIR::TypeKind::ReferenceType &&
			from.type->kind == SpiteIR::TypeKind::ReferenceType)
		{
			BuildDereference(label, BuildRegisterOperand(to), BuildRegisterOperand(from));
		}
		// ref = value - StorePtr
		else if (to.type->kind == SpiteIR::TypeKind::ReferenceType &&
			from.type->kind != SpiteIR::TypeKind::ReferenceType)
		{
			BuildStorePtr(label, BuildRegisterOperand(to), BuildRegisterOperand(from));
		}
		// ref = ref - dereference src onto stack and StorePtr
		else
		{
			SpiteIR::Operand alloc = AllocateToOperand(BuildAllocate(from.type->reference.type));
			BuildDereference(label, alloc, BuildRegisterOperand(from));
			BuildStorePtr(label, BuildRegisterOperand(to), alloc);
		}
	}

	SpiteIR::Label* BuildCondition(const eastl::string& thenName, const eastl::string& elseName, Stmnt* stmnt)
	{
		Assert(stmnt->nodeID == StmntID::Conditional);

		SpiteIR::Label* fromLabel = GetCurrentLabel();
		ScopeValue cond = BuildExpr(stmnt->conditional.condition, stmnt);
		SpiteIR::Instruction* fromBranch = BuildBranch(fromLabel, BuildRegisterOperand(cond));

		SpiteIR::Label* ifThenLabel = BuildLabelBody(thenName, stmnt->conditional.body);
		fromBranch->branch.true_ = ifThenLabel;

		SpiteIR::Label* ifElseLabel = BuildLabel(elseName);
		fromBranch->branch.false_ = ifElseLabel;

		return ifThenLabel;
	}

	void BuildIfStmnt(Stmnt* stmnt)
	{
		Assert(stmnt->ifStmnt.condition->nodeID == StmntID::Conditional);
		auto& if_ = stmnt->ifStmnt;

		eastl::vector<SpiteIR::Label*> nonTerminated = eastl::vector<SpiteIR::Label*>();

		size_t ifCount = funcContext.ifCount;
		funcContext.ifCount += 1;
		eastl::string ifThenName = "if_then" + eastl::to_string(ifCount);
		eastl::string ifElseName = "if_else" + eastl::to_string(ifCount);
		SpiteIR::Label* ifThenLabel = BuildCondition(ifThenName, ifElseName, if_.condition);
		if (!ifThenLabel->terminator) nonTerminated.push_back(ifThenLabel);

		for (size_t i = 0; i < if_.elifs->size(); i++)
		{
			Stmnt* elseIfStmnt = if_.elifs->at(i);
			eastl::string elseIfThenName = "else_if_then" + eastl::to_string(ifCount) +
				"_" + eastl::to_string(i);
			eastl::string elseIfElseName = "else_if_else" + eastl::to_string(ifCount) +
				"_" + eastl::to_string(i);
			SpiteIR::Label* elseifThenLabel = BuildCondition(elseIfThenName, elseIfElseName, elseIfStmnt);
			if (!elseifThenLabel->terminator) nonTerminated.push_back(elseifThenLabel);
		}

		if (if_.elseCondition)
		{
			BuildBody(if_.elseCondition);
		}

		SpiteIR::Label* currLabel = GetCurrentLabel();
		if (!currLabel->terminator) nonTerminated.push_back(currLabel);

		if (nonTerminated.size())
		{
			eastl::string ifEndName = "if_end" + eastl::to_string(ifCount);
			SpiteIR::Label* ifEndLabel = BuildLabel(ifEndName);
			for (SpiteIR::Label* label : nonTerminated)
			{
				BuildJump(label, ifEndLabel);
			}
		}
	}

	void BuildForStmnt(Stmnt* stmnt)
	{
		Assert(stmnt->forStmnt.isDeclaration);
		auto& for_ = stmnt->forStmnt;
		Stmnt* defStmnt = for_.iterated.declaration;
		auto& def = defStmnt->definition;

		eastl::string forStartName = "for_cond" + eastl::to_string(funcContext.forCount);
		eastl::string forLoopName = "for_body" + eastl::to_string(funcContext.forCount);
		eastl::string forIncName = "for_inc" + eastl::to_string(funcContext.forCount);
		eastl::string forEndName = "for_end" + eastl::to_string(funcContext.forCount);
		funcContext.forCount++;

		SpiteIR::Label* fromLabel = GetCurrentLabel();
		SpiteIR::Instruction* alloc = BuildAllocateForType(def.type);
		ScopeValue init = InvalidScopeValue;
		if (for_.rangeFor)
		{
			init = BuildDefaultValue(alloc->allocate.type, alloc->allocate.result, fromLabel);
		}
		else
		{

		}

		AddValueToCurrentScope(def.name->val, init, defStmnt);

		ScopeValue to = BuildExpr(for_.toIterate, stmnt);

		SpiteIR::Instruction* toCond = BuildJump(fromLabel);
		SpiteIR::Label* forCondLabel = BuildLabel(forStartName);
		toCond->jump.label = forCondLabel;

		ScopeValue cmp = BuildBinaryOp(init, to, SpiteIR::BinaryOpKind::Less, forCondLabel);
		SpiteIR::Operand test = BuildRegisterOperand(cmp);
		SpiteIR::Instruction* branch = BuildBranch(forCondLabel, test);

		SpiteIR::Label* forLoopLabel = BuildLabelBody(forLoopName, for_.body);

		SpiteIR::Label* currentBodyLabel = GetCurrentLabel();
		SpiteIR::Instruction* bodyToInc = BuildJump(currentBodyLabel);
		SpiteIR::Label* forIncLabel = BuildLabel(forIncName);
		bodyToInc->jump.label = forIncLabel;

		if (for_.rangeFor)
		{
			SpiteIR::Operand incremented = BuildRegisterOperand(BuildIncrement(forIncLabel, init));
			SpiteIR::Instruction* storeInc = BuildStore(forIncLabel, BuildRegisterOperand(init),
				incremented);
		}
		else
		{

		}

		SpiteIR::Instruction* loopToCond = BuildJump(forIncLabel, forCondLabel);

		SpiteIR::Label* forEndLabel = BuildLabel(forEndName);
		branch->branch.true_ = forLoopLabel;
		branch->branch.false_ = forEndLabel;
	}

	inline void BuildVoidReturn(SpiteIR::Label* label)
	{
		SpiteIR::Operand voidOp = SpiteIR::Operand();
		voidOp.kind = SpiteIR::OperandKind::Void;
		BuildReturnOp(label, voidOp);
	}

	void BuildReturn(Stmnt* stmnt)
	{
		Assert(stmnt->returnStmnt.expr);
		auto& ret = stmnt->returnStmnt;


		SpiteIR::Label* label = GetCurrentLabel();
		if (ret.expr->typeID == ExprID::TypeExpr &&
			ret.expr->typeExpr.type->typeID == TypeID::PrimitiveType &&
			ret.expr->typeExpr.type->primitiveType.type == UniqueType::Void)
		{
			BuildVoidReturn(label);
		}
		else
		{
			ScopeValue value = BuildExpr(ret.expr, stmnt);
			if (value.type) BuildReturnOp(label, BuildRegisterOperand(HandleAutoCast(value, 
				funcContext.function->returnType)));
		}
	}

	ScopeValue BuildIncrement(SpiteIR::Label* label, ScopeValue& toIncrement)
	{
		Assert(toIncrement.type->kind == SpiteIR::TypeKind::PrimitiveType);
		SpiteIR::Operand amount = SpiteIR::Operand();
		amount.kind = SpiteIR::OperandKind::Literal;
		amount.type = toIncrement.type;
		amount.literal = {
			SpiteIR::PrimitiveKind::Int,
			1
		};
		SpiteIR::Instruction* allocate = BuildAllocate(toIncrement.type);
		SpiteIR::Instruction* store = BuildStore(label, AllocateToOperand(allocate), amount);

		ScopeValue right = { allocate->allocate.result, toIncrement.type };
		return BuildBinaryOp(toIncrement, right, SpiteIR::BinaryOpKind::Add, label);
	}

	void BuildBlock(Stmnt* stmnt)
	{
		SpiteIR::Label* startLabel = GetCurrentLabel();
		eastl::string blockName = "block" + eastl::to_string(funcContext.blockCount);
		eastl::string blockEndName = "block_end" + eastl::to_string(funcContext.blockCount);

		SpiteIR::Instruction* toBlock = BuildJump(startLabel);
		SpiteIR::Label* blockLabel = BuildLabel(blockName);
		toBlock->jump.label = blockLabel;

		for (Stmnt* stmnt : *stmnt->block.inner)
		{
			BuildStmntForBlock(stmnt);
		}

		SpiteIR::Label* currLabel = GetCurrentLabel();
		if (!currLabel->terminator)
		{
			SpiteIR::Instruction* toBlockEnd = BuildJump(currLabel);
			SpiteIR::Label* blockEndLabel = BuildLabel(blockEndName);
			toBlockEnd->jump.label = blockEndLabel;
		}
	}

	ScopeValue BuildExpr(Expr* expr, Stmnt* stmnt)
	{
		switch (expr->typeID)
		{
		case LiteralExpr:
			return BuildLiteral(expr);
		case IdentifierExpr:
			return FindValueForIndent(expr);
		case PrimitiveExpr:
			return BuildPrimitive(expr);
		case SelectorExpr:
			return BuildSelector(expr, stmnt);
		case IndexExpr:
			return BuildIndexExpr(expr, stmnt);
		case FunctionCallExpr:
			return BuildFunctionCall(expr, stmnt);
		case NewExpr:
			break;
		case FixedExpr:
			break;
		case TypeLiteralExpr:
			return BuildTypeLiteral(expr, stmnt);
		case ExplicitTypeExpr:
			break;
		case AsExpr:
			return BuildCastExpr(expr, stmnt);
		case DereferenceExpr:
			return BuildDereferenceExpr(expr, stmnt);
		case ReferenceExpr:
			return BuildReferenceExpr(expr, stmnt);
		case BinaryExpr:
			return BuildBinaryExpression(expr, stmnt);
		case UnaryExpr:
			break;
		case GroupedExpr:
			return BuildExpr(expr->groupedExpr.expr, stmnt);
		case TemplateExpr:
			break;
		case TypeExpr:
			return BuildTypeExpr(expr, stmnt);
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		case ConstantIntExpr:
			break;
		default:
			break;
		}

		return InvalidScopeValue;
	}

	ScopeValue FindValueForIndent(Expr* expr)
	{
		StringView& ident = expr->identifierExpr.identifier->val;
		return FindScopeValue(ident);
	}

	ScopeValue BuildPrimitive(Expr* expr)
	{
		UniqueType primEnum = expr->primitiveExpr.primitive->uniqueType;
		SpiteIR::Type* primType = ToIRType(symbolTable->CreatePrimitive(primEnum));
		return { InvalidRegister, primType };
	}

	SpiteIR::Member* FindStateMember(SpiteIR::State* state, StringView& ident)
	{
		eastl::vector<SpiteIR::Member*>& members = state->members;
		for (SpiteIR::Member* member : members)
		{
			if (member->value->name == ident)
			{
				return member;
			}
		}

		return nullptr;
	}

	ScopeValue BuildSelected(ScopeValue& value, Expr* selected)
	{
		Assert(selected->typeID == ExprID::IdentifierExpr);

		StringView& ident = selected->identifierExpr.identifier->val;

		// Package selector
		if (!value.type)
		{
			return InvalidScopeValue;
		}

		if (value.type->kind == SpiteIR::TypeKind::StateType)
		{
			SpiteIR::State* state = value.type->stateType.state;
			SpiteIR::Member* member = FindStateMember(state, ident);
			Assert(member);
			return { value.reg + member->offset, member->value->type };
		}
		else if (value.type->kind == SpiteIR::TypeKind::PointerType ||
			value.type->kind == SpiteIR::TypeKind::ReferenceType)
		{
			SpiteIR::Type* derefed = GetDereferencedType(value.type);
			if (derefed->kind == SpiteIR::TypeKind::StateType)
			{
				SpiteIR::State* state = derefed->stateType.state;
				SpiteIR::Member* member = FindStateMember(state, ident);
				Assert(member);
				SpiteIR::Type* referencedMember = MakeReferenceType(member->value->type,
					context.ir);
				SpiteIR::Instruction* alloc = BuildAllocate(referencedMember);
				ScopeValue dstValue = { alloc->allocate.result, referencedMember };
				SpiteIR::Operand offset = BuildRegisterOperand(BuildLiteralInt(member->offset));
				SpiteIR::Operand src = BuildRegisterOperand(value);
				SpiteIR::Operand dst = BuildRegisterOperand(dstValue);
				SpiteIR::Instruction* loadPtr = BuildLoadPtrOffset(GetCurrentLabel(), dst, src, offset);
				return dstValue;
			}
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildSelector(Expr* expr, Stmnt* stmnt)
	{
		Expr* left = expr->selectorExpr.on;
		Expr* right = expr->selectorExpr.select;
		ScopeValue leftVal = BuildExpr(left, stmnt);

		return BuildSelected(leftVal, right);
	}

	ScopeValue BuildDefaultValue(SpiteIR::Type* type, size_t dst, SpiteIR::Label* label)
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
		{
			SpiteIR::Operand defaultOp = SpiteIR::Operand();
			defaultOp.type = type;
			defaultOp.kind = SpiteIR::OperandKind::Literal;
			SpiteIR::Literal& literal = defaultOp.literal;

			switch (type->primitive.kind)
			{
			case SpiteIR::PrimitiveKind::Bool:
				literal.kind = SpiteIR::PrimitiveKind::Bool;
				literal.byteLiteral = 0;
				break;
			case SpiteIR::PrimitiveKind::Byte:
				literal.kind = SpiteIR::PrimitiveKind::Bool;
				literal.byteLiteral = 0;
				break;
			case SpiteIR::PrimitiveKind::Int:
				literal.kind = SpiteIR::PrimitiveKind::Int;
				literal.intLiteral = 0;
				break;
			case SpiteIR::PrimitiveKind::Float:
				literal.kind = SpiteIR::PrimitiveKind::Float;
				literal.floatLiteral = 0.0f;
				break;
			case SpiteIR::PrimitiveKind::String:
				literal.kind = SpiteIR::PrimitiveKind::String;
				literal.stringLiteral = context.ir->AllocateString();
				break;
			default:
				return InvalidScopeValue;
				break;
			}

			SpiteIR::Instruction* store = BuildStore(label, BuildRegisterOperand({ dst, type }),
				defaultOp);
			break;
		}
		case SpiteIR::TypeKind::StateType:
		{
			for (SpiteIR::Member* memberType : type->stateType.state->members)
			{
				BuildDefaultValue(memberType->value->type, dst + memberType->offset, label);
			}
			break;
		}
		case SpiteIR::TypeKind::StructureType:
			break;
		case SpiteIR::TypeKind::PointerType:
		{
			SpiteIR::Operand defaultOp = SpiteIR::Operand();
			defaultOp.type = type;
			defaultOp.kind = SpiteIR::OperandKind::Literal;
			defaultOp.literal.kind = SpiteIR::PrimitiveKind::Int;
			defaultOp.literal.intLiteral = 0;

			SpiteIR::Instruction* store = BuildStore(label, BuildRegisterOperand({ dst, type }),
				defaultOp);
		}
		break;
		case SpiteIR::TypeKind::DynamicArrayType:
			break;
		case SpiteIR::TypeKind::FixedArrayType:
			break;
		case SpiteIR::TypeKind::FunctionType:
			break;
		default:
			break;
		}

		return { dst, type };
	}

	ScopeValue BuildLiteral(Expr* expr)
	{
		auto& lit = expr->literalExpr;
		SpiteIR::Operand literalOp = SpiteIR::Operand();
		literalOp.kind = SpiteIR::OperandKind::Literal;
		SpiteIR::Literal& literal = literalOp.literal;

		switch (lit.val->uniqueType)
		{
		case UniqueType::IntLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Int;
			literal.intLiteral = IntLiteralStringToInt(lit.val->val);
			break;
		case UniqueType::HexLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Int;
			literal.intLiteral = std::stoul(lit.val->val.ToString().c_str(), nullptr, 16);
			break;
		case UniqueType::FloatLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Float;
			literal.floatLiteral = std::stof(lit.val->val.ToString().c_str());
			break;
		case UniqueType::StringLiteral:
			literal.kind = SpiteIR::PrimitiveKind::String;
			literal.stringLiteral = context.ir->AllocateString();
			*literal.stringLiteral = lit.val->val.ToString();
			break;
		case UniqueType::TrueLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Byte;
			literal.byteLiteral = 1;
			break;
		case UniqueType::FalseLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Byte;
			literal.byteLiteral = 0;
			break;
		default:
			literal.kind = SpiteIR::PrimitiveKind::Void;
			break;
		}

		SpiteIR::Type* irType = context.ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::PrimitiveType;
		irType->primitive.kind = literal.kind;
		irType->size = irType->primitive.kind == SpiteIR::PrimitiveKind::String ?
			config.targetArchBitWidth * 2 : config.targetArchBitWidth;
		irType->primitive.isSigned = true;

		literalOp.type = irType;

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Instruction* allocate = BuildAllocate(irType);
		SpiteIR::Instruction* store = BuildStore(label, AllocateToOperand(allocate), literalOp);
		return { store->store.dst.reg, irType };
	}

	ScopeValue BuildLiteralInt(size_t value)
	{
		SpiteIR::Operand literalOp = SpiteIR::Operand();
		literalOp.kind = SpiteIR::OperandKind::Literal;
		SpiteIR::Literal& literal = literalOp.literal;
		literal.kind = SpiteIR::PrimitiveKind::Int;
		literal.intLiteral = value;

		SpiteIR::Type* intType = CreateIntType(context.ir);
		literalOp.type = intType;

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Instruction* allocate = BuildAllocate(intType);
		SpiteIR::Instruction* store = BuildStore(label, AllocateToOperand(allocate), literalOp);
		return { store->store.dst.reg, intType };
	}

	void BuildStoreArrayCount(size_t reg, size_t count)
	{
		SpiteIR::Operand literalOp = SpiteIR::Operand();
		literalOp.kind = SpiteIR::OperandKind::Literal;
		SpiteIR::Literal& literal = literalOp.literal;
		literal.kind = SpiteIR::PrimitiveKind::Int;
		literal.intLiteral = count;

		SpiteIR::Type* irType = context.ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::PrimitiveType;
		irType->size = config.targetArchBitWidth;
		irType->primitive.kind = literal.kind;
		irType->primitive.isSigned = false;

		literalOp.type = irType;

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Instruction* store = BuildStore(label, BuildRegisterOperand({ reg, irType }),
			literalOp);
	}

	ScopeValue BuildTypeLiteral(Expr* expr, Stmnt* stmnt)
	{
		Assert(expr->typeLiteralExpr.values->size());
		SpiteIR::Label* label = GetCurrentLabel();
		eastl::vector<ScopeValue> values;
		for (Expr* val : *expr->typeLiteralExpr.values)
		{
			values.push_back(BuildExpr(val, stmnt));
		}

		SpiteIR::Type* derivedType = nullptr;
		if (expr->typeLiteralExpr.array)
		{
			derivedType = BuildFixedArray(context.ir, values.size(), values.at(0).type);
		}
		else
		{
			derivedType = context.ir->AllocateType();
			derivedType->kind = SpiteIR::TypeKind::StructureType;
			derivedType->size = 0;
			derivedType->structureType.types = context.ir->AllocateArray<SpiteIR::Type*>();
			derivedType->structureType.names = nullptr;
			for (ScopeValue& value : values)
			{
				derivedType->structureType.types->push_back(value.type);
				derivedType->size += value.type->size;
			}
		}

		SpiteIR::Instruction* alloc = BuildAllocate(derivedType);
		size_t offset = 0;
		for (ScopeValue& value : values)
		{
			SpiteIR::Operand dstOp = BuildRegisterOperand({ alloc->allocate.result + offset, alloc->allocate.type });
			BuildStore(label, dstOp, BuildRegisterOperand(value));
			offset += value.type->size;
		}

		return { alloc->allocate.result, derivedType };
	}

	ScopeValue BuildCastExpr(Expr* expr, Stmnt* stmnt)
	{
		auto& as = expr->asExpr;
		ScopeValue toCast = BuildExpr(as.of, stmnt);
		SpiteIR::Type* toType = ToIRType(as.to);
		SpiteIR::Instruction* alloc = BuildAllocate(toType);
		ScopeValue to = { alloc->allocate.result, toType };
		SpiteIR::Instruction* cast = BuildCast(GetCurrentLabel(), BuildRegisterOperand(toCast),
			BuildRegisterOperand(to));
		return to;
	}

	SpiteIR::Type* GetDereferencedType(SpiteIR::Type* type)
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PointerType:
			return type->pointer.type;
		case SpiteIR::TypeKind::ReferenceType:
			return type->reference.type;
		case SpiteIR::TypeKind::DynamicArrayType:
			return type->dynamicArray.type;
		case SpiteIR::TypeKind::FixedArrayType:
			return type->fixedArray.type;
		default:
			break;
		}
		return nullptr;
	}

	ScopeValue BuildIndexExpr(Expr* expr, Stmnt* stmnt)
	{
		SpiteIR::Label* label = GetCurrentLabel();
		ScopeValue toIndex = BuildExpr(expr->indexExpr.of, stmnt);
		ScopeValue index = BuildExpr(expr->indexExpr.index, stmnt);

		switch (toIndex.type->kind)
		{
		case SpiteIR::TypeKind::ReferenceType:
		{
			SpiteIR::Type* derefedType = toIndex.type->reference.type;
			switch (derefedType->kind)
			{
			case SpiteIR::TypeKind::StateType:
				return BuildStateOperatorCall(toIndex, UniqueType::Array, &index);
			case SpiteIR::TypeKind::DynamicArrayType:
				break;
			case SpiteIR::TypeKind::PointerType:
			{
				SpiteIR::Type* type = derefedType->pointer.type;
				SpiteIR::Instruction* alloc = BuildAllocate(derefedType);
				ScopeValue dst = { alloc->allocate.result, type };
				ScopeValue value = BuildTypeDereference(label, toIndex);
				ScopeValue offset = BuildLiteralInt(type->size);
				ScopeValue sizedOffset = BuildBinaryOp(index, offset, SpiteIR::BinaryOpKind::Multiply, label);
				SpiteIR::Instruction* loadPtr = BuildLoadPtrOffset(label, BuildRegisterOperand(dst),
					BuildRegisterOperand(value), BuildRegisterOperand(sizedOffset));
				return dst;
			}
			case SpiteIR::TypeKind::FixedArrayType:
				break;
			case SpiteIR::TypeKind::PrimitiveType:
			case SpiteIR::TypeKind::StructureType:
			case SpiteIR::TypeKind::FunctionType:
				Assert(false);
				break;
			default:
				break;
			}

			break;
		}
		case SpiteIR::TypeKind::StateType:
			return BuildStateOperatorCall(toIndex, UniqueType::Array, &index);
			break;
		case SpiteIR::TypeKind::DynamicArrayType:
			break;
		case SpiteIR::TypeKind::PointerType:
		{
			SpiteIR::Type* type = toIndex.type->pointer.type;
			SpiteIR::Instruction* alloc = BuildAllocate(toIndex.type);
			ScopeValue dst = { alloc->allocate.result, type };

			SpiteIR::Instruction* loadPtr = BuildLoadPtrOffset(label, BuildRegisterOperand(dst),
				BuildRegisterOperand(toIndex), BuildRegisterOperand(index));
			return dst;
		}
		case SpiteIR::TypeKind::FixedArrayType:
		{
			SpiteIR::Type* type = GetDereferencedType(toIndex.type);
			if (type->byValue)
			{
				SpiteIR::Instruction* alloc = BuildAllocate(type);
				ScopeValue dst = { alloc->allocate.result, type };
				SpiteIR::Operand src = BuildRegisterOperand(toIndex);
				SpiteIR::Operand offset = BuildRegisterOperand(index);
				SpiteIR::Instruction* load = BuildLoad(label, BuildRegisterOperand(dst), src, offset);
				return dst;
			}
			else
			{

			}
			break;
		}
		case SpiteIR::TypeKind::PrimitiveType:
		case SpiteIR::TypeKind::StructureType:
		case SpiteIR::TypeKind::FunctionType:
			Assert(false);
			break;
		default:
			break;
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildDereferenceExpr(Expr* expr, Stmnt* stmnt)
	{
		Expr* toDeref = expr->dereferenceExpr.of;
		ScopeValue value = BuildExpr(toDeref, stmnt);
		Assert(value.type->kind == SpiteIR::TypeKind::PointerType);

		// This looks wrong since we're making the type a reference type in a function called dereference
		// But a reference type signals to treat operations against it as a value
		// which is what we want when we dereference a pointer
		SpiteIR::Type* innerType = value.type->pointer.type;
		value.type->kind = SpiteIR::TypeKind::ReferenceType;
		value.type->reference.type = innerType;
		return value;
	}

	ScopeValue BuildReferenceExpr(Expr* expr, Stmnt* stmnt)
	{
		Expr* toRef = expr->referenceExpr.of;
		ScopeValue value = BuildExpr(toRef, stmnt);
		Assert(value.type->kind == SpiteIR::TypeKind::PointerType);

		return InvalidScopeValue;
	}

	ScopeValue PointerToInt(const ScopeValue& pointer)
	{

	}

	ScopeValue BuildBinaryOpValue(const ScopeValue& left, const ScopeValue& right, SpiteIR::BinaryOpKind op)
	{
		if (!left.type || !right.type) return InvalidScopeValue;

		SpiteIR::Label* label = GetCurrentLabel();

		if (left.type->kind == SpiteIR::TypeKind::ReferenceType &&
			left.type->reference.type->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return BuildBinaryOpValue(BuildTypeDereference(label, left), right, op);
		}

		if (right.type->kind == SpiteIR::TypeKind::ReferenceType &&
			right.type->reference.type->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return BuildBinaryOpValue(left, BuildTypeDereference(label, right), op);
		}

		if (left.type->kind == SpiteIR::TypeKind::PrimitiveType &&
			right.type->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return BuildBinaryOp(left, right, op, label);
		}

		if (left.type->kind == SpiteIR::TypeKind::PointerType)
		{
			return BuildBinaryOp(PointerToInt(left), right, op, label);
		}

		if (right.type->kind == SpiteIR::TypeKind::PointerType)
		{
			return BuildBinaryOp(left, PointerToInt(right), op, label);
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildBinaryExpression(Expr* expr, Stmnt* stmnt)
	{
		Expr* left = expr->binaryExpr.left;
		Expr* right = expr->binaryExpr.right;
		SpiteIR::BinaryOpKind op = BinaryOpToIR(expr->binaryExpr.op->uniqueType);

		ScopeValue leftVal = BuildExpr(left, stmnt);
		ScopeValue rightVal = BuildExpr(right, stmnt);

		return BuildBinaryOpValue(leftVal, rightVal, op);
	}

	void MakeDynamicArray(SpiteIR::Type* irType, size_t dst, Type* type)
	{
		SpiteIR::Label* label = GetCurrentLabel();
		size_t arrayItemSize = irType->dynamicArray.type->size;
		ScopeValue itemSize = BuildLiteralInt(arrayItemSize);
		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		params->push_back(BuildRegisterOperand(itemSize));
		BuildCall(makeArray, dst, params, label);
	}

	ScopeValue BuildTypeExpr(Expr* expr, Stmnt* stmnt)
	{
		Type* type = expr->typeExpr.type;
		SpiteIR::Instruction* alloc = BuildAllocateForType(type);
		SpiteIR::Type* irType = alloc->allocate.type;
		size_t reg = alloc->allocate.result;

		if (irType->kind == SpiteIR::TypeKind::DynamicArrayType)
		{
			MakeDynamicArray(irType, reg, type);
			return { reg, irType };
		}

		return BuildDefaultValue(alloc->allocate.type, alloc->allocate.result, GetCurrentLabel());
	}

	SpiteIR::Instruction* CreateInstruction(SpiteIR::Label* label)
	{
		SpiteIR::Instruction* inst = context.ir->AllocateInstruction();
		label->values.push_back(inst);
		return inst;
	}

	SpiteIR::Instruction* CreateTerminator(SpiteIR::Label* label)
	{
		SpiteIR::Instruction* inst = context.ir->AllocateInstruction();
		label->terminator = inst;
		return inst;
	}

	void HandlePrimitivePromotion(ScopeValue& left, ScopeValue& right)
	{
		Assert(left.type->kind == SpiteIR::TypeKind::PrimitiveType &&
			right.type->kind == SpiteIR::TypeKind::PrimitiveType);

		// Types are same, return
		if (left.type->size == right.type->size &&
			left.type->primitive.kind == right.type->primitive.kind &&
			left.type->primitive.isSigned == right.type->primitive.isSigned) return;

	}

	ScopeValue BuildBinaryOp(ScopeValue leftVal, ScopeValue rightVal, SpiteIR::BinaryOpKind kind,
		SpiteIR::Label* label)
	{
		SpiteIR::Instruction* binOp = CreateInstruction(label);
		binOp->kind = SpiteIR::InstructionKind::BinOp;
		binOp->binOp.kind = kind;
		binOp->binOp.left = BuildRegisterOperand(leftVal);
		binOp->binOp.right = BuildRegisterOperand(rightVal);
		binOp->binOp.result = funcContext.curr;

		ScopeValue value;
		if (kind >= SpiteIR::BinaryOpKind::LogicAnd) value = { binOp->binOp.result, CreateBoolType(context.ir) };
		else value = { binOp->binOp.result, leftVal.type };

		BuildAllocate(value.type);
		return value;
	}

	ScopeValue BuildTypeReference(SpiteIR::Label* label, const ScopeValue& value)
	{
		if (value.type->kind == SpiteIR::TypeKind::ReferenceType) return value;
		SpiteIR::Type* refType = MakeReferenceType(value.type, context.ir);
		SpiteIR::Instruction* alloc = BuildAllocate(refType);
		SpiteIR::Instruction* reference = BuildReference(label, AllocateToOperand(alloc),
			BuildRegisterOperand(value));
		return { alloc->allocate.result, alloc->allocate.type };
	}

	ScopeValue BuildTypeDereference(SpiteIR::Label* label, const ScopeValue& value)
	{
		Assert(value.type->kind == SpiteIR::TypeKind::ReferenceType);
		SpiteIR::Type* valType = GetDereferencedType(value.type);
		SpiteIR::Instruction* alloc = BuildAllocate(valType);
		SpiteIR::Instruction* reference = BuildDereference(label, AllocateToOperand(alloc),
			BuildRegisterOperand(value));
		return { alloc->allocate.result, alloc->allocate.type };
	}

	SpiteIR::Function* FindStateOperator(SpiteIR::State* state, UniqueType op, SpiteIR::Type* rhs = nullptr)
	{
		eastl::string opStr = OperatorToString(op);
		eastl::vector<SpiteIR::Function*>& stateOperators = state->operators;
		SpiteIR::Function* opFunc = nullptr;
		for (SpiteIR::Function* func : stateOperators)
		{
			if (func->name.rfind(opStr, 0) == 0)
			{
				if (rhs)
				{
					eastl::vector<SpiteIR::Argument*>& args = func->arguments;
					SpiteIR::Type* argType = args.at(1)->value->type;
					int match = IsIRTypeAssignable(argType, rhs);
					if (match == 2 && !opFunc) opFunc = func;
					else if (match == 1)
					{
						opFunc = func;
						break;
					}
				}
				else
				{
					opFunc = func;
					break;
				}
			}
		}

		Assert(opFunc);
		return opFunc;
	}

	ScopeValue BuildStateOperatorCall(const ScopeValue& of, UniqueType op, ScopeValue* rhs = nullptr)
	{
		SpiteIR::State* state = of.type->kind == SpiteIR::TypeKind::ReferenceType ?
			of.type->reference.type->stateType.state : of.type->stateType.state;

		SpiteIR::Type* rhsType = rhs ? rhs->type : nullptr;
		SpiteIR::Function* operatorFunc = FindStateOperator(state, op, rhsType);

		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		params->push_back(BuildRegisterOperand(HandleAutoCast(of, operatorFunc->arguments.at(0)->value->type)));
		if (rhs)
		{
			params->push_back(BuildRegisterOperand(HandleAutoCast(*rhs, 
				operatorFunc->arguments.at(1)->value->type)));
		}

		SpiteIR::Instruction* alloc = BuildAllocate(operatorFunc->returnType);
		SpiteIR::Instruction* call = BuildCall(operatorFunc, alloc->allocate.result, params, GetCurrentLabel());
		return { alloc->allocate.result, alloc->allocate.type };
	}

	ScopeValue BuildFunctionCall(Expr* expr, Stmnt* stmnt)
	{
		Assert(expr && expr->typeID == ExprID::FunctionCallExpr);
		Assert(expr->functionCallExpr.callKind != FunctionCallKind::UnknownCall);
		auto& funcCall = expr->functionCallExpr;
		SpiteIR::Function* irFunction = nullptr;
		SpiteIR::Label* label = GetCurrentLabel();

		switch (funcCall.callKind)
		{
		case FunctionCall:
			irFunction = FindFunctionForFunctionCall(expr);
			break;
		case ConstructorCall:
			irFunction = FindFunctionForConstructor(expr);
			break;
		case MemberMethodCall:
			irFunction = FindFunctionForMemberCall(expr);
			break;
		case UniformMethodCall:
			break;
		case FunctionTypeCall:
			break;
		case UnresolvedGenericCall:
			break;
		case ExternalCall:
			irFunction = FindExternalFunctionForFunctionCall(expr);
			break;
		default:
			break;
		}

		if (!irFunction) return { InvalidRegister, nullptr };

		ScopeValue ret = InvalidScopeValue;
		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		if (funcCall.callKind == MemberMethodCall)
		{
			Expr* caller = GetCallerExprMethodCall(funcCall.function);
			ScopeValue thisValue = BuildExpr(caller, stmnt);
			SpiteIR::Operand ref = BuildRegisterOperand(BuildTypeReference(label, thisValue));
			params->push_back(ref);
		}
		else if (funcCall.callKind == ConstructorCall)
		{
			Stmnt* state = funcCall.functionStmnt;
			if (state->nodeID != StmntID::StateStmnt)
			{
				Token* stateName = state->constructor.stateName;
				state = context.globalTable->FindScopedState(stateName, symbolTable);
			}

			eastl::string stateName;
			if (funcCall.function->typeID == ExprID::TemplateExpr)
			{
				eastl::vector<Expr*> templates = ExpandTemplates(funcCall.function->templateExpr.templateArgs);
				stateName = BuildTemplatedStateName(state, &templates);
			}
			else
			{
				stateName = BuildStateName(state);
			}

			SpiteIR::State* irState = context.FindState(stateName);
			SpiteIR::Type* type = context.ir->AllocateType();
			type->kind = SpiteIR::TypeKind::StateType;
			type->size = irState->size;
			type->stateType.state = irState;

			SpiteIR::Instruction* alloc = BuildAllocate(type);
			SpiteIR::Operand ref = BuildRegisterOperand(BuildTypeReference(label,
				{ alloc->allocate.result, alloc->allocate.type }));
			params->push_back(ref);
			ret = { ref.reg, ref.type };
		}

		eastl::vector<Expr*>* exprParams = expr->functionCallExpr.params;
		size_t argOffset = params->size();
		for (size_t i = 0; i < exprParams->size(); i++)
		{
			Expr* param = exprParams->at(i);
			ScopeValue value = BuildExpr(param, stmnt);

			SpiteIR::Type* argType = irFunction->arguments.at(argOffset + i)->value->type;
			params->push_back(BuildRegisterOperand(HandleAutoCast(value, argType)));
		}

		if (!ret.type)
		{
			SpiteIR::Instruction* alloc = BuildAllocate(irFunction->returnType);
			ret = { alloc->allocate.result, alloc->allocate.type };
		}
		SpiteIR::Instruction* call = BuildCall(irFunction, ret.reg, params, label);
		return ret;
	}

	Stmnt* FindFunctionStmnt(Expr* expr)
	{
		switch (expr->typeID)
		{
		case IdentifierExpr:
			return context.globalTable->FindScopedFunction(expr->identifierExpr.identifier, symbolTable);
		case SelectorExpr:
			return context.globalTable->FindStatementForPackage(expr->selectorExpr.on->identifierExpr.identifier,
				expr->selectorExpr.select->identifierExpr.identifier);
		case TemplateExpr:
			return FindFunctionStmnt(expr->templateExpr.expr);
		default:
			break;
		}

		return nullptr;
	}

	SpiteIR::Function* FindFunction(const StringView& packageName,
		const eastl::string& functionName)
	{
		Assert(MapHas(context.packageMap, packageName));
		SpiteIR::Package* package = context.packageMap[packageName];
		Assert(MapHas(package->functions, functionName));
		SpiteIR::Function* function = package->functions[functionName];

		return function;
	}

	SpiteIR::Function* FindFunctionForMemberCall(Expr* expr)
	{
		Expr* caller = expr->functionCallExpr.function;
		Stmnt* methodStmnt = expr->functionCallExpr.functionStmnt;
		StringView& packageName = methodStmnt->package->val;
		eastl::string methodName;

		if (caller->typeID == ExprID::TemplateExpr)
		{
			eastl::vector<Expr*>* templates = caller->templateExpr.templateArgs;
			eastl::vector<Expr*> expandedTemplates = ExpandTemplates(templates);
			methodName = BuildTemplatedMethodName(methodStmnt, &expandedTemplates);
		}
		else
		{
			methodName = BuildMethodName(methodStmnt);
		}

		return FindFunction(packageName, methodName);
	}


	SpiteIR::Function* FindFunctionForConstructor(Expr* expr)
	{
		Expr* caller = expr->functionCallExpr.function;
		Stmnt* constructorStmnt = expr->functionCallExpr.functionStmnt;
		Stmnt* stateStmnt = constructorStmnt->nodeID == StmntID::StateStmnt ? constructorStmnt :
			context.globalTable->FindScopedState(constructorStmnt->constructor.stateName, symbolTable);

		StringView& packageName = stateStmnt->package->val;
		eastl::vector<Token*>* generics = stateStmnt->state.generics ?
			stateStmnt->state.generics->generics.names : nullptr;
		eastl::vector<Expr*> templates;
		if (caller->typeID == ExprID::TemplateExpr)
		{
			templates = ExpandTemplates(caller->templateExpr.templateArgs);
		}

		eastl::string constructorName = constructorStmnt->nodeID == StmntID::StateStmnt ?
			BuildDefaultConstructorName(constructorStmnt, &templates) :
			BuildConstructorName(constructorStmnt, generics, &templates);

		return FindFunction(packageName, constructorName);
	}

	SpiteIR::Function* FindExternalFunctionForFunctionCall(Expr* expr)
	{
		Expr* caller = expr->functionCallExpr.function;
		Stmnt* stmnt = FindFunctionStmnt(caller);

		eastl::string functionName = stmnt->externFunction.callName->val.ToString();
		StringView& packageName = stmnt->package->val;
		Assert(MapHas(context.packageMap, packageName));
		SpiteIR::Package* package = context.packageMap[packageName];
		Assert(MapHas(package->functions, functionName));
		SpiteIR::Function* function = package->functions[functionName];
		return function;
	}

	SpiteIR::Function* FindFunctionForFunctionCall(Expr* expr)
	{
		Assert(expr);
		Expr* caller = expr->functionCallExpr.function;
		Stmnt* func = expr->functionCallExpr.functionStmnt;
		StringView& packageName = func->package->val;
		eastl::string functionName;

		if (caller->typeID == ExprID::TemplateExpr)
		{
			eastl::vector<Expr*>* templates = caller->templateExpr.templateArgs;
			eastl::vector<Expr*> expandedTemplates = ExpandTemplates(templates);
			functionName = BuildTemplatedFunctionName(func, &expandedTemplates);
		}
		else
		{
			functionName = BuildFunctionName(func);
		}

		return FindFunction(packageName, functionName);
	}

	SpiteIR::Operand AllocateToOperand(SpiteIR::Instruction* alloc)
	{
		return BuildRegisterOperand({ alloc->allocate.result, alloc->allocate.type });
	}

	SpiteIR::Instruction* BuildAllocate(SpiteIR::Type* type)
	{
		SpiteIR::Instruction* allocate = context.ir->AllocateInstruction();
		allocate->kind = SpiteIR::InstructionKind::Allocate;
		allocate->allocate.type = type;
		allocate->allocate.result = funcContext.curr;

		funcContext.function->block->allocations.push_back(allocate);
		funcContext.IncrementRegister(type);
		return allocate;
	}

	SpiteIR::Instruction* BuildAllocateForType(Type* type)
	{
		SpiteIR::Type* irType = ToIRType(type);
		return BuildAllocate(irType);
	}

	SpiteIR::Instruction* BuildStore(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = CreateInstruction(label);
		store->kind = SpiteIR::InstructionKind::Store;
		store->store.dst = dst;
		store->store.src = src;
		return store;
	}

	SpiteIR::Instruction* BuildStorePtr(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = BuildStore(label, dst, src);
		store->kind = SpiteIR::InstructionKind::StorePtr;
		return store;
	}

	SpiteIR::Instruction* BuildDereference(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = BuildStore(label, dst, src);
		store->kind = SpiteIR::InstructionKind::Dereference;
		return store;
	}

	SpiteIR::Instruction* BuildReference(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = BuildStore(label, dst, src);
		store->kind = SpiteIR::InstructionKind::Reference;
		return store;
	}

	SpiteIR::Instruction* BuildLoad(SpiteIR::Label* label, const SpiteIR::Operand dst,
		const SpiteIR::Operand& src, const SpiteIR::Operand& offset)
	{
		SpiteIR::Instruction* load = CreateInstruction(label);
		load->kind = SpiteIR::InstructionKind::Load;
		load->load.dst = dst;
		load->load.src = src;
		load->load.offset = offset;
		return load;
	}

	SpiteIR::Instruction* BuildLoadPtrOffset(SpiteIR::Label* label, const SpiteIR::Operand dst,
		const SpiteIR::Operand& src, const SpiteIR::Operand& offset)
	{
		SpiteIR::Instruction* load = BuildLoad(label, dst, src, offset);
		load->kind = SpiteIR::InstructionKind::LoadPtrOffset;
		return load;
	}

	SpiteIR::Instruction* BuildCast(SpiteIR::Label* label, const SpiteIR::Operand from,
		const SpiteIR::Operand& to)
	{
		SpiteIR::Instruction* cast = CreateInstruction(label);
		cast->kind = SpiteIR::InstructionKind::Cast;
		cast->cast.from = from;
		cast->cast.to = to;
		return cast;
	}

	SpiteIR::Operand BuildRegisterOperand(const ScopeValue& value)
	{
		SpiteIR::Operand operand = SpiteIR::Operand();
		operand.type = value.type;
		operand.kind = SpiteIR::OperandKind::Register;
		operand.reg = value.reg;
		return operand;
	}

	SpiteIR::InstructionKind GetCallKind(SpiteIR::Function* function)
	{
		if (function->metadata.externFunc) return SpiteIR::InstructionKind::ExternCall;

		return SpiteIR::InstructionKind::Call;
	}

	SpiteIR::Instruction* BuildCall(SpiteIR::Function* function, size_t returnReg,
		eastl::vector<SpiteIR::Operand>* params, SpiteIR::Label* label)
	{
		SpiteIR::Instruction* call = CreateInstruction(label);
		call->kind = GetCallKind(function);
		call->call.function = function;
		call->call.params = params;
		call->call.result = returnReg;
		return call;
	}

	SpiteIR::Instruction* BuildJump(SpiteIR::Label* label, SpiteIR::Label* to = nullptr)
	{
		Assert(!label->terminator);
		SpiteIR::Instruction* jump = CreateTerminator(label);
		jump->kind = SpiteIR::InstructionKind::Jump;
		jump->jump.label = to;
		return jump;
	}

	SpiteIR::Instruction* BuildBranch(SpiteIR::Label* label, const SpiteIR::Operand& test,
		SpiteIR::Label* true_ = nullptr, SpiteIR::Label* false_ = nullptr)
	{
		Assert(!label->terminator);
		SpiteIR::Instruction* branch = CreateTerminator(label);
		branch->kind = SpiteIR::InstructionKind::Branch;
		branch->branch.test = test;
		branch->branch.true_ = true_;
		branch->branch.false_ = false_;
		return branch;
	}

	SpiteIR::Instruction* BuildReturnOp(SpiteIR::Label* label, const SpiteIR::Operand& operand)
	{
		Assert(!label->terminator);
		SpiteIR::Instruction* ret = CreateTerminator(label);
		ret->kind = SpiteIR::InstructionKind::Return;
		ret->return_.operand = operand;
		return ret;
	}

	SpiteIR::Instruction* BuildLog(SpiteIR::Label* label, const SpiteIR::Operand& operand)
	{
		SpiteIR::Instruction* log = CreateInstruction(label);
		log->kind = SpiteIR::InstructionKind::Log;
		log->log.operand = operand;
		return log;
	}
};