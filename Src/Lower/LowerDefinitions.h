#include "EASTL/deque.h"
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerContext.h"

extern Config config;

struct ScopeValue
{
	size_t reg = 0;;
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
	size_t curr = 0;
	size_t forCount = 0;
	size_t ifCount = 0;
	size_t blockCount = 0;

	void IncrementRegister(SpiteIR::Type* type)
	{
		curr += type->size;
	}

	void Reset(SpiteIR::Function* function)
	{
		this->function = function;
		scopeQueue.clear();
		curr = 0;
		forCount = 0;
		ifCount = 0;
		blockCount = 0;
	}
};

struct LowerDefinitions
{
	LowerContext& context;
	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolve;
	FunctionContext funcContext;
	SymbolTable* symbolTable = nullptr;
	eastl::vector<Expr*>* currTemplates = nullptr;
	eastl::vector<Token*>* currGenerics = nullptr;
	SpiteIR::Package* currPackage = nullptr;


	LowerDefinitions(LowerContext& context) : context(context)
	{}

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
			currPackage = package;
			symbolTable = context.packageToSymbolTableMap[package];
			for (auto& [key, state] : package->states)
			{
				ASTContainer& stateContainer = context.stateASTMap[state];
				currTemplates = stateContainer.templates;
				SetCurrentGenerics(stateContainer.node);
				BuildState(state, stateContainer.node);
			}

			for (auto& [key, function] : package->functions)
			{
				ASTContainer& funcContainer = context.functionASTMap[function];
				currTemplates = funcContainer.templates;
				SetCurrentGenerics(funcContainer.node);
				BuildFunction(function, funcContainer.node);
			}
		}
	}

	void AddScope()
	{
		funcContext.scopeQueue.emplace_back();
	}

	void PopScope()
	{
		FunctionScope scope = funcContext.scopeQueue.back();
		funcContext.scopeQueue.pop_back();
	}

	void AddValueToCurrentScope(const StringView& name, const ScopeValue& value)
	{
		funcContext.scopeQueue.back().scopeMap[name] = value;
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

		return { 0, nullptr };
	}

	SpiteIR::Label* GetCurrentLabel()
	{
		return funcContext.function->block->labels.back();
	}

	void BuildState(SpiteIR::State* state, Stmnt* stateStmnt)
	{
		BuildStateDefault(state, stateStmnt);
	}

	void BuildStateDefault(SpiteIR::State* state, Stmnt* stateStmnt)
	{
	}

	SpiteIR::Type* ToIRType(Type* type)
	{
		SpiteIR::Type* irType = TypeToIRType(context.ir, type, this, currGenerics, currTemplates);
		Assert(irType);
		return irType;
	}

	Stmnt* GetDeclForFunc(Stmnt* func)
	{
		switch (func->nodeID)
		{
		case FunctionStmnt:
			return func->function.decl;
		case Method:
			return func->method.decl;
		case StateOperator:
			return func->stateOperator.decl;
		case Constructor:
			return func->constructor.decl;
		case Destructor:
			return func->destructor.decl;
		default:
			break;
		}

		return nullptr;
	}

	void BuildFunction(SpiteIR::Function* function, Stmnt* funcStmnt)
	{
		Assert(function);
		Stmnt* decl = GetDeclForFunc(funcStmnt);
		Assert(decl);

		function->block = context.ir->AllocateBlock();
		funcContext.Reset(function);
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

	void BuildFunctionArguments(SpiteIR::Function* function, Stmnt* funcStmnt)
	{
		for (SpiteIR::Argument* arg : function->arguments)
		{
			SpiteIR::Instruction* alloc = BuildAllocate(arg->value->type);
			StringView name = StringView(arg->value->name.c_str());
			AddValueToCurrentScope(name, { alloc->allocate.result, alloc->allocate.type });
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
		SpiteIR::Label* Label = BuildLabel(name);
		BuildBody(body);

		return Label;
	}

	void BuildBody(Body& body)
	{
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
		default:
			Logger::Error("LowerDefinitions:BuildStmnt Invalid Statement");
			break;
		}
	}

	void BuildVarDefinition(Stmnt* stmnt)
	{
		auto& def = stmnt->definition;
		ScopeValue value = BuildExpr(def.assignment, stmnt);
		if (value.type) AddValueToCurrentScope(def.name->val, value);
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
		BuildStore(GetCurrentLabel(),assignTo.reg, BuildRegisterOperand(assignment));
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
		auto& def = for_.iterated.declaration->definition;

		eastl::string forStartName = "for_cond" + eastl::to_string(funcContext.forCount);
		eastl::string forLoopName = "for_body" + eastl::to_string(funcContext.forCount);
		eastl::string forIncName = "for_inc" + eastl::to_string(funcContext.forCount);
		eastl::string forEndName = "for_end" + eastl::to_string(funcContext.forCount);
		funcContext.forCount++;

		SpiteIR::Label* fromLabel = GetCurrentLabel();
		SpiteIR::Instruction* alloc = BuildAllocateForType(def.type);
		ScopeValue init = { 0, nullptr };
		if (for_.rangeFor)
		{
			init = BuildDefaultValue(alloc->allocate.type, alloc->allocate.result, fromLabel);
		}
		else
		{

		}

		AddValueToCurrentScope(def.name->val, init);

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
			SpiteIR::Instruction* storeInc = BuildStore(forIncLabel, init.reg, incremented);
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
			if (value.type) BuildReturnOp(label, BuildRegisterOperand(value));
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
		SpiteIR::Instruction* store = BuildStore(label, allocate->allocate.result, amount);

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
			break;
		case SelectorExpr:
			break;
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
			break;
		case DereferenceExpr:
			break;
		case ReferenceExpr:
			break;
		case BinaryExpr:
			return BuildBinaryExpression(expr, stmnt);
		case UnaryExpr:
			break;
		case GroupedExpr:
			return BuildExpr(expr->groupedExpr.expr, stmnt);
		case TemplateExpr:
			break;
		case TypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		case ConstantIntExpr:
			break;
		default:
			break;
		}

		return { 0, nullptr };
	}

	ScopeValue BuildDefaultValue(SpiteIR::Type* type, size_t result, SpiteIR::Label* label)
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
				return { 0, nullptr };
				break;
			}

			SpiteIR::Instruction* store = BuildStore(label, result, defaultOp);
			break;
		}
		case SpiteIR::TypeKind::StateType:
			break;
		case SpiteIR::TypeKind::StructureType:
			break;
		case SpiteIR::TypeKind::PointerType:
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

		return { result, type };
	}

	ScopeValue BuildLiteral(Expr* expr)
	{
		auto& lit = expr->literalExpr;
		SpiteIR::Operand literalOp = SpiteIR::Operand();
		literalOp.kind = SpiteIR::OperandKind::Literal;
		SpiteIR::Literal& literal = literalOp.literal;

		switch (lit.type)
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
		SpiteIR::Instruction* store = BuildStore(label, allocate->allocate.result, literalOp);
		return { store->store.dst, irType };
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

		SpiteIR::Type* derivedType = context.ir->AllocateType();
		if (expr->typeLiteralExpr.array)
		{
			derivedType->kind = SpiteIR::TypeKind::FixedArrayType;
			derivedType->fixedArray.count = values.size();
			derivedType->fixedArray.type = values.at(0).type;
			derivedType->size = derivedType->fixedArray.count * derivedType->fixedArray.type->size;
		}
		else
		{
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
			BuildStore(label, alloc->allocate.result + offset, BuildRegisterOperand(value));
			offset += value.type->size;
		}

		return { alloc->allocate.result, derivedType };
	}

	ScopeValue FindValueForIndent(Expr* expr)
	{
		StringView& ident = expr->identifierExpr.identifier->val;
		return FindScopeValue(ident);
	}

	SpiteIR::Type* GetDereferencedType(SpiteIR::Type* type)
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PointerType:
			return type->pointer.type;
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

		ScopeValue dst = { 0, nullptr };

		switch (toIndex.type->kind)
		{
		case SpiteIR::TypeKind::StateType:
			break;
		case SpiteIR::TypeKind::PointerType:
		case SpiteIR::TypeKind::DynamicArrayType:
		case SpiteIR::TypeKind::FixedArrayType:
		{
			SpiteIR::Type* type = GetDereferencedType(toIndex.type);
			SpiteIR::Instruction* alloc = BuildAllocate(type);
			SpiteIR::Operand dst = BuildRegisterOperand({ alloc->allocate.result, type });
			SpiteIR::Operand src = BuildRegisterOperand(toIndex);
			SpiteIR::Operand offset = BuildRegisterOperand(index);
			SpiteIR::Instruction* load = BuildLoad(label, dst, src, offset);
			break;
		}
		case SpiteIR::TypeKind::PrimitiveType:
		case SpiteIR::TypeKind::StructureType:
		case SpiteIR::TypeKind::FunctionType:
			break;
		default:
			break;
		}

		return { 0, nullptr };
	}

	ScopeValue BuildBinaryExpression(Expr* expr, Stmnt* stmnt)
	{
		Expr* left = expr->binaryExpr.left;
		Expr* right = expr->binaryExpr.right;
		SpiteIR::BinaryOpKind op = BinaryOpToIR(expr->binaryExpr.opType);

		ScopeValue leftVal = BuildExpr(left, stmnt);
		ScopeValue rightVal = BuildExpr(right, stmnt);

		if (!leftVal.type || !rightVal.type) return { 0, nullptr };

		if (leftVal.type->kind == SpiteIR::TypeKind::PrimitiveType &&
			rightVal.type->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return BuildBinaryOp(leftVal, rightVal, op, GetCurrentLabel());
		}

		return { 0, nullptr };
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
		if (kind >= SpiteIR::BinaryOpKind::LogicAnd) value = { binOp->binOp.result, &_boolType };
		else value = { binOp->binOp.result, leftVal.type };

		BuildAllocate(value.type);
		return value;
	}

	ScopeValue BuildFunctionCall(Expr* expr, Stmnt* stmnt)
	{
		Assert(expr && expr->typeID == ExprID::FunctionCallExpr);
		Assert(expr->functionCallExpr.callKind != FunctionCallKind::UnknownCall);
		auto& funcCall = expr->functionCallExpr;
		SpiteIR::Function* irFunction = nullptr;

		switch (funcCall.callKind)
		{
		case FunctionCall:
			irFunction = FindFunctionForFunctionCall(expr);
			break;
		case ConstructorCall:
			break;
		case MemberMethodCall:
			break;
		case UniformMethodCall:
			break;
		case FunctionTypeCall:
			break;
		case UnresolvedGenericCall:
			break;
		default:
			break;
		}

		if (!irFunction) return { 0, nullptr };

		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		eastl::vector<Expr*>* exprParams = expr->functionCallExpr.params;
		for (size_t i = 0; i < exprParams->size(); i++)
		{
			Expr* param = exprParams->at(i);
			ScopeValue value = BuildExpr(param, stmnt);
			params->push_back(BuildRegisterOperand(value));
		}

		SpiteIR::Instruction* alloc = BuildAllocate(irFunction->returnType);
		SpiteIR::Instruction* call = BuildCall(irFunction, alloc->allocate.result, params, 
			GetCurrentLabel());
		return { call->call.result, irFunction->returnType };
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

	SpiteIR::Function* FindFunctionForFunctionStmnt(Stmnt* func, eastl::vector<Expr*>* templates = nullptr)
	{
		Assert(func);
		StringView& packageName = func->package->val;
		eastl::string functionName;
		if (templates)
		{
			eastl::vector<Expr*> expandedTemplates = ExpandTemplates(templates);
			functionName = BuildTemplatedFunctionName(func, &expandedTemplates);
		}
		else
		{
			functionName = BuildFunctionName(func);
		}

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
		Stmnt* stmnt = FindFunctionStmnt(caller);
		eastl::vector<Expr*>* templates = nullptr;
		if (caller->typeID == ExprID::TemplateExpr)
		{
			templates = caller->templateExpr.templateArgs;
		}

		return FindFunctionForFunctionStmnt(stmnt, templates);
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

	SpiteIR::Instruction* BuildStore(SpiteIR::Label* label, size_t dst, const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = CreateInstruction(label);
		store->kind = SpiteIR::InstructionKind::Store;
		store->store.dst = dst;
		store->store.src = src;

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

	SpiteIR::Operand BuildRegisterOperand(const ScopeValue& value)
	{
		SpiteIR::Operand operand = SpiteIR::Operand();
		operand.type = value.type;
		operand.kind = SpiteIR::OperandKind::Register;
		operand.reg = value.reg;
		return operand;
	}

	SpiteIR::Instruction* BuildCall(SpiteIR::Function* function, size_t returnReg, 
		eastl::vector<SpiteIR::Operand>* params, SpiteIR::Label* label)
	{
		SpiteIR::Instruction* call = CreateInstruction(label);
		call->kind = SpiteIR::InstructionKind::Call;
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
};