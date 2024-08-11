#include "EASTL/deque.h"
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerContext.h"

extern Config config;

struct ScopeValue
{
	size_t reg;
	SpiteIR::Type* type;
};

struct FunctionContext
{
	SpiteIR::Function* function;
	eastl::hash_map<StringView, ScopeValue, StringViewHash> scopeMap;
	eastl::vector<Expr*> deferred;
	eastl::vector<size_t> toDestroy;
	size_t curr = 0;
	size_t forCount = 0;

	void IncrementRegister(SpiteIR::Type* type)
	{
		curr += type->size;
	}

	void Reset(SpiteIR::Function* function)
	{
		this->function = function;
		scopeMap.clear();
		deferred.clear();
		toDestroy.clear();
		curr = 0;
		forCount = 0;
	}
};

struct LowerDefinitions
{
	LowerContext& context;
	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolve;
	FunctionContext scope;
	SymbolTable* symbolTable = nullptr;
	eastl::vector<Expr*>* currTemplates = nullptr;
	eastl::vector<Token*>* currGenerics = nullptr;
	
	LowerDefinitions(LowerContext& context): context(context)
	{}

	void SetCurrentGenerics(Stmnt* stmnt)
	{
		Stmnt* generics = GetGenerics(stmnt);
		if (generics) currGenerics = generics->generics.names;
		else currGenerics = nullptr;
	}

	void BuildDefinitions()
	{
		for (SpiteIR::Package* package : context.ir->packages)
		{
			symbolTable = context.packageToSymbolTableMap[package];
			for (auto& [key, state] : package->states)
			{
				ASTContainer& stateContainer = context.stateASTMap[state];
				currTemplates = stateContainer.templates;
				SetCurrentGenerics(stateContainer.node);
				BuildStateDefault(state, stateContainer.node);
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
	
	void BuildStateDefault(SpiteIR::State* state, Stmnt* stateStmnt)
	{

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
		Stmnt* decl = GetDeclForFunc(funcStmnt);
		Assert(decl);

		function->block = context.ir->AllocateBlock();
		scope.Reset(function);
		BuildEntryLabel(function, funcStmnt, decl);
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
		scope.function->block->labels.push_back(label);

		return label;
	}

	SpiteIR::Label* BuildLabelBody(const eastl::string& name, Body& body)
	{
		SpiteIR::Label* Label = BuildLabel(name);

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

		return Label;
	}

	void BuildStmntForBlock(Stmnt* stmnt)
	{

		switch (stmnt->nodeID)
		{
		case ExpressionStmnt:
			BuildExpr(stmnt->expressionStmnt.expression);
			break;
		case Definition:
			BuildVarDefinition(stmnt);
			break;
		case InlineDefinition:
			break;
		case FunctionStmnt:
			break;
		case AnonFunction:
			break;
		case Conditional:
			break;
		case AssignmentStmnt:
			break;
		case IfStmnt:
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
			break;
		case CompileStmnt:
			break;
		case CompileDebugStmnt:
			break;
		case Block:
		{
		}
		default:
			break;
		}
	}

	void BuildVarDefinition(Stmnt* stmnt)
	{
		auto& def = stmnt->definition;
		ScopeValue value = BuildExpr(def.assignment);
		scope.scopeMap[def.name->val] = value;
	}

	void BuildForStmnt(Stmnt* stmnt)
	{
		Assert(stmnt->forStmnt.isDeclaration);
		auto& for_ = stmnt->forStmnt;
		auto& def = for_.iterated.declaration->definition;

		SpiteIR::Label* fromLabel = scope.function->block->labels.back();
		SpiteIR::Instruction* alloc = BuildAllocateForType(def.type);
		ScopeValue init = { 0, nullptr };
		if (for_.rangeFor)
		{
			init = BuildDefaultValue(alloc->allocate.type, alloc->allocate.result, fromLabel);
		}
		else
		{

		}

		ScopeValue to = BuildExpr(for_.toIterate);

		SpiteIR::Instruction* toCond = BuildJump(fromLabel);
		eastl::string forStartName = "for_cond" + eastl::to_string(scope.forCount);
		SpiteIR::Label* forCondLabel = BuildLabel(forStartName);
		toCond->jump.label = forCondLabel;

		ScopeValue cmp = BuildBinaryOp(init, to, SpiteIR::BinaryOpKind::LessEqual, forCondLabel);
		SpiteIR::Operand test = BuildRegisterOperand(cmp);
		SpiteIR::Instruction* branch = BuildBranch(forCondLabel, test);

		eastl::string forLoopName = "for_loop" + eastl::to_string(scope.forCount);
		SpiteIR::Label* forLoopLabel = BuildLabelBody(forLoopName, for_.body);

		if (for_.rangeFor)
		{
			SpiteIR::Operand incremented = BuildRegisterOperand(BuildIncrement(forLoopLabel, init));
			SpiteIR::Instruction* storeInc = BuildStore(init.type, forLoopLabel, init.reg, incremented);
		}
		else
		{

		}
		
		SpiteIR::Instruction* loopToCond = BuildJump(forLoopLabel, forCondLabel);

		eastl::string forEndName = "for_end" + eastl::to_string(scope.forCount);
		SpiteIR::Label* forEndLabel = BuildLabel(forEndName);
		branch->branch.true_ = forLoopLabel;
		branch->branch.false_ = forEndLabel;
		scope.forCount++;
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
		SpiteIR::Instruction* store = BuildStore(toIncrement.type, label, allocate->allocate.result, amount);
		
		ScopeValue right = { allocate->allocate.result, toIncrement.type };
		return BuildBinaryOp(toIncrement, right, SpiteIR::BinaryOpKind::Add, label);
	}

	ScopeValue BuildExpr(Expr* expr)
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
			break;
		case FunctionCallExpr:
			return BuildFunctionCall(expr);
		case NewExpr:
			break;
		case FixedExpr:
			break;
		case TypeLiteralExpr:
			break;
		case ExplicitTypeExpr:
			break;
		case AsExpr:
			break;
		case DereferenceExpr:
			break;
		case ReferenceExpr:
			break;
		case BinaryExpr:
			return BuildBinaryExpression(expr);
		case UnaryExpr:
			break;
		case GroupedExpr:
			break;
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

			SpiteIR::Instruction* store = BuildStore(type, label, result, defaultOp);
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
		irType->primitive.isSigned = false;

		literalOp.type = irType;

		SpiteIR::Label* label = scope.function->block->labels.back();
		SpiteIR::Instruction* allocate = BuildAllocate(irType);
		SpiteIR::Instruction* store = BuildStore(irType, label, allocate->allocate.result, literalOp);
		return { store->store.dst, irType };
	}

	ScopeValue FindValueForIndent(Expr* expr)
	{
		StringView& ident = expr->identifierExpr.identifier->val;
		if (scope.scopeMap.find(ident) != scope.scopeMap.end())
		{
			return scope.scopeMap[ident];
		}

		return { 0, nullptr };
	}

	ScopeValue BuildBinaryExpression(Expr* expr)
	{
		Expr* left = expr->binaryExpr.left;
		Expr* right = expr->binaryExpr.right;
		SpiteIR::BinaryOpKind op = BinaryOpToIR(expr->binaryExpr.opType);

		ScopeValue leftVal = BuildExpr(left);
		ScopeValue rightVal = BuildExpr(right);

		if (!leftVal.type || !rightVal.type) return { 0, nullptr };

		if (leftVal.type->kind == SpiteIR::TypeKind::PrimitiveType &&
			rightVal.type->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return BuildBinaryOp(leftVal, rightVal, op, scope.function->block->labels.back());
		}

		return { 0, nullptr };
	}

	SpiteIR::Instruction* CreateInstruction(SpiteIR::Label* label)
	{
		SpiteIR::Instruction* inst = context.ir->AllocateInstruction();
		label->values.push_back(inst);
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
		binOp->binOp.result = scope.curr;
		
		ScopeValue value;
		if(kind >= SpiteIR::BinaryOpKind::LogicAnd) value = { binOp->binOp.result, &_boolType };
		else value = { binOp->binOp.result, leftVal.type };

		BuildAllocate(value.type);
		return value;
	}

	ScopeValue BuildFunctionCall(Expr* expr)
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

		if (!irFunction) return {0, nullptr};

		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		eastl::vector<Expr*>* exprParams = expr->functionCallExpr.params;
		for (size_t i = 0; i < exprParams->size(); i++)
		{
			Expr* param = exprParams->at(i);
			ScopeValue value = BuildExpr(param);
			params->push_back(BuildRegisterOperand(value));
		}

		SpiteIR::Instruction* call = BuildCall(irFunction, params, scope.function->block->labels.back());
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
			functionName = BuildTemplatedFunctionName(func, templates);
		}
		else
		{
			functionName = BuildFunctionName(func);
		}

		SpiteIR::Package* package = context.packageMap[packageName];
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
		allocate->allocate.result = scope.curr;

		scope.function->block->allocations.push_back(allocate);
		scope.IncrementRegister(type);
		return allocate;
	}

	SpiteIR::Instruction* BuildAllocateForType(Type* type)
	{
		SpiteIR::Type* irType = TypeToIRType(context.ir, type, this, currGenerics, currTemplates);
		return BuildAllocate(irType);
	}

	SpiteIR::Instruction* BuildStore(SpiteIR::Type* type, SpiteIR::Label* label, size_t dst,
		SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = CreateInstruction(label);
		store->kind = SpiteIR::InstructionKind::Store;
		store->store.dst = dst;
		store->store.src = src;

		return store;
	}

	SpiteIR::Operand BuildRegisterOperand(const ScopeValue& value)
	{
		SpiteIR::Operand operand = SpiteIR::Operand();
		operand.type = value.type;
		operand.kind = SpiteIR::OperandKind::Register;
		operand.reg = value.reg;
		return operand;
	}

	SpiteIR::Instruction* BuildCall(SpiteIR::Function* function, eastl::vector<SpiteIR::Operand>* params,
		SpiteIR::Label* label)
	{
		SpiteIR::Instruction* call = CreateInstruction(label);
		call->kind = SpiteIR::InstructionKind::Call;
		call->call.function = function;
		call->call.params = params;
		call->call.result = scope.curr;

		scope.IncrementRegister(function->returnType);
		return call;
	}

	SpiteIR::Instruction* BuildJump(SpiteIR::Label* label, SpiteIR::Label* to = nullptr)
	{
		SpiteIR::Instruction* jump = CreateInstruction(label);
		jump->kind = SpiteIR::InstructionKind::Jump;
		jump->jump.label = to;
		return jump;
	}

	SpiteIR::Instruction* BuildBranch(SpiteIR::Label* label, SpiteIR::Operand& test, 
		SpiteIR::Label* true_ = nullptr, SpiteIR::Label* false_ = nullptr)
	{
		SpiteIR::Instruction* branch = CreateInstruction(label);
		branch->kind = SpiteIR::InstructionKind::Branch;
		branch->branch.test = test;
		branch->branch.true_ = true_;
		branch->branch.false_ = false_;
		return branch;
	}
};