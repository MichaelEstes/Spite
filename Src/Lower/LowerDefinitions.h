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

struct BlockScope
{
	SpiteIR::Block* block;
	eastl::hash_map<StringView, ScopeValue, StringViewHash> scopeMap;
	eastl::vector<Expr*> deferred;
	eastl::vector<size_t> toDestroy;
	size_t curr = 0;

	void IncrementRegister(SpiteIR::Type* type)
	{
		curr += type->size;
	}
};

struct FunctionContext
{
	size_t forCount = 0;
};

struct LowerDefinitions
{
	LowerContext& context;
	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolve;
	eastl::deque<BlockScope> scopeQueue;
	eastl::hash_map<SpiteIR::Function*, FunctionContext> funcContextMap;
	SymbolTable* symbolTable = nullptr;
	eastl::vector<Expr*>* currTemplates = nullptr;
	
	LowerDefinitions(LowerContext& context): context(context)
	{}

	void BuildDefinitions()
	{
		for (SpiteIR::Package* package : context.ir->packages)
		{
			symbolTable = context.packageToSymbolTableMap[package];
			for (auto& [key, state] : package->states)
			{
				ASTContainer& stateContainer = context.stateASTMap[state];
				currTemplates = stateContainer.templates;
				BuildStateDefault(state, stateContainer.node);
			}

			for (auto& [key, function] : package->functions)
			{
				ASTContainer& funcContainer = context.functionASTMap[function];
				currTemplates = funcContainer.templates;
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

		funcContextMap[function] = FunctionContext();
		BuildEntryBlock(function, funcStmnt, decl);
	}

	void BuildEntryBlock(SpiteIR::Function* function, Stmnt* funcStmnt, Stmnt* funcDecl)
	{
		auto& decl = funcDecl->functionDecl;
		Body& body = decl.body;
		BuildBodyBlock("entry", function, body);
	}

	SpiteIR::Block* BuildBlock(const eastl::string& name, SpiteIR::Function* function)
	{
		SpiteIR::Block* block = context.ir->AllocateBlock();
		block->name = name;
		block->parent = function;
		function->blocks[block->name] = block;

		return block;
	}

	void BuildBodyBlock(const eastl::string& name, SpiteIR::Function* function, Body& body)
	{
		SpiteIR::Block* block = BuildBlock(name, function);

		scopeQueue.emplace_back();
		BlockScope& scope = scopeQueue.back();
		scope.block = block;

		if (body.body->nodeID == StmntID::Block)
		{
			for (Stmnt* stmnt : *body.body->block.inner)
			{
				BuildStmntForBlock(stmnt, block, scope);
			}
		}
		else
		{
			BuildStmntForBlock(body.body, block, scope);
		}
	}

	void BuildStmntForBlock(Stmnt* stmnt, SpiteIR::Block* block, BlockScope& scope)
	{
		eastl::vector<SpiteIR::Instruction>& values = block->values;

		switch (stmnt->nodeID)
		{
		case ExpressionStmnt:
			BuildExpr(stmnt->expressionStmnt.expression, block, scope);
			break;
		case Definition:
			BuildVarDefinition(stmnt, block, scope);
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
			BuildForStmnt(stmnt, block, scope);
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

	void BuildVarDefinition(Stmnt* stmnt, SpiteIR::Block* block, BlockScope& scope)
	{
		auto& def = stmnt->definition;
		ScopeValue value = BuildExpr(def.assignment, block, scope);
		scope.scopeMap[def.name->val] = value;
	}

	void BuildForStmnt(Stmnt* stmnt, SpiteIR::Block* block, BlockScope& scope)
	{
		Assert(stmnt->forStmnt.isDeclaration);
		Assert(block->parent.kind == SpiteIR::ParentKind::Function);
		SpiteIR::Function* func = block->parent.functionParent;
		FunctionContext& funcContext = funcContextMap[func];

		auto& for_ = stmnt->forStmnt;

		SpiteIR::Block* start = BuildForStart(stmnt, func, funcContext.forCount);
	}

	SpiteIR::Block* BuildForStart(Stmnt* stmnt, SpiteIR::Function* function, size_t index)
	{
		eastl::string forStartName = "for_start" + eastl::to_string(index);
		SpiteIR::Block* block = BuildBlock(forStartName, function);
	}

	ScopeValue BuildExpr(Expr* expr, SpiteIR::Block* block, BlockScope& scope)
	{
		switch (expr->typeID)
		{
		case LiteralExpr:
			return BuildLiteral(expr, block, scope);
		case IdentifierExpr:
			return FindValueForIndent(expr, block, scope);
		case PrimitiveExpr:
			break;
		case SelectorExpr:
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			return BuildFunctionCall(expr, block, scope);
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
			return BuildBinaryExpression(expr, block, scope);
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

	ScopeValue BuildLiteral(Expr* expr, SpiteIR::Block* block, BlockScope& scope)
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

		SpiteIR::Instruction& allocate = BuildAllocate(irType, block, scope);
		SpiteIR::Instruction& store = BuildStore(irType, block, allocate.allocate.result, literalOp);
		return { store.store.dst, irType };
	}

	ScopeValue FindValueForIndent(Expr* expr, SpiteIR::Block* block, BlockScope& scope)
	{
		StringView& ident = expr->identifierExpr.identifier->val;
		if (scope.scopeMap.find(ident) != scope.scopeMap.end())
		{
			return scope.scopeMap[ident];
		}

		return { 0, nullptr };
	}

	ScopeValue BuildBinaryExpression(Expr* expr, SpiteIR::Block* block, BlockScope& scope)
	{
		Expr* left = expr->binaryExpr.left;
		Expr* right = expr->binaryExpr.right;
		SpiteIR::BinaryOpKind op = BinaryOpToIR(expr->binaryExpr.opType);

		ScopeValue leftVal = BuildExpr(left, block, scope);
		ScopeValue rightVal = BuildExpr(right, block, scope);

		if (!leftVal.type || !rightVal.type) return { 0, nullptr };

		if (leftVal.type->kind == SpiteIR::TypeKind::PrimitiveType &&
			rightVal.type->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return BuildBinaryOp(leftVal, rightVal, op, block, scope);
		}

		return { 0, nullptr };
	}

	ScopeValue BuildBinaryOp(ScopeValue leftVal, ScopeValue rightVal, SpiteIR::BinaryOpKind kind,
		SpiteIR::Block* block, BlockScope& scope)
	{
		SpiteIR::Instruction& binOp = block->values.emplace_back();
		binOp.kind = SpiteIR::InstructionKind::BinOp;
		binOp.binOp.kind = kind;
		binOp.binOp.left = BuildRegisterOperand(leftVal.reg, leftVal.type);
		binOp.binOp.right = BuildRegisterOperand(rightVal.reg, rightVal.type);
		binOp.binOp.result = scope.curr;
		
		scope.IncrementRegister(leftVal.type);
		return { binOp.binOp.result, leftVal.type };
	}

	ScopeValue BuildFunctionCall(Expr* expr, SpiteIR::Block* block, BlockScope& scope)
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
			ScopeValue value = BuildExpr(param, block, scope);
			params->push_back(BuildRegisterOperand(value.reg, value.type));
		}

		SpiteIR::Instruction& call = BuildCall(irFunction, params, block, scope);
		return { call.call.result, irFunction->returnType };
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

	SpiteIR::Instruction& BuildAllocate(SpiteIR::Type* type, SpiteIR::Block* block, BlockScope& scope)
	{
		SpiteIR::Instruction& allocate = block->values.emplace_back();
		allocate.kind = SpiteIR::InstructionKind::Allocate;
		allocate.allocate.type = type;
		allocate.allocate.result = scope.curr;

		scope.IncrementRegister(type);
		return allocate;
	}

	SpiteIR::Instruction& BuildAllocateForType(Type* type, SpiteIR::Block* block, BlockScope& scope)
	{
		SpiteIR::Type* irType = TypeToIRType(context.ir, type, this);
		return BuildAllocate(irType, block, scope);
	}

	SpiteIR::Instruction& BuildStore(SpiteIR::Type* type, SpiteIR::Block* block, size_t dst,
		SpiteIR::Operand& src)
	{
		SpiteIR::Instruction& store = block->values.emplace_back();
		store.kind = SpiteIR::InstructionKind::Store;
		store.store.dst = dst;
		store.store.src = src;

		return store;
	}

	SpiteIR::Operand BuildRegisterOperand(size_t reg, SpiteIR::Type* type)
	{
		SpiteIR::Operand operand = SpiteIR::Operand();
		operand.type = type;
		operand.kind = SpiteIR::OperandKind::Register;
		operand.reg = reg;
		return operand;
	}

	SpiteIR::Instruction& BuildCall(SpiteIR::Function* function, eastl::vector<SpiteIR::Operand>* params,
		SpiteIR::Block* block, BlockScope& scope)
	{
		SpiteIR::Instruction& call = block->values.emplace_back();
		call.kind = SpiteIR::InstructionKind::Call;
		call.call.function = function;
		call.call.params = params;
		call.call.result = scope.curr;

		scope.IncrementRegister(function->returnType);
		return call;
	}
};