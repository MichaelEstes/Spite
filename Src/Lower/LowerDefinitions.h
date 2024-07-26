#include "EASTL/deque.h"
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerContext.h"
#include "../IR/Interpreter/Interpreter.h"

extern Config config;

struct BlockScope
{
	eastl::hash_map<StringView, size_t, StringViewHash> scopeMap;
	eastl::vector<Expr*> deferred;
	eastl::vector<size_t> toDestroy;
	size_t prevReg = 0;
	size_t curr = 0;

	void IncrementRegister(size_t amount)
	{
		prevReg = curr;
		curr += amount;
	}
};

struct LowerDefinitions
{
	LowerContext& context;
	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolve;
	eastl::deque<BlockScope> scopeQueue;
	SymbolTable* symbolTable = nullptr;
	eastl::vector<Expr*>* currTemplates = nullptr;
	
	
	Interpreter interpreter;

	LowerDefinitions(LowerContext& context): context(context), interpreter(2000000)
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
		default:
			break;
		}

		return nullptr;
	}

	void BuildFunction(SpiteIR::Function* function, Stmnt* funcStmnt)
	{
		Stmnt* decl = GetDeclForFunc(funcStmnt);
		if (!decl)
		{
			AddError(funcStmnt->start, "LowerDefinitions:BuildEntryBlock No declaration found for function");
			return;
		}
		BuildEntryBlock(function, funcStmnt, decl);

		for (SpiteIR::Block* block : function->blocks)
		{
			interpreter.InterpretBlock(block);
		}
	}

	void BuildEntryBlock(SpiteIR::Function* function, Stmnt* funcStmnt, Stmnt* funcDecl)
	{
		auto& decl = funcDecl->functionDecl;
		SpiteIR::Block* block = context.ir->AllocateBlock();
		function->blocks.push_back(block);
		block->parent = function;
		block->name = "entry";
		scopeQueue.emplace_back();
		BlockScope& scope = scopeQueue.back();

		auto& body = decl.body;
		if (body.body->nodeID == StmntID::Block)
		{
			for (Stmnt* stmnt : *decl.body.body->block.inner)
			{
				BuildStmntForBlock(stmnt, block, scope);
			}
		}
		else
		{
			BuildStmntForBlock(decl.body.body, block, scope);
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
		BuildExpr(def.assignment, block, scope);
		scope.scopeMap[def.name->val] = scope.prevReg;
	}

	void BuildExpr(Expr* expr, SpiteIR::Block* block, BlockScope& scope)
	{
		switch (expr->typeID)
		{
		case LiteralExpr:
			BuildLiteral(expr, block, scope);
			break;
		case IdentifierExpr:
			break;
		case PrimitiveExpr:
			break;
		case SelectorExpr:
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			BuildFunctionCall(expr, block, scope);
			break;
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
			break;
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
	}

	void BuildLiteral(Expr* expr, SpiteIR::Block* block, BlockScope& scope)
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
			literal.kind = SpiteIR::PrimitiveKind::Int;
			literal.intLiteral = 1;
			break;
		case UniqueType::FalseLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Int;
			literal.intLiteral = 0;
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
	}

	void BuildFunctionCall(Expr* expr, SpiteIR::Block* block, BlockScope& scope)
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

		//Assert(irFunction);

		eastl::vector<size_t>* params = context.ir->AllocateArray<size_t>();
		eastl::vector<Expr*>* exprParams = expr->functionCallExpr.params;
		for (size_t i = 0; i < exprParams->size(); i++)
		{
			Expr* param = exprParams->at(i);
			BuildExpr(param, block, scope);
			params->push_back(scope.prevReg);
		}

		BuildCall(irFunction, params, block);
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

	SpiteIR::Function* FindFunctionForFunctionCall(Expr* expr)
	{
		Expr* caller = expr->functionCallExpr.function;
		Stmnt* stmnt = FindFunctionStmnt(caller);
		StringView& packageName = stmnt->package->val;
		eastl::string functionName;
		if (caller->typeID == ExprID::TemplateExpr)
		{
			functionName = BuildTemplatedFunctionName(stmnt, caller->templateExpr.templateArgs);
		}
		else
		{
			functionName = BuildFunctionName(stmnt);
		}

		SpiteIR::Package* package = context.packageMap[packageName];
		SpiteIR::Function* function = package->functions[functionName];
		
		Assert(function);
		return function;
	}

	SpiteIR::Instruction& BuildAllocate(SpiteIR::Type* type, SpiteIR::Block* block, BlockScope& scope)
	{
		size_t result = scope.curr;
		SpiteIR::Instruction& allocate = block->values.emplace_back();
		allocate.kind = SpiteIR::InstructionKind::Allocate;
		allocate.allocate.type = type;
		allocate.allocate.result = result;

		scope.IncrementRegister(type->size);

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

	SpiteIR::Instruction& BuildCall(SpiteIR::Function* function, eastl::vector<size_t>* params,
		SpiteIR::Block* block)
	{
		SpiteIR::Instruction& call = block->values.emplace_back();
		call.kind = SpiteIR::InstructionKind::Call;
		call.call.function = function;
		call.call.params = params;
		return call;
	}
};