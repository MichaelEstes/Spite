#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerContext.h"

struct LowerDefinitions
{
	LowerContext& context;
	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolve;

	LowerDefinitions(LowerContext& context): context(context)
	{}

	void BuildDefinitions()
	{
		for (SpiteIR::Package* package : context.ir->packages)
		{
			for (auto& [key, state] : package->states)
			{
				ASTContainer& stateContainer = context.stateASTMap[state];
				BuildStateDefault(state, stateContainer.node, stateContainer.templates);
			}

			for (auto& [key, function] : package->functions)
			{
				ASTContainer& funcContainer = context.functionASTMap[function];
				BuildFunction(function, funcContainer.node, funcContainer.templates);
			}
		}
	}
	
	void BuildStateDefault(SpiteIR::State* state, Stmnt* stateStmnt, eastl::vector<Expr*>* templates = nullptr)
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

	void BuildFunction(SpiteIR::Function* function, Stmnt* funcStmnt, eastl::vector<Expr*>* templates = nullptr)
	{
		Stmnt* decl = GetDeclForFunc(funcStmnt);
		if (!decl)
		{
			AddError(funcStmnt->start, "LowerDefinitions:BuildEntryBlock No declaration found for function");
			return;
		}
		BuildEntryBlock(function, funcStmnt, decl, templates);
	}

	void BuildEntryBlock(SpiteIR::Function* function, Stmnt* funcStmnt, Stmnt* funcDecl,
		eastl::vector<Expr*>* templates = nullptr)
	{
		auto& decl = funcDecl->functionDecl;
		SpiteIR::Block& block = function->blocks.emplace_back();
		block.parent = function;
		block.name = "entry";
		eastl::hash_map<StringView, size_t, StringViewHash> scopeMap;
		size_t reg = 0;

		auto& body = decl.body;
		if (body.body->nodeID == StmntID::Block)
		{
			for (Stmnt* stmnt : *decl.body.body->block.inner)
			{
				BuildStmntForBlock(stmnt, block, reg, scopeMap);
			}
		}
		else
		{
			BuildStmntForBlock(decl.body.body, block, reg, scopeMap);
		}
	}

	void BuildStmntForBlock(Stmnt* stmnt, SpiteIR::Block& block, size_t& reg, 
		eastl::hash_map<StringView, size_t, StringViewHash>& scopeMap)
	{
		eastl::vector<SpiteIR::Instruction>& values = block.values;

		switch (stmnt->nodeID)
		{
		case ExpressionStmnt:
			break;
		case Definition:
			BuildVarDefinition(stmnt, block, reg, scopeMap);
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

	void BuildVarDefinition(Stmnt* stmnt, SpiteIR::Block& block, size_t& reg,
		eastl::hash_map<StringView, size_t, StringViewHash>& scopeMap)
	{
		auto& def = stmnt->definition;
		BuildAllocate(def.type, block, reg);
	}

	SpiteIR::Instruction* BuildAllocate(Type* type, SpiteIR::Block& block, size_t& reg)
	{
		SpiteIR::Instruction* allocate = context.ir->AllocateInstruction();
		allocate->kind = SpiteIR::InstructionKind::Allocate;
		SpiteIR::Type* irType = TypeToIRType(context.ir, type, this);

		return allocate;
	}

	void BuildExpr(Expr* expr, SpiteIR::Block& block, size_t& reg,
		eastl::hash_map<StringView, size_t, StringViewHash>& scopeMap)
	{
		switch (expr->typeID)
		{
		case LiteralExpr:
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
};