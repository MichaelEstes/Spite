#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"

struct LowerDefinitions
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;

	LowerDefinitions(GlobalTable* globalTable)
	{
		this->globalTable = globalTable;
	}

	void BuildMemberDefinition(SpiteIR::State* state, SpiteIR::Member* member, Stmnt* memberStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		Expr* assignment = memberStmnt->definition.assignment;
		if (!assignment) return;

		BuildValueSet(member->name, assignment, member->values);
	}

	void BuildValueSet(eastl::string& initialName, Expr* expr, eastl::vector<SpiteIR::Value*>& values)
	{
		eastl::string currentName = initialName + eastl::to_string(values.size());

		switch (expr->typeID)
		{
		case InvalidExpr:
			// AddError
			break;
		case LiteralExpr:
			values.push_back(BuildValue(currentName, expr));
			break;
		case IdentifierExpr:
			values.push_back(BuildValue(currentName, expr));
			break;
		case PrimitiveExpr:
			values.push_back(BuildValue(currentName, expr));
			break;
		case SelectorExpr:
			values.push_back(BuildValue(currentName, expr));
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			break;
		case NewExpr:
			break;
		case FixedExpr:
			break;
		case AnonTypeExpr:
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
		default:
			break;
		}
	}

	SpiteIR::Value* BuildValue(eastl::string& name, Expr* expr)
	{

	}

	void BuildMethodDefinition(SpiteIR::State* state, SpiteIR::Function* method, Stmnt* methodStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{

	}

	void BuildFunctionDefinition(SpiteIR::Function* func, Stmnt* funcStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{

	}
	
};