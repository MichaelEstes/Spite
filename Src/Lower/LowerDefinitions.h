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