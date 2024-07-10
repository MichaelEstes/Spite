#pragma once
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"

struct ASTContainer
{
	Stmnt* node;
	eastl::vector<Expr*>* templates = nullptr;
};

struct LowerContext
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable = nullptr;
	SpiteIR::IR* ir;

	eastl::hash_map<eastl::string, SpiteIR::Package*> packageMap;
	eastl::hash_map<eastl::string, SpiteIR::State*> stateMap;
	eastl::hash_map<eastl::string, SpiteIR::Function*> functionMap;
	eastl::hash_map<SpiteIR::State*, ASTContainer> stateASTMap;
	eastl::hash_map<SpiteIR::Function*, ASTContainer> functionASTMap;

	SpiteIR::State* FindState(const eastl::string& val)
	{
		if (auto entry = stateMap.find(val); entry != stateMap.end())
		{
			return entry->second;
		}

		return nullptr;
	}
};