#pragma once
#include "../Syntax/GlobalTable.h"
#include "../IR/IR.h"
#include "../IR/Interpreter/Interpreter.h"

struct ASTContainer
{
	Stmnt* node;
	eastl::vector<Expr*>* templates = nullptr;
};

struct LowerContext
{
	GlobalTable* globalTable;
	SpiteIR::IR* ir;

	eastl::hash_map<SpiteIR::Package*, SymbolTable*> packageToSymbolTableMap;
	eastl::hash_map<StringView, SpiteIR::Package*, StringViewHash> packageMap;
	eastl::hash_map<eastl::string, SpiteIR::State*> stateMap;
	eastl::hash_map<eastl::string, SpiteIR::Function*> functionMap;
	eastl::hash_map<SpiteIR::State*, ASTContainer> stateASTMap;
	eastl::hash_map<SpiteIR::Function*, ASTContainer> functionASTMap;
	eastl::hash_map<SpiteIR::GlobalVariable*, Stmnt*> globalVarASTMap;

	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolveStateType;
	eastl::vector<SpiteIR::Type*> toResolveStateSize;
	eastl::vector<SpiteIR::Type*> toResolveSizeAndAlignment;

	Interpreter* interpreter = nullptr;

	SpiteIR::State* FindState(const eastl::string& val)
	{
		if (auto entry = stateMap.find(val); entry != stateMap.end())
		{
			return entry->second;
		}

		return nullptr;
	}
};