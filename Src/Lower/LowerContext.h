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
	eastl::hash_map<Stmnt*, eastl::hash_map<StringView, intmax_t, StringViewHash>> enumMap;

	eastl::vector<eastl::tuple<eastl::string, SpiteIR::Type*>> toResolveStateType;
	eastl::hash_set<SpiteIR::Type*> toResolveSizeAndAlignment;
	eastl::hash_set<SpiteIR::Type*> toResolveFunctionType;

	Interpreter* interpreter = nullptr;

	SpiteIR::State* FindState(const eastl::string& val)
	{
		if (auto entry = stateMap.find(val); entry != stateMap.end())
		{
			return entry->second;
		}

		return nullptr;
	}

	inline intmax_t FindEnumValue(Stmnt* enumStmnt, StringView& val)
	{
		Assert(MapHas(enumMap, enumStmnt));
		eastl::hash_map<StringView, intmax_t, StringViewHash>& enumValues = enumMap[enumStmnt];
		Assert(MapHas(enumValues, val));
		return enumValues[val];
	}
};