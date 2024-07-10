#pragma once
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerDeclarations.h"

struct Lower
{
	SpiteIR::IR* ir;
	LowerContext context;


	Lower(GlobalTable* globalTable)
	{
		context.globalTable = globalTable;
		this->ir = new SpiteIR::IR(globalTable->GetSize());
		context.ir = this->ir;
	}

	SpiteIR::IR* BuildIR(SymbolTable* entry)
	{
		LowerDeclarations lowerDecl = LowerDeclarations(context);
		//lowerDecl.BuildDeclarations(entry);

		for (auto& [key, value] : context.globalTable->packageToSymbolTable)
		{
			lowerDecl.BuildDeclarations(value);
		}

		LowerDefinitions lowerDef = LowerDefinitions(context);
		lowerDef.BuildDefinitions();

		return ir;
	}
};