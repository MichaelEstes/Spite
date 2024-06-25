#pragma once
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerDeclarations.h"

struct Lower
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable = nullptr;
	SpiteIR::IR* ir;


	Lower(GlobalTable* globalTable)
	{
		this->globalTable = globalTable;
		this->ir = new SpiteIR::IR(globalTable->GetSize());
	}

	SpiteIR::IR* BuildIR(SymbolTable* entry)
	{
		LowerDeclarations lowerDecl = LowerDeclarations(globalTable, ir);

		lowerDecl.BuildDeclarations(entry);

		/*for (auto& [key, value] : globalTable->packageToSymbolTable)
		{
			lowerDecl.BuildDeclarations(value);
		}*/

		return ir;
	}
};