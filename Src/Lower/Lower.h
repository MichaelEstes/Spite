#pragma once
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerDeclarations.h"

struct Lower
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable = nullptr;
	SpiteIR::IR* ir;

	eastl::hash_map<eastl::string, SpiteIR::Package*> packageMap;

	Lower(GlobalTable* globalTable)
	{
		this->globalTable = globalTable;
		this->ir = new SpiteIR::IR(globalTable->GetSize());
	}

	SpiteIR::IR* BuildIR()
	{
		LowerDeclarations lowerDecl = LowerDeclarations(globalTable, ir);
		for (auto& [key, value] : globalTable->packageToSymbolTable)
		{
			symbolTable = value;
			SpiteIR::Package* package = ir->AddPackage();

			package->file = *symbolTable->package->pos.file;
			package->name = BuildPackageName(symbolTable->package);
			package->parent = ir;

			packageMap[package->name] = package;
			lowerDecl.BuildDeclarations(package, symbolTable);
		}

		return ir;
	}
};