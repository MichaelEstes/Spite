#pragma once
#include "../Syntax/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerDeclarations.h"

struct Lower
{
	SpiteIR::IR* ir;
	LowerContext context;

	Lower(GlobalTable* globalTable, Interpreter* interpreter)
	{
		context.globalTable = globalTable;
		context.interpreter = interpreter;
		this->ir = new SpiteIR::IR(globalTable->GetSize());
		context.ir = this->ir;
	}

	SpiteIR::IR* BuildIR(SymbolTable* entry)
	{
		LowerDeclarations lowerDecl = LowerDeclarations(context);
		SpiteIR::Package* runtime = lowerDecl.BuildPackageDeclarations(context.globalTable->runtimeTable);
		ir->SetRuntimePackage(runtime);
		//lowerDecl.BuildDeclarations();
		SpiteIR::Package* entryPkg = lowerDecl.BuildPackageDeclarations(entry);

		LowerDefinitions lowerDef = LowerDefinitions(context);
		//lowerDef.BuildDefinitions();
		lowerDef.BuildPackageDefinitions(runtime);
		lowerDef.BuildPackageDefinitions(entryPkg);

		Stmnt* entryFunc = context.globalTable->entryFunc;
		eastl::string entryFuncName = BuildFunctionName(entryFunc);
		ir->entry = lowerDef.FindFunction(entryFunc->package->val, entryFuncName);
		return ir;
	}
};