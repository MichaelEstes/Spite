#pragma once
#include "../Syntax/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerDeclarations.h"

struct Lower
{
	SpiteIR::IR* ir;
	LowerContext context;

	Lower(GlobalTable* globalTable, SpiteIR::IR* ir, Interpreter* interpreter)
	{
		this->ir = ir;
		context.globalTable = globalTable;
		context.interpreter = interpreter;
		context.ir = this->ir;
	}

	void BuildIR(SymbolTable* entry)
	{
		LowerDeclarations lowerDecl = LowerDeclarations(context);
		SpiteIR::Package* runtime = lowerDecl.BuildPackageDeclarations(context.globalTable->runtimeTable);
		ir->SetRuntimePackage(runtime);
		SpiteIR::Package* entryPkg = lowerDecl.BuildPackageDeclarations(entry);
		lowerDecl.Resolve();

		LowerDefinitions lowerDef = LowerDefinitions(context);
		lowerDef.BuildPackageDefinitions(runtime);
		lowerDef.BuildPackageDefinitions(entryPkg);

		Stmnt* entryFunc = context.globalTable->entryFunc;
		eastl::string entryFuncName = BuildFunctionName(entryFunc);
		ir->entry = lowerDef.FindFunction(entryFunc->package->val, entryFuncName);
	}
};