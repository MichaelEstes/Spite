#pragma once

#include "llvm/IR/Dominators.h"
#include "llvm/Transforms/Utils.h"
#include "./LLVMContext.h"

struct LLVMOptimize
{
	LLVMContext& llvmContext;

	llvm::Module& module;

	LLVMOptimize(LLVMContext& llvmContext) : llvmContext(llvmContext),
		module(llvmContext.module)
	{}

	void Optimize()
	{
		llvm::legacy::FunctionPassManager passManager(&module);
		passManager.add(llvm::createPromoteMemoryToRegisterPass());

		for (llvm::Function& func : module) 
		{
			if (!func.isDeclaration()) 
			{ 
				passManager.run(func);
			}
		}
	}
};