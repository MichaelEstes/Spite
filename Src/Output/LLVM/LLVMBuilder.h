#pragma once

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/StringRef.h"

#include "../../Intermediate/Syntax.h"

struct LLVMBuilder
{
	Syntax& syntax;
	llvm::LLVMContext context;
	llvm::IRBuilder<> builder;
	llvm::Module* module;

	LLVMBuilder(Syntax& syntax) : syntax(syntax), builder(context)
	{
		module = new llvm::Module(ToStringRef(syntax.package->package.name->val), context);
	}

	~LLVMBuilder()
	{
		delete module;
	}

	inline llvm::StringRef ToStringRef(InplaceString& str)
	{
		return llvm::StringRef(str.start, str.count);
	}

};