#pragma once

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/ADT/StringRef.h"

#include "../../Intermediate/Syntax.h"

using LType = llvm::Type;
using namespace llvm;
struct LLVMBuilder
{
	Syntax& syntax;
	LLVMContext context;
	IRBuilder<> builder;
	Module* module;

	LLVMBuilder(Syntax& syntax) : syntax(syntax), builder(IRBuilder<>(context))
	{
		module = new Module(ToStringRef(syntax.package->package.name->val), context);
	}

	~LLVMBuilder()
	{
		delete module;
	}

	void Build()
	{
		BuildGlobals();

		module->print(outs(), nullptr);
	}

	void BuildTypes()
	{

	}

	void BuildGlobals()
	{
		for (auto& [key, value] : syntax.symbolTable->globalValMap)
		{
			LType* int32Type = LType::getInt32Ty(context);
			Constant* initialValue = ConstantInt::get(int32Type, 42);
			GlobalVariable* globalVar = new llvm::GlobalVariable(
				*module,
				int32Type,
				false,
				llvm::GlobalValue::ExternalLinkage,
				initialValue,
				ToStringRef(value->definition.name->val)
			);
		}
	}

	inline StringRef ToStringRef(InplaceString& str)
	{
		return StringRef(str.start, str.count);
	}

};