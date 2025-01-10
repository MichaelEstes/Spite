#pragma once
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/MC/TargetRegistry.h"

#include "../../IR/IR.h"
#include "../../Lower/LowerUtils.h"
#include "./LLVMTypes.h"

extern Config config;
extern SpiteIR::State* stringState;
extern SpiteIR::State* arrayState;

struct LLVMContext
{
	SpiteIR::IR* ir;

	eastl::hash_map<SpiteIR::Function*, llvm::Function*> functionMap;
	eastl::hash_map<size_t, llvm::GlobalVariable*> globalVarMap;
	eastl::hash_map<size_t, llvm::Value*> localVarMap;
	eastl::hash_map<SpiteIR::Label*, llvm::BasicBlock*> labelMap;
	eastl::hash_map<eastl::string, llvm::Function*> externalFunctionMap;
	eastl::hash_map<SpiteIR::Type*, llvm::GlobalVariable*, IRTypeHash, IRTypeEqual> typeMetadataMap;
	Arena arena;

	llvm::LLVMContext context;
	llvm::IRBuilder<> builder;
	llvm::Module module;

	llvm::Type* boolType;
	llvm::IntegerType* int32Type;
	llvm::Type* intType;
	llvm::StructType* strType;

	LLVMContext(SpiteIR::IR* ir) : builder(context), module(ToStringRef(config.name), context)
	{
		this->ir = ir;
		boolType = llvm::IntegerType::getInt1Ty(context);
		intType = ToLLVMType(CreateIntType(ir), context);
		int32Type = llvm::IntegerType::getInt32Ty(context);
		strType = StateToLLVMType(stringState, context);
	}

	llvm::Twine GlobalVariableName(SpiteIR::GlobalVariable* globalVar)
	{
		eastl::string* name = arena.EmplaceScalar<eastl::string>();
		*name = "g" + eastl::to_string(globalVar->index);
		return ToTwine(*name);
	}

	llvm::Twine RegisterName(size_t reg)
	{
		eastl::string* name = arena.EmplaceScalar<eastl::string>();
		*name = "r" + eastl::to_string(reg);
		return ToTwine(*name);
	}

	llvm::GlobalVariable* BuildTypeValue(SpiteIR::Type* type)
	{
		if (MapHas(typeMetadataMap, type))
		{
			return typeMetadataMap[type];
		}

		llvm::StructType* llvmType = StateToLLVMType(typeMetaState, context);
		llvm::GlobalVariable* typeVar = new llvm::GlobalVariable(
			module,
			llvmType,
			false,
			llvm::GlobalValue::ExternalLinkage,
			llvm::UndefValue::get(llvmType),
			""
		);

		typeMetadataMap[type] = typeVar;
		return typeVar;
	}

	void BuildGEPInst(llvm::Type* type, llvm::Value* src, llvm::Value* index, llvm::Value* dst,
		bool inBounds)
	{
		llvm::Value* zero = llvm::ConstantInt::get(int32Type, 0);
		llvm::Value* gepValue = builder.CreateGEP(type, src, { zero, index }, "", inBounds);
		builder.CreateStore(gepValue, dst);
	}
};