#pragma once

#include "llvm/IR/Function.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Verifier.h"

#include "../../Syntax/SymbolTable.h"
#include "../../Config/Config.h"

extern Config config;

using LType = llvm::Type;
using LLVMContext = llvm::LLVMContext;
using Module = llvm::Module;
using IRBuilder = llvm::IRBuilder<>;
using StructType = llvm::StructType;
using LPointerType = llvm::PointerType;
using LFunctionType = llvm::FunctionType;
using Function = llvm::Function;
using Argument = llvm::Argument;
using Constant = llvm::Constant;
using ConstantInt = llvm::ConstantInt;
using ConstantFloat = llvm::ConstantFP;
using ConstantExpr = llvm::ConstantExpr;
using GlobalVariable = llvm::GlobalVariable;
using GlobalValue = llvm::GlobalValue;
using BasicBlock = llvm::BasicBlock;
using AllocaInst = llvm::AllocaInst;
using Value = llvm::Value;
using StringRef = llvm::StringRef;

struct LPrimitives
{
	LType* voidType;

	LType* boolType;
	LType* byteType;

	LType* int16Type;
	LType* int32Type;
	LType* int64Type;
	LType* int128Type;
	LType* intType;

	LType* float32Type;
	LType* float64Type;
	LType* float128Type;
	LType* floatType;

	LPrimitives(LLVMContext& context)
	{
		voidType = LType::getVoidTy(context);

		boolType = LType::getInt1Ty(context);
		byteType = LType::getInt8Ty(context);

		int16Type = LType::getInt16Ty(context);
		int32Type = LType::getInt32Ty(context);
		int64Type = LType::getInt64Ty(context);
		int128Type = LType::getInt128Ty(context);
		intType = config.targetArchByteWidth == 64 ? int64Type : int32Type;

		float32Type = LType::getFloatTy(context);
		float64Type = LType::getDoubleTy(context);
		float128Type = LType::getFP128Ty(context);
		floatType = config.targetArchByteWidth == 64 ? float64Type : float32Type;
	}
};

struct LLVMBuilder
{
	SymbolTable* symbolTable;
	LLVMContext context;
	IRBuilder* builder;
	Module* module;
	LPrimitives primitives;
	eastl::hash_map<StringView, AllocaInst*, StringViewHash> localVariableMap;

	LLVMBuilder(SymbolTable* symbolTable) : primitives(context)
	{
		this->symbolTable = symbolTable;
		//module = new Module(ToStringRef(symbolTable->package->val), context);
		//builder = new IRBuilder(context);
	}

	~LLVMBuilder()
	{
		delete module;
		delete builder;
	}

	void Build()
	{
		module->print(llvm::outs(), nullptr);
	}

	template<typename T>
	inline llvm::ArrayRef<T> ToArrayRef(const eastl::vector<T>& vec)
	{
		return llvm::ArrayRef<T>(vec.begin(), vec.end());
	}
};