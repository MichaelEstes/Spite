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
#include "llvm/ADT/SmallString.h"

#include "../../Config/Config.h"
#include "./LLVMTypes.h"
#include "../../IR/IR.h"

extern Config config;

struct LLVMBuilder
{
	SpiteIR::IR* ir;

	eastl::hash_map<SpiteIR::Function*, llvm::Function*> functionMap;
	Arena arena;

	llvm::LLVMContext context;
	llvm::IRBuilder<> builder;
	llvm::Module module;

	LLVMBuilder(SpiteIR::IR* ir) : builder(context), module(ToStringRef(config.name), context)
	{
		this->ir = ir;
		SetTypedStateValues(ir);
	}

	void Build()
	{
		BuildPackageDeclarations(ir->runtime);
		for (SpiteIR::Package* package : ir->packages)
			BuildPackageDeclarations(package);

		BuildPackage(ir->runtime);
		for (SpiteIR::Package* package : ir->packages)
			BuildPackage(package);

		module.print(llvm::outs(), nullptr);
	}

	void BuildPackageDeclarations(SpiteIR::Package* package)
	{
		for (auto& [key, state] : package->states)
		{
			BuildState(state);
		}

		for (SpiteIR::GlobalVariable* globalVar : package->globalVariables)
		{
			BuildGlobalVariable(globalVar);
		}

		if (package->initializer)
		{
			BuildFunctionDeclaration(package->initializer);
		}

		for (auto& [key, function] : package->functions)
		{
			BuildFunctionDeclaration(function);
		}
	}

	void BuildState(SpiteIR::State* state)
	{
		StateToLLVMType(state, context);
	}

	llvm::Twine GlobalVariableName(SpiteIR::GlobalVariable* globalVar)
	{
		eastl::string* name = arena.EmplaceScalar<eastl::string>();
		*name = "g" + eastl::to_string(globalVar->index);
		return ToTwine(*name);
	}

	void BuildGlobalVariable(SpiteIR::GlobalVariable* globalVar)
	{
		llvm::GlobalVariable* llvmGlobalVar = new llvm::GlobalVariable(
			module,
			ToLLVMType(globalVar->type, context),
			false,
			llvm::GlobalValue::ExternalLinkage,
			nullptr,
			GlobalVariableName(globalVar)
		);
	}

	void BuildFunctionDeclaration(SpiteIR::Function* function)
	{
		llvm::Function* llvmFunc = llvm::Function::Create(
			FunctionToLLVMType(function, context),
			llvm::Function::ExternalLinkage,
			ToTwine(function->name),
			module
		);

		if (function->metadata.flags & (1 << SpiteIR::FunctionFlags::Inline))
		{
			llvmFunc->addFnAttr(llvm::Attribute::InlineHint);
		}

		functionMap[function] = llvmFunc;
	}

	void BuildPackage(SpiteIR::Package* package)
	{
		if (package->initializer)
		{
			BuildFunction(package->initializer);
		}

		for (auto& [key, function] : package->functions)
		{
			BuildFunction(function);
		}
	}

	void BuildFunction(SpiteIR::Function* function)
	{
		if (function->metadata.externFunc)
		{
			return;
		}

		llvm::Function* llvmFunc = functionMap.at(function);
		BuildFunctionArgs(function, llvmFunc);
		BuildBlock(function, llvmFunc, function->block);
	}

	llvm::Twine RegisterName(size_t reg)
	{
		eastl::string* name = arena.EmplaceScalar<eastl::string>();
		*name = "r" + eastl::to_string(reg);
		return ToTwine(*name);
	}

	void BuildFunctionArgs(SpiteIR::Function* function, llvm::Function* llvmFunc)
	{
		size_t reg = 0;
		size_t i = 0;
		for (llvm::Argument& arg : llvmFunc->args())
		{
			arg.setName(RegisterName(reg));
			reg += function->arguments.at(i)->value->type->size;
			i += 1;
		}
	}

	void BuildBlock(SpiteIR::Function* function, llvm::Function* llvmFunc, SpiteIR::Block* block)
	{
		BuildEntry(function, llvmFunc, block, block->labels.front());

		for (size_t i = 1; i < block->labels.size(); i++)
		{
			SpiteIR::Label* label = block->labels.at(i);
			BuildLabel(llvmFunc, label);
		}
	}

	void BuildEntry(SpiteIR::Function* function, llvm::Function* llvmFunc, SpiteIR::Block* block, 
		SpiteIR::Label* label)
	{
		llvm::BasicBlock* entryBlock = CreateBasicBlock(llvmFunc, label);
		builder.SetInsertPoint(entryBlock);

		// Skip allocations for function arguments
		for (size_t i = function->arguments.size(); i < block->allocations.size(); i++)
		{
			SpiteIR::Allocate& alloc = block->allocations.at(i);
			BuildAllocate(alloc);
		}

		BuildInstructions(label);
	}

	void BuildAllocate(SpiteIR::Allocate& alloc)
	{
		llvm::AllocaInst* allocaInst = builder.CreateAlloca(
			ToLLVMType(alloc.type, context),
			nullptr, 
			RegisterName(alloc.result)
		);
	}

	llvm::BasicBlock* CreateBasicBlock(llvm::Function* llvmFunc, SpiteIR::Label* label)
	{
		return llvm::BasicBlock::Create(context, ToTwine(label->name),
			llvmFunc);
	}

	void BuildInstructions(SpiteIR::Label* label)
	{
		for (SpiteIR::Instruction* inst : label->values)
			BuildInstruction(inst);

		BuildInstruction(label->terminator);
	}

	void BuildLabel(llvm::Function* llvmFunc, SpiteIR::Label* label)
	{
		llvm::BasicBlock* basicBlock = CreateBasicBlock(llvmFunc, label);
		builder.SetInsertPoint(basicBlock);
		BuildInstructions(label);
	}

	void BuildInstruction(SpiteIR::Instruction* inst)
	{
		switch (inst->kind)
		{
		case SpiteIR::InstructionKind::Return:
			BuildReturn(inst);
			break;
		case SpiteIR::InstructionKind::Jump:
			BuildJump(inst);
			break;
		case SpiteIR::InstructionKind::Branch:
			BuildBranch(inst);
			break;
		case SpiteIR::InstructionKind::Switch:
			BuildSwitch(inst);
			break;
		case SpiteIR::InstructionKind::ExternCall:
			BuildExternCall(inst);
			break;
		case SpiteIR::InstructionKind::Call:
			BuildCall(inst);
			break;
		case SpiteIR::InstructionKind::CallPtr:
			BuildCallPtr(inst);
			break;
		case SpiteIR::InstructionKind::Load:
			BuildLoad(inst);
			break;
		case SpiteIR::InstructionKind::LoadPtrOffset:
			BuildLoadPtrOffset(inst);
			break;
		case SpiteIR::InstructionKind::LoadGlobal:
			BuildLoadGlobal(inst);
			break;
		case SpiteIR::InstructionKind::Store:
			BuildStore(inst);
			break;
		case SpiteIR::InstructionKind::StorePtr:
			BuildStorePtr(inst);
			break;
		case SpiteIR::InstructionKind::Move:
			BuildMove(inst);
			break;
		case SpiteIR::InstructionKind::StoreFunc:
			BuildStoreFunc(inst);
			break;
		case SpiteIR::InstructionKind::Reference:
			BuildReference(inst);
			break;
		case SpiteIR::InstructionKind::Dereference:
			BuildDereference(inst);
			break;
		case SpiteIR::InstructionKind::Cast:
			BuildCast(inst);
			break;
		case SpiteIR::InstructionKind::BinOp:
			BuildBinaryOp(inst);
			break;
		case SpiteIR::InstructionKind::UnOp:
			BuildUnaryOp(inst);
			break;
		case SpiteIR::InstructionKind::Assert:
			BuildAssert(inst);
			break;
		case SpiteIR::InstructionKind::Log:
			BuildLog(inst);
			break;
		default:
			break;
		}
	}

	void BuildReturn(SpiteIR::Instruction* inst)
	{
	}

	void BuildJump(SpiteIR::Instruction* inst)
	{
	}

	void BuildBranch(SpiteIR::Instruction* inst)
	{
	}

	void BuildSwitch(SpiteIR::Instruction* inst)
	{
	}

	void BuildExternCall(SpiteIR::Instruction* inst)
	{
	}

	void BuildCall(SpiteIR::Instruction* inst)
	{
	}

	void BuildCallPtr(SpiteIR::Instruction* inst)
	{
	}

	void BuildLoad(SpiteIR::Instruction* inst)
	{
	}

	void BuildLoadPtrOffset(SpiteIR::Instruction* inst)
	{
	}

	void BuildLoadGlobal(SpiteIR::Instruction* inst)
	{
	}

	void BuildStore(SpiteIR::Instruction* inst)
	{
	}

	void BuildStorePtr(SpiteIR::Instruction* inst)
	{
	}

	void BuildMove(SpiteIR::Instruction* inst)
	{
	}

	void BuildStoreFunc(SpiteIR::Instruction* inst)
	{
	}

	void BuildReference(SpiteIR::Instruction* inst)
	{
	}

	void BuildDereference(SpiteIR::Instruction* inst)
	{
	}

	void BuildCast(SpiteIR::Instruction* inst)
	{
	}

	void BuildBinaryOp(SpiteIR::Instruction* inst)
	{
	}

	void BuildUnaryOp(SpiteIR::Instruction* inst)
	{
	}

	void BuildAssert(SpiteIR::Instruction* inst)
	{
	}

	void BuildLog(SpiteIR::Instruction* inst)
	{
	}


};