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
#include "../../Lower/LowerUtils.h"

extern Config config;
extern SpiteIR::State* stringState;
extern SpiteIR::State* arrayState;

struct LLVMBuilder
{
	SpiteIR::IR* ir;

	eastl::hash_map<SpiteIR::Function*, llvm::Function*> functionMap;
	eastl::hash_map<size_t, llvm::GlobalVariable*> globalVarMap;
	eastl::hash_map<size_t, llvm::Value*> localVarMap;
	eastl::hash_map<SpiteIR::Label*, llvm::BasicBlock*> labelMap;
	SpiteIR::Function* currFunction;
	Arena arena;

	llvm::LLVMContext context;
	llvm::IRBuilder<> builder;
	llvm::Module module;

	llvm::Type* intType;
	llvm::StructType* strType;

	LLVMBuilder(SpiteIR::IR* ir) : builder(context), module(ToStringRef(config.name), context)
	{
		this->ir = ir;
		intType = ToLLVMType(CreateIntType(ir), context);
		strType = StateToLLVMType(stringState, context);
	}

	void Build()
	{
		BuildPackageDeclarations(ir->runtime);
		for (SpiteIR::Package* package : ir->packages)
			BuildPackageDeclarations(package);

		BuildPackage(ir->runtime);
		for (SpiteIR::Package* package : ir->packages)
			BuildPackage(package);

		//module.print(llvm::outs(), nullptr);
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

		globalVarMap[globalVar->index] = llvmGlobalVar;
	}

	void BuildFunctionDeclaration(SpiteIR::Function* function)
	{
		llvm::Function* llvmFunc = llvm::Function::Create(
			FunctionToLLVMType(function, context),
			llvm::Function::ExternalLinkage,
			ToTwine(function->name),
			module
		);

		if (function->IsInline())
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
		currFunction = function;
		if (function->metadata.externFunc)
		{
			return;
		}

		localVarMap.clear();
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
			localVarMap[reg] = &arg;
			SpiteIR::Type* argType = function->arguments.at(i)->value->type;
			reg += argType->size;
			llvm::AttrBuilder attrBuilder(context);
			if (argType->byValue)
			{
				attrBuilder.addByValAttr(arg.getType());
				arg.addAttrs(attrBuilder);
			}
			else
			{
				attrBuilder.addByRefAttr(arg.getType());
				arg.addAttrs(attrBuilder);
			}
			i += 1;
		}
	}

	void BuildBlock(SpiteIR::Function* function, llvm::Function* llvmFunc, SpiteIR::Block* block)
	{
		SpiteIR::Label* entryLabel = block->labels.front();
		llvm::BasicBlock* entryBlock = CreateBasicBlock(llvmFunc, entryLabel);
		builder.SetInsertPoint(entryBlock);

		for (size_t i = 1; i < block->labels.size(); i++)
		{
			SpiteIR::Label* label = block->labels.at(i);
			labelMap[label] = CreateBasicBlock(llvmFunc, label);
		}

		// Skip allocations for function arguments
		for (size_t i = function->arguments.size(); i < block->allocations.size(); i++)
		{
			SpiteIR::Allocate& alloc = block->allocations.at(i);
			BuildAllocate(alloc);
		}

		BuildInstructions(entryLabel);

		for (size_t i = 1; i < block->labels.size(); i++)
		{
			SpiteIR::Label* label = block->labels.at(i);
			BuildLabel(labelMap[label], label);
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
		localVarMap[alloc.result] = allocaInst;
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

	void BuildLabel(llvm::BasicBlock* basicBlock, SpiteIR::Label* label)
	{
		builder.SetInsertPoint(basicBlock);
		BuildInstructions(label);
	}

	llvm::Value* GetLocalValue(size_t reg)
	{
		return localVarMap[reg];
	}

	llvm::Value* GetGlobalValue(size_t reg)
	{
		return globalVarMap[reg];
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
		if (inst->return_.operand.kind == SpiteIR::OperandKind::Void)
			builder.CreateRetVoid();
		else builder.CreateRet(GetLocalValue(inst->return_.operand.reg));
	}

	void BuildJump(SpiteIR::Instruction* inst)
	{
		builder.CreateBr(labelMap[inst->jump.label]);
	}

	void BuildBranch(SpiteIR::Instruction* inst)
	{
		llvm::Value* test = builder.CreateLoad(ToLLVMType(inst->branch.test.type, context),
			localVarMap[inst->branch.test.reg]);
		builder.CreateCondBr(test, labelMap[inst->branch.true_],
			labelMap[inst->branch.false_]);
	}

	void BuildSwitch(SpiteIR::Instruction* inst)
	{
	}

	void BuildExternCall(SpiteIR::Instruction* inst)
	{
	}

	void BuildParams(eastl::vector<SpiteIR::Operand>* params,
		std::vector<llvm::Value*>& outArgs)
	{
		for (SpiteIR::Operand& param : *params)
		{
			llvm::Value* argValue = GetLocalValue(param.reg);
			if (param.type->byValue)
			{
				argValue = builder.CreateLoad(ToLLVMType(param.type, context),
					argValue);
			}

			outArgs.push_back(argValue);
		}
	}


	void BuildCall(SpiteIR::Instruction* inst)
	{
		std::vector<llvm::Value*> args;
		BuildParams(inst->call.params, args);

		llvm::Function* llvmFunc = functionMap[inst->call.function];
		llvm::Value* callResult = builder.CreateCall(llvmFunc, args);
		if (!IsVoidType(inst->call.function->returnType))
		{
			builder.CreateStore(callResult, GetLocalValue(inst->call.result));
		}
	}

	void BuildCallPtr(SpiteIR::Instruction* inst)
	{
		std::vector<llvm::Value*> args;
		BuildParams(inst->callPtr.params, args);

		llvm::FunctionType* funcType = FunctionTypeToLLVMType(inst->callPtr.funcPtr.type, context);
		llvm::Value* funcPtrValue = GetLocalValue(inst->callPtr.funcPtr.reg);
		llvm::Value* funcPtr = builder.CreateLoad(llvm::PointerType::get(funcType, 0),
			funcPtrValue);
		llvm::Value* callResult = builder.CreateCall(funcType, funcPtr, args);

		if (!IsVoidType(inst->callPtr.funcPtr.type->function.returnType))
		{
			builder.CreateStore(callResult, GetLocalValue(inst->callPtr.result));
		}
	}

	size_t GetMemberIndexForOffset(eastl::vector<SpiteIR::Member>* members, size_t offset)
	{
		for (size_t i = 0; i < members->size(); i++)
		{
			SpiteIR::Member& member = members->at(i);
			if (member.offset == offset) return i;
		}

		Logger::FatalError("LLVMBuilder:GetMemberIndexForOffset Unable to find member index");
		return 0;
	}

	eastl::vector<SpiteIR::Member>* GetMembersForType(SpiteIR::Type* type)
	{
		SpiteIR::State* state = GetStateForType(type);
		if (state)
		{
			return &state->members;
		}
		else if (type->kind == SpiteIR::TypeKind::StructureType)
		{
			return type->structureType.members;
		}

		return nullptr;
	}

	void BuildGEPInst(llvm::Type* type, llvm::Value* src, llvm::Value* index, size_t dst,
		bool inBounds)
	{
		llvm::Value* zero = llvm::ConstantInt::get(intType, 0);
		llvm::Value* gepValue = builder.CreateGEP(type, src, { zero, index }, "", inBounds);
		llvm::Value* dstPtr = GetLocalValue(dst);
		builder.CreateStore(gepValue, dstPtr);
	}

	void BuildLoad(SpiteIR::Instruction* inst)
	{
		auto& load = inst->load;

		llvm::Type* type = ToLLVMType(load.src.type, context);
		llvm::Value* ptr = GetLocalValue(load.src.reg);

		eastl::vector<SpiteIR::Member>* members = GetMembersForType(load.src.type);
		if (members)
		{
			Assert(load.offset.kind == SpiteIR::OperandKind::Literal);

			intmax_t offset = load.offset.literal.intLiteral;
			size_t memberIndex = GetMemberIndexForOffset(members, offset);
			llvm::Value* index = llvm::ConstantInt::get(intType, memberIndex);
			BuildGEPInst(type, ptr, index, load.dst.reg, true);
		}
		else if (load.src.type->kind == SpiteIR::TypeKind::FixedArrayType)
		{
			Assert(load.offset.kind == SpiteIR::OperandKind::Register);

			llvm::Value* offset = GetLocalValue(load.offset.reg);
			BuildGEPInst(type, ptr, offset, load.dst.reg, false);
		}
		else
		{
			Assert(false);
		}
	}

	void BuildLoadPtrOffset(SpiteIR::Instruction* inst)
	{
		auto& load = inst->load;
		llvm::Type* type = ToLLVMType(load.src.type, context);
		llvm::Value* ptr = builder.CreateLoad(type, GetLocalValue(load.src.reg));
		SpiteIR::Type* srcType = GetDereferencedType(load.src.type);

		eastl::vector<SpiteIR::Member>* members = GetMembersForType(srcType);
		if (members)
		{
			Assert(load.offset.kind == SpiteIR::OperandKind::Literal);

			intmax_t offset = load.offset.literal.intLiteral;
			size_t memberIndex = GetMemberIndexForOffset(members, offset);
			llvm::Value* index = llvm::ConstantInt::get(intType, memberIndex);
			BuildGEPInst(type, ptr, index, load.dst.reg, true);
		}
		else
		{
			llvm::Value* offset;
			if (load.offset.kind == SpiteIR::OperandKind::Literal)
			{
				intmax_t index = load.offset.literal.intLiteral;
				offset = llvm::ConstantInt::get(intType, index);
			}
			else
			{
				Assert(load.offset.kind == SpiteIR::OperandKind::Register);
				offset = GetLocalValue(load.offset.reg);
			}

			BuildGEPInst(type, ptr, offset, load.dst.reg, false);
		}
	}

	void BuildLoadGlobal(SpiteIR::Instruction* inst)
	{
		llvm::Value* globalPtrValue = GetGlobalValue(inst->loadGlobal.src);
		llvm::Value* dst = GetLocalValue(inst->loadGlobal.dst.reg);
		builder.CreateStore(globalPtrValue, dst);
	}

	inline llvm::Constant* BuildStringLiteral(eastl::string* str) 
	{
		llvm::StringRef strRef = ToStringRef(*str);
		llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, strRef, true);
		llvm::ArrayType* byteArrayType = llvm::ArrayType::get(
			llvm::Type::getInt8Ty(context), 
			str->size() + 1
		);
		llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
			module,
			byteArrayType,
			true,                              
			llvm::GlobalValue::PrivateLinkage, 
			strConstant,  
			""
		);

		llvm::Constant* zero = llvm::ConstantInt::get(intType, 0);
		llvm::Constant* indices[] = { zero, zero };
		return llvm::ConstantExpr::getGetElementPtr(
			byteArrayType,
			globalStr,
			indices
		);
	}

	inline llvm::Value* BuildOperandValue(SpiteIR::Operand& src)
	{
		llvm::Value* value = nullptr;
		llvm::Type* type = ToLLVMType(src.type, context);

		switch (src.kind)
		{
		case SpiteIR::OperandKind::Register:
		{
			value = GetLocalValue(src.reg);
			break;
		}
		case SpiteIR::OperandKind::Literal:
		{
			switch (src.literal.kind)
			{
			case SpiteIR::PrimitiveKind::Bool:
				value = llvm::ConstantInt::get(type, src.literal.byteLiteral);
				break;
			case SpiteIR::PrimitiveKind::Byte:
				value = llvm::ConstantInt::get(type, src.literal.byteLiteral);
				break;
			case SpiteIR::PrimitiveKind::I16:
				value = llvm::ConstantInt::get(type, src.literal.i16Literal);
				break;
			case SpiteIR::PrimitiveKind::I32:
				value = llvm::ConstantInt::get(type, src.literal.i32Literal);
				break;
			case SpiteIR::PrimitiveKind::I64:
				value = llvm::ConstantInt::get(type, src.literal.i64Literal);
				break;
			case SpiteIR::PrimitiveKind::Int:
			{
				if (type->isPointerTy())
				{
					// The only case where a pointer is assigned a literal should be setting
					// it to a null default value
					if (!src.literal.intLiteral)
					{
						value = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(type));
						break;
					}
					// But fuck it, we ball
					else
					{
						llvm::Constant* intValue = llvm::ConstantInt::get(intType, src.literal.intLiteral);
						value = builder.CreateIntToPtr(intValue, type);
						break;
					}
				}

				value = llvm::ConstantInt::get(type, src.literal.intLiteral);
				break;
			}
			case SpiteIR::PrimitiveKind::F32:
				value = llvm::ConstantFP::get(type, src.literal.f32Literal);
				break;
			case SpiteIR::PrimitiveKind::Float:
				value = llvm::ConstantFP::get(type, src.literal.floatLiteral);
				break;
			case SpiteIR::PrimitiveKind::String:
			{
				eastl::string* str = src.literal.stringLiteral;
				size_t count = str->size();
				llvm::Constant* countValue = llvm::ConstantInt::get(intType, count);

				if (!count)
				{
					llvm::Constant* byteNullPtr = llvm::ConstantPointerNull::get(
						llvm::Type::getInt8PtrTy(context)
					);
					value = llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(type), 
							{ countValue, byteNullPtr });
					break;
				}


				value = llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(type),
					{ countValue, BuildStringLiteral(src.literal.stringLiteral)});
				break;
			}
			break;
			default:
				break;
			}
			break;
		}
		case SpiteIR::OperandKind::StructLiteral:
		{
			llvm::Constant* zero = llvm::ConstantInt::get(intType, 0);
			eastl::vector<SpiteIR::Member>* members = GetMembersForType(src.type);
			llvm::Value* valuePtr = builder.CreateAlloca(type, nullptr);
			for (size_t i = 0; i < src.structLiteral->size(); i++)
			{
				SpiteIR::Operand& op = src.structLiteral->at(i);
				llvm::Value* itemValue = BuildOperandValue(op);
				llvm::Value* itemPointer = builder.CreateInBoundsGEP(
					type,
					valuePtr,
					{ zero, llvm::ConstantInt::get(intType, i) }
				);
				builder.CreateStore(itemValue, itemPointer);
			}
			value = valuePtr;
			break;
		}
		case SpiteIR::OperandKind::Function:
		{
			llvm::Function* llvmFunc = functionMap[src.function];
			value = llvmFunc;
			break;
		}
		default:
			break;
		}

		return value;
	}

	void BuildStore(SpiteIR::Instruction* inst)
	{
		llvm::Value* dst = GetLocalValue(inst->store.dst.reg);
		llvm::Value* value = BuildOperandValue(inst->store.src);
		if (value) builder.CreateStore(value, dst);
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