#pragma once

#include "./LLVMEntry.h"
#include "./LLVMCompile.h"
#include "./LLVMOptimize.h"

struct LLVMBuilder
{
	LLVMContext llvmContext;

	llvm::LLVMContext& context;
	llvm::IRBuilder<>& builder;
	llvm::Module& module;

	LLVMBuilder(SpiteIR::IR* ir) : llvmContext(ir),
		context(llvmContext.context),
		builder(llvmContext.builder),
		module(llvmContext.module)
	{}

	void Build()
	{
		LLVMCompile compiler = LLVMCompile(llvmContext);
		if (!compiler.Initialize()) return;

		auto initDeclaration = [](SpiteIR::Package* package, LLVMBuilder& builder)
		{
			builder.BuildPackageDeclarations(package);
		};

		auto initPackage = [](SpiteIR::Package* package, LLVMBuilder& builder)
		{
			builder.BuildPackage(package);
		};

		initDeclaration(llvmContext.ir->runtime, *this);
		llvmContext.ir->IterateImports<LLVMBuilder&>(llvmContext.ir->entry->parent, *this, initDeclaration);
		Logger::Debug("LLVMBuilder: Built LLVM Declarations");

		initPackage(llvmContext.ir->runtime, *this);
		llvmContext.ir->IterateImports<LLVMBuilder&>(llvmContext.ir->entry->parent, *this, initPackage);
		Logger::Debug("LLVMBuilder: Built LLVM Definitions");

		LLVMEntry(llvmContext).BuildMain();

		Logger::Debug("LLVMBuilder: Built entry point");

		if (llvm::verifyModule(module, &llvm::errs()))
		{
			llvm::errs() << "Module verification failed!\n";
			return;
		}
		Logger::Debug("LLVMBuilder: Verified module");

		LLVMOptimize(llvmContext).Optimize();
		Logger::Debug("LLVMBuilder: Optimized module");

		if (config.output == Ir)
		{
			std::filesystem::path output = compiler.CreateOutputPath(".ll");
			std::error_code ec;
			llvm::raw_fd_ostream dest(output.string(), ec, llvm::sys::fs::OF_None);
			module.print(dest, nullptr);
			return;
		}

		if (compiler.Compile())
			Logger::Debug("LLVMBuilder: Compiled module");
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

	void BuildGlobalVariable(SpiteIR::GlobalVariable* globalVar)
	{
		llvm::Type* type = ToLLVMType(globalVar->type, context);
		llvm::GlobalVariable* llvmGlobalVar = new llvm::GlobalVariable(
			module,
			type,
			false,
			llvm::GlobalValue::ExternalLinkage,
			llvm::UndefValue::get(type),
			llvmContext.GlobalVariableName(globalVar)
		);

		llvmContext.globalVarMap[globalVar->index] = llvmGlobalVar;
	}

	bool ExternFunctionIsForTarget(eastl::vector<SpiteIR::PlatformLib>* libs)
	{
		for (SpiteIR::PlatformLib& lib : *libs)
		{
			switch (config.os)
			{
			case Windows:
				if (lib.platform == "windows") return true;
				break;
			case Linux:
				if (lib.platform == "linux") return true;
				break;
			case Mac:
				if (lib.platform == "mac") return true;
				break;
			case Android:
				if (lib.platform == "android" || lib.platform == "linux") return true;
				break;
			case Ios:
				if (lib.platform == "ios") return true;
				break;
			default:
				break;
			}
		}

		return false;
	}

	void BuildFunctionDeclaration(SpiteIR::Function* function)
	{
		llvm::Function* llvmFunc;
		if (!function->metadata.externFunc)
		{
			llvmFunc = llvm::Function::Create(
				FunctionToLLVMType(function, context),
				llvm::Function::ExternalLinkage,
				ToTwine(function->name),
				module
			);

			if (function->IsInline())
			{
				llvmFunc->addFnAttr(llvm::Attribute::InlineHint);
			}
		}
		else
		{
			if (!ExternFunctionIsForTarget(function->metadata.externFunc->libs)) return;

			eastl::string& funcName = function->name;
			if (MapHas(llvmContext.externalFunctionMap, funcName))
			{
				Logger::Warning("LLVMBuilder:BuildFunctionDeclaration multiple external functions with the name \""
					+ funcName + "\", if they're linked to different libs one will be wrong");

				llvmContext.functionMap[function] = llvmContext.externalFunctionMap[funcName];
				return;
			}

			llvmFunc = llvm::Function::Create(
				FunctionToLLVMType(function, context),
				llvm::Function::ExternalLinkage,
				"",
				module
			);
			llvm::Twine name = ToTwine(funcName);
			llvmFunc->setName(name);

			llvmContext.externalFunctionMap[funcName] = llvmFunc;
		}

		llvmContext.functionMap[function] = llvmFunc;
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

		llvmContext.localVarMap.clear();
		llvm::Function* llvmFunc = llvmContext.functionMap.at(function);
		BuildBlock(function, llvmFunc, function->block);
	}

	void BuildBlock(SpiteIR::Function* function, llvm::Function* llvmFunc, SpiteIR::Block* block)
	{
		SpiteIR::Label* entryLabel = block->labels.front();
		llvm::BasicBlock* entryBlock = CreateBasicBlock(llvmFunc, entryLabel);
		builder.SetInsertPoint(entryBlock);

		for (size_t i = 1; i < block->labels.size(); i++)
		{
			SpiteIR::Label* label = block->labels.at(i);
			llvmContext.labelMap[label] = CreateBasicBlock(llvmFunc, label);
		}

		for (size_t i = 0; i < block->allocations.size(); i++)
		{
			SpiteIR::Allocate& alloc = block->allocations.at(i);
			BuildAllocate(alloc);
		}

		size_t reg = 0;
		size_t i = 0;
		for (llvm::Argument& arg : llvmFunc->args())
		{
			builder.CreateStore(&arg, GetLocalValue(reg));
			SpiteIR::Type* argType = function->arguments.at(i)->value.type;
			reg += argType->size;
			i += 1;
		}

		BuildInstructions(entryLabel);

		for (size_t i = 1; i < block->labels.size(); i++)
		{
			SpiteIR::Label* label = block->labels.at(i);
			BuildLabel(llvmContext.labelMap[label], label);
		}
	}

	void BuildAllocate(SpiteIR::Allocate& alloc)
	{
		llvm::AllocaInst* allocaInst = builder.CreateAlloca(
			ToLLVMType(alloc.type, context),
			nullptr,
			llvmContext.RegisterName(alloc.result)
		);
		llvmContext.localVarMap[alloc.result] = allocaInst;
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
		return llvmContext.localVarMap[reg];
	}

	llvm::Value* GetGlobalValue(size_t reg)
	{
		return llvmContext.globalVarMap[reg];
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
		default:
			break;
		}
	}

	void BuildReturn(SpiteIR::Instruction* inst)
	{
		if (inst->return_.operand.kind == SpiteIR::OperandKind::Void)
			builder.CreateRetVoid();
		else
		{
			llvm::Value* returnValuePtr = GetLocalValue(inst->return_.operand.reg);
			llvm::Value* returnValue = builder.CreateLoad(
				ToLLVMType(inst->return_.operand.type, context),
				returnValuePtr
			);
			builder.CreateRet(returnValue);
		}
	}

	void BuildJump(SpiteIR::Instruction* inst)
	{
		builder.CreateBr(llvmContext.labelMap[inst->jump.label]);
	}

	void BuildBranch(SpiteIR::Instruction* inst)
	{
		llvm::Value* test = builder.CreateLoad(ToLLVMType(inst->branch.test.type, context),
			llvmContext.localVarMap[inst->branch.test.reg]);
		builder.CreateCondBr(test, llvmContext.labelMap[inst->branch.true_],
			llvmContext.labelMap[inst->branch.false_]);
	}

	void BuildSwitch(SpiteIR::Instruction* inst)
	{
		llvm::IntegerType* testType = llvm::cast<llvm::IntegerType>(ToLLVMType(inst->switch_.test.type, context));
		llvm::Value* testPtr = GetLocalValue(inst->switch_.test.reg);
		llvm::Value* testValue = builder.CreateLoad(
			ToLLVMType(inst->switch_.test.type, context),
			testPtr
		);

		size_t caseCount = inst->switch_.cases->size();
		llvm::BasicBlock* defaultBlock = llvmContext.labelMap[inst->switch_.defaultCase];
		llvm::SwitchInst* switchInst = builder.CreateSwitch(testValue, defaultBlock, caseCount);

		for (auto& [caseTest, label] : *inst->switch_.cases)
		{
			llvm::ConstantInt* caseTestValue = llvm::ConstantInt::get(testType, caseTest);
			llvm::BasicBlock* caseBlock = llvmContext.labelMap[label];
			switchInst->addCase(caseTestValue, caseBlock);
		}
	}

	void BuildParams(eastl::vector<SpiteIR::Operand>* params,
		std::vector<llvm::Value*>& outArgs)
	{
		for (SpiteIR::Operand& param : *params)
		{
			llvm::Value* argValue = GetLocalValue(param.reg);
			argValue = builder.CreateLoad(ToLLVMType(param.type, context),
				argValue);
			outArgs.push_back(argValue);
		}
	}

	void BuildCall(SpiteIR::Instruction* inst)
	{
		std::vector<llvm::Value*> args;
		BuildParams(inst->call.params, args);

		SpiteIR::Function* func = inst->call.function;
		llvm::Function* llvmFunc = llvmContext.functionMap[func];
		// Platform specific functions will not be in the function map when building for
		// a different target, this is fine as long as that code is behind a target check
		if (llvmFunc)
		{
			llvm::Value* callResult = builder.CreateCall(llvmFunc, args);
			if (!IsVoidType(inst->call.function->returnType))
			{
				builder.CreateStore(callResult, GetLocalValue(inst->call.result));
			}
		}
	}

	void BuildExternCall(SpiteIR::Instruction* inst)
	{
		BuildCall(inst);
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

	size_t GetMemberIndexForOffset(eastl::vector<SpiteIR::Member*>* members, size_t offset)
	{
		for (size_t i = 0; i < members->size(); i++)
		{
			SpiteIR::Member* member = members->at(i);
			if (member->offset == offset) return i;
		}

		Logger::FatalError("LLVMBuilder:GetMemberIndexForOffset Unable to find member index");
		return 0;
	}

	eastl::vector<SpiteIR::Member*>* GetMembersForType(SpiteIR::Type* type)
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

	void BuildLoad(SpiteIR::Instruction* inst)
	{
		auto& load = inst->load;

		llvm::Type* type = ToLLVMType(load.src.type, context);
		llvm::Value* ptr = GetLocalValue(load.src.reg);
		llvm::Value* dstPtr = GetLocalValue(load.dst.reg);

		eastl::vector<SpiteIR::Member*>* members = GetMembersForType(load.src.type);
		if (members)
		{
			Assert(load.offset.kind == SpiteIR::OperandKind::Literal);

			intmax_t offset = load.offset.literal.intLiteral;
			size_t memberIndex = GetMemberIndexForOffset(members, offset);
			llvm::Value* memberPtr = builder.CreateStructGEP(
				type,
				ptr,
				memberIndex
			);
			llvm::StoreInst* inst = builder.CreateStore(memberPtr, dstPtr);
			inst->setMetadata("load", llvm::MDNode::get(context, llvm::MDString::get(context, "load")));
		}
		else if (load.src.type->kind == SpiteIR::TypeKind::FixedArrayType)
		{
			Assert(load.offset.kind == SpiteIR::OperandKind::Register);

			llvm::LoadInst* offset = builder.CreateLoad(
				ToLLVMType(load.offset.type, context),
				GetLocalValue(load.offset.reg)
			);
			offset->setMetadata("load_offset", llvm::MDNode::get(context, llvm::MDString::get(context, "load_offset")));
			llvm::StoreInst* inst = llvmContext.BuildGEPInst(type, ptr, offset, dstPtr, false);
			inst->setMetadata("load_gep", llvm::MDNode::get(context, llvm::MDString::get(context, "load_gep")));
		}
		else
		{
			Logger::FatalError("LLVMBuilder:BuildLoad Invalid load instruction");
		}
	}

	void BuildLoadPtrOffset(SpiteIR::Instruction* inst)
	{
		auto& load = inst->load;

		SpiteIR::Type* srcType = GetDereferencedType(load.src.type);
		llvm::Type* ptrType = ToLLVMType(load.src.type, context);
		llvm::Type* type = ToLLVMType(srcType, context, true);
		
		llvm::LoadInst* ptr = builder.CreateLoad(
			ptrType,
			GetLocalValue(load.src.reg)
		);
		ptr->setMetadata("load_ptr", llvm::MDNode::get(context, llvm::MDString::get(context, "load_ptr")));
		
		llvm::Value* dstPtr = GetLocalValue(load.dst.reg);

		eastl::vector<SpiteIR::Member*>* members = GetMembersForType(srcType);
		if (members && load.offset.kind == SpiteIR::OperandKind::Literal)
		{
			intmax_t offset = load.offset.literal.intLiteral;
			size_t memberIndex = GetMemberIndexForOffset(members, offset);
			llvm::Value* memberPtr = builder.CreateStructGEP(
				type,
				ptr,
				memberIndex
			);
			llvm::StoreInst* storeInst = builder.CreateStore(memberPtr, dstPtr);
			storeInst->setMetadata("load_ptr_store", llvm::MDNode::get(context, llvm::MDString::get(context, "load_ptr_store")));
		}
		else
		{
			llvm::Value* offset;
			if (load.offset.kind == SpiteIR::OperandKind::Literal)
			{
				intmax_t index = load.offset.literal.intLiteral;
				offset = llvm::ConstantInt::get(llvmContext.int32Type, index);
			}
			else
			{
				Assert(load.offset.kind == SpiteIR::OperandKind::Register);
				llvm::LoadInst* loadInst = builder.CreateLoad(
					ToLLVMType(load.offset.type, context),
					GetLocalValue(load.offset.reg)
				);
				loadInst->setMetadata("load_ptr_offset", llvm::MDNode::get(context, llvm::MDString::get(context, "load_ptr_offset")));
				offset = loadInst;
			}
		
			if (srcType->kind != SpiteIR::TypeKind::FixedArrayType)
			{
				// Pointer arithmetic
				llvm::Value* gepPtr = builder.CreateGEP(type, ptr, offset);
				llvm::StoreInst* storeInst = builder.CreateStore(gepPtr, dstPtr);
				storeInst->setMetadata("load_ptr_arith", llvm::MDNode::get(context, llvm::MDString::get(context, "load_ptr_arith")));
			}
			else
			{
				llvm::StoreInst* storeInst = llvmContext.BuildGEPInst(type, ptr, offset, dstPtr, false);
				storeInst->setMetadata("load_ptr_gep", llvm::MDNode::get(context, llvm::MDString::get(context, "load_ptr_gep")));
			}
		}
	}

	void BuildLoadGlobal(SpiteIR::Instruction* inst)
	{
		llvm::Value* globalPtrValue = GetGlobalValue(inst->loadGlobal.src);
		llvm::Value* dst = GetLocalValue(inst->loadGlobal.dst.reg);
		llvm::StoreInst* storeInst = builder.CreateStore(globalPtrValue, dst);
		storeInst->setMetadata("load_global", llvm::MDNode::get(context, llvm::MDString::get(context, "load_global")));
	}

	inline llvm::Constant* BuildStringLiteral(eastl::string* str)
	{
		llvm::StringRef strRef = ToStringRef(*str);
		llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, strRef, true);
		llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
			module,
			strConstant->getType(),
			false,
			llvm::GlobalValue::PrivateLinkage,
			strConstant,
			""
		);

		return globalStr;
	}

	inline void BuildOperandValue(SpiteIR::Operand& src, llvm::Value* dstPtr)
	{
		llvm::Value* value = nullptr;
		llvm::Type* type = ToLLVMType(src.type, context);

		switch (src.kind)
		{
		case SpiteIR::OperandKind::Void:
			return;
		case SpiteIR::OperandKind::Register:
		{
			llvm::LoadInst* loadInst = builder.CreateLoad(
				type,
				GetLocalValue(src.reg)			
			);
			loadInst->setMetadata("store_value", llvm::MDNode::get(context, llvm::MDString::get(context, "store_value")));
			value = loadInst;
			break;
		}
		case SpiteIR::OperandKind::Literal:
		{
			switch (src.literal.kind)
			{
			case SpiteIR::PrimitiveKind::Void:
				return;
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
						llvm::Constant* intValue = llvm::ConstantInt::get(llvmContext.intType,
							src.literal.intLiteral);
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
				llvm::Constant* countValue = llvm::ConstantInt::get(llvmContext.intType,
					count);
				
				value = llvm::ConstantStruct::get(
					llvm::cast<llvm::StructType>(type),
					{ countValue, BuildStringLiteral(src.literal.stringLiteral) }
				);
				break;
			}
			default:
				break;
			}
			break;
		}
		case SpiteIR::OperandKind::StructLiteral:
		{
			llvm::Type* structType = ToLLVMType(src.type, context);
			for (size_t i = 0; i < src.structLiteral->size(); i++)
			{
				SpiteIR::Operand& op = src.structLiteral->at(i);
				llvm::Value* indexedPtr = builder.CreateStructGEP(structType, dstPtr, i);
				BuildOperandValue(op, indexedPtr);
			}
			return;
		}
		case SpiteIR::OperandKind::Function:
		{
			llvm::Function* llvmFunc = llvmContext.functionMap[src.function];
			// Function is not available for current target
			if (!llvmFunc) return;
			value = llvmFunc;
			break;
		}
		case SpiteIR::OperandKind::TypeData:
		{
			value = llvmContext.BuildTypeValue(src.type);
			break;
		}
		default:
			break;
		}

		llvm::StoreInst* storeInst = builder.CreateStore(value, dstPtr);
		storeInst->setMetadata("store_operand", llvm::MDNode::get(context, llvm::MDString::get(context, "store_operand")));
	}

	void BuildStore(SpiteIR::Instruction* inst)
	{
		BuildOperandValue(inst->store.src, GetLocalValue(inst->store.dst.reg));
	}

	void BuildStorePtr(SpiteIR::Instruction* inst)
	{
		llvm::Value* srcPtr = GetLocalValue(inst->store.src.reg);
		llvm::Type* srcType = ToLLVMType(inst->store.src.type, context);
		llvm::LoadInst* srcValue = builder.CreateLoad(srcType, srcPtr);
		srcValue->setMetadata("store_ptr_src", llvm::MDNode::get(context, llvm::MDString::get(context, "store_ptr_src")));

		llvm::Value* dstPtr = GetLocalValue(inst->store.dst.reg);
		llvm::Type* dstType = ToLLVMType(inst->store.dst.type, context);
		llvm::LoadInst* dstValuePtr = builder.CreateLoad(dstType, dstPtr);
		dstValuePtr->setMetadata("store_ptr_load", llvm::MDNode::get(context, llvm::MDString::get(context, "store_ptr_load")));

		llvm::StoreInst* storeInst = builder.CreateStore(srcValue, dstValuePtr);
		storeInst->setMetadata("store_ptr", llvm::MDNode::get(context, llvm::MDString::get(context, "store_ptr")));
	}

	void BuildMove(SpiteIR::Instruction* inst)
	{
		llvm::Value* srcPtr = GetLocalValue(inst->store.src.reg);
		llvm::Value* dstPtr = GetLocalValue(inst->store.dst.reg);

		llvm::LoadInst* srcValuePtr = builder.CreateLoad(
			ToLLVMType(inst->store.src.type, context),
			srcPtr
		);
		srcValuePtr->setMetadata("move_src_ptr", llvm::MDNode::get(context, llvm::MDString::get(context, "move_src_ptr")));

		llvm::LoadInst* srcValue = builder.CreateLoad(
			ToLLVMType(inst->store.dst.type, context),
			srcValuePtr
		);
		srcValue->setMetadata("move_src_value", llvm::MDNode::get(context, llvm::MDString::get(context, "move_src_value")));


		llvm::LoadInst* dstValuePtr = builder.CreateLoad(
			ToLLVMType(inst->store.src.type, context),
			dstPtr
		);
		dstValuePtr->setMetadata("move_dst_ptr", llvm::MDNode::get(context, llvm::MDString::get(context, "move_dst_ptr")));


		llvm::StoreInst* store = builder.CreateStore(srcValue, dstValuePtr);
		store->setMetadata("move_store", llvm::MDNode::get(context, llvm::MDString::get(context, "move_store")));
	}

	void BuildReference(SpiteIR::Instruction* inst)
	{
		llvm::Value* srcPtr = GetLocalValue(inst->store.src.reg);
		llvm::Value* dstPtr = GetLocalValue(inst->store.dst.reg);
		llvm::StoreInst* store = builder.CreateStore(srcPtr, dstPtr);
		store->setMetadata("reference", llvm::MDNode::get(context, llvm::MDString::get(context, "reference")));
	}

	void BuildDereference(SpiteIR::Instruction* inst)
	{
		llvm::Value* srcPtr = GetLocalValue(inst->store.src.reg);
		llvm::LoadInst* srcValuePtr = builder.CreateLoad(
			ToLLVMType(inst->store.src.type, context),
			srcPtr
		);
		srcValuePtr->setMetadata("dereference_load_ptr", llvm::MDNode::get(context, llvm::MDString::get(context, "dereference_load_ptr")));
		
		llvm::LoadInst* srcValue = builder.CreateLoad(
			ToLLVMType(inst->store.dst.type, context),
			srcValuePtr
		);
		srcValue->setMetadata("dereference_load_value", llvm::MDNode::get(context, llvm::MDString::get(context, "dereference_load_value")));

		llvm::Value* dstPtr = GetLocalValue(inst->store.dst.reg);
		llvm::StoreInst* store = builder.CreateStore(srcValue, dstPtr);
		store->setMetadata("dereference_store", llvm::MDNode::get(context, llvm::MDString::get(context, "dereference_store")));
	}

	void BuildCast(SpiteIR::Instruction* inst)
	{
		llvm::Type* fromType = ToLLVMType(inst->cast.from.type, context);
		llvm::Type* toType = ToLLVMType(inst->cast.to.type, context);

		llvm::Value* fromPtr = GetLocalValue(inst->cast.from.reg);
		llvm::Value* toPtr = GetLocalValue(inst->cast.to.reg);

		switch (inst->cast.from.type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
		{
			llvm::Value* fromValue = builder.CreateLoad(fromType, fromPtr);
			if (inst->cast.to.type->kind == SpiteIR::TypeKind::PointerType)
			{
				llvm::Value* intToPtr = builder.CreateIntToPtr(fromValue, toType);
				builder.CreateStore(intToPtr, toPtr);
				return;
			}

			if (IsBoolType(inst->cast.to.type))
			{
				llvm::Value* cmp;
				if (IsFloatLikeType(inst->cast.from.type))
				{
					cmp = builder.CreateFCmp(
						llvm::CmpInst::FCMP_ONE, 
						fromValue, 
						llvm::ConstantFP::get(fromType, 0.0f)
					);
				}
				else
				{
					cmp = builder.CreateICmp(
						llvm::CmpInst::ICMP_NE, 
						fromValue, 
						llvm::ConstantInt::get(fromType, 0)
					);
				}
				builder.CreateStore(cmp, toPtr);
				return;
			}

			llvm::Value* castedValue = nullptr;
			if (IsIntLikeType(inst->cast.from.type) &&
				IsIntLikeType(inst->cast.to.type))
			{
				if (inst->cast.from.type->primitive.isSigned)
				{
					castedValue = builder.CreateSExtOrTrunc(fromValue, toType);
				}
				else
				{
					castedValue = builder.CreateZExtOrTrunc(fromValue, toType);
				}
			}
			else if (IsFloatLikeType(inst->cast.from.type) &&
				IsFloatLikeType(inst->cast.to.type))
			{
				if (inst->cast.to.type->size > inst->cast.from.type->size)
				{
					castedValue = builder.CreateFPExt(fromValue, toType);
				}
				else
				{
					castedValue = builder.CreateFPTrunc(fromValue, toType);
				}
			}
			else if (IsIntLikeType(inst->cast.from.type))
			{
				if (inst->cast.from.type->primitive.isSigned)
				{
					castedValue = builder.CreateSIToFP(fromValue, toType);
				}
				else
				{
					castedValue = builder.CreateUIToFP(fromValue, toType);
				}
			}
			else
			{
				if (inst->cast.to.type->primitive.isSigned)
				{
					castedValue = builder.CreateFPToSI(fromValue, toType);
				}
				else
				{
					castedValue = builder.CreateFPToUI(fromValue, toType);
				}
			}

			builder.CreateStore(castedValue, toPtr);
			return;
		}
		case SpiteIR::TypeKind::PointerType:
		{
			if (IsIntLikeType(inst->cast.to.type))
			{
				llvm::Value* fromValuePtr = builder.CreateLoad(fromType, fromPtr);
				llvm::Value* ptrToInt = builder.CreatePtrToInt(fromValuePtr, toType);
				builder.CreateStore(ptrToInt, toPtr);
				return;
			}
			//continue
		}
		case SpiteIR::TypeKind::ReferenceType:
		{
			llvm::Value* fromValuePtr = builder.CreateLoad(fromType, fromPtr);
			llvm::Value* castedPtr = builder.CreateBitCast(fromValuePtr, toType);
			builder.CreateStore(castedPtr, toPtr);
			return;
		}
		case SpiteIR::TypeKind::StateType:
		case SpiteIR::TypeKind::StructureType:
		case SpiteIR::TypeKind::DynamicArrayType:
		case SpiteIR::TypeKind::FixedArrayType:
		case SpiteIR::TypeKind::FunctionType:
			break;
		default:
			break;
		}

		Logger::FatalError("LLVMBuilder:BuildCast Bit cast value should never be a non pointer type");
	}

	void BuildBinaryOp(SpiteIR::Instruction* inst)
	{
		SpiteIR::Type* leftType = inst->binOp.left.type;
		SpiteIR::Type* rightType = inst->binOp.right.type;

		llvm::Type* leftLLVMType = ToLLVMType(leftType, context);
		llvm::Type* rightLLVMType = ToLLVMType(rightType, context);

		llvm::Value* leftPtr = GetLocalValue(inst->binOp.left.reg);
		llvm::Value* rightPtr = GetLocalValue(inst->binOp.right.reg);

		llvm::Value* leftValue = builder.CreateLoad(leftLLVMType, leftPtr);
		llvm::Value* rightValue = builder.CreateLoad(rightLLVMType, rightPtr);

		llvm::Value* resultValue = nullptr;
		llvm::Value* dstPtr = GetLocalValue(inst->binOp.result);

		switch (inst->binOp.kind)
		{
		case SpiteIR::BinaryOpKind::Add:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFAdd(leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateAdd(leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::Subtract:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFSub(leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateSub(leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::Multiply:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFMul(leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateMul(leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::Divide:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFDiv(leftValue, rightValue);
			}
			else if (leftType->primitive.isSigned)
			{
				resultValue = builder.CreateSDiv(leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateUDiv(leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::Modulo:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFRem(leftValue, rightValue);
			}
			else if (leftType->primitive.isSigned)
			{
				resultValue = builder.CreateSRem(leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateURem(leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::And:
			resultValue = builder.CreateAnd(leftValue, rightValue);
			break;
		case SpiteIR::BinaryOpKind::Or:
			resultValue = builder.CreateOr(leftValue, rightValue);
			break;
		case SpiteIR::BinaryOpKind::Xor:
			resultValue = builder.CreateXor(leftValue, rightValue);
			break;
		case SpiteIR::BinaryOpKind::ShiftLeft:
			resultValue = builder.CreateShl(leftValue, rightValue);
			break;
		case SpiteIR::BinaryOpKind::ShiftRight:
		{
			if (leftType->primitive.isSigned)
			{
				resultValue = builder.CreateAShr(leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateLShr(leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::AndNot:
		{
			llvm::Value* not_ = builder.CreateNot(rightValue);
			resultValue = builder.CreateLShr(leftValue, not_);
			break;
		}
		case SpiteIR::BinaryOpKind::Equal:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFCmp(llvm::CmpInst::FCMP_OEQ, leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_EQ, leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::NotEql:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFCmp(llvm::CmpInst::FCMP_ONE, leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_NE, leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::Less:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFCmp(llvm::CmpInst::FCMP_OLT, leftValue, rightValue);
			}
			else if (leftType->primitive.isSigned)
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_SLT, leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_ULT, leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::Greater:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFCmp(llvm::CmpInst::FCMP_OGT, leftValue, rightValue);
			}
			else if (leftType->primitive.isSigned)
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_SGT, leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_UGT, leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::LessEqual:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFCmp(llvm::CmpInst::FCMP_OLE, leftValue, rightValue);
			}
			else if (leftType->primitive.isSigned)
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_SLE, leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_ULE, leftValue, rightValue);
			}
			break;
		}
		case SpiteIR::BinaryOpKind::GreaterEqual:
		{
			if (IsFloatLikeType(leftType))
			{
				resultValue = builder.CreateFCmp(llvm::CmpInst::FCMP_OGE, leftValue, rightValue);
			}
			else if (leftType->primitive.isSigned)
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_SGE, leftValue, rightValue);
			}
			else
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_UGE, leftValue, rightValue);
			}
			break;
		}
		default:
			Logger::FatalError("LLVMBuilder:BuildBinaryOp Invalid operation");
			break;
		}

		builder.CreateStore(resultValue, dstPtr);
	}

	void BuildUnaryOp(SpiteIR::Instruction* inst)
	{
		SpiteIR::Type* type = inst->unOp.operand.type;
		llvm::Type* llvmType = ToLLVMType(type, context);
		llvm::Value* ptr = GetLocalValue(inst->binOp.left.reg);
		llvm::Value* value = builder.CreateLoad(llvmType, ptr);

		llvm::Value* resultValue = nullptr;
		llvm::Value* dstPtr = GetLocalValue(inst->unOp.result);

		switch (inst->unOp.kind)
		{
		case SpiteIR::UnaryOpKind::Subtract:
		{
			if (IsFloatLikeType(type))
			{
				resultValue = builder.CreateFNeg(value);
			}
			else
			{
				resultValue = builder.CreateNeg(value);
			}
			break;
		}
		case SpiteIR::UnaryOpKind::Not:
		{
			llvm::Value* zero = llvm::ConstantInt::get(llvmType, 0);
			if (IsFloatLikeType(type))
			{
				resultValue = builder.CreateFCmp(llvm::CmpInst::FCMP_OEQ, value, zero);
			}
			else
			{
				resultValue = builder.CreateICmp(llvm::CmpInst::ICMP_EQ, value, zero);
			}
			break;
		}
		case SpiteIR::UnaryOpKind::XOr:
			resultValue = builder.CreateNot(value);
			break;
		default:
			Logger::FatalError("LLVMBuilder:BuildUnaryOp Invalid operation");
			break;
		}

		builder.CreateStore(resultValue, dstPtr);
	}

	void BuildAssert(SpiteIR::Instruction* inst)
	{
	}
};