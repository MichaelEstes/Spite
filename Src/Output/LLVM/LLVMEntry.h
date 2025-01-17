#pragma once

#include "./LLVMContext.h"

struct LLVMEntry
{
	LLVMContext& llvmContext;

	llvm::LLVMContext& context;
	llvm::IRBuilder<>& builder;
	llvm::Module& module;

	LLVMEntry(LLVMContext& llvmContext) : llvmContext(llvmContext),
		context(llvmContext.context),
		builder(llvmContext.builder),
		module(llvmContext.module)
	{}

	void BuildMain()
	{
		llvm::PointerType* argvType = llvm::PointerType::get(
			llvm::PointerType::get(llvm::IntegerType::getInt8Ty(context), 0),
			0
		);
		std::vector<llvm::Type*> mainParams = { llvmContext.int32Type, argvType };
		llvm::FunctionType* mainFuncType = llvm::FunctionType::get(llvmContext.int32Type, 
			mainParams, false);

		llvm::Function* mainFunc = llvm::Function::Create(
			mainFuncType,
			llvm::Function::ExternalLinkage,
			"main",
			module
		);

		llvm::BasicBlock* mainEntry = llvm::BasicBlock::Create(context, "entry", mainFunc);
		builder.SetInsertPoint(mainEntry);

		BuildTypeData();
		
		for (SpiteIR::Package* package : llvmContext.ir->packages)
		{
			if (package->initializer)
			{
				llvm::Function* llvmFunc = llvmContext.functionMap[package->initializer];
				llvm::Value* initCall = builder.CreateCall(llvmFunc, {});
			}
		}

		SpiteIR::Function* entryFunc = llvmContext.ir->entry;
		llvm::Function* entryLLVMFunc = llvmContext.functionMap[entryFunc];
		llvm::Value* entryCall = builder.CreateCall(entryLLVMFunc, {});
		if (!IsVoidType(entryFunc->returnType))
		{
			builder.CreateRet(entryCall);
		}
		else
		{
			builder.CreateRet(llvm::ConstantInt::get(llvmContext.intType, 0));
		}
	}

	void BuildTypeIntData(int offset, intmax_t value, llvm::Value* valuePtr,
		llvm::Type* structType, llvm::Type* valueType)
	{
		llvm::Value* ptr = builder.CreateStructGEP(
			structType,
			valuePtr,
			offset
		);
		builder.CreateStore(
			llvm::ConstantInt::get(valueType, value),
			ptr
		);
	}

	void BuildTypeBoolData(int offset, bool value, llvm::Value* valuePtr, llvm::Type* structType)
	{
		llvm::Value* ptr = builder.CreateStructGEP(
			structType,
			valuePtr,
			offset
		);

		if (value)
		{
			builder.CreateStore(
				llvm::ConstantInt::getTrue(context),
				ptr
			);
		}
		else
		{
			builder.CreateStore(
				llvm::ConstantInt::getFalse(context),
				ptr
			);
		}

	}

	llvm::Value* CastMember(SpiteIR::Member* member, llvm::Value* ptr)
	{
		llvm::Type* type = llvm::PointerType::get(ToLLVMType(member->value.type, context), 0);
		llvm::Value* castedPtr = builder.CreateBitCast(ptr, type);
		return castedPtr;
	}

	llvm::Value* CreateMember(SpiteIR::Member* member, SpiteIR::Type* memberType)
	{
		llvm::Type* llvmType = ToLLVMType(memberType, context);
		llvm::GlobalVariable* memberVar = new llvm::GlobalVariable(
			module,
			llvmType,
			false,
			llvm::GlobalValue::PrivateLinkage,
			llvm::UndefValue::get(llvmType),
			""
		);

		llvm::Value* valuePtr = builder.CreateStructGEP(
			llvmType,
			memberVar,
			0
		);
		SpiteIR::Type* valueType = memberType->stateType.state->members.at(0)->value.type;
		llvm::Type* llvmValueType = ToLLVMType(valueType, context);
		llvm::Value* valueTypePtr = builder.CreateStructGEP(
			llvmValueType,
			valuePtr,
			0
		);

		llvm::Value* typeValue = llvmContext.BuildTypeValue(member->value.type);
		builder.CreateStore(typeValue, valueTypePtr);
		llvm::Value* valueNamePtr = builder.CreateStructGEP(
			llvmValueType,
			valuePtr,
			1
		);
		SpiteIR::Type* strType = valueType->stateType.state->members.at(1)->value.type;
		BuildInteropString(member->value.name, strType, valueNamePtr);
		BuildTypeIntData(1, member->offset, memberVar, llvmType, llvmContext.intType);

		return memberVar;
	}

	void BuildMemberArray(eastl::vector<SpiteIR::Member*>& members, llvm::Value* ptr,
		SpiteIR::Type* memberArrayType)
	{
		llvm::Type* llvmType = ToLLVMType(memberArrayType, context);
		SpiteIR::Type* memberPtrType = memberArrayType->stateType.state->members.at(0)->value.type->pointer.type;
		llvm::Type* llvmMemberArrayType = llvm::ArrayType::get(
			ToLLVMType(memberPtrType, context),
			members.size()
		);

		llvm::GlobalVariable* memberArr = new llvm::GlobalVariable(
			module,
			llvmMemberArrayType,
			false,
			llvm::GlobalValue::PrivateLinkage,
			llvm::UndefValue::get(llvmMemberArrayType),
			""
		);

		SpiteIR::Type* memberType = memberPtrType->pointer.type;
		for (size_t i = 0; i < members.size(); i++)
		{
			SpiteIR::Member* member = members.at(i);
			llvm::Value* memberPtr = CreateMember(member, memberType);
			llvm::Value* arrPtr = builder.CreateStructGEP(llvmMemberArrayType, memberArr, i);
			llvm::StoreInst* memberStore = builder.CreateStore(memberPtr, arrPtr);
		}
		
		llvm::Value* beginPtr = builder.CreateStructGEP(llvmType, ptr, 0);
		llvm::Value* endPtr = builder.CreateStructGEP(llvmType, ptr, 1);
		
		llvm::Value* beginPtrValue = builder.CreateStructGEP(
			llvmMemberArrayType,
			memberArr,
			0
		);
		llvm::Value* endPtrValue = builder.CreateStructGEP(
			llvmMemberArrayType, 
			memberArr,
			members.size()
		);
		
		llvm::StoreInst* beginStore = builder.CreateStore(beginPtrValue, beginPtr);
		llvm::StoreInst* endStore = builder.CreateStore(endPtrValue, endPtr);
	}

	void BuildInteropString(eastl::string& str, SpiteIR::Type* interopStringType, llvm::Value* ptr)
	{
		llvm::StringRef strRef = ToStringRef(str);
		llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, strRef, true);
		llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
			module,
			strConstant->getType(),
			true,
			llvm::GlobalValue::PrivateLinkage,
			strConstant,
			""
		);

		llvm::Type* strType = ToLLVMType(interopStringType, context);
		llvm::Value* strBeginPtr = builder.CreateStructGEP(
			strType,
			ptr,
			0
		);
		llvm::StoreInst* beginStore = builder.CreateStore(globalStr, strBeginPtr);
		BuildTypeIntData(1, str.size(), ptr, strType, llvmContext.intType);
		// To set the heap allocated bit flag that C++ strings use
		BuildTypeIntData(2, 9223372036854775808, ptr, strType, llvmContext.intType);
	}

	void StoreStateData(llvm::Value* ptr, SpiteIR::State* state, SpiteIR::Type* stateType)
	{
		llvm::Type* llvmType = ToLLVMType(stateType, context);
		llvm::GlobalVariable* stateVarPtr = new llvm::GlobalVariable(
			module,
			llvmType,
			false,
			llvm::GlobalValue::PrivateLinkage,
			llvm::UndefValue::get(llvmType),
			""
		);

		llvm::Value* packagePtr = builder.CreateStructGEP(llvmType, stateVarPtr, 0);
		builder.CreateStore(
			llvm::ConstantPointerNull::get(builder.getPtrTy()),
			packagePtr
		);
		BuildTypeIntData(1, state->size, stateVarPtr, llvmType, llvmContext.intType);
		BuildTypeIntData(2, state->alignment, stateVarPtr, llvmType, llvmContext.intType);
		BuildTypeIntData(3, state->flags, stateVarPtr, llvmType, llvmContext.intType);
		llvm::Value* strPtr = builder.CreateStructGEP(
			llvmType,
			stateVarPtr,
			4
		);
		SpiteIR::Type* strType = stateType->stateType.state->members.at(4)->value.type;
		BuildInteropString(state->name, strType, strPtr);

		SpiteIR::Type* memberArrayType = stateType->stateType.state->members.at(5)->value.type;
		llvm::Value* membersPtr = builder.CreateStructGEP(
			llvmType,
			stateVarPtr,
			5
		);
		BuildMemberArray(state->members, membersPtr, memberArrayType);

		llvm::StoreInst* storeStatePtr = builder.CreateStore(stateVarPtr, ptr);
	}

	void StoreStructureData(llvm::Value* ptr, eastl::vector<SpiteIR::Member*>* members, 
		SpiteIR::Type* memberArrayType)
	{
		llvm::Type* llvmType = ToLLVMType(memberArrayType, context);
		llvm::GlobalVariable* memberArrayPtr = new llvm::GlobalVariable(
			module,
			llvmType,
			false,
			llvm::GlobalValue::PrivateLinkage,
			llvm::UndefValue::get(llvmType),
			""
		);
		BuildMemberArray(*members, memberArrayPtr, memberArrayType);
		builder.CreateStore(memberArrayPtr, ptr);
	}

	void StoreTypeArray(llvm::Value* ptr, eastl::vector<SpiteIR::Type*>* types, SpiteIR::Type* typeArrayType)
	{
		llvm::Type* llvmType = ToLLVMType(typeArrayType, context);
		SpiteIR::Type* typePtrType = typeArrayType->stateType.state->members.at(0)->value.type->pointer.type;
		llvm::Type* llvmTypeArrayType = llvm::ArrayType::get(
			ToLLVMType(typePtrType, context),
			types->size()
		);

		llvm::GlobalVariable* typeArr = new llvm::GlobalVariable(
			module,
			llvmTypeArrayType,
			false,
			llvm::GlobalValue::PrivateLinkage,
			llvm::UndefValue::get(llvmTypeArrayType),
			""
		);

		SpiteIR::Type* typeOfType = typePtrType->pointer.type;
		for (size_t i = 0; i < types->size(); i++)
		{
			SpiteIR::Type* type = types->at(i);
			llvm::Value* typePtr = llvmContext.BuildTypeValue(type);
			llvm::Value* arrPtr = builder.CreateStructGEP(llvmTypeArrayType, typeArr, i);
			llvm::StoreInst* typeStore = builder.CreateStore(typePtr, arrPtr);
		}

		llvm::Value* beginPtr = builder.CreateStructGEP(llvmType, ptr, 0);
		llvm::Value* endPtr = builder.CreateStructGEP(llvmType, ptr, 1);

		llvm::Value* beginPtrValue = builder.CreateStructGEP(
			llvmTypeArrayType,
			typeArr,
			0
		);
		llvm::Value* endPtrValue = builder.CreateStructGEP(
			llvmTypeArrayType,
			typeArr,
			types->size()
		);

		llvm::StoreInst* beginStore = builder.CreateStore(beginPtrValue, beginPtr);
		llvm::StoreInst* endStore = builder.CreateStore(endPtrValue, endPtr);
	}

	void BuildTypeUnionData(SpiteIR::Type* type, llvm::Value* unionPtr)
	{
		eastl::vector<SpiteIR::Member*>* unionMembers = typeMetaState->members.back()->value.type->structureType.members;
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
		{
			SpiteIR::Member* member = unionMembers->at(0);
			llvm::Value* ptr = CastMember(member, unionPtr);
			llvm::Type* primitiveType = ToLLVMType(member->value.type, context);
			BuildTypeBoolData(0, type->primitive.isSigned, ptr, primitiveType);
			BuildTypeIntData(1, static_cast<int>(type->primitive.kind), ptr, primitiveType, llvmContext.int32Type);
			break;
		}
		case SpiteIR::TypeKind::StateType:
		{
			SpiteIR::Member* member = unionMembers->at(1);
			llvm::Value* ptr = CastMember(member, unionPtr);
			SpiteIR::Type* stateType = member->value.type->pointer.type;
			StoreStateData(ptr, type->stateType.state, stateType);
			break;
		}
		case SpiteIR::TypeKind::UnionType:
		case SpiteIR::TypeKind::StructureType:
		{
			SpiteIR::Member* member = unionMembers->at(2);
			llvm::Value* ptr = CastMember(member, unionPtr);
			SpiteIR::Type* memberArrayType = member->value.type->pointer.type;
			StoreStructureData(ptr, type->structureType.members, memberArrayType);
			break;
		}
		case SpiteIR::TypeKind::PointerType:
		{
			llvm::Value* ptr = CastMember(unionMembers->at(3), unionPtr);
			builder.CreateStore(llvmContext.BuildTypeValue(type->pointer.type), ptr);
			break;
		}
		case SpiteIR::TypeKind::ReferenceType:
		{
			llvm::Value* ptr = CastMember(unionMembers->at(4), unionPtr);
			builder.CreateStore(llvmContext.BuildTypeValue(type->reference.type), ptr);
			break;
		}
		case SpiteIR::TypeKind::DynamicArrayType:
		{
			llvm::Value* ptr = CastMember(unionMembers->at(5), unionPtr);
			builder.CreateStore(llvmContext.BuildTypeValue(type->dynamicArray.type), ptr);
			break;
		}
		case SpiteIR::TypeKind::FixedArrayType:
		{
			SpiteIR::Member* member = unionMembers->at(6);
			llvm::Value* ptr = CastMember(member, unionPtr);
			llvm::Type* fixedArrayType = ToLLVMType(member->value.type, context);
			BuildTypeIntData(0, type->fixedArray.count, ptr, fixedArrayType, llvmContext.intType);
			llvm::Value* typePtr = builder.CreateStructGEP(
				fixedArrayType,
				ptr,
				1
			);
			builder.CreateStore(llvmContext.BuildTypeValue(type->fixedArray.type), typePtr);
			break;
		}
		case SpiteIR::TypeKind::FunctionType:
		{
			SpiteIR::Member* member = unionMembers->at(7);
			llvm::Value* ptr = CastMember(member, unionPtr);
			llvm::Type* functionType = ToLLVMType(member->value.type, context);
			llvm::Value* returnTypePtr = builder.CreateStructGEP(
				functionType,
				ptr,
				0
			);
			builder.CreateStore(llvmContext.BuildTypeValue(type->function.returnType), returnTypePtr);
			llvm::Value* paramTypesPtr = builder.CreateStructGEP(
				functionType,
				ptr,
				1
			);
			SpiteIR::Type* typeArrayType = member->value.type->structureType.members->at(1)->value.type->pointer.type;
			StoreTypeArray(paramTypesPtr, type->function.params, typeArrayType);
			break;
		}
		default:
			break;
		}

	}

	void BuildTypeData()
	{
		llvm::StructType* llvmType = StateToLLVMType(typeMetaState, context);

		eastl::hash_set<SpiteIR::Type*, IRTypeHash, IRTypeEqual> seenTypes;
		while (seenTypes.size() != llvmContext.typeMetadataMap.size())
		{
			// @TODO make a custom structure for the type/variable lookup that can check if an iterator has been invalidated
			eastl::hash_map<SpiteIR::Type*, llvm::GlobalVariable*, IRTypeHash, IRTypeEqual> toIterate(llvmContext.typeMetadataMap);
			for (auto& [type, globalVar] : toIterate)
			{
				if (MapHas(seenTypes, type)) continue;
				seenTypes.insert(type);

				BuildTypeIntData(0, type->size, globalVar, llvmType, llvmContext.intType);
				BuildTypeIntData(1, type->alignment, globalVar, llvmType, llvmContext.intType);
				BuildTypeIntData(2, static_cast<int>(type->kind), globalVar, llvmType, llvmContext.int32Type);
				BuildTypeBoolData(3, type->byValue, globalVar, llvmType);
				llvm::Value* unionPtr = builder.CreateStructGEP(
					llvmType,
					globalVar,
					4
				);
				BuildTypeUnionData(type, unionPtr);
			}
		}
	}
};