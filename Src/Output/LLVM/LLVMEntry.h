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

		SpiteIR::Type* valueType = memberType->stateType.state->members.at(0)->value.type;
		llvm::Type* llvmValueType = ToLLVMType(valueType, context);
		llvm::Value* valuePtr = builder.CreateStructGEP(
			llvmType,
			memberVar,
			0
		);
		llvm::Value* valueTypePtr = builder.CreateStructGEP(
			llvmValueType,
			valuePtr,
			0
		);
		builder.CreateStore(llvmContext.BuildTypeValue(member->value.type), valueTypePtr);
		llvm::Value* valueNamePtr = builder.CreateStructGEP(
			llvmValueType,
			valuePtr,
			1
		);
		//SpiteIR::Type* strType = valueType->stateType.state->members.at(1)->value.type;
		//BuildInteropString(member->value.name, strType, valueNamePtr);
		//BuildTypeIntData(1, member->offset, memberVar, llvmType, llvmContext.intType);

		return memberVar;
	}

	void BuildMemberArray(eastl::vector<SpiteIR::Member*>& members, llvm::Value* ptr,
		SpiteIR::Type* memberArrayType)
	{
		llvm::Type* llvmType = ToLLVMType(memberArrayType, context);
		SpiteIR::Type* memberPtrType = memberArrayType->stateType.state->members.at(0)->value.type->pointer.type;
		llvm::Type* llvmMemberArrayType = llvm::ArrayType::get(ToLLVMType(memberPtrType, context),
			members.size());

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
			//llvm::Value* index = llvm::ConstantInt::get(llvmContext.int32Type, i);
			//llvmContext.BuildGEPInst(llvmMemberArrayType, memberPtr, index, memberArr, true);
		}
		
		//llvm::Value* beginPtr = builder.CreateStructGEP(llvmType, ptr, 0);
		//llvm::Value* endPtr = builder.CreateStructGEP(llvmType, ptr, 1);
		//
		//llvm::Value* beginPtrValue = builder.CreateGEP(llvmMemberArrayType, memberArr,
		//	{ llvm::ConstantInt::get(llvmContext.int32Type, 0) });
		//llvm::Value* endPtrValue = builder.CreateGEP(llvmMemberArrayType, memberArr,
		//	{ llvm::ConstantInt::get(llvmContext.int32Type, members.size() - 1) });
		//
		//builder.CreateStore(beginPtrValue, beginPtr);
		//builder.CreateStore(endPtrValue, endPtr);
	}

	void BuildInteropString(eastl::string& str, SpiteIR::Type* interopStringType, llvm::Value* ptr)
	{
		llvm::StringRef strRef = ToStringRef(str);
		llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, strRef, true);
		llvm::ArrayType* byteArrayType = llvm::ArrayType::get(
			llvm::Type::getInt8Ty(context),
			str.size() + 1
		);
		llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
			module,
			byteArrayType,
			true,
			llvm::GlobalValue::PrivateLinkage,
			strConstant,
			""
		);

		llvm::Type* charPtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
		std::vector<llvm::Type*> structElemTypes = { charPtrType, llvmContext.intType, 
			llvmContext.intType, llvmContext.intType };
		llvm::Type* strType = llvm::StructType::get(context, structElemTypes);
		llvm::Value* strValuePtr = builder.CreateBitCast(ptr, llvm::PointerType::get(strType, 0));

		llvm::Value* strBeginPtr = builder.CreateStructGEP(
			strType,
			strValuePtr,
			0
		);
		builder.CreateStore(globalStr, strBeginPtr);
		BuildTypeIntData(1, str.size(), strValuePtr, strType, llvmContext.intType);
		BuildTypeIntData(2, 0, strValuePtr, strType, llvmContext.intType);
	}

	void StoreStateData(llvm::Value* ptr, SpiteIR::State* state, SpiteIR::Type* stateType)
	{
		llvm::Type* llvmType = ToLLVMType(stateType, context);
		llvm::GlobalVariable* stateVarPtr = new llvm::GlobalVariable(
			module,
			llvmType,
			false,
			llvm::GlobalValue::ExternalLinkage,
			llvm::UndefValue::get(llvmType),
			""
		);

		BuildTypeIntData(0, 0, stateVarPtr, llvmType, llvmContext.intType);
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

		builder.CreateStore(stateVarPtr, ptr);
	}

	llvm::Value* BuildTypeArray(eastl::vector<SpiteIR::Type*> types)
	{

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
			BuildTypeIntData(0, type->primitive.isSigned, ptr, primitiveType, llvmContext.boolType);
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
			llvm::Value* ptr = CastMember(unionMembers->at(2), unionPtr);
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
			builder.CreateStore(llvmContext.BuildTypeValue(type->fixedArray.type), returnTypePtr);

			break;
		}
		default:
			break;
		}

	}

	void BuildTypeData()
	{
		llvm::StructType* llvmType = StateToLLVMType(typeMetaState, context);
		while (llvmContext.typeMetadataMap.size())
		{
			auto& [type, globalVar] = *llvmContext.typeMetadataMap.begin();

			BuildTypeIntData(0, type->size, globalVar, llvmType, llvmContext.intType);
			BuildTypeIntData(1, type->alignment, globalVar, llvmType, llvmContext.intType);
			BuildTypeIntData(2, static_cast<int>(type->kind), globalVar, llvmType, llvmContext.int32Type);
			BuildTypeIntData(3, type->byValue, globalVar, llvmType, llvmContext.boolType);
			llvm::Value* unionPtr = builder.CreateStructGEP(
				llvmType,
				globalVar,
				4
			);
			BuildTypeUnionData(type, unionPtr);

			llvmContext.typeMetadataMap.erase(type);
		}
	}
};