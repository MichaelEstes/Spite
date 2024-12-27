#pragma once

#include "../../IR/IR.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Verifier.h"

llvm::Type* ToLLVMType(SpiteIR::Type* type, llvm::LLVMContext& context, bool pointerType = false);

extern SpiteIR::State* stringState;
extern SpiteIR::State* arrayState;

eastl::hash_map<SpiteIR::State*, llvm::Type*> stateTypeLookup;


template<typename T>
inline llvm::ArrayRef<T> ToArrayRef(const eastl::vector<T>& vec)
{
	return llvm::ArrayRef<T>(vec.begin(), vec.end());
}

inline llvm::StringRef ToStringRef(const eastl::string& str)
{
	return llvm::StringRef(str.cbegin());
}

inline llvm::Twine ToTwine(const eastl::string& str)
{
	return llvm::Twine(str.cbegin());
}

void CreateStructType(llvm::StructType* structType, eastl::vector<SpiteIR::Member>& members, llvm::LLVMContext& context)
{
	std::vector<llvm::Type*> memberTypes;

	for (SpiteIR::Member& member : members)
	{
		memberTypes.push_back(ToLLVMType(member.value->type, context));
	}

	structType->setBody(memberTypes);
}

llvm::Type* StateToLLVMType(SpiteIR::State* state, llvm::LLVMContext& context)
{
	if (MapHas(stateTypeLookup, state)) return stateTypeLookup.at(state);
	llvm::StructType* stateType = llvm::StructType::create(context, ToStringRef(state->name));
	stateTypeLookup[state] = stateType;
	CreateStructType(stateType, state->members, context);
	return stateType;
}

llvm::FunctionType* FunctionToLLVMType(SpiteIR::Function* function, llvm::LLVMContext& context)
{
	llvm::Type* returnType = ToLLVMType(function->returnType, context);
	std::vector<llvm::Type*> paramTypes;
	for (SpiteIR::Argument* param : function->arguments)
		paramTypes.push_back(ToLLVMType(param->value->type, context));

	return llvm::FunctionType::get(returnType, paramTypes, false);
}

llvm::FunctionType* FunctionTypeToLLVMType(SpiteIR::Type* functionType, llvm::LLVMContext& context)
{
	llvm::Type* returnType = ToLLVMType(functionType->function.returnType, context);
	std::vector<llvm::Type*> paramTypes;
	for (SpiteIR::Type* param : *functionType->function.params)
		paramTypes.push_back(ToLLVMType(param, context));

	return llvm::FunctionType::get(returnType, paramTypes, false);
}

llvm::Type* ToLLVMType(SpiteIR::Type* type, llvm::LLVMContext& context, bool pointerType)
{
	switch (type->kind)
	{
		case SpiteIR::TypeKind::PrimitiveType:
			switch (type->primitive.kind)
			{
				case SpiteIR::PrimitiveKind::Void:
				{
					if (pointerType) return llvm::Type::getInt8Ty(context);
					else return llvm::Type::getVoidTy(context);
				}
				case SpiteIR::PrimitiveKind::Bool:
					return llvm::Type::getInt1Ty(context);
				case SpiteIR::PrimitiveKind::Byte:
				case SpiteIR::PrimitiveKind::I16:
				case SpiteIR::PrimitiveKind::I32:
				case SpiteIR::PrimitiveKind::I64:
				case SpiteIR::PrimitiveKind::Int:
					return llvm::IntegerType::get(context, type->size * 8);
				case SpiteIR::PrimitiveKind::F32:
					return llvm::Type::getFloatTy(context);
				case SpiteIR::PrimitiveKind::Float:
					return llvm::Type::getDoubleTy(context);
				case SpiteIR::PrimitiveKind::String:
					return StateToLLVMType(stringState, context);
			default:
				break;
			}
			break;
		case SpiteIR::TypeKind::StateType:
			return StateToLLVMType(type->stateType.state, context);
		case SpiteIR::TypeKind::StructureType:
		{
			llvm::StructType* structType = llvm::StructType::create(context);
			CreateStructType(structType, *type->structureType.members, context);
			return structType;
		}
		case SpiteIR::TypeKind::PointerType:
			return llvm::PointerType::get(ToLLVMType(type->pointer.type, context, true), 0);
		case SpiteIR::TypeKind::ReferenceType:
			return llvm::PointerType::get(ToLLVMType(type->reference.type, context, true), 0);
		case SpiteIR::TypeKind::DynamicArrayType:
			return StateToLLVMType(arrayState, context);
		case SpiteIR::TypeKind::FixedArrayType:
			return llvm::ArrayType::get(ToLLVMType(type->fixedArray.type, context), type->fixedArray.count);
		case SpiteIR::TypeKind::FunctionType:
		{
			return llvm::PointerType::get(FunctionTypeToLLVMType(type, context), 0);
		}
		case SpiteIR::TypeKind::UnionType:
			return llvm::ArrayType::get(llvm::Type::getInt8Ty(context), type->size);
	default:
		break;
	}

	return nullptr;
}