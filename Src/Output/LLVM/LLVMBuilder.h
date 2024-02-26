#pragma once

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/ADT/StringRef.h"

#include "../../Intermediate/Syntax.h"

extern int targetArchBitWidth;

using LType = llvm::Type;
using LLVMContext = llvm::LLVMContext;
using Module = llvm::Module;
using IRBuilder = llvm::IRBuilder<>;
using StructType = llvm::StructType;
using Constant = llvm::Constant;
using ConstantInt = llvm::ConstantInt;
using GlobalVariable = llvm::GlobalVariable;
using GlobalValue = llvm::GlobalValue;
using StringRef = llvm::StringRef;

struct LPrimitives
{
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
		boolType = LType::getInt1Ty(context);
		byteType = LType::getInt8Ty(context);

		int16Type = LType::getInt16Ty(context);
		int32Type = LType::getInt32Ty(context);
		int64Type = LType::getInt64Ty(context);
		int128Type = LType::getInt128Ty(context);
		intType = targetArchBitWidth == 64 ? int64Type : int32Type;

		float32Type = LType::getFloatTy(context);
		float64Type = LType::getDoubleTy(context);
		float128Type = LType::getFP128Ty(context);
		floatType = targetArchBitWidth == 64 ? float64Type : float32Type;
	}
};

struct LLVMBuilder
{
	Syntax& syntax;
	LLVMContext context;
	IRBuilder builder;
	Module* module;
	LPrimitives primitives;

	LLVMBuilder(Syntax& syntax) : syntax(syntax), builder(context), primitives(context)
	{
		module = new Module(ToStringRef(syntax.package->package.name->val), context);
	}

	~LLVMBuilder()
	{
		delete module;
	}

	void Build()
	{
		BuildTypes();
		BuildGlobals();

		module->print(llvm::outs(), nullptr);
	}

	void BuildTypes()
	{
		for (auto& [key, value] : syntax.symbolTable->stateMap)
		{
			StructType::create(context, ToStringRef(key));
		}

		for (auto& [key, value] : syntax.symbolTable->stateMap)
		{
			auto& state = value.state->state;
			eastl::vector<LType*> members = eastl::vector<LType*>();
			for (Node* member : *state.members)
			{
				Type& type = member->definition.type;
				LType* lType = TypeToLType(type);
				if (lType) members.push_back(lType);
			}

			StructType* structType = StructType::getTypeByName(context, ToStringRef(key));
			structType->setBody(llvm::ArrayRef<LType*>(members.begin(), members.end()));

			structType->print(llvm::outs(), true);
			std::cout << '\n';
		}
	}

	LType* TypeToLType(Type& type)
	{
		switch (type.typeID)
		{
		case InvalidType:
			break;
		case UnknownType:
			break;
		case PrimitiveType:
			return GetPrimitiveType(type.primitiveType.type);
		case NamedType:
			break;
		case ExplicitType:
			break;
		case ImplicitType:
			break;
		case PointerType:
			break;
		case ValueType:
			break;
		case ArrayType:
			break;
		case GenericsType:
			break;
		case FunctionType:
			break;
		case ImportedType:
			break;
		default:
			break;
		}

		return nullptr;
	}

	LType* GetPrimitiveType(UniqueType primitive)
	{
		switch (primitive)
		{
		case Bool:
			return primitives.boolType;
		case Byte:
		case Ubyte:
			return primitives.byteType;
		case Int:
		case Uint:
			return primitives.intType;
		case Int16:
		case Uint16:
			return primitives.int16Type;
		case Int32:
		case Uint32:
			return primitives.int32Type;
		case Int64:
		case Uint64:
			return primitives.int64Type;
		case Int128:
		case Uint128:
			return primitives.int128Type;
		case Float:
			return primitives.floatType;
		case Float32:
			return primitives.float32Type;
		case Float64:
			return primitives.float64Type;
		case Float128:
			return primitives.float128Type;
		case String:
		default:
			return primitives.intType;
		}
	}

	void BuildGlobals()
	{
		for (auto& [key, value] : syntax.symbolTable->globalValMap)
		{
			auto& decl = value->definition;
			LType* type = TypeToLType(decl.type);
			Constant* initialValue = ConstantInt::get(type, 42);
			GlobalVariable* globalVar = new GlobalVariable(
				*module,
				type,
				false,
				GlobalValue::ExternalLinkage,
				initialValue,
				ToStringRef(key)
			);
		}
	}

	inline StringRef ToStringRef(const InplaceString& str)
	{
		return StringRef(str.start, str.count);
	}

};