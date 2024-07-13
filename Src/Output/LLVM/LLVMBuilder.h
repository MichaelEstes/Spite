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

#include "../../Intermediate/SymbolTable.h"
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
		intType = config.targetArchBitWidth == 64 ? int64Type : int32Type;

		float32Type = LType::getFloatTy(context);
		float64Type = LType::getDoubleTy(context);
		float128Type = LType::getFP128Ty(context);
		floatType = config.targetArchBitWidth == 64 ? float64Type : float32Type;
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
		module = new Module(ToStringRef(symbolTable->package->val), context);
		builder = new IRBuilder(context);
	}

	~LLVMBuilder()
	{
		delete module;
		delete builder;
	}

	void Build()
	{
		BuildTypes();
		BuildGlobals();
		BuildFunctions();

		module->print(llvm::outs(), nullptr);
	}

	void BuildTypes()
	{
		for (auto& [key, value] : symbolTable->stateMap)
		{
			StructType::create(context, PackageName(key));
		}

		for (auto& [key, value] : symbolTable->stateMap)
		{
			auto& state = value.state->state;
			eastl::vector<LType*> members = eastl::vector<LType*>();
			for (Stmnt* member : *state.members)
			{
				Type* type = member->definition.type;
				LType* lType = TypeToLType(type);
				if (lType) members.push_back(lType);
			}

			StringRef typeName = PackageName(key);
			StructType* structType = StructType::getTypeByName(context, typeName);
			structType->setBody(ToArrayRef<LType*>(members));

			for (Stmnt* node : value.methods)
			{
				auto& method = node->method;
				auto& funcDecl = method.decl->functionDecl;
				auto& body = funcDecl.body;
				auto& parameters = funcDecl.parameters;
				eastl::vector<LType*> params = CreateParams(parameters);
				params.push_back(structType->getPointerTo());
				CreateFunction(method.returnType, MethodName(key, method.name), params, body);
			}

			structType->print(llvm::outs(), true);
			std::cout << '\n';
		}
	}

	LType* TypeToLType(Type* type)
	{
		switch (type->typeID)
		{
		case InvalidType:
			break;
		case UnknownType:
			break;
		case PrimitiveType:
			return GetPrimitiveType(type->primitiveType.type);
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
		case TemplatedType:
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
		case Void:
			return primitives.voidType;
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

	inline eastl::vector<LType*> CreateParams(eastl::vector<Stmnt*>* parameters)
	{
		eastl::vector<LType*> params = eastl::vector<LType*>();
		for (Stmnt* node : *parameters)
		{
			Type* type = node->definition.type;
			LType* lType = TypeToLType(type);
			if (lType) params.push_back(lType);
		}

		return params;
	}

	inline void CreateFunction(Type* returnType, const StringRef& name, const eastl::vector<LType*>& params, Body& body)
	{
		LType* lReturnType = TypeToLType(returnType);
		LFunctionType* type = LFunctionType::get(lReturnType, ToArrayRef<LType*>(params), false);
		Function* llvmFunc = Function::Create(type, Function::ExternalLinkage, name, module);
		if (body.statement) llvmFunc->addFnAttr(llvm::Attribute::AlwaysInline);
	}

	inline Function* InitFunction(const StringRef& funcName, eastl::vector<Stmnt*>* parameters)
	{
		Function* llvmFunc = module->getFunction(funcName);
		BasicBlock* entryBasicBlock = BasicBlock::Create(context, "entry", llvmFunc);
		builder->SetInsertPoint(entryBasicBlock);

		localVariableMap.clear();
		for (unsigned i = 0; i < parameters->size(); i++)
		{
			Argument* param = llvmFunc->getArg(i);
			Stmnt* defNode = parameters->at(i);
			auto& paramDef = defNode->definition;
			StringView paramName = paramDef.name->val;
			LType* type = param->getType();
			AllocaInst* allocInst = builder->CreateAlloca(type, GetDefinitionValue(defNode), ToStringRef(paramName));
			localVariableMap[paramName] = allocInst;
			builder->CreateStore(param, allocInst);
		}

		return llvmFunc;
	}

	void BuildFunctions()
	{
		for (auto& [key, value] : symbolTable->functionMap)
		{
			auto& func = value->function;
			auto& funcDecl = func.decl->functionDecl;
			auto& body = funcDecl.body;
			auto& parameters = funcDecl.parameters;
			eastl::vector<LType*> params = CreateParams(parameters);
			CreateFunction(func.returnType, PackageName(key), params, body);
		}

		for (auto& [key, value] : symbolTable->functionMap)
		{
			auto& func = value->function;
			auto& funcDecl = func.decl->functionDecl;
			auto& body = funcDecl.body;
			auto& parameters = funcDecl.parameters;

			InitFunction(PackageName(key), parameters);
			NodeGenerator(body.body);
		}

		for (auto& [key, value] : symbolTable->stateMap)
		{
			for (Stmnt* methodNode : value.methods)
			{
				auto& method = methodNode->method;
				auto& funcDecl = method.decl->functionDecl;
				auto& body = funcDecl.body;
				auto& parameters = funcDecl.parameters;

				Function* llvmFunc = InitFunction(MethodName(key, method.name), parameters);
				Argument* param = llvmFunc->getArg(parameters->size());
				LType* type = param->getType();
				AllocaInst* allocInst = builder->CreateAlloca(type, nullptr, "this");
				builder->CreateStore(param, allocInst);

				NodeGenerator(body.body);
			}
		}
	}

	Value* GetDefinitionValue(Stmnt* definitionNode)
	{
		auto& def = definitionNode->definition;
		if (def.assignment)
		{

		}

		return nullptr;
	}

	void BuildGlobals()
	{
		for (auto& [key, value] : symbolTable->globalValMap)
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
				PackageName(key)
			);
		}
	}

	void NodeGenerator(Stmnt* node)
	{
		switch (node->nodeID)
		{
		case ExpressionStmnt:
		{
			auto& expressionStmnt = node->expressionStmnt;
			break;
		}
		case Definition:
		{
			auto& definition = node->definition;
			break;
		}
		case InlineDefinition:
		{
			auto& inlineDefinition = node->inlineDefinition;
			break;
		}
		case Conditional:
		{
			auto& conditional = node->conditional;
			break;
		}
		case AssignmentStmnt:
		{
			auto& assignmentStmnt = node->assignmentStmnt;
			break;
		}
		case IfStmnt:
		{
			auto& ifStmnt = node->ifStmnt;
			break;
		}
		case ForStmnt:
		{
			auto& forStmnt = node->forStmnt;
			break;
		}
		case WhileStmnt:
		{
			auto& whileStmnt = node->whileStmnt;
			break;
		}
		case SwitchStmnt:
		{
			auto& switchStmnt = node->switchStmnt;
			break;
		}
		case DeleteStmnt:
		{
			auto& deleteStmnt = node->deleteStmnt;
			break;
		}
		case DeferStmnt:
		{
			auto& deferStmnt = node->deferStmnt;
			break;
		}
		case ContinueStmnt:
		{
			auto& continueStmnt = node->continueStmnt;
			break;
		}
		case BreakStmnt:
		{
			auto& breakStmnt = node->breakStmnt;
			break;
		}
		case ReturnStmnt:
		{
			auto& returnStmnt = node->returnStmnt;
			break;
		}
		case Block:
		{
			auto& block = node->block;
			for (Stmnt* node : *block.inner) NodeGenerator(node);
			break;
		}
		default:
			break;
		}
	}

	void ExprGenerator(Expr* expr)
	{
		switch (expr->typeID)
		{
		case InvalidExpr:
			break;
		case LiteralExpr:
			break;
		case IdentifierExpr:
			break;
		case PrimitiveExpr:
			break;
		case SelectorExpr:
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			break;
		case NewExpr:
			break;
		case FixedExpr:
			break;
		case TypeLiteralExpr:
			break;
		case AsExpr:
			break;
		case DereferenceExpr:
			break;
		case ReferenceExpr:
			break;
		case BinaryExpr:
			break;
		case UnaryExpr:
			break;
		case GroupedExpr:
			break;
		case TemplateExpr:
			break;
		case TypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		default:
			break;
		}
	}

	inline StringRef ToStringRef(const StringView& str)
	{
		return StringRef(str.start, str.count);
	}

	inline StringRef MethodName(const StringView& stateName, const Token* nameTok)
	{
		const StringView& methodName = nameTok->val;
		StringView& packageStr = symbolTable->package->val;
		size_t count = packageStr.count + stateName.count + methodName.count + 2;
		char* concated = new char[count];
		memcpy(concated, packageStr.start, packageStr.count);
		concated[packageStr.count] = ':';
		memcpy(concated + packageStr.count + 1, stateName.start, stateName.count);
		concated[packageStr.count + stateName.count + 1] = ':';
		memcpy(concated + packageStr.count + stateName.count + 2, methodName.start, methodName.count);
		return StringRef(concated, count);
	}

	inline StringRef PackageName(const StringView& str)
	{
		StringView& packageStr = symbolTable->package->val;
		size_t count = packageStr.count + str.count + 1;
		char* concated = new char[count];
		memcpy(concated, packageStr.start, packageStr.count);
		concated[packageStr.count] = ':';
		memcpy(concated + packageStr.count + 1, str.start, str.count);
		return StringRef(concated, count);
	}

	template<typename T>
	inline llvm::ArrayRef<T> ToArrayRef(const eastl::vector<T>& vec)
	{
		return llvm::ArrayRef<T>(vec.begin(), vec.end());
	}
};