#pragma once

#include <filesystem>

#include "llvm/ADT/SmallVector.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DIBuilder.h"

#include "./LLVMContext.h"

extern SpiteIR::State* stringState;

struct ILLVMDebugInfo
{
	virtual ~ILLVMDebugInfo() = default;

	virtual void EnterFunction(SpiteIR::Function* function, llvm::Function* llvmFunc) = 0;
	virtual void ExitFunction() = 0;
	virtual void SetLocation(SpiteIR::Instruction* inst) = 0;
	virtual void DeclareLocal(size_t reg, llvm::AllocaInst* alloca) = 0;
	virtual void DeclareGlobal(SpiteIR::GlobalVariable* globalVar, llvm::GlobalVariable* llvmGlobalVar) = 0;
	virtual void Finalize() = 0;
};

struct NoOpLLVMDebugInfo : ILLVMDebugInfo
{
	void EnterFunction(SpiteIR::Function* function, llvm::Function* llvmFunc) override {}
	void ExitFunction() override {}
	void SetLocation(SpiteIR::Instruction* inst) override {}
	void DeclareLocal(size_t reg, llvm::AllocaInst* alloca) override {}
	void DeclareGlobal(SpiteIR::GlobalVariable* globalVar, llvm::GlobalVariable* llvmGlobalVar) override {}
	void Finalize() override {}
};

#ifndef _NO_DEBUG
struct LLVMDebugInfo : ILLVMDebugInfo
{
	LLVMContext& llvmContext;
	llvm::Module& module;
	llvm::LLVMContext& context;
	llvm::DIBuilder diBuilder;

	llvm::DICompileUnit* compileUnit = nullptr;

	eastl::hash_map<eastl::string, llvm::DIFile*> fileCache;
	eastl::hash_map<SpiteIR::Function*, llvm::DISubprogram*> subprogramCache;
	eastl::hash_map<SpiteIR::State*, llvm::DICompositeType*> stateTypeCache;
	eastl::hash_map<SpiteIR::Type*, llvm::DIType*> typeCache;
	llvm::DIType* byteDIType = nullptr;

	SpiteIR::Function* currentFunction = nullptr;
	llvm::DISubprogram* currentSubprogram = nullptr;
	SpiteIR::DebugSymbolGraph* currentGraph = nullptr;
	eastl::hash_map<size_t, llvm::DILexicalBlock*> lexicalBlockCache;

	LLVMDebugInfo(LLVMContext& llvmContext) : llvmContext(llvmContext),
		module(llvmContext.module),
		context(llvmContext.context),
		diBuilder(llvmContext.module)
	{
		eastl::string mainPath = !config.file.empty() ? config.file : config.name;
		llvm::DIFile* mainFile = GetOrCreateFile(mainPath);
		compileUnit = diBuilder.createCompileUnit(
			llvm::dwarf::DW_LANG_C99, mainFile, "Spite", false, "", 0);

		module.addModuleFlag(llvm::Module::Warning, "Debug Info Version",
			(uint32_t)llvm::DEBUG_METADATA_VERSION);

		if (config.os == Windows)
		{
			module.addModuleFlag(llvm::Module::Warning, "CodeView", (uint32_t)1);
		}
		else
		{
			module.addModuleFlag(llvm::Module::Warning, "Dwarf Version", (uint32_t)4);
		}
	}

	llvm::DIFile* GetOrCreateFile(const eastl::string& path)
	{
		if (MapHas(fileCache, path)) return fileCache.at(path);

		std::filesystem::path fsPath(path.c_str());
		std::string filename = fsPath.has_filename() ? fsPath.filename().string() : std::string(path.c_str());
		std::string directory = fsPath.has_parent_path() ? fsPath.parent_path().string() : "";

		llvm::DIFile* file = diBuilder.createFile(filename, directory);
		fileCache[path] = file;
		return file;
	}

	llvm::DIFile* GetOrCreateFile(const eastl::string* path)
	{
		if (!path || path->empty()) return compileUnit ? compileUnit->getFile() : nullptr;
		return GetOrCreateFile(*path);
	}

	SpiteIR::DebugSymbolGraph* GetDebugGraph(SpiteIR::Function* function)
	{
		if (!llvmContext.ir->debugSymbolLookup) return nullptr;
		if (!MapHas(*llvmContext.ir->debugSymbolLookup, function)) return nullptr;
		return llvmContext.ir->debugSymbolLookup->at(function);
	}

	void EnterFunction(SpiteIR::Function* function, llvm::Function* llvmFunc) override
	{
		lexicalBlockCache.clear();

		currentFunction = function;
		currentGraph = GetDebugGraph(function);
		currentSubprogram = GetOrCreateSubprogram(function, llvmFunc);
		llvmContext.builder.SetCurrentDebugLocation(llvm::DebugLoc());
	}

	void ExitFunction() override
	{
		if (currentSubprogram)
		{
			diBuilder.finalizeSubprogram(currentSubprogram);
		}

		llvmContext.builder.SetCurrentDebugLocation(llvm::DebugLoc());
		currentFunction = nullptr;
		currentSubprogram = nullptr;
		currentGraph = nullptr;
	}

	llvm::DISubprogram* GetOrCreateSubprogram(SpiteIR::Function* function, llvm::Function* llvmFunc)
	{
		if (MapHas(subprogramCache, function))
		{
			llvm::DISubprogram* subprogram = subprogramCache.at(function);
			llvmFunc->setSubprogram(subprogram);
			return subprogram;
		}

		Position* pos = function->metadata.position;
		llvm::DIFile* file = (pos && pos->file) ? GetOrCreateFile(pos->file) : compileUnit->getFile();
		unsigned line = pos ? (unsigned)pos->line : 0;

		llvm::DISubroutineType* subroutineType = BuildSubroutineType(function);

		llvm::DISubprogram* subprogram = diBuilder.createFunction(
			file,
			ToStringRef(function->name),
			llvmFunc->getName(),
			file,
			line,
			subroutineType,
			line,
			llvm::DINode::FlagPrototyped,
			llvm::DISubprogram::SPFlagDefinition
		);

		llvmFunc->setSubprogram(subprogram);
		subprogramCache[function] = subprogram;
		return subprogram;
	}

	llvm::DISubroutineType* BuildSubroutineType(SpiteIR::Function* function)
	{
		std::vector<llvm::Metadata*> types;
		types.push_back(ToDIType(function->returnType));
		for (SpiteIR::Argument* arg : function->arguments)
			types.push_back(ToDIType(arg->value.type));

		return diBuilder.createSubroutineType(diBuilder.getOrCreateTypeArray(types));
	}

	void SetLocation(SpiteIR::Instruction* inst) override
	{
		if (!currentSubprogram)
		{
			llvmContext.builder.SetCurrentDebugLocation(llvm::DebugLoc());
			return;
		}

		const Position& pos = inst->metadata->statementPosition;
		llvm::DILocalScope* scope = ResolveScope(pos);
		unsigned line = pos.line ? (unsigned)pos.line : currentSubprogram->getLine();
		unsigned col = (unsigned)pos.columnOffset;

		llvm::DILocation* loc = llvm::DILocation::get(context, line, col, scope);
		llvmContext.builder.SetCurrentDebugLocation(loc);
	}

	unsigned FindArgumentNumber(size_t reg)
	{
		if (!currentFunction) return 0;

		size_t offset = 0;
		for (size_t i = 0; i < currentFunction->arguments.size(); i++)
		{
			if (reg == offset) return (unsigned)(i + 1);
			offset += currentFunction->arguments.at(i)->value.type->size;
		}
		return 0;
	}

	void DeclareLocal(size_t reg, llvm::AllocaInst* alloca) override
	{
		if (!currentGraph || !currentSubprogram) return;
		if (!MapHas(currentGraph->regToSymbol, reg)) return;

		size_t symbolIndex = currentGraph->regToSymbol.at(reg);
		if (symbolIndex >= currentGraph->symbols.size()) return;

		SpiteIR::DebugLocalSymbol* symbol = currentGraph->symbols[symbolIndex];
		if (!symbol->type) return;

		llvm::DIFile* file = GetOrCreateFile(symbol->declarationPosition.file);
		unsigned line = (unsigned)symbol->declarationPosition.line;
		llvm::DIType* diType = ToDIType(symbol->type);
		if (!diType) return;

		llvm::DILocalVariable* localVar;
		unsigned argNo = FindArgumentNumber(reg);
		if (argNo)
		{
			localVar = diBuilder.createParameterVariable(
				currentSubprogram, ToStringRef(symbol->name), argNo, file, line, diType);
		}
		else
		{
			llvm::DILocalScope* scope = currentSubprogram;
			if (symbol->scopeId != SpiteIR::InvalidDebugIndex && symbol->scopeId < currentGraph->scopes.size())
			{
				scope = GetOrCreateLexicalBlock(currentGraph->scopes[symbol->scopeId]);
			}

			localVar = diBuilder.createAutoVariable(
				scope, ToStringRef(symbol->name), file, line, diType);
		}

		llvm::DILocation* loc = llvm::DILocation::get(
			context, line, (unsigned)symbol->declarationPosition.columnOffset, localVar->getScope());

		diBuilder.insertDeclare(alloca, localVar, diBuilder.createExpression(), loc, alloca->getParent());
	}

	void DeclareGlobal(SpiteIR::GlobalVariable* globalVar, llvm::GlobalVariable* llvmGlobalVar) override
	{
		llvm::DIType* diType = ToDIType(globalVar->type);
		if (!diType) return;

		llvm::DIFile* file = (globalVar->parent) ? GetOrCreateFile(globalVar->parent->file) :
			(compileUnit ? compileUnit->getFile() : nullptr);

		llvm::DIGlobalVariableExpression* diGlobalVarExpr = diBuilder.createGlobalVariableExpression(
			compileUnit, ToStringRef(globalVar->name), llvmGlobalVar->getName(), file, 0, diType,
			false);

		llvmGlobalVar->addDebugInfo(diGlobalVarExpr);
	}

	bool PositionInScope(const Position& pos, SpiteIR::DebugScope* scope)
	{
		if (!scope->startPosition.file || !pos.file) return false;
		if (*scope->startPosition.file != *pos.file) return false;
		if (pos.line < scope->startPosition.line) return false;
		if (scope->endPosition.file && pos.line > scope->endPosition.line) return false;
		return true;
	}

	size_t ScopeDepth(SpiteIR::DebugScope* scope)
	{
		size_t depth = 0;
		size_t parentId = scope->parentId;
		while (parentId != SpiteIR::InvalidDebugIndex && parentId < currentGraph->scopes.size())
		{
			depth++;
			parentId = currentGraph->scopes[parentId]->parentId;
		}
		return depth;
	}

	llvm::DILocalScope* ResolveScope(const Position& pos)
	{
		if (!currentGraph || !pos.file) return currentSubprogram;

		SpiteIR::DebugScope* best = nullptr;
		size_t bestDepth = 0;
		for (SpiteIR::DebugScope* scope : currentGraph->scopes)
		{
			if (!PositionInScope(pos, scope)) continue;

			size_t depth = ScopeDepth(scope);
			if (!best || depth > bestDepth)
			{
				best = scope;
				bestDepth = depth;
			}
		}

		if (!best) return currentSubprogram;
		return GetOrCreateLexicalBlock(best);
	}

	llvm::DILexicalBlock* GetOrCreateLexicalBlock(SpiteIR::DebugScope* scope)
	{
		if (MapHas(lexicalBlockCache, scope->id)) return lexicalBlockCache.at(scope->id);

		llvm::DILocalScope* parentScope = currentSubprogram;
		if (scope->parentId != SpiteIR::InvalidDebugIndex && scope->parentId < currentGraph->scopes.size())
		{
			parentScope = GetOrCreateLexicalBlock(currentGraph->scopes[scope->parentId]);
		}

		llvm::DIFile* file = GetOrCreateFile(scope->startPosition.file);
		llvm::DILexicalBlock* block = diBuilder.createLexicalBlock(
			parentScope, file, (unsigned)scope->startPosition.line, (unsigned)scope->startPosition.columnOffset);

		lexicalBlockCache[scope->id] = block;
		return block;
	}

	llvm::DIType* GetByteDIType()
	{
		if (!byteDIType) byteDIType = diBuilder.createBasicType("byte", 8, llvm::dwarf::DW_ATE_unsigned_char);
		return byteDIType;
	}

	llvm::DIType* ToDIType(SpiteIR::Type* type)
	{
		if (!type) return nullptr;

		if (MapHas(typeCache, type)) return typeCache.at(type);

		llvm::DIType* result = nullptr;
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
			result = BuildPrimitiveDIType(type);
			break;
		case SpiteIR::TypeKind::StateType:
			return ToDIStateType(type->stateType.state);
		case SpiteIR::TypeKind::StructureType:
			result = BuildStructureDIType(type, *type->structureType.members);
			break;
		case SpiteIR::TypeKind::PointerType:
			result = diBuilder.createPointerType(ToDIType(type->pointer.type), (uint64_t)type->size * 8);
			break;
		case SpiteIR::TypeKind::ReferenceType:
			result = diBuilder.createReferenceType(llvm::dwarf::DW_TAG_reference_type,
				ToDIType(type->reference.type), (uint64_t)type->size * 8);
			break;
		case SpiteIR::TypeKind::DynamicArrayType:
			return ToDIStateType(arrayState);
		case SpiteIR::TypeKind::FixedArrayType:
			result = BuildFixedArrayDIType(type);
			break;
		case SpiteIR::TypeKind::FunctionType:
			result = BuildFunctionPointerDIType(type);
			break;
		case SpiteIR::TypeKind::UnionType:
			result = BuildUnionApproxDIType(type);
			break;
		default:
			break;
		}

		typeCache[type] = result;
		return result;
	}

	llvm::DIType* BuildPrimitiveDIType(SpiteIR::Type* type)
	{
		uint64_t sizeInBits = (uint64_t)type->size * 8;
		switch (type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Void:
			return diBuilder.createUnspecifiedType("void");
		case SpiteIR::PrimitiveKind::Bool:
			return diBuilder.createBasicType("bool", sizeInBits, llvm::dwarf::DW_ATE_boolean);
		case SpiteIR::PrimitiveKind::Byte:
			return diBuilder.createBasicType("byte", sizeInBits, llvm::dwarf::DW_ATE_unsigned_char);
		case SpiteIR::PrimitiveKind::I16:
			return diBuilder.createBasicType("i16", sizeInBits, SignedEncoding(type));
		case SpiteIR::PrimitiveKind::I32:
			return diBuilder.createBasicType("i32", sizeInBits, SignedEncoding(type));
		case SpiteIR::PrimitiveKind::I64:
			return diBuilder.createBasicType("i64", sizeInBits, SignedEncoding(type));
		case SpiteIR::PrimitiveKind::Int:
			return diBuilder.createBasicType("int", sizeInBits, SignedEncoding(type));
		case SpiteIR::PrimitiveKind::F32:
			return diBuilder.createBasicType("f32", sizeInBits, llvm::dwarf::DW_ATE_float);
		case SpiteIR::PrimitiveKind::Float:
			return diBuilder.createBasicType("float", sizeInBits, llvm::dwarf::DW_ATE_float);
		case SpiteIR::PrimitiveKind::String:
			return ToDIStateType(stringState);
		default:
			return diBuilder.createUnspecifiedType("unknown");
		}
	}

	unsigned SignedEncoding(SpiteIR::Type* type)
	{
		return type->primitive.isSigned ? llvm::dwarf::DW_ATE_signed : llvm::dwarf::DW_ATE_unsigned;
	}

	llvm::DIType* ToDIStateType(SpiteIR::State* state)
	{
		if (MapHas(stateTypeCache, state)) return stateTypeCache.at(state);

		llvm::DIFile* file = state->parent ? GetOrCreateFile(state->parent->file) : nullptr;
		uint64_t sizeInBits = (uint64_t)state->size * 8;
		uint64_t alignInBits = (uint64_t)(state->alignment ? state->alignment : 1) * 8;

		llvm::DICompositeType* composite = diBuilder.createStructType(
			compileUnit, ToStringRef(state->name), file, 0, sizeInBits, alignInBits,
			llvm::DINode::FlagZero, nullptr, llvm::DINodeArray());
		stateTypeCache[state] = composite;

		std::vector<llvm::Metadata*> elements;
		for (SpiteIR::Member* member : state->members)
		{
			llvm::DIType* memberType = ToDIType(member->value.type);
			uint64_t memberSizeBits = (uint64_t)member->value.type->size * 8;
			elements.push_back(diBuilder.createMemberType(
				composite, ToStringRef(member->value.name), file, 0,
				memberSizeBits, 0, member->offset * 8, llvm::DINode::FlagZero, memberType));
		}
		diBuilder.replaceArrays(composite, diBuilder.getOrCreateArray(elements));

		return composite;
	}

	llvm::DIType* BuildStructureDIType(SpiteIR::Type* type, eastl::vector<SpiteIR::Member*>& members)
	{
		uint64_t sizeInBits = (uint64_t)type->size * 8;
		uint64_t alignInBits = (uint64_t)(type->alignment ? type->alignment : 1) * 8;

		llvm::DICompositeType* composite = diBuilder.createStructType(
			compileUnit, "", compileUnit->getFile(), 0, sizeInBits, alignInBits,
			llvm::DINode::FlagZero, nullptr, llvm::DINodeArray());
		typeCache[type] = composite;

		std::vector<llvm::Metadata*> elements;
		for (SpiteIR::Member* member : members)
		{
			llvm::DIType* memberType = ToDIType(member->value.type);
			uint64_t memberSizeBits = (uint64_t)member->value.type->size * 8;
			elements.push_back(diBuilder.createMemberType(
				composite, ToStringRef(member->value.name), compileUnit->getFile(), 0,
				memberSizeBits, 0, member->offset * 8, llvm::DINode::FlagZero, memberType));
		}
		diBuilder.replaceArrays(composite, diBuilder.getOrCreateArray(elements));

		return composite;
	}

	llvm::DIType* BuildFixedArrayDIType(SpiteIR::Type* type)
	{
		llvm::DIType* elemType = ToDIType(type->fixedArray.type);
		uint64_t sizeInBits = (uint64_t)type->size * 8;
		uint64_t alignInBits = (uint64_t)(type->alignment ? type->alignment : 1) * 8;

		llvm::SmallVector<llvm::Metadata*, 1> subscripts;
		subscripts.push_back(diBuilder.getOrCreateSubrange(0, (int64_t)type->fixedArray.count));

		return diBuilder.createArrayType(sizeInBits, (uint32_t)alignInBits, elemType,
			diBuilder.getOrCreateArray(subscripts));
	}

	llvm::DIType* BuildFunctionPointerDIType(SpiteIR::Type* type)
	{
		std::vector<llvm::Metadata*> paramTypes;
		paramTypes.push_back(ToDIType(type->function.returnType));
		for (SpiteIR::Type* param : *type->function.params)
			paramTypes.push_back(ToDIType(param));

		llvm::DISubroutineType* subroutine = diBuilder.createSubroutineType(
			diBuilder.getOrCreateTypeArray(paramTypes));

		return diBuilder.createPointerType(subroutine, (uint64_t)type->size * 8);
	}

	llvm::DIType* BuildUnionApproxDIType(SpiteIR::Type* type)
	{
		uint64_t sizeInBits = (uint64_t)type->size * 8;

		llvm::SmallVector<llvm::Metadata*, 1> subscripts;
		subscripts.push_back(diBuilder.getOrCreateSubrange(0, (int64_t)type->size));

		return diBuilder.createArrayType(sizeInBits, 8, GetByteDIType(),
			diBuilder.getOrCreateArray(subscripts));
	}

	void Finalize() override
	{
		diBuilder.finalize();
	}
};
#endif

inline ILLVMDebugInfo* CreateLLVMDebugInfo(LLVMContext& llvmContext)
{
	#ifndef _NO_DEBUG
	if (config.debug) return new LLVMDebugInfo(llvmContext);
	#endif
	return new NoOpLLVMDebugInfo();
}
