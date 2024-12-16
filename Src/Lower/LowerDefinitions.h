#include "EASTL/deque.h"
#include "../Syntax/GlobalTable.h"
#include "../Syntax/ScopeUtils.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerContext.h"
#include "../Syntax/TypeInference.h"

extern Config config;

const size_t InvalidRegister = (size_t)-1;
const size_t PackageRegister = (size_t)-2;
const size_t StateRegister = (size_t)-3;
const size_t FunctionRegister = (size_t)-4;
const size_t StmntRegister = (size_t)-5;

struct ScopeValue
{
	size_t reg = InvalidRegister;

	union
	{
		SpiteIR::Type* type = nullptr;
		SpiteIR::Package* package;
		SpiteIR::State* state;
		SpiteIR::Function* function;
		Stmnt* stmnt;
	};
};

struct DeferredBody
{
	ScopeValue runTest;
	Body body;
};

struct FunctionScope
{
	eastl::hash_map<StringView, ScopeValue, StringViewHash> scopeMap;
	eastl::vector<DeferredBody> deferred;
	eastl::vector<size_t> toDestroy;
};

enum FunctionContextFlag
{
	ReturnFunctionScopeValue
};

struct FunctionContext
{
	SpiteIR::Function* function;
	SpiteIR::State* templatedMethodState = nullptr;
	eastl::deque<FunctionScope> scopeQueue;
	ScopeUtils scopeUtils = ScopeUtils(nullptr, nullptr);
	eastl::deque<SpiteIR::Label*> breakLabels;
	eastl::deque<SpiteIR::Label*> continueLabels;
	size_t curr = 0;
	size_t forCount = 0;
	size_t whileCount = 0;
	size_t ifCount = 0;
	size_t blockCount = 0;
	size_t anonFuncCount = 0;
	size_t deferCount = 0;
	size_t switchCaseCount = 0;
	Stmnt* currStmnt;
	Expr* currExpr;
	Flags<64> flags;

	void IncrementRegister(SpiteIR::Type* type)
	{
		curr += type->size;
	}

	inline void SetFlag(FunctionContextFlag flag)
	{
		flags.Set(flag);
	}

	inline void ClearFlag(FunctionContextFlag flag)
	{
		flags.Clear(flag);
	}

	inline bool HasFlag(FunctionContextFlag flag)
	{
		return flags[flag];
	}

	void Reset(SpiteIR::Function* function, SymbolTable* symbolTable, GlobalTable* globalTable)
	{
		this->function = function;
		scopeQueue.clear();
		scopeUtils.scopeQueue.clear();
		scopeUtils.globalTable = globalTable;
		scopeUtils.symbolTable = symbolTable;
		templatedMethodState = nullptr;
		breakLabels.clear();
		continueLabels.clear();
		curr = 0;
		forCount = 0;
		whileCount = 0;
		ifCount = 0;
		blockCount = 0;
		anonFuncCount = 0;
		deferCount = 0;
		switchCaseCount = 0;
		flags.ClearAll();
	}
};

const ScopeValue InvalidScopeValue = { InvalidRegister, nullptr };

struct DeferredCompile
{
	SpiteIR::Function* compileFunc;
	SpiteIR::Instruction* storeInst = nullptr;
};

struct LowerDefinitions
{
	LowerContext& context;
	FunctionContext funcContext;
	SymbolTable* symbolTable = nullptr;

	eastl::hash_set<SpiteIR::Package*> loweredPackages;
	eastl::deque<DeferredCompile> deferredCompiles;

	Stmnt* currentStmnt = nullptr;
	eastl::vector<Expr*>* currTemplates = nullptr;
	eastl::vector<Token*> currGenerics;
	SpiteIR::Package* currPackage = nullptr;

	SpiteIR::State* arrayState = nullptr;
	SpiteIR::Function* makeArray = nullptr;
	SpiteIR::Function* makeArrayFrom = nullptr;
	SpiteIR::Function* sizeArray = nullptr;

	SpiteIR::Function* allocFunc = nullptr;
	SpiteIR::Function* deallocFunc = nullptr;

	SpiteIR::State* stringState = nullptr;


	SpiteIR::Type* castBool;
	SpiteIR::Type* castInt;
	SpiteIR::Type* indexByte;

	LowerDefinitions(LowerContext& context) : context(context)
	{
		context.interpreter->Initialize(context.ir);
		castBool = CreateBoolType(context.ir);
		castInt = CreateIntType(context.ir);
		indexByte = CreateByteType(context.ir);
		AssignRuntimeDeclarations(context.ir);
	}

	void AssignRuntimeDeclarations(SpiteIR::IR* ir)
	{
		SpiteIR::Package* runtime = ir->runtime;
		arrayState = FindPackageState(runtime, "__array");
		makeArray = FindPackageFunction(runtime, "__make_array");
		makeArrayFrom = FindPackageFunction(runtime, "__make_array_from");
		sizeArray = FindPackageFunction(runtime, "__size_array");

		allocFunc = FindPackageFunction(runtime, "__alloc");
		deallocFunc = FindPackageFunction(runtime, "__dealloc");

		stringState = FindPackageState(runtime, "___string");

	}

	ScopeUtils& GetScopeUtils()
	{
		return funcContext.scopeUtils;
	}

	SpiteIR::State* FindPackageState(SpiteIR::Package* package, const eastl::string& name)
	{
		if (MapHas(package->states, name))
		{
			return package->states[name];
		}

		return nullptr;
	}

	SpiteIR::Function* FindPackageFunction(SpiteIR::Package* package, const eastl::string& name)
	{
		if (MapHas(package->functions, name))
		{
			return package->functions[name];
		}

		return nullptr;
	}

	void AddGenericsToCurrent(Stmnt* stmnt)
	{
		Stmnt* generics = GetGenerics(stmnt);
		if (generics)
		{
			for (Token* genericName : *generics->generics.names)
				currGenerics.push_back(genericName);
		}
	}

	void SetCurrentGenerics(Stmnt* stmnt)
	{
		currGenerics.clear();
		if (IsStateFunction(stmnt))
		{
			Token* stateName = GetStateName(stmnt);
			Stmnt* state = symbolTable->FindState(stateName->val);
			AddGenericsToCurrent(state);
		}

		AddGenericsToCurrent(stmnt);
	}

	Expr* ExpandTemplate(Expr* expr)
	{
		return _ExpandTemplate(expr, &currGenerics, currTemplates);
	}

	eastl::vector<Expr*> ExpandTemplates(eastl::vector<Expr*>* exprs)
	{
		return _ExpandTemplates(exprs, &currGenerics, currTemplates);
	}

	void BuildDefinitions()
	{
		for (SpiteIR::Package* package : context.ir->packages)
		{
			BuildPackageDefinitions(package);
		}
	}

	void BuildPackageDefinitions(SpiteIR::Package* package)
	{
		if (MapHas(loweredPackages, package)) return;
		loweredPackages.insert(package);

		for (SpiteIR::Package* importPkg : package->imports)
			BuildPackageDefinitions(importPkg);

		currPackage = package;
		symbolTable = context.packageToSymbolTableMap[package];
		for (auto& [key, state] : package->states)
		{
			ASTContainer& stateContainer = context.stateASTMap[state];
			currentStmnt = stateContainer.node;
			currTemplates = stateContainer.templates;
			SetCurrentGenerics(stateContainer.node);
			BuildState(state, stateContainer.node);
		}

		for (auto& [key, function] : package->functions)
		{
			ASTContainer& funcContainer = context.functionASTMap[function];
			currentStmnt = funcContainer.node;
			currTemplates = funcContainer.templates;
			SetCurrentGenerics(funcContainer.node);
			BuildFunction(function, funcContainer.node);
		}

		BuildGlobalVariables(package);

		if (deferredCompiles.size())
		{
			context.interpreter->Initialize(context.ir);
			while (deferredCompiles.size() > 0)
			{
				DeferredCompile comp = deferredCompiles.back();
				SpiteIR::Function* func = comp.compileFunc;
				SpiteIR::Instruction* store = comp.storeInst;

				void* ret = context.interpreter->InterpretFunction(func, 0);
				if (store)
				{
					SpiteIR::Operand src = CreateValueOperand(ret, func->returnType);
					store->store.src = src;
				}
				deferredCompiles.pop_back();
			}
		}
	}

	SpiteIR::Operand CreateValueOperand(void* value, SpiteIR::Type* type)
	{
		SpiteIR::Operand storeOp = SpiteIR::Operand();
		storeOp.type = type;
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
			storeOp.kind = SpiteIR::OperandKind::Literal;
			switch (type->primitive.kind)
			{
			case SpiteIR::PrimitiveKind::Bool:
			case SpiteIR::PrimitiveKind::Byte:
				storeOp.literal.kind = SpiteIR::PrimitiveKind::Byte;
				storeOp.literal.byteLiteral = *(char*)value;
				break;
			case SpiteIR::PrimitiveKind::I16:
				storeOp.literal.kind = SpiteIR::PrimitiveKind::I16;
				storeOp.literal.i16Literal = *(int16_t*)value;
				break;
			case SpiteIR::PrimitiveKind::I32:
				storeOp.literal.kind = SpiteIR::PrimitiveKind::I32;
				storeOp.literal.i32Literal = *(int32_t*)value;
				break;
			case SpiteIR::PrimitiveKind::I64:
				storeOp.literal.kind = SpiteIR::PrimitiveKind::I64;
				storeOp.literal.i64Literal = *(int64_t*)value;
				break;
			case SpiteIR::PrimitiveKind::Int:
				storeOp.literal.kind = SpiteIR::PrimitiveKind::Int;
				storeOp.literal.intLiteral = *(intmax_t*)value;
				break;
			case SpiteIR::PrimitiveKind::F32:
				storeOp.literal.kind = SpiteIR::PrimitiveKind::F32;
				storeOp.literal.f32Literal = *(float*)value;
				break;
			case SpiteIR::PrimitiveKind::Float:
				storeOp.literal.kind = SpiteIR::PrimitiveKind::Float;
				storeOp.literal.floatLiteral = *(double*)value;
				break;
			case SpiteIR::PrimitiveKind::String:
			{
				storeOp.literal.kind = SpiteIR::PrimitiveKind::String;
				size_t* size = (size_t*)value;
				char** strPtr = (char**)(size + 1);
				eastl::string* str = context.ir->AllocateString();
				*str = eastl::string(*strPtr, *size);
				storeOp.literal.stringLiteral = str;
				break;
			}
			}
			break;
		case SpiteIR::TypeKind::StateType:
		{
			storeOp.kind = SpiteIR::OperandKind::StructLiteral;
			SpiteIR::State* state = type->stateType.state;
			eastl::vector<SpiteIR::Operand>* opArr = context.ir->AllocateArray<SpiteIR::Operand>();
			for (SpiteIR::Member* member : state->members)
			{
				size_t offset = member->offset;
				opArr->push_back(CreateValueOperand((char*)value + offset, member->value->type));
			}
			storeOp.structLiteral = opArr;
			break;
		}
		case SpiteIR::TypeKind::StructureType:
		{
			storeOp.kind = SpiteIR::OperandKind::StructLiteral;
			eastl::vector<SpiteIR::Operand>* opArr = context.ir->AllocateArray<SpiteIR::Operand>();
			size_t offset = 0;
			for (SpiteIR::Type* member : *type->structureType.types)
			{
				opArr->push_back(CreateValueOperand((char*)value + offset, member));
				offset += member->size;
			}
			storeOp.structLiteral = opArr;
			break;
		}
		case SpiteIR::TypeKind::FixedArrayType:
		{
			storeOp.kind = SpiteIR::OperandKind::StructLiteral;
			eastl::vector<SpiteIR::Operand>* opArr = context.ir->AllocateArray<SpiteIR::Operand>();
			size_t count = type->fixedArray.count;
			SpiteIR::Type* ofType = type->fixedArray.type;
			for (size_t i = 0; i < count; i++)
			{
				opArr->push_back(CreateValueOperand((char*)value + (i * ofType->size), ofType));
			}
			storeOp.structLiteral = opArr;
			break;
		}
		case SpiteIR::TypeKind::FunctionType:
			storeOp.kind = SpiteIR::OperandKind::Function;
			storeOp.function = *(SpiteIR::Function**)value;
			break;
		case SpiteIR::TypeKind::DynamicArrayType:
		case SpiteIR::TypeKind::PointerType:
		case SpiteIR::TypeKind::ReferenceType:
			break;
		default:
			break;
		}

		return storeOp;
	}

	void AddScope()
	{
		funcContext.scopeQueue.emplace_back();
		funcContext.scopeUtils.AddScope();
	}

	void PopScope()
	{
		FunctionScope scope = funcContext.scopeQueue.back();
		for (DeferredBody& deferred : scope.deferred) BuildDeferred(deferred);

		funcContext.scopeQueue.pop_back();
		funcContext.scopeUtils.PopScope();
	}

	void AddValueToCurrentScope(const StringView& name, const ScopeValue& value, Stmnt* stmnt)
	{
		funcContext.scopeQueue.back().scopeMap[name] = value;
		funcContext.scopeUtils.AddToTopScope(name, stmnt);
	}

	ScopeValue FindScopeValue(StringView& name)
	{
		for (auto it = funcContext.scopeQueue.rbegin(); it != funcContext.scopeQueue.rend(); it++)
		{
			if (MapHas(it->scopeMap, name))
			{
				return it->scopeMap[name];
			}
		}

		return InvalidScopeValue;
	}

	SpiteIR::Label* GetCurrentLabel()
	{
		return funcContext.function->block->labels.back();
	}

	void BuildPackageInitializerName(eastl::string& out, SpiteIR::Package* package)
	{
		out = package->name + "___global";
	}

	void BuildGlobalVariables(SpiteIR::Package* package)
	{
		if (!package->globalVariables.size()) return;
		SpiteIR::Function* func = context.ir->AllocateFunction();
		BuildPackageInitializerName(func->name, package);
		func->parent = package;
		func->returnType = CreateVoidType(context.ir);
		func->block = context.ir->AllocateBlock();
		funcContext.Reset(func, symbolTable, context.globalTable);

		AddScope();
		SpiteIR::Label* entry = BuildLabel("entry");
		AddLabel(entry);

		for (SpiteIR::GlobalVariable* globalVar : package->globalVariables)
		{
			Stmnt* globalVarStmnt = context.globalVarASTMap[globalVar];
			BuildGlobalVariable(globalVar, globalVarStmnt);
		}

		SpiteIR::Label* lastLabel = GetCurrentLabel();
		BuildVoidReturn(lastLabel);
		PopScope();
		package->initializer = func;
	}

	void BuildGlobalVariable(SpiteIR::GlobalVariable* globalVar, Stmnt* globalVarStmnt)
	{
		ScopeValue globalValue = BuildExpr(globalVarStmnt->definition.assignment, globalVarStmnt);
		SpiteIR::Label* label = GetCurrentLabel();
		ScopeValue value = BuildTypeDereference(label, globalValue);

		SpiteIR::Allocate alloc = BuildAllocate(MakeReferenceType(value.type, context.ir));
		SpiteIR::Operand globalRef = AllocateToOperand(alloc);
		SpiteIR::Instruction* loadGlobal = BuildLoadGlobal(label, globalRef, globalVar->index);
		SpiteIR::Instruction* storeGlobal = BuildStorePtr(label, globalRef, BuildRegisterOperand(value));
	}

	void BuildState(SpiteIR::State* state, Stmnt* stateStmnt)
	{

	}

	SpiteIR::Type* ToIRType(Type* type)
	{
		SpiteIR::Type* irType = TypeToIRType(context.ir, type, this, &currGenerics, currTemplates);
		Assert(irType);
		return irType;
	}

	inline SpiteIR::Type* GetConversionType(SpiteIR::Type* type)
	{
		return type->kind == SpiteIR::TypeKind::ReferenceType ? type->reference.type : type;
	}

	ScopeValue CastFixedArrayToDynamic(const ScopeValue& fixedArr, SpiteIR::Type* dynArr)
	{
		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Type* fixedArrType = GetConversionType(fixedArr.type);
		SpiteIR::Type* dynArrType = GetConversionType(dynArr);

		ScopeValue itemSize = BuildLiteralInt(fixedArrType->fixedArray.type->size);
		ScopeValue count = BuildLiteralInt(fixedArrType->fixedArray.count);
		ScopeValue start = BuildTypeReference(label, fixedArr);

		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		params->push_back(BuildRegisterOperand(itemSize));
		params->push_back(BuildRegisterOperand(count));
		params->push_back(BuildRegisterOperand(start));

		SpiteIR::Allocate alloc = BuildAllocate(dynArrType);
		SpiteIR::Instruction* call = BuildCall(makeArrayFrom, alloc.result, params, label);
		if (dynArr->kind == SpiteIR::TypeKind::ReferenceType)
		{
			ScopeValue arrRef = BuildTypeReference(label, { alloc.result, dynArrType });
			return arrRef;
		}

		return { alloc.result, dynArrType };
	}

	ScopeValue HandleAutoCast(const ScopeValue& from, SpiteIR::Type* to, bool skipRefCheck = false)
	{
		if (!from.type || !to) return InvalidScopeValue;

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Type* fromConversionType = GetConversionType(from.type);
		SpiteIR::Type* toConversionType = GetConversionType(to);

		if (IsIRTypeAssignable(toConversionType, fromConversionType) == 2)
		{
			return HandleAutoCast(CastValue(from, toConversionType), to, skipRefCheck);
		}

		if (!skipRefCheck)
		{
			SpiteIR::Type* fromType = from.type;
			if (to->kind == SpiteIR::TypeKind::ReferenceType &&
				fromType->kind != SpiteIR::TypeKind::ReferenceType)
			{
				if (IsAnyType(to) && fromType->kind == SpiteIR::TypeKind::PointerType) return from;
				return HandleAutoCast(BuildTypeReference(label, from), to);
			}

			if (from.type->kind == SpiteIR::TypeKind::ReferenceType &&
				to->kind != SpiteIR::TypeKind::ReferenceType)
			{
				// Can't dereference any type
				if (IsAnyType(from.type)) return from;
				return HandleAutoCast(BuildTypeDereference(label, from), to);
			}
		}

		return from;
	}

	void BuildFunction(SpiteIR::Function* function, Stmnt* funcStmnt)
	{
		Assert(function);

		function->block = context.ir->AllocateBlock();
		funcContext.Reset(function, symbolTable, context.globalTable);

		if (funcStmnt->nodeID == StmntID::StateStmnt)
		{
			//Default state constructor
			SpiteIR::Type* argRefType = function->arguments.front()->value->type;
			SpiteIR::Type* stateType = argRefType->reference.type;
			SpiteIR::Label* label = BuildLabel("entry");
			AddLabel(label);
			SpiteIR::Allocate argAlloc = BuildAllocate(argRefType);
			SpiteIR::Allocate defaultAlloc = BuildAllocate(stateType);
			ScopeValue defaultVal = BuildDefaultValue(stateType, defaultAlloc.result, label);
			BuildStorePtr(label, AllocateToOperand(argAlloc), BuildRegisterOperand(defaultVal));
			BuildVoidReturn(label);
			return;
		}

		Stmnt* decl = GetDeclForFunc(funcStmnt);

		if (!function->metadata.externFunc)
		{
			BuildFunctionDecl(function, decl);
		}
	}

	void BuildFunctionDecl(SpiteIR::Function* function, Stmnt* funcDecl)
	{
		AddScope();
		BuildFunctionArguments(function, funcDecl);
		BuildEntryLabel(function, funcDecl);
		SpiteIR::Label* lastLabel = GetCurrentLabel();
		if (!lastLabel->terminator && IsVoidType(function->returnType))
		{
			BuildVoidReturn(lastLabel);
		}
		PopScope();
	}

	void BuildFunctionArguments(SpiteIR::Function* function, Stmnt* funcDecl)
	{
		Assert(funcDecl->functionDecl.parameters->size() == function->arguments.size());

		for (size_t i = 0; i < function->arguments.size(); i++)
		{
			SpiteIR::Argument* arg = function->arguments.at(i);
			Stmnt* param = funcDecl->functionDecl.parameters->at(i);

			SpiteIR::Allocate alloc = BuildAllocate(arg->value->type);
			StringView name = StringView(arg->value->name.c_str());
			AddValueToCurrentScope(name, { alloc.result, alloc.type }, param);
		}
	}

	void BuildEntryLabel(SpiteIR::Function* function, Stmnt* funcDecl)
	{
		auto& decl = funcDecl->functionDecl;
		Body& body = decl.body;
		BuildLabelBody("entry", body);
	}

	SpiteIR::Label* BuildLabel(const eastl::string& name)
	{
		SpiteIR::Label* label = context.ir->AllocateLabel();
		label->name = name;

		return label;
	}

	void AddLabel(SpiteIR::Label* label)
	{
		funcContext.function->block->labels.push_back(label);
	}

	SpiteIR::Label* BuildLabelBody(const eastl::string& name, Body& body)
	{
		SpiteIR::Label* label = BuildLabel(name);
		AddLabel(label);
		BuildBody(body);

		return label;
	}

	void BuildBody(Body& body)
	{
		AddScope();
		if (body.body->nodeID == StmntID::Block)
		{
			for (Stmnt* stmnt : *body.body->block.inner)
			{
				BuildStmntForBlock(stmnt);
			}
		}
		else
		{
			BuildStmntForBlock(body.body);
		}
		PopScope();
	}

	void BuildStmntForBlock(Stmnt* stmnt)
	{
		funcContext.currStmnt = stmnt;
		switch (stmnt->nodeID)
		{
		case ExpressionStmnt:
			BuildExpr(stmnt->expressionStmnt.expression, stmnt);
			break;
		case Definition:
			BuildVarDefinition(stmnt);
			break;
		case InlineDefinition:
			BuildInlineDefinition(stmnt);
			break;
		case AssignmentStmnt:
			BuildAssignment(stmnt);
			break;
		case IfStmnt:
			BuildIfStmnt(stmnt);
			break;
		case ForStmnt:
			BuildForStmnt(stmnt);
			break;
		case WhileStmnt:
			BuildWhileStmnt(stmnt);
			break;
		case SwitchStmnt:
			BuildSwitchStmnt(stmnt);
			break;
		case DeleteStmnt:
			BuildDelete(stmnt);
			break;
		case DeferStmnt:
			BuildDefer(stmnt);
			break;
		case ContinueStmnt:
			BuildJump(GetCurrentLabel(), funcContext.continueLabels.back());
			break;
		case BreakStmnt:
			BuildJump(GetCurrentLabel(), funcContext.breakLabels.back());
			break;
		case ReturnStmnt:
			BuildReturn(stmnt);
			break;
		case CompileStmnt:
			break;
		case CompileDebugStmnt:
			break;
		case Block:
			BuildBlock(stmnt);
			break;
		case AssertStmnt:
			BuildAssertStmnt(stmnt);
			break;
		case LogStmnt:
		{
			eastl::vector<SpiteIR::Operand>* toLog = context.ir->AllocateArray<SpiteIR::Operand>();
			for (Expr* expr : *stmnt->logStmnt.exprs)
			{
				toLog->push_back(BuildRegisterOperand(BuildExpr(expr, stmnt)));
			}
			SpiteIR::Instruction* logInst = BuildLog(GetCurrentLabel(), toLog);
			break;
		}
		default:
			Logger::ErrorAt("LowerDefinitions:BuildStmnt Invalid Statement", stmnt->start->pos);
			break;
		}
	}

	ScopeValue BuildVarDefinition(Stmnt* stmnt)
	{
		auto& def = stmnt->definition;
		ScopeValue value = HandleAutoCast(BuildExpr(def.assignment, stmnt), ToIRType(def.type), true);
		if (value.type->kind == SpiteIR::TypeKind::ReferenceType && value.type->reference.type->byValue)
		{
			value = BuildTypeDereference(GetCurrentLabel(), value);
		}
		AddValueToCurrentScope(def.name->val, value, stmnt);
		funcContext.scopeUtils.AddToTopScope(def.name->val, stmnt);
		return value;
	}

	void BuildInlineDefinition(Stmnt* stmnt)
	{
		Assert(stmnt->inlineDefinition.type->typeID == TypeID::ExplicitType &&
			stmnt->inlineDefinition.assignment);
		auto& def = stmnt->inlineDefinition;
		ScopeValue value = BuildExpr(def.assignment, stmnt);
	}

	void BuildAssignment(Stmnt* stmnt)
	{
		ScopeValue assignTo = BuildExpr(stmnt->assignmentStmnt.assignTo, stmnt);
		ScopeValue assignment = HandleAutoCast(BuildExpr(stmnt->assignmentStmnt.assignment, stmnt), 
			assignTo.type, true);

		if (stmnt->assignmentStmnt.op->uniqueType != UniqueType::Assign)
		{
			StringView opString = stmnt->assignmentStmnt.op->val.Preceding('=');
			TokenTree::TokenNode* node = tokenTypeLookup.Find(opString);
			Assert(node);

			UniqueType opType = node->uniqueType;
			SpiteIR::State* state = GetStateForType(assignTo.type);
			if (state)
			{
				assignment = BuildStateOperatorCall(state, assignTo, opType, &assignment);
			}

			if (IsBinaryOperator(opType))
			{
				assignment = BuildBinaryOpValue(assignTo, assignment, BinaryOpToIR(opType));
			}
			else 
			{
				//Error
			}
		}

		if (!assignTo.type || !assignment.type) return;
		AssignValues(assignTo, assignment);
	}

	void AssignValues(ScopeValue& dst, ScopeValue& src)
	{
		if (dst.type->kind == SpiteIR::TypeKind::ReferenceType ||
			src.type->kind == SpiteIR::TypeKind::ReferenceType)
			BuildReferenceAssignment(dst, src);
		else
			BuildStore(GetCurrentLabel(), BuildRegisterOperand(dst), BuildRegisterOperand(src));
	}

	void BuildReferenceAssignment(ScopeValue& dst, ScopeValue& src)
	{
		SpiteIR::Label* label = GetCurrentLabel();

		// value = ref - Dereference pointer into value
		if (dst.type->kind != SpiteIR::TypeKind::ReferenceType &&
			src.type->kind == SpiteIR::TypeKind::ReferenceType)
		{
			BuildDereference(label, BuildRegisterOperand(dst), BuildRegisterOperand(src));
		}
		// ref = value - StorePtr
		else if (dst.type->kind == SpiteIR::TypeKind::ReferenceType &&
			src.type->kind != SpiteIR::TypeKind::ReferenceType)
		{
			BuildStorePtr(label, BuildRegisterOperand(dst), BuildRegisterOperand(src));
		}
		// ref = ref - dereference src onto stack and StorePtr
		else
		{
			BuildMove(label, BuildRegisterOperand(dst), BuildRegisterOperand(src));
		}
	}

	SpiteIR::Label* BuildCondition(const eastl::string& thenName, const eastl::string& elseName, Stmnt* stmnt)
	{
		Assert(stmnt->nodeID == StmntID::Conditional);

		ScopeValue cond = HandleAutoCast(BuildExpr(stmnt->conditional.condition, stmnt), castBool);
		SpiteIR::Instruction* fromBranch = BuildBranch(GetCurrentLabel(), BuildRegisterOperand(cond));

		SpiteIR::Label* ifThenLabel = BuildLabelBody(thenName, stmnt->conditional.body);
		fromBranch->branch.true_ = ifThenLabel;

		SpiteIR::Label* ifElseLabel = BuildLabel(elseName);
		AddLabel(ifElseLabel);
		fromBranch->branch.false_ = ifElseLabel;

		return ifThenLabel;
	}

	void BuildIfStmnt(Stmnt* stmnt)
	{
		Assert(stmnt->ifStmnt.condition->nodeID == StmntID::Conditional);
		auto& if_ = stmnt->ifStmnt;

		eastl::vector<SpiteIR::Label*> nonTerminated = eastl::vector<SpiteIR::Label*>();

		size_t ifCount = funcContext.ifCount;
		funcContext.ifCount += 1;
		eastl::string ifThenName = "if_then" + eastl::to_string(ifCount);
		eastl::string ifElseName = "if_else" + eastl::to_string(ifCount);
		SpiteIR::Label* ifThenLabel = BuildCondition(ifThenName, ifElseName, if_.condition);
		if (!ifThenLabel->terminator) nonTerminated.push_back(ifThenLabel);

		for (size_t i = 0; i < if_.elifs->size(); i++)
		{
			Stmnt* elseIfStmnt = if_.elifs->at(i);
			eastl::string elseIfThenName = "else_if_then" + eastl::to_string(ifCount) +
				"_" + eastl::to_string(i);
			eastl::string elseIfElseName = "else_if_else" + eastl::to_string(ifCount) +
				"_" + eastl::to_string(i);
			SpiteIR::Label* elseifThenLabel = BuildCondition(elseIfThenName, elseIfElseName, elseIfStmnt);
			if (!elseifThenLabel->terminator) nonTerminated.push_back(elseifThenLabel);
		}

		if (if_.elseCondition)
		{
			BuildBody(if_.elseCondition);
		}

		SpiteIR::Label* currLabel = GetCurrentLabel();
		if (!currLabel->terminator) nonTerminated.push_back(currLabel);

		if (nonTerminated.size())
		{
			eastl::string ifEndName = "if_end" + eastl::to_string(ifCount);
			SpiteIR::Label* ifEndLabel = BuildLabel(ifEndName);
			AddLabel(ifEndLabel);
			for (SpiteIR::Label* label : nonTerminated)
			{
				BuildJump(label, ifEndLabel);
			}
		}
	}

	void BuildForInStmnt(Stmnt* stmnt)
	{
		auto& for_ = stmnt->forStmnt;
		Assert(!for_.rangeFor);

		Stmnt* defStmnt = for_.iterated.declaration;
		auto& def = defStmnt->definition;
		SpiteIR::Type* defType = ToIRType(def.type);

		ScopeValue iterateValue = BuildExpr(for_.toIterate, stmnt);
		SpiteIR::Type* derefType = GetDereferencedType(iterateValue.type);

		if (derefType->kind == SpiteIR::TypeKind::FixedArrayType)
		{
			//Handle fixed arrays
		}

		SpiteIR::State* iteratedState = GetStateForType(iterateValue.type);
		Assert(iteratedState);

		SpiteIR::Function* nextFunc = FindPackageFunction(iteratedState->parent,
			iteratedState->name + "_" + "next");
		SpiteIR::Function* currFunc = FindPackageFunction(iteratedState->parent,
			iteratedState->name + "_" + "current");
		Assert(nextFunc && currFunc);

		SpiteIR::Type* currReturnType = currFunc->returnType;
		if (derefType->kind == SpiteIR::TypeKind::DynamicArrayType)
			currReturnType = MakeReferenceType(derefType->dynamicArray.type, context.ir);
		
		ScopeValue iterator = BuildStateOperatorCall(iteratedState, iterateValue, UniqueType::In);
		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		params->push_back(BuildRegisterOperand(BuildTypeReference(GetCurrentLabel(), iterateValue)));
		params->push_back(BuildRegisterOperand(BuildTypeReference(GetCurrentLabel(), iterator)));

		eastl::string forInIterName = "for_in_iter" + eastl::to_string(funcContext.forCount);
		eastl::string forInBodyName = "for_in_body" + eastl::to_string(funcContext.forCount);
		eastl::string forInEndName = "for_in_end" + eastl::to_string(funcContext.forCount);
		funcContext.forCount += 1;

		SpiteIR::Label* fromLabel = GetCurrentLabel();
		SpiteIR::Label* forInIterLabel = BuildLabel(forInIterName);
		SpiteIR::Label* forInEndLabel = BuildLabel(forInEndName);

		funcContext.breakLabels.push_back(forInEndLabel);
		funcContext.continueLabels.push_back(forInIterLabel);

		SpiteIR::Instruction* toCond = BuildJump(fromLabel);
		AddLabel(forInIterLabel);
		toCond->jump.label = forInIterLabel;

		SpiteIR::Allocate nextAlloc = BuildAllocate(nextFunc->returnType);
		BuildCall(nextFunc, nextAlloc.result, params, GetCurrentLabel());
		SpiteIR::Operand nextTest = BuildRegisterOperand(HandleAutoCast({nextAlloc.result, nextAlloc.type}, 
			castBool));
		SpiteIR::Instruction* branch = BuildBranch(GetCurrentLabel(), nextTest);

		SpiteIR::Label* forInBodyLabel = BuildLabel(forInBodyName);
		AddLabel(forInBodyLabel);
		SpiteIR::Allocate currentAlloc = BuildAllocate(currReturnType);
		BuildCall(currFunc, currentAlloc.result, params, GetCurrentLabel());
		ScopeValue currValue = HandleAutoCast({ currentAlloc.result, currentAlloc.type }, defType);
		AddValueToCurrentScope(def.name->val, currValue, defStmnt);

		BuildBody(for_.body);

		SpiteIR::Label* currentBodyLabel = GetCurrentLabel();
		SpiteIR::Instruction* bodyToCond = BuildJump(currentBodyLabel);
		bodyToCond->jump.label = forInIterLabel;

		AddLabel(forInEndLabel);
		branch->branch.true_ = forInBodyLabel;
		branch->branch.false_ = forInEndLabel;

		funcContext.breakLabels.pop_back();
		funcContext.continueLabels.pop_back();
	}

	void BuildForStmnt(Stmnt* stmnt)
	{
		Assert(stmnt->forStmnt.isDeclaration);
		auto& for_ = stmnt->forStmnt;

		if (!for_.rangeFor)
		{
			BuildForInStmnt(stmnt);
			return;
		}

		Stmnt* defStmnt = for_.iterated.declaration;
		auto& def = defStmnt->definition;

		eastl::string forStartName = "for_cond" + eastl::to_string(funcContext.forCount);
		eastl::string forLoopName = "for_body" + eastl::to_string(funcContext.forCount);
		eastl::string forIncName = "for_inc" + eastl::to_string(funcContext.forCount);
		eastl::string forEndName = "for_end" + eastl::to_string(funcContext.forCount);
		funcContext.forCount += 1;

		SpiteIR::Label* fromLabel = GetCurrentLabel();
		SpiteIR::Label* forIncLabel = BuildLabel(forIncName);
		SpiteIR::Label* forEndLabel = BuildLabel(forEndName);

		funcContext.breakLabels.push_back(forEndLabel);
		funcContext.continueLabels.push_back(forIncLabel);

		ScopeValue toValue = BuildExpr(for_.toIterate, stmnt);
		ScopeValue to = BuildTypeDereference(GetCurrentLabel(), toValue);

		SpiteIR::Allocate alloc = BuildAllocate(to.type);
		ScopeValue init = BuildDefaultValue(alloc.type, alloc.result, fromLabel);
		AddValueToCurrentScope(def.name->val, init, defStmnt);

		SpiteIR::Instruction* toCond = BuildJump(fromLabel);
		SpiteIR::Label* forCondLabel = BuildLabel(forStartName);
		AddLabel(forCondLabel);
		toCond->jump.label = forCondLabel;

		ScopeValue cmp = BuildBinaryOp(init, to, SpiteIR::BinaryOpKind::Less, forCondLabel);
		SpiteIR::Operand test = BuildRegisterOperand(HandleAutoCast(cmp, castBool));
		SpiteIR::Instruction* branch = BuildBranch(forCondLabel, test);

		SpiteIR::Label* forLoopLabel = BuildLabelBody(forLoopName, for_.body);

		SpiteIR::Label* currentBodyLabel = GetCurrentLabel();
		SpiteIR::Instruction* bodyToInc = BuildJump(currentBodyLabel);
		AddLabel(forIncLabel);
		bodyToInc->jump.label = forIncLabel;


		SpiteIR::Operand incremented = BuildRegisterOperand(BuildIncrement(forIncLabel, init));
		SpiteIR::Instruction* storeInc = BuildStore(forIncLabel, BuildRegisterOperand(init),
			incremented);

		SpiteIR::Instruction* loopToCond = BuildJump(forIncLabel, forCondLabel);
		AddLabel(forEndLabel);
		branch->branch.true_ = forLoopLabel;
		branch->branch.false_ = forEndLabel;

		funcContext.breakLabels.pop_back();
		funcContext.continueLabels.pop_back();
	}

	void BuildWhileStmnt(Stmnt* stmnt)
	{
		auto& while_ = stmnt->whileStmnt;
		auto& condition = while_.conditional->conditional;

		eastl::string whileCondName = "while_cond" + eastl::to_string(funcContext.whileCount);
		eastl::string whileBodyName = "while_body" + eastl::to_string(funcContext.whileCount);
		eastl::string whileEndName = "while_end" + eastl::to_string(funcContext.whileCount);
		funcContext.whileCount += 1;

		SpiteIR::Label* fromLabel = GetCurrentLabel();
		SpiteIR::Label* whileCondLabel = BuildLabel(whileCondName);
		SpiteIR::Label* whileEndLabel = BuildLabel(whileEndName);

		funcContext.breakLabels.push_back(whileEndLabel);
		funcContext.continueLabels.push_back(whileCondLabel);

		SpiteIR::Instruction* toCond = BuildJump(fromLabel);
		AddLabel(whileCondLabel);
		toCond->jump.label = whileCondLabel;

		ScopeValue test = BuildExpr(condition.condition, stmnt);
		SpiteIR::Operand cmpOp = BuildRegisterOperand(HandleAutoCast(test, castBool));
		SpiteIR::Instruction* branch = BuildBranch(GetCurrentLabel(), cmpOp);

		SpiteIR::Label* whileBodyLabel = BuildLabelBody(whileBodyName, condition.body);

		SpiteIR::Label* currentBodyLabel = GetCurrentLabel();
		SpiteIR::Instruction* bodyToCond = BuildJump(currentBodyLabel);
		bodyToCond->jump.label = whileCondLabel;

		AddLabel(whileEndLabel);
		branch->branch.true_ = whileBodyLabel;
		branch->branch.false_ = whileEndLabel;

		funcContext.breakLabels.pop_back();
		funcContext.continueLabels.pop_back();
	}

	void BuildSwitchStmnt(Stmnt* stmnt)
	{
		auto& switch_ = stmnt->switchStmnt;
		Expr* switchCond = switch_.switchOn;
		eastl::vector<Stmnt*>* cases = switch_.cases;
		Body& defaultCase = switch_.defaultCase;

		ScopeValue test = HandleAutoCast(BuildExpr(switchCond, stmnt), castInt);
		SpiteIR::Label* fromLabel = GetCurrentLabel();

		eastl::vector<SpiteIR::Label*> caseLabels = eastl::vector<SpiteIR::Label*>();

		size_t caseCount = cases->size();
		for (size_t i = 0; i < caseCount; i++)
		{
			eastl::string caseName = "case_" + eastl::to_string(i) + "_" + 
				eastl::to_string(funcContext.switchCaseCount);
			caseLabels.push_back(BuildLabel(caseName));
		}

		eastl::string defaultCaseName = "default_case" + eastl::to_string(funcContext.switchCaseCount);
		caseLabels.push_back(BuildLabel(defaultCaseName));

		SpiteIR::Label* switchEndLabel = BuildLabel("switch_end" + eastl::to_string(funcContext.switchCaseCount));

		funcContext.switchCaseCount += 1;

		eastl::hash_map<intmax_t, SpiteIR::Label*>* caseMap = context.ir->AllocateHashMap<intmax_t, SpiteIR::Label*>();

		funcContext.breakLabels.push_back(switchEndLabel);

		ScopeUtils& scopeUtils = GetScopeUtils();

		for (size_t i = 0; i < caseCount; i++)
		{
			Stmnt* caseCond = cases->at(i);
			SpiteIR::Label* caseLabel = caseLabels.at(i);
			SpiteIR::Label* nextLabel = caseLabels.at(i + 1);
			
			funcContext.continueLabels.push_back(nextLabel);

			auto& conditional = caseCond->conditional;
			Expr* caseExpr = ExpandTemplate(conditional.condition);
			Assert(scopeUtils.IsConstantIntExpr(caseExpr));

			intmax_t caseValue = scopeUtils.EvaluateConstantIntExpr(caseExpr);
			caseMap->emplace(caseValue, caseLabel);
			AddLabel(caseLabel);
			BuildBody(conditional.body);

			SpiteIR::Label* currLabel = GetCurrentLabel();
			if (!currLabel->terminator)
			{
				BuildJump(currLabel, switchEndLabel);
			}

			funcContext.continueLabels.pop_back();
		}

		SpiteIR::Label* defaultLabel = caseLabels.back();

		AddLabel(defaultLabel);
		if (defaultCase)
		{
			funcContext.continueLabels.push_back(switchEndLabel);
			BuildBody(defaultCase);
			funcContext.continueLabels.pop_back();
		}

		SpiteIR::Label* currLabel = GetCurrentLabel();
		if (!currLabel->terminator)
		{
			BuildJump(currLabel, switchEndLabel);
		}

		AddLabel(switchEndLabel);
		SpiteIR::Instruction* switchInst = BuildSwitch(fromLabel, BuildRegisterOperand(test), caseMap,
			defaultLabel);
	}

	SpiteIR::Function* GetDeleteOperator(SpiteIR::Type* type)
	{
		if (type->kind == SpiteIR::TypeKind::PointerType &&
			type->pointer.type->kind == SpiteIR::TypeKind::StateType)
		{
			SpiteIR::State* state = type->pointer.type->stateType.state;
			eastl::string destructorName = BuildDestructorName(state);
			SpiteIR::Package* package = state->parent;
			if (MapHas(package->functions, destructorName))
			{
				return package->functions[destructorName];
			}
		}

		return nullptr;
	}

	void BuildDelete(Stmnt* stmnt)
	{
		ScopeValue deleteValue = BuildExpr(stmnt->deleteStmnt.primaryExpr, stmnt);
		ScopeValue value = BuildTypeDereference(GetCurrentLabel(), deleteValue);

		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		params->push_back(BuildRegisterOperand(value));

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Function* deleteOp = GetDeleteOperator(value.type);
		if (deleteOp)
		{
			BuildCall(deleteOp, funcContext.curr, params, label);
		}

		if (value.type->kind == SpiteIR::TypeKind::PointerType)
			BuildCall(deallocFunc, funcContext.curr, params, label);
	}

	void BuildDefer(Stmnt* stmnt)
	{
		auto& deferStmnt = stmnt->deferStmnt;
		DeferredBody deferred;

		if (deferStmnt.deferIf)
		{
			deferred.runTest = BuildExpr(deferStmnt.conditional->conditional.condition, stmnt);
			deferred.body = deferStmnt.conditional->conditional.body;
			funcContext.scopeQueue.back().deferred.push_back(deferred);
		}
		else
		{
			deferred.runTest = InvalidScopeValue;
			deferred.body = deferStmnt.body;
			funcContext.scopeQueue.back().deferred.push_back(deferred);
		}
	}

	void BuildDeferred(DeferredBody& deferred)
	{
		if (deferred.runTest.reg != InvalidRegister)
		{
			SpiteIR::Label* currentLabel = GetCurrentLabel();
			//Move label terminator to defer end label
			SpiteIR::Instruction* term = currentLabel->terminator;
			currentLabel->terminator = nullptr;

			eastl::string deferStartName = "defer_" + eastl::to_string(funcContext.deferCount);
			eastl::string deferEndName = "defer_end_" + eastl::to_string(funcContext.deferCount);
			SpiteIR::Label* deferEndLabel = BuildLabel(deferEndName);
			SpiteIR::Operand cmpOp = BuildRegisterOperand(HandleAutoCast(deferred.runTest, castBool));
			SpiteIR::Instruction* branch = BuildBranch(GetCurrentLabel(), cmpOp);

			SpiteIR::Label* deferBodyLabel = BuildLabelBody(deferStartName, deferred.body);
			SpiteIR::Instruction* bodyToEnd = BuildJump(deferBodyLabel);
			bodyToEnd->jump.label = deferEndLabel;

			AddLabel(deferEndLabel);
			branch->branch.true_ = deferBodyLabel;
			branch->branch.false_ = deferEndLabel;
			deferEndLabel->terminator = term;
		}
		else
		{
			BuildBody(deferred.body);
		}
	}

	inline void BuildVoidReturn(SpiteIR::Label* label)
	{
		SpiteIR::Operand voidOp = SpiteIR::Operand();
		voidOp.kind = SpiteIR::OperandKind::Void;
		BuildReturnOp(label, voidOp);
	}

	void BuildReturn(Stmnt* stmnt)
	{
		Assert(stmnt->returnStmnt.expr);
		auto& ret = stmnt->returnStmnt;


		SpiteIR::Label* label = GetCurrentLabel();
		if (ret.expr->typeID == ExprID::TypeExpr &&
			ret.expr->typeExpr.type->typeID == TypeID::PrimitiveType &&
			ret.expr->typeExpr.type->primitiveType.type == UniqueType::Void)
		{
			BuildVoidReturn(label);
		}
		else
		{
			ScopeValue value = BuildExpr(ret.expr, stmnt);
			BuildReturnOp(label, BuildRegisterOperand(HandleAutoCast(value, funcContext.function->returnType)));
		}
	}

	ScopeValue BuildIncrement(SpiteIR::Label* label, ScopeValue& toIncrement)
	{
		Assert(toIncrement.type->kind == SpiteIR::TypeKind::PrimitiveType);
		SpiteIR::Operand amount = SpiteIR::Operand();
		amount.kind = SpiteIR::OperandKind::Literal;
		amount.type = toIncrement.type;
		amount.literal = {
			SpiteIR::PrimitiveKind::Int,
			1
		};
		SpiteIR::Allocate alloc = BuildAllocate(toIncrement.type);
		SpiteIR::Instruction* store = BuildStore(label, AllocateToOperand(alloc), amount);

		ScopeValue right = { alloc.result, toIncrement.type };
		return BuildBinaryOp(toIncrement, right, SpiteIR::BinaryOpKind::Add, label);
	}

	void BuildBlock(Stmnt* stmnt)
	{
		SpiteIR::Label* startLabel = GetCurrentLabel();
		eastl::string blockName = "block" + eastl::to_string(funcContext.blockCount);
		eastl::string blockEndName = "block_end" + eastl::to_string(funcContext.blockCount);

		SpiteIR::Instruction* toBlock = BuildJump(startLabel);
		SpiteIR::Label* blockLabel = BuildLabel(blockName);
		toBlock->jump.label = blockLabel;

		for (Stmnt* stmnt : *stmnt->block.inner)
		{
			BuildStmntForBlock(stmnt);
		}

		SpiteIR::Label* currLabel = GetCurrentLabel();
		if (!currLabel->terminator)
		{
			SpiteIR::Instruction* toBlockEnd = BuildJump(currLabel);
			SpiteIR::Label* blockEndLabel = BuildLabel(blockEndName);
			toBlockEnd->jump.label = blockEndLabel;
		}
	}

	void BuildAssertStmnt(Stmnt* stmnt)
	{
		ScopeValue assertValue = HandleAutoCast(BuildExpr(stmnt->assertStmnt.expr, stmnt), castBool);
		SpiteIR::Operand message;
		if (stmnt->assertStmnt.message)
		{
			message = BuildRegisterOperand(BuildExpr(stmnt->assertStmnt.message, stmnt));
		}
		else
		{
			message.kind = SpiteIR::OperandKind::Void;
		}

		BuildAssert(GetCurrentLabel(), BuildRegisterOperand(assertValue), message);
	}


	ScopeValue BuildExpr(Expr* expr, Stmnt* stmnt)
	{
		funcContext.currExpr = expr;
		expr = ExpandTemplate(expr);

		ScopeValue ret;
		switch (expr->typeID)
		{
		case LiteralExpr:
			ret = BuildLiteral(expr);
			break;
		case IdentifierExpr:
			ret = FindValueForIndent(expr);
			break;
		case PrimitiveExpr:
			ret = BuildPrimitive(expr);
			break;
		case SelectorExpr:
			ret = BuildSelector(expr, stmnt);
			break;
		case IndexExpr:
			ret = BuildIndexExpr(expr, stmnt);
			break;
		case FunctionCallExpr:
			ret = BuildFunctionCall(expr, stmnt);
			break;
		case NewExpr:
			ret = BuildNewExpr(expr, stmnt);
			break;
		case FixedExpr:
			ret = BuildFixedExpr(expr, stmnt);
			break;
		case TypeLiteralExpr:
			ret = BuildTypeLiteral(expr, stmnt);
			break;
		case ExplicitTypeExpr:
			ret = BuildExplicitTypeExpr(expr, stmnt);
			break;
		case AsExpr:
			ret = BuildCastExpr(expr, stmnt);
			break;
		case DereferenceExpr:
			ret = BuildDereferenceExpr(expr, stmnt);
			break;
		case ReferenceExpr:
			ret = BuildReferenceExpr(expr, stmnt);
			break;
		case BinaryExpr:
			ret = BuildBinaryExpression(expr, stmnt);
			break;
		case UnaryExpr:
			ret = BuildUnaryExpression(expr, stmnt);
			break;
		case GroupedExpr:
			ret = BuildExpr(expr->groupedExpr.expr, stmnt);
			break;
		case TemplateExpr:
			ret = BuildTemplateExpr(expr, stmnt);
			break;
		case TypeExpr:
			ret = BuildTypeExpr(expr, stmnt);
			break;
		case FunctionTypeDeclExpr:
			ret = BuildAnonFunction(expr, stmnt);
			break;
		case CompileExpr:
			ret = BuildCompileExpr(expr, stmnt);
			break;
		case SizeOfExpr:
			ret = BuildSizeOf(expr, stmnt);
			break;
		case AlignOfExpr:
			ret = BuildAlignOf(expr, stmnt);
			break;
		default:
			ret = InvalidScopeValue;
			break;
		}

		if (ret.reg == InvalidRegister)
		{
			Logger::FatalErrorAt("LowerDefinitions:BuildExpr Unable to create valid value for expression", expr->start->pos);
		}

		return ret;
	}

	ScopeValue FindGlobalVar(SpiteIR::Package* package, Stmnt* globalVarStmnt)
	{
		eastl::string globalVarName = BuildGlobalVariableName(globalVarStmnt);
		size_t index = package->globalVariableLookup[globalVarName];
		SpiteIR::GlobalVariable* globalVar = package->globalVariables.at(index);
		SpiteIR::Type* globalRef = MakeReferenceType(globalVar->type, context.ir);
		SpiteIR::Allocate alloc = BuildAllocate(globalRef);
		SpiteIR::Instruction* loadGlobal = BuildLoadGlobal(GetCurrentLabel(), AllocateToOperand(alloc),
			globalVar->index);
		return { alloc.result, alloc.type };
	}

	ScopeValue FindFunctionValue(Stmnt* funcStmnt)
	{
		Stmnt* generics = GetGenerics(funcStmnt);
		if (generics)
		{
			ScopeValue funcValue = ScopeValue();
			funcValue.reg = StmntRegister;
			funcValue.stmnt = funcStmnt;
			return funcValue;
		}

		SpiteIR::Function* func = nullptr;
		switch (funcStmnt->nodeID)
		{
		case ExternFunctionDecl:
			func = FindFunction(funcStmnt->package->val, funcStmnt->externFunction.callName->val.ToString());
			break;
		case FunctionStmnt:
		default:
			func = FindFunction(funcStmnt->package->val, BuildFunctionName(funcStmnt));
			break;
		}

		return StoreFunctionValue(func);
	}

	ScopeValue StoreFunctionValue(SpiteIR::Function* func)
	{
		if (funcContext.HasFlag(FunctionContextFlag::ReturnFunctionScopeValue))
		{
			ScopeValue funcValue = ScopeValue();
			funcValue.reg = FunctionRegister;
			funcValue.function = func;
			return funcValue;
		}

		SpiteIR::Type* funcType = IRFunctionToFunctionType(context.ir, func);
		SpiteIR::Allocate alloc = BuildAllocate(funcType);

		SpiteIR::Operand funcOperand = SpiteIR::Operand();
		funcOperand.kind = SpiteIR::OperandKind::Function;
		funcOperand.type = funcType;
		funcOperand.function = func;

		SpiteIR::Instruction* storeFunc = BuildStoreFunc(GetCurrentLabel(), AllocateToOperand(alloc),
			funcOperand);
		return { alloc.result, alloc.type };

	}

	ScopeValue FindValueForIndent(Expr* expr)
	{
		Token* identTok = expr->identifierExpr.identifier;
		StringView& ident = identTok->val;
		ScopeValue identVal = FindScopeValue(ident);
		if (identVal.type) return BuildTypeReference(GetCurrentLabel(), FindScopeValue(ident));

		Stmnt* globalVar = context.globalTable->FindScopedGlobalVar(identTok, symbolTable);
		if (globalVar)
		{
			SpiteIR::Package* package = context.packageMap[globalVar->package->val];
			return FindGlobalVar(package, globalVar);
		}

		Stmnt* funcStmnt = context.globalTable->FindScopedFunction(identTok, symbolTable);
		if (funcStmnt)
		{
			return FindFunctionValue(funcStmnt);
		}

		Stmnt* stateStmnt = context.globalTable->FindScopedState(identTok, symbolTable);
		if (stateStmnt)
		{
			SpiteIR::Package* package = context.packageMap[stateStmnt->package->val];
			SpiteIR::State* state = FindPackageState(package, BuildStateName(stateStmnt));
			ScopeValue stateValue = ScopeValue();
			if (state)
			{
				stateValue.reg = StateRegister;
				stateValue.state = state;
			}
			else
			{
				stateValue.reg = StmntRegister;
				stateValue.stmnt = stateStmnt;
			}
			return stateValue;
		}

		Stmnt* enumStmnt = context.globalTable->FindScopedEnum(identTok, symbolTable);
		if (enumStmnt)
		{
			ScopeValue enumValue = ScopeValue();
			enumValue.reg = StmntRegister;
			enumValue.stmnt = enumStmnt;
			return enumValue;
		}

		if (context.globalTable->IsPackage(ident))
		{
			ScopeValue packageValue = InvalidScopeValue;
			packageValue.reg = PackageRegister;
			packageValue.package = context.packageMap[ident];
			return packageValue;
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildPrimitive(Expr* expr)
	{
		UniqueType primEnum = expr->primitiveExpr.primitive->uniqueType;
		SpiteIR::Type* primType = ToIRType(symbolTable->CreatePrimitive(primEnum));
		return { InvalidRegister, primType };
	}

	SpiteIR::Member* FindStateMember(SpiteIR::State* state, StringView& ident)
	{
		eastl::vector<SpiteIR::Member*>& members = state->members;
		for (SpiteIR::Member* member : members)
		{
			if (member->value->name == ident)
			{
				return member;
			}
		}

		return nullptr;
	}

	ScopeValue FindStructureTypeMember(SpiteIR::Type* type, StringView& ident)
	{
		Assert(type->structureType.names);

		size_t offset = 0;
		eastl::vector<eastl::string>* names = type->structureType.names;
		for (size_t i = 0; i < names->size(); i++)
		{
			eastl::string& name = names->at(i);
			SpiteIR::Type* memberType = type->structureType.types->at(i);
			if (name == ident)
			{
				return { offset, memberType };
			}
			offset += memberType->size;
		}

		return InvalidScopeValue;
	}

	SpiteIR::State* GetStateForType(SpiteIR::Type* type)
	{
		if (type->kind == SpiteIR::TypeKind::ReferenceType)
		{
			return GetStateForType(type->reference.type);
		}
		if (type->kind == SpiteIR::TypeKind::StateType)
		{
			return type->stateType.state;
		}
		else if (type->kind == SpiteIR::TypeKind::DynamicArrayType)
		{
			return arrayState;
		}
		else if (IsStringType(type))
		{
			return stringState;
		}

		return nullptr;
	}

	// Dereferencing is hidden on member selection to a single degree, multiple degrees of pointers require
	// manual dereferencing (val: **Type, val~.member)
	ScopeValue DereferenceToSinglePointer(ScopeValue value)
	{
		// Reference to a pointer, dereference
		if (value.type->kind == SpiteIR::TypeKind::ReferenceType &&
			value.type->reference.type->kind == SpiteIR::TypeKind::PointerType)
		{
			return BuildTypeDereference(GetCurrentLabel(), value);
		}

		return value;
	}

	ScopeValue BuildSelectedPackageValue(SpiteIR::Package* package, Expr* selected)
	{
		SymbolTable* symbolTable = context.packageToSymbolTableMap[package];

		switch (selected->typeID)
		{
		case ExprID::IdentifierExpr:
		{
			Stmnt* stmnt = symbolTable->FindStatement(selected->identifierExpr.identifier->val);
			switch (stmnt->nodeID)
			{
			case Definition:
			{
				return FindGlobalVar(package, stmnt);
			}
			case StateStmnt:
			{
				SpiteIR::State* state = FindPackageState(package, BuildStateName(stmnt));
				ScopeValue value = ScopeValue();
				// Templated state lookup without templates
				if (!state)
				{
					value.reg = StmntRegister;
					value.stmnt = stmnt;
				}
				else
				{
					value.reg = StateRegister;
					value.state = state;
				}

				return value;
			}
			case EnumStmnt:
			{
				ScopeValue value = ScopeValue();
				value.reg = StmntRegister;
				value.stmnt = stmnt;
			}
			case FunctionStmnt:
				return FindFunctionValue(stmnt);
			default:
				break;
			}
		}
		break;
		default:
			break;
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildSelectedMethodValue(SpiteIR::State* state, Expr* selected)
	{
		Stmnt* stateStmnt = context.stateASTMap[state].node;
		StringView& packageStr = stateStmnt->package->val;
		SymbolTable* symbolTable = context.globalTable->FindSymbolTable(packageStr);
		StateSymbol* stateSymbol = symbolTable->FindStateSymbol(stateStmnt->state.name->val);

		switch (selected->typeID)
		{
		case ExprID::IdentifierExpr:
		{
			Stmnt* methodStmnt = FindStateMethod(stateSymbol, selected->identifierExpr.identifier->val);
			eastl::string methodName = BuildMethodName(state, methodStmnt);
			if (FunctionExists(packageStr, methodName))
			{
				SpiteIR::Function* method = FindFunction(packageStr, methodName);
				return StoreFunctionValue(method);
			}

			// Templated method being selected
			funcContext.templatedMethodState = state;
			ScopeValue methodValue = ScopeValue();
			methodValue.reg = StmntRegister;
			methodValue.stmnt = methodStmnt;
			return methodValue;
		}
		default:
			break;
		}
		return InvalidScopeValue;
	}

	ScopeValue BuildSelected(ScopeValue& value, Expr* selected)
	{
		StringView& ident = selected->identifierExpr.identifier->val;

		if (value.reg == PackageRegister)
		{
			return BuildSelectedPackageValue(value.package, selected);
		}
		else if (value.reg == StateRegister)
		{
			return BuildSelectedMethodValue(value.state, selected);
		}
		else if (value.reg == StmntRegister)
		{
			switch (value.stmnt->nodeID)
			{
			case EnumStmnt:
			{
				Stmnt* enumStmnt = value.stmnt;
				intmax_t value = FindEnumValue(enumStmnt, ident);
				return BuildLiteralIntForType(value, ToIRType(enumStmnt->enumStmnt.type));
			}
			default:
				break;
			}
		}


		if (value.type->kind == SpiteIR::TypeKind::StateType ||
			value.type->kind == SpiteIR::TypeKind::DynamicArrayType ||
			IsStringType(value.type))
		{
			SpiteIR::State* state = GetStateForType(value.type);
			SpiteIR::Member* member = FindStateMember(state, ident);
			Assert(member);
			return { value.reg + member->offset, member->value->type };
		}
		else if (value.type->kind == SpiteIR::TypeKind::StructureType)
		{
			ScopeValue offsetAndType = FindStructureTypeMember(value.type, ident);
			Assert(offsetAndType.type);
			return { value.reg + offsetAndType.reg, offsetAndType.type };
		}
		else if (value.type->kind == SpiteIR::TypeKind::PointerType ||
			value.type->kind == SpiteIR::TypeKind::ReferenceType)
		{
			value = DereferenceToSinglePointer(value);
			SpiteIR::Type* derefed = GetDereferencedType(value.type);
			ScopeValue offsetAndType = InvalidScopeValue;
			if (derefed->kind == SpiteIR::TypeKind::StructureType)
			{
				offsetAndType = FindStructureTypeMember(derefed, ident);
				Assert(offsetAndType.type);
			}
			else
			{
				SpiteIR::State* state = GetStateForType(derefed);
				SpiteIR::Member* member = FindStateMember(state, ident);
				if (!member)
				{
					Assert(member);
				}
				offsetAndType = { member->offset, member->value->type };
			}

			SpiteIR::Type* referencedMember = MakeReferenceType(offsetAndType.type, context.ir);
			SpiteIR::Allocate alloc = BuildAllocate(referencedMember);
			ScopeValue dstValue = { alloc.result, referencedMember };
			SpiteIR::Operand offset = BuildRegisterOperand(BuildLiteralInt(offsetAndType.reg));
			SpiteIR::Operand src = BuildRegisterOperand(value);
			SpiteIR::Operand dst = BuildRegisterOperand(dstValue);
			SpiteIR::Instruction* loadPtr = BuildLoadPtrOffset(GetCurrentLabel(), dst, src, offset, indexByte);
			return dstValue;
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildSelector(Expr* expr, Stmnt* stmnt)
	{
		Expr* left = expr->selectorExpr.on;
		Expr* right = expr->selectorExpr.select;
		ScopeValue leftVal = BuildExpr(left, stmnt);

		return BuildSelected(leftVal, right);
	}

	ScopeValue BuildDefaultValue(SpiteIR::Type* type, size_t dst, SpiteIR::Label* label)
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
		{
			SpiteIR::Operand defaultOp = SpiteIR::Operand();
			defaultOp.type = type;
			defaultOp.kind = SpiteIR::OperandKind::Literal;
			SpiteIR::Literal& literal = defaultOp.literal;
			literal.kind = type->primitive.kind;

			switch (type->primitive.kind)
			{
			case SpiteIR::PrimitiveKind::Bool:
			case SpiteIR::PrimitiveKind::Byte:
				literal.byteLiteral = 0;
				break;
			case SpiteIR::PrimitiveKind::I16:
				literal.i16Literal = 0;
				break;
			case SpiteIR::PrimitiveKind::I32:
				literal.i32Literal = 0;
				break;
			case SpiteIR::PrimitiveKind::I64:
				literal.i64Literal = 0;
				break;
			case SpiteIR::PrimitiveKind::Int:
				literal.intLiteral = 0;
				break;
			case SpiteIR::PrimitiveKind::F32:
				literal.f32Literal = 0.0f;
				break;
			case SpiteIR::PrimitiveKind::Float:
				literal.floatLiteral = 0.0f;
				break;
			case SpiteIR::PrimitiveKind::String:
				literal.stringLiteral = context.ir->AllocateString();
				break;
			default:
				return InvalidScopeValue;
				break;
			}

			SpiteIR::Instruction* store = BuildStore(label, BuildRegisterOperand({ dst, type }),
				defaultOp);
			break;
		}
		case SpiteIR::TypeKind::StateType:
		{
			ASTContainer stateAST = context.stateASTMap[type->stateType.state];
			Stmnt* stateStmnt = stateAST.node;
			for (size_t i = 0; i < type->stateType.state->members.size(); i++)
			{
				SpiteIR::Member* memberType = type->stateType.state->members.at(i);
				Stmnt* memberDecl = stateStmnt->state.members->at(i);
				if (memberDecl->definition.assignment)
				{
					ScopeValue value = BuildExpr(memberDecl->definition.assignment, stateStmnt);
					ScopeValue dstValue = { dst + memberType->offset, memberType->value->type };
					AssignValues(dstValue, value);
				}
				else BuildDefaultValue(memberType->value->type, dst + memberType->offset, label);
			}
			break;
		}
		case SpiteIR::TypeKind::StructureType:
			break;
		case SpiteIR::TypeKind::PointerType:
		case SpiteIR::TypeKind::FunctionType:
		{
			SpiteIR::Operand defaultOp = SpiteIR::Operand();
			defaultOp.type = type;
			defaultOp.kind = SpiteIR::OperandKind::Literal;
			defaultOp.literal.kind = SpiteIR::PrimitiveKind::Int;
			defaultOp.literal.intLiteral = 0;

			SpiteIR::Instruction* store = BuildStore(label, BuildRegisterOperand({ dst, type }),
				defaultOp);
			break;
		}
		case SpiteIR::TypeKind::DynamicArrayType:
		{
			MakeDynamicArray(type, dst, label);
			break;
		}
		case SpiteIR::TypeKind::FixedArrayType:
		case SpiteIR::TypeKind::ReferenceType:
			break;
		default:
			break;
		}

		return { dst, type };
	}

	ScopeValue BuildLiteral(Expr* expr)
	{
		auto& lit = expr->literalExpr;
		SpiteIR::Operand literalOp = SpiteIR::Operand();
		literalOp.kind = SpiteIR::OperandKind::Literal;
		SpiteIR::Literal& literal = literalOp.literal;

		switch (lit.val->uniqueType)
		{
		case UniqueType::IntLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Int;
			literal.intLiteral = IntLiteralStringToInt(lit.val->val);
			break;
		case UniqueType::HexLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Int;
			literal.intLiteral = std::stoul(lit.val->val.ToString().c_str(), nullptr, 16);
			break;
		case UniqueType::FloatLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Float;
			literal.floatLiteral = std::stof(lit.val->val.ToString().c_str());
			break;
		case UniqueType::ByteLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Byte;
			literal.byteLiteral = *lit.val->val.start;
			break;
		case UniqueType::StringLiteral:
			literal.kind = SpiteIR::PrimitiveKind::String;
			literal.stringLiteral = context.ir->AllocateString();
			*literal.stringLiteral = lit.val->val.ToString();
			break;
		case UniqueType::TrueLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Bool;
			literal.byteLiteral = 1;
			break;
		case UniqueType::FalseLiteral:
			literal.kind = SpiteIR::PrimitiveKind::Bool;
			literal.byteLiteral = 0;
			break;
		default:
			literal.kind = SpiteIR::PrimitiveKind::Void;
			break;
		}

		SpiteIR::Type* irType = context.ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::PrimitiveType;
		irType->primitive.kind = literal.kind;
		irType->size = irType->primitive.kind == SpiteIR::PrimitiveKind::String ?
			config.targetArchByteWidth * 2 : config.targetArchByteWidth;
		irType->primitive.isSigned = true;
		irType->byValue = true;

		literalOp.type = irType;

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Allocate alloc = BuildAllocate(irType);
		SpiteIR::Instruction* store = BuildStore(label, AllocateToOperand(alloc), literalOp);
		return { store->store.dst.reg, irType };
	}

	ScopeValue BuildLiteralInt(size_t value)
	{
		SpiteIR::Operand literalOp = SpiteIR::Operand();
		literalOp.kind = SpiteIR::OperandKind::Literal;
		SpiteIR::Literal& literal = literalOp.literal;
		literal.kind = SpiteIR::PrimitiveKind::Int;
		literal.intLiteral = value;

		SpiteIR::Type* intType = CreateIntType(context.ir);
		literalOp.type = intType;

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Allocate alloc = BuildAllocate(intType);
		SpiteIR::Instruction* store = BuildStore(label, AllocateToOperand(alloc), literalOp);
		return { store->store.dst.reg, intType };
	}

	ScopeValue BuildLiteralIntForType(intmax_t i, SpiteIR::Type* type)
	{
		Assert(type->kind == SpiteIR::TypeKind::PrimitiveType);

		SpiteIR::Allocate alloc = BuildAllocate(type);
		SpiteIR::Operand operand = SpiteIR::Operand();
		operand.type = type;
		operand.kind = SpiteIR::OperandKind::Literal;
		SpiteIR::Literal& literal = operand.literal;
		literal.kind = type->primitive.kind;

		switch (type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Bool:
		case SpiteIR::PrimitiveKind::Byte:
			literal.byteLiteral = i;
			break;
		case SpiteIR::PrimitiveKind::I16:
			literal.i16Literal = i;
			break;
		case SpiteIR::PrimitiveKind::I32:
			literal.i32Literal = i;
			break;
		case SpiteIR::PrimitiveKind::I64:
			literal.i64Literal = i;
			break;
		case SpiteIR::PrimitiveKind::Int:
			literal.intLiteral = i;
			break;
		case SpiteIR::PrimitiveKind::F32:
		case SpiteIR::PrimitiveKind::Float:
		case SpiteIR::PrimitiveKind::String:
			Logger::FatalError("BuildLiteralIntForType used for non integer type");
			break;
		}

		SpiteIR::Instruction* store = BuildStore(GetCurrentLabel(), AllocateToOperand(alloc), operand);
		return { store->store.dst.reg, type };
	}

	void BuildStoreArrayCount(size_t reg, size_t count)
	{
		SpiteIR::Operand literalOp = SpiteIR::Operand();
		literalOp.kind = SpiteIR::OperandKind::Literal;
		SpiteIR::Literal& literal = literalOp.literal;
		literal.kind = SpiteIR::PrimitiveKind::Int;
		literal.intLiteral = count;

		SpiteIR::Type* irType = context.ir->AllocateType();
		irType->kind = SpiteIR::TypeKind::PrimitiveType;
		irType->size = config.targetArchByteWidth;
		irType->primitive.kind = literal.kind;
		irType->primitive.isSigned = false;

		literalOp.type = irType;

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Instruction* store = BuildStore(label, BuildRegisterOperand({ reg, irType }),
			literalOp);
	}

	ScopeValue BuildNewExpr(Expr* expr, Stmnt* stmnt)
	{
		ScopeValue newValue = BuildExpr(expr->newExpr.primaryExpr, stmnt);
		ScopeValue value = BuildTypeDereference(GetCurrentLabel(), newValue);

		if (expr->newExpr.atExpr)
		{
			ScopeValue placement = BuildExpr(expr->newExpr.atExpr, stmnt);
			SpiteIR::Instruction* storePtr = BuildStorePtr(GetCurrentLabel(), BuildRegisterOperand(placement),
				BuildRegisterOperand(value));
			return placement;
		}
		else
		{
			SpiteIR::Type* ptr = MakePointerType(value.type, context.ir);
			ScopeValue allocSize = BuildLiteralInt(value.type->size);
			eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
			params->push_back(BuildRegisterOperand(allocSize));

			SpiteIR::Label* label = GetCurrentLabel();
			SpiteIR::Allocate alloc = BuildAllocate(ptr);
			BuildCall(allocFunc, alloc.result, params, label);
			SpiteIR::Instruction* storePtr = BuildStorePtr(label, AllocateToOperand(alloc),
				BuildRegisterOperand(value));
			return { alloc.result, ptr };
		}
	}

	ScopeValue BuildFixedExpr(Expr* expr, Stmnt* stmnt)
	{
		ScopeValue value = BuildExpr(expr->fixedExpr.atExpr, stmnt);
		ScopeValue ref = BuildTypeReference(GetCurrentLabel(), value);
		SpiteIR::Type* fixedArrayType = GetDereferencedType(ref.type);
		Assert(fixedArrayType->kind == SpiteIR::TypeKind::FixedArrayType);
		SpiteIR::Type* pointer = MakePointerType(fixedArrayType->fixedArray.type, context.ir);
		return { ref.reg, pointer };
	}

	ScopeValue BuildTypeLiteral(Expr* expr, Stmnt* stmnt)
	{
		Assert(expr->typeLiteralExpr.values->size());
		eastl::vector<ScopeValue> values;
		for (Expr* val : *expr->typeLiteralExpr.values)
		{
			ScopeValue typeValue = BuildExpr(val, stmnt);
			values.push_back(BuildTypeDereference(GetCurrentLabel(), typeValue));
		}

		SpiteIR::Type* derivedType = nullptr;
		if (expr->typeLiteralExpr.array)
		{
			derivedType = BuildFixedArray(context.ir, values.size(), values.at(0).type);
		}
		else
		{
			derivedType = context.ir->AllocateType();
			derivedType->kind = SpiteIR::TypeKind::StructureType;
			derivedType->size = 0;
			derivedType->structureType.types = context.ir->AllocateArray<SpiteIR::Type*>();
			derivedType->structureType.names = nullptr;
			for (ScopeValue& value : values)
			{
				derivedType->structureType.types->push_back(value.type);
				derivedType->size += value.type->size;
			}
		}

		SpiteIR::Allocate alloc = BuildAllocate(derivedType);
		size_t offset = 0;
		for (ScopeValue& value : values)
		{
			SpiteIR::Operand dstOp = BuildRegisterOperand({ alloc.result + offset, value.type });

			BuildStore(GetCurrentLabel(), dstOp, BuildRegisterOperand(value));
			offset += value.type->size;
		}

		return { alloc.result, derivedType };
	}

	ScopeValue BuildExplicitTypeExpr(Expr* expr, Stmnt* stmnt)
	{
		SpiteIR::Label* label = GetCurrentLabel();
		auto& explicitType = expr->explicitTypeExpr;

		SpiteIR::Type* structType = context.ir->AllocateType();
		structType->kind = SpiteIR::TypeKind::StructureType;
		structType->structureType.names = context.ir->AllocateArray<eastl::string>();
		structType->structureType.types = context.ir->AllocateArray<SpiteIR::Type*>();

		eastl::vector<ScopeValue> values;
		for (Stmnt* def : *explicitType.values)
		{
			structType->structureType.names->push_back(def->definition.name->ToString());
			ScopeValue value = BuildVarDefinition(def);
			structType->structureType.types->push_back(value.type);
			structType->size += value.type->size;
			values.push_back(value);
		}

		SpiteIR::Allocate alloc = BuildAllocate(structType);
		size_t offset = 0;
		for (ScopeValue& value : values)
		{
			SpiteIR::Operand dstOp = BuildRegisterOperand({ alloc.result + offset, value.type });
			BuildStore(label, dstOp, BuildRegisterOperand(value));
			offset += value.type->size;
		}

		return { alloc.result, structType };
	}

	bool RequiresBitCast(SpiteIR::Type* from, SpiteIR::Type* to)
	{
		if (from->kind == SpiteIR::TypeKind::ReferenceType) from = from->reference.type;
		if (from->kind == SpiteIR::TypeKind::PrimitiveType &&
			to->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return from->primitive.kind != SpiteIR::PrimitiveKind::String &&
				to->primitive.kind != SpiteIR::PrimitiveKind::String;
		}

		return (IsIntLikeType(from) && to->kind == SpiteIR::TypeKind::PointerType) ||
			(from->kind == SpiteIR::TypeKind::PointerType && IsIntLikeType(to));
	}

	ScopeValue CastValue(ScopeValue toCast, SpiteIR::Type* toType)
	{
		if (RequiresBitCast(toCast.type, toType))
		{
			toCast = BuildTypeDereference(GetCurrentLabel(), toCast);
			if (toType->kind == SpiteIR::TypeKind::PointerType)
			{
				return IntToPointer(toCast, toType);
			}

			if (toCast.type->kind == SpiteIR::TypeKind::PointerType)
			{
				toCast = PointerToInt(toCast);
			}

			return BuildTypeCast(toCast, toType);
		}
		else
		{
			SpiteIR::Type* fromConversionType = GetConversionType(toCast.type);
			SpiteIR::Type* toConversionType = GetConversionType(toType);

			if (fromConversionType->kind == SpiteIR::TypeKind::FixedArrayType &&
				toConversionType->kind == SpiteIR::TypeKind::DynamicArrayType)
			{
				return CastFixedArrayToDynamic(toCast, toType);
			}

			return { toCast.reg, toType };
		}
	}

	ScopeValue BuildCastExpr(Expr* expr, Stmnt* stmnt)
	{
		auto& as = expr->asExpr;
		ScopeValue toCast = BuildExpr(as.of, stmnt);
		SpiteIR::Type* toType = ToIRType(as.to);

		return CastValue(toCast, toType);
	}

	ScopeValue BuildTypeCast(const ScopeValue& from, SpiteIR::Type* toType)
	{
		SpiteIR::Allocate alloc = BuildAllocate(toType);
		SpiteIR::Instruction* cast = BuildCast(GetCurrentLabel(),
			BuildRegisterOperand(BuildTypeDereference(GetCurrentLabel(), from)),
			AllocateToOperand(alloc));
		return { alloc.result, toType };
	}

	SpiteIR::Type* GetDereferencedType(SpiteIR::Type* type)
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PointerType:
			return type->pointer.type;
		case SpiteIR::TypeKind::ReferenceType:
			return type->reference.type;
		default:
			break;
		}
		return type;
	}

	ScopeValue BuildIndexExpr(Expr* expr, Stmnt* stmnt)
	{
		ScopeValue toIndex = BuildExpr(expr->indexExpr.of, stmnt);
		ScopeValue indexValue = BuildExpr(expr->indexExpr.index, stmnt);
		SpiteIR::Label* label = GetCurrentLabel();
		ScopeValue index = BuildTypeDereference(label, indexValue);

		switch (toIndex.type->kind)
		{
		case SpiteIR::TypeKind::ReferenceType:
		{
			SpiteIR::Type* derefedType = toIndex.type->reference.type;
			switch (derefedType->kind)
			{
			case SpiteIR::TypeKind::StateType:
				return BuildStateOperatorCallForType(toIndex, UniqueType::Array, &index);
			case SpiteIR::TypeKind::DynamicArrayType:
			{
				ScopeValue ret = BuildStateOperatorCall(arrayState, toIndex, UniqueType::Array, &index);
				SpiteIR::Type* retType = MakeReferenceType(derefedType->dynamicArray.type, context.ir);
				ret.type = retType;
				return ret;
			}
			case SpiteIR::TypeKind::PointerType:
			{
				SpiteIR::Type* type = derefedType->pointer.type;
				SpiteIR::Allocate alloc = BuildAllocate(derefedType);
				ScopeValue dst = { alloc.result, derefedType };
				ScopeValue value = BuildTypeDereference(label, toIndex);
				ScopeValue intIndex = CastValue(index, castInt);
				SpiteIR::Instruction* loadPtr = BuildLoadPtrOffset(label, BuildRegisterOperand(dst),
					BuildRegisterOperand(value), BuildRegisterOperand(intIndex), type);
				return dst;
			}
			case SpiteIR::TypeKind::FixedArrayType:
			{
				SpiteIR::Type* type = derefedType->fixedArray.type;
				SpiteIR::Type* returnType = MakeReferenceType(type, context.ir);
				SpiteIR::Allocate alloc = BuildAllocate(returnType);
				ScopeValue dst = { alloc.result, returnType };
				ScopeValue intIndex = CastValue(index, castInt);
				SpiteIR::Instruction* loadPtr = BuildLoadPtrOffset(label, BuildRegisterOperand(dst),
					BuildRegisterOperand(toIndex), BuildRegisterOperand(intIndex), type);
				return dst;
			}
			case SpiteIR::TypeKind::PrimitiveType:
			{
				if (derefedType->primitive.kind == SpiteIR::PrimitiveKind::String)
					return BuildStateOperatorCall(stringState, toIndex, UniqueType::Array, &index);
				break;
			}
			case SpiteIR::TypeKind::StructureType:
			case SpiteIR::TypeKind::FunctionType:
				Assert(false);
				break;
			default:
				break;
			}

			break;
		}
		case SpiteIR::TypeKind::StateType:
			return BuildStateOperatorCallForType(toIndex, UniqueType::Array, &index);
		case SpiteIR::TypeKind::DynamicArrayType:
		{
			ScopeValue ret = BuildStateOperatorCall(arrayState, toIndex, UniqueType::Array, &index);
			ret.type->reference.type = toIndex.type->dynamicArray.type;
			return ret;
		}
		case SpiteIR::TypeKind::PointerType:
		{
			SpiteIR::Type* type = toIndex.type->pointer.type;
			SpiteIR::Allocate alloc = BuildAllocate(toIndex.type);
			ScopeValue dst = { alloc.result, toIndex.type };
			ScopeValue intIndex = CastValue(index, castInt);
			SpiteIR::Instruction* loadPtr = BuildLoadPtrOffset(label, BuildRegisterOperand(dst),
				BuildRegisterOperand(toIndex), BuildRegisterOperand(intIndex), type);
			return dst;
		}
		case SpiteIR::TypeKind::FixedArrayType:
		{
			SpiteIR::Type* type = toIndex.type->fixedArray.type;
			SpiteIR::Type* returnType = MakeReferenceType(type, context.ir);
			SpiteIR::Allocate alloc = BuildAllocate(returnType);
			ScopeValue dst = { alloc.result, returnType };
			ScopeValue intIndex = CastValue(index, castInt);
			SpiteIR::Instruction* load = BuildLoad(label, BuildRegisterOperand(dst),
				BuildRegisterOperand(toIndex), BuildRegisterOperand(intIndex), type);
			return dst;
		}
		case SpiteIR::TypeKind::PrimitiveType:
		{
			if (toIndex.type->primitive.kind == SpiteIR::PrimitiveKind::String)
				return BuildStateOperatorCall(stringState, toIndex, UniqueType::Array, &index);
			break;
		}
		case SpiteIR::TypeKind::StructureType:
		case SpiteIR::TypeKind::FunctionType:
			Assert(false);
			break;
		default:
			break;
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildDereferenceExpr(Expr* expr, Stmnt* stmnt)
	{
		Expr* toDeref = expr->dereferenceExpr.of;
		ScopeValue value = BuildTypeDereference(GetCurrentLabel(), BuildExpr(toDeref, stmnt));
		Assert(value.type->kind == SpiteIR::TypeKind::PointerType);

		// This looks wrong since we're making the type a reference type in a function called dereference
		// But a reference type signals to treat operations against it as a value
		// which is what we want when we dereference a pointer
		SpiteIR::Type* type = MakeReferenceType(value.type->pointer.type, context.ir);
		SpiteIR::Allocate alloc = BuildAllocate(type);
		SpiteIR::Instruction* store = BuildStore(GetCurrentLabel(), AllocateToOperand(alloc),
			BuildRegisterOperand(value));
		return { alloc.result, alloc.type };
	}

	ScopeValue BuildReferenceExpr(Expr* expr, Stmnt* stmnt)
	{
		Expr* toRef = expr->referenceExpr.of;
		ScopeValue refValue = BuildExpr(toRef, stmnt);
		ScopeValue value = BuildTypeReference(GetCurrentLabel(), refValue);
		value.type->kind = SpiteIR::TypeKind::PointerType;
		return value;
	}

	// Pointer int conversions do a copy to avoid register-type clashes
	ScopeValue PointerToInt(const ScopeValue& pointer)
	{
		SpiteIR::Type* ptrInt = CreateUnsignedIntType(context.ir);
		SpiteIR::Allocate alloc = BuildAllocate(ptrInt);
		SpiteIR::Instruction* cast = BuildCast(GetCurrentLabel(), BuildRegisterOperand(pointer),
			AllocateToOperand(alloc));
		return { alloc.result, alloc.type };
	}

	ScopeValue IntToPointer(const ScopeValue& value, SpiteIR::Type* type)
	{
		SpiteIR::Allocate alloc = BuildAllocate(type);
		SpiteIR::Instruction* cast = BuildCast(GetCurrentLabel(), BuildRegisterOperand(value),
			AllocateToOperand(alloc));
		return { alloc.result, alloc.type };
	}

	ScopeValue BuildBinaryOpValue(const ScopeValue& left, const ScopeValue& right, SpiteIR::BinaryOpKind op)
	{
		if (!left.type || !right.type) return InvalidScopeValue;

		SpiteIR::Label* label = GetCurrentLabel();

		if (left.type->kind == SpiteIR::TypeKind::ReferenceType && left.type->reference.type->byValue)
		{
			return BuildBinaryOpValue(BuildTypeDereference(label, left), right, op);
		}

		if (right.type->kind == SpiteIR::TypeKind::ReferenceType && right.type->reference.type->byValue)
		{
			return BuildBinaryOpValue(left, BuildTypeDereference(label, right), op);
		}

		if (left.type->kind == SpiteIR::TypeKind::PrimitiveType &&
			right.type->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return BuildBinaryOp(left, right, op, label);
		}

		if (left.type->kind == SpiteIR::TypeKind::PointerType)
		{
			ScopeValue intValue = BuildBinaryOpValue(PointerToInt(left), right, op);
			if (intValue.type->primitive.kind != SpiteIR::PrimitiveKind::Bool)
				intValue = IntToPointer(intValue, left.type);

			return intValue;
		}

		if (right.type->kind == SpiteIR::TypeKind::PointerType)
		{

			ScopeValue intValue = BuildBinaryOpValue(left, PointerToInt(right), op);
			if (intValue.type->primitive.kind != SpiteIR::PrimitiveKind::Bool)
				intValue = IntToPointer(intValue, right.type);

			return intValue;
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildLogicAndExpr(Expr* left, Expr* right, Stmnt* stmnt)
	{
		ScopeValue result = HandleAutoCast(BuildExpr(left, stmnt), castBool);
		SpiteIR::Instruction* fromBranch = BuildBranch(GetCurrentLabel(), BuildRegisterOperand(result));

		size_t count = funcContext.ifCount;
		eastl::string lhsTrueStr = "and_lhs_true" + eastl::to_string(count);
		eastl::string andEndStr = "and_end" + eastl::to_string(count);
		funcContext.ifCount = count + 1;

		SpiteIR::Label* lhsTrueLabel = BuildLabel(lhsTrueStr);
		SpiteIR::Label* andEndLabel = BuildLabel(andEndStr);
		fromBranch->branch.true_ = lhsTrueLabel;
		fromBranch->branch.false_ = andEndLabel;

		AddLabel(lhsTrueLabel);
		ScopeValue rhResult = HandleAutoCast(BuildExpr(right, stmnt), castBool);
		BuildStore(lhsTrueLabel, BuildRegisterOperand(result), BuildRegisterOperand(rhResult));
		BuildJump(lhsTrueLabel, andEndLabel);

		AddLabel(andEndLabel);

		return result;
	}

	ScopeValue BuildLogicOrExpr(Expr* left, Expr* right, Stmnt* stmnt)
	{
		ScopeValue result = HandleAutoCast(BuildExpr(left, stmnt), castBool);
		SpiteIR::Instruction* fromBranch = BuildBranch(GetCurrentLabel(), BuildRegisterOperand(result));

		size_t count = funcContext.ifCount;
		eastl::string lhsFalseStr = "or_lhs_false" + eastl::to_string(count);
		eastl::string orEndStr = "or_end" + eastl::to_string(count);
		funcContext.ifCount = count + 1;

		SpiteIR::Label* lhsFalseLabel = BuildLabel(lhsFalseStr);
		SpiteIR::Label* orEndLabel = BuildLabel(orEndStr);
		fromBranch->branch.true_ = orEndLabel;
		fromBranch->branch.false_ = lhsFalseLabel;

		AddLabel(lhsFalseLabel);
		ScopeValue rhResult = HandleAutoCast(BuildExpr(right, stmnt), castBool);
		BuildStore(lhsFalseLabel, BuildRegisterOperand(result), BuildRegisterOperand(rhResult));
		BuildJump(lhsFalseLabel, orEndLabel);

		AddLabel(orEndLabel);

		return result;
	}

	ScopeValue BuildBinaryExpression(Expr* expr, Stmnt* stmnt)
	{
		Expr* left = expr->binaryExpr.left;
		Expr* right = expr->binaryExpr.right;
		UniqueType opType = expr->binaryExpr.op->uniqueType;
		SpiteIR::BinaryOpKind op = BinaryOpToIR(opType);

		if (op == SpiteIR::BinaryOpKind::LogicAnd)
		{
			return BuildLogicAndExpr(left, right, stmnt);
		}
		else if (op == SpiteIR::BinaryOpKind::LogicOr)
		{
			return BuildLogicOrExpr(left, right, stmnt);
		}

		ScopeValue leftVal = BuildExpr(left, stmnt);
		ScopeValue rightVal = BuildExpr(right, stmnt);

		SpiteIR::State* state = GetStateForType(leftVal.type);
		if (state)
		{
			return BuildStateOperatorCall(state, leftVal, opType, &rightVal);
		}

		return BuildBinaryOpValue(leftVal, rightVal, op);
	}

	ScopeValue BuildUnaryOpValue(const ScopeValue& value, SpiteIR::UnaryOpKind op)
	{
		if (!value.type) return InvalidScopeValue;

		SpiteIR::Label* label = GetCurrentLabel();

		if (value.type->kind == SpiteIR::TypeKind::ReferenceType && value.type->reference.type->byValue)
		{
			return BuildUnaryOpValue(BuildTypeDereference(label, value), op);
		}

		if (value.type->kind == SpiteIR::TypeKind::PrimitiveType)
		{
			return BuildUnaryOp(value, op, label);
		}

		if (value.type->kind == SpiteIR::TypeKind::PointerType)
		{
			ScopeValue intValue = BuildUnaryOpValue(PointerToInt(value), op);
			if (intValue.type->primitive.kind != SpiteIR::PrimitiveKind::Bool)
				intValue = IntToPointer(intValue, value.type);

			return intValue;
		}

		return InvalidScopeValue;
	}

	ScopeValue BuildUnaryExpression(Expr* expr, Stmnt* stmnt)
	{
		Expr* operand = expr->unaryExpr.expr;
		UniqueType opType = expr->unaryExpr.op->uniqueType;
		SpiteIR::UnaryOpKind op = UnaryOpToIR(opType);

		ScopeValue val = BuildExpr(operand, stmnt);

		SpiteIR::State* state = GetStateForType(val.type);
		if (state)
		{
			return BuildStateOperatorCall(state, val, opType);
		}

		return BuildUnaryOpValue(val, op);
	}

	ScopeValue BuildTemplateExpr(Expr* expr, Stmnt* stmnt)
	{
		auto& templated = expr->templateExpr;
		Expr* of = templated.expr;
		eastl::vector<Expr*> templateArgs = ExpandTemplates(templated.templateArgs);

		ScopeValue ofValue = BuildExpr(of, stmnt);
		if (ofValue.reg == StmntRegister)
		{
			Stmnt* templStmnt = ofValue.stmnt;
			switch (templStmnt->nodeID)
			{
			case StmntID::StateStmnt:
			{
				SpiteIR::Package* package = context.packageMap[templStmnt->package->val];
				SpiteIR::State* state = FindPackageState(package,
					BuildTemplatedStateName(templStmnt, &templateArgs));
				ScopeValue stateValue = ScopeValue();
				stateValue.reg = StateRegister;
				stateValue.state = state;
				return stateValue;
			}
			case StmntID::Method:
			{
				SpiteIR::State* state = funcContext.templatedMethodState;
				Assert(state);
				SpiteIR::Function* method = FindFunction(templStmnt->package->val,
					BuildTemplatedMethodName(state, templStmnt, &templateArgs));
				funcContext.templatedMethodState = nullptr;
				return StoreFunctionValue(method);
			}
			case StmntID::FunctionStmnt:
			{
				SpiteIR::Function* func = FindFunction(templStmnt->package->val,
					BuildTemplatedFunctionName(templStmnt, &templateArgs));
				return StoreFunctionValue(func);
			}
			default:
				break;
			}
		}

		return ofValue;
	}

	void MakeDynamicArray(SpiteIR::Type* irType, size_t dst, SpiteIR::Label* label)
	{
		size_t arrayItemSize = irType->dynamicArray.type->size;
		ScopeValue itemSize = BuildLiteralInt(arrayItemSize);
		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		params->push_back(BuildRegisterOperand(HandleAutoCast(itemSize, CreateIntType(context.ir))));
		BuildCall(makeArray, dst, params, label);
	}

	void SizeDynamicArray(ScopeValue arrValue, Expr* size, Stmnt* stmnt)
	{
		ScopeValue sizeValue = BuildExpr(size, stmnt);
		SpiteIR::Label* label = GetCurrentLabel();

		ScopeValue arrRef = BuildTypeReference(label, arrValue);
		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		params->push_back(BuildRegisterOperand(arrRef));
		params->push_back(BuildRegisterOperand(HandleAutoCast(sizeValue, CreateIntType(context.ir))));
		BuildCall(sizeArray, funcContext.curr, params, label);
	}

	ScopeValue BuildTypeExpr(Expr* expr, Stmnt* stmnt)
	{
		SpiteIR::Label* label = GetCurrentLabel();
		Type* type = expr->typeExpr.type;
		SpiteIR::Allocate alloc = BuildAllocateForType(type);
		SpiteIR::Type* irType = alloc.type;
		size_t reg = alloc.result;

		if (irType->kind == SpiteIR::TypeKind::DynamicArrayType)
		{
			MakeDynamicArray(irType, reg, label);
			if (type->arrayType.size)
			{
				SizeDynamicArray({ reg, irType }, type->arrayType.size, stmnt);
			}
			return { reg, irType };
		}

		return BuildDefaultValue(irType, reg, label);
	}

	void BuildAnonFunctionName(eastl::string& name)
	{
		name = "_anon_" + funcContext.function->name + "_" + eastl::to_string(funcContext.anonFuncCount);
		funcContext.anonFuncCount += 1;
	}

	ScopeValue BuildAnonFunction(Expr* expr, Stmnt* stmnt)
	{
		Stmnt* funcStmnt = expr->functionTypeDeclExpr.anonFunction;
		SpiteIR::Type* returnType = ToIRType(funcStmnt->anonFunction.returnType);
		auto& funcDecl = funcStmnt->anonFunction.decl->functionDecl;

		SpiteIR::Function* func = context.ir->AllocateFunction();
		BuildAnonFunctionName(func->name);
		func->parent = funcContext.function->parent;
		func->returnType = returnType;
		func->block = context.ir->AllocateBlock();

		for (size_t i = 0; i < funcDecl.parameters->size(); i++)
		{
			Stmnt* param = funcDecl.parameters->at(i);
			SpiteIR::Type* argType = ToIRType(param->definition.type);
			if (!argType->byValue) argType = MakeReferenceType(argType, context.ir);

			SpiteIR::Argument* arg = context.ir->AllocateArgument();
			arg->value = context.ir->AllocateValue();
			arg->value->parent = SpiteIR::Parent(arg);
			arg->value->type = argType;
			arg->value->name = param->definition.name->val.ToString();
			arg->parent = func;
			func->arguments.push_back(arg);
		}

		FunctionContext prev = funcContext;
		funcContext = FunctionContext();
		funcContext.Reset(func, symbolTable, context.globalTable);
		BuildFunctionDecl(func, funcStmnt->anonFunction.decl);
		funcContext = prev;

		SpiteIR::Type* funcType = IRFunctionToFunctionType(context.ir, func);
		SpiteIR::Allocate alloc = BuildAllocate(funcType);

		SpiteIR::Operand funcOperand = SpiteIR::Operand();
		funcOperand.kind = SpiteIR::OperandKind::Function;
		funcOperand.type = funcType;
		funcOperand.function = func;

		SpiteIR::Instruction* storeFunc = BuildStoreFunc(GetCurrentLabel(), AllocateToOperand(alloc),
			funcOperand);
		return { alloc.result, alloc.type };
	}

	SpiteIR::Function* BuildAnonBlockFunction(SpiteIR::Type* returnType, Body& body)
	{
		SpiteIR::Function* func = context.ir->AllocateFunction();
		BuildAnonFunctionName(func->name);
		func->parent = funcContext.function->parent;
		func->returnType = returnType;
		func->block = context.ir->AllocateBlock();

		FunctionContext prev = funcContext;
		funcContext = FunctionContext();
		funcContext.Reset(func, symbolTable, context.globalTable);
		AddScope();
		BuildLabelBody("entry", body);
		SpiteIR::Label* lastLabel = GetCurrentLabel();
		if (!lastLabel->terminator && IsVoidType(func->returnType))
		{
			BuildVoidReturn(lastLabel);
		}
		PopScope();
		funcContext = prev;

		return func;
	}

	ScopeValue BuildCompileExpr(Expr* expr, Stmnt* stmnt)
	{
		Stmnt* compileStmnt = expr->compileExpr.compile;
		SpiteIR::Function* compileFunc = BuildCompileStmnt(compileStmnt);

		SpiteIR::Allocate alloc = BuildAllocate(compileFunc->returnType);
		SpiteIR::Instruction* store = BuildStore(GetCurrentLabel(), AllocateToOperand(alloc),
			BuildRegisterOperand(InvalidScopeValue));

		deferredCompiles.push_back({ compileFunc, store });
		return { alloc.result,	alloc.type };
	}

	SpiteIR::Function* BuildCompileStmnt(Stmnt* stmnt)
	{
		SpiteIR::Function* compileFunc = BuildAnonBlockFunction(ToIRType(stmnt->compileStmnt.returnType),
			stmnt->compileStmnt.body);

		return compileFunc;
	}

	ScopeValue BuildSizeOf(Expr* expr, Stmnt* stmnt)
	{
		if (expr->sizeOfExpr.expr->typeID == ExprID::TypeExpr)
		{
			SpiteIR::Type* type = ToIRType(expr->sizeOfExpr.expr->typeExpr.type);
			return BuildLiteralInt(type->size);
		}

		ScopeValue value = BuildExpr(expr->sizeOfExpr.expr, stmnt);
		SpiteIR::Type* type = value.type;
		if (type->kind == SpiteIR::TypeKind::ReferenceType) type = type->reference.type;
		return BuildLiteralInt(type->size);
	}

	ScopeValue BuildAlignOf(Expr* expr, Stmnt* stmnt)
	{
		return BuildLiteralInt(0);
	}

	SpiteIR::InstructionMetadata* CreateInstructionMetadata(SpiteIR::Label* label)
	{
		SpiteIR::InstructionMetadata* metadata = context.ir->AllocateInstructionMetadata();
		metadata->statementPosition = funcContext.currStmnt->start->pos;
		metadata->expressionPosition = funcContext.currExpr->start->pos;
		return metadata;
	}

	SpiteIR::Instruction* CreateInstruction(SpiteIR::Label* label)
	{
		SpiteIR::Instruction* inst = context.ir->AllocateInstruction();
		inst->metadata = CreateInstructionMetadata(label);
		label->values.push_back(inst);
		return inst;
	}

	SpiteIR::Instruction* CreateTerminator(SpiteIR::Label* label)
	{
		SpiteIR::Instruction* inst = context.ir->AllocateInstruction();
		inst->metadata = CreateInstructionMetadata(label);
		label->terminator = inst;
		return inst;
	}

	bool AreSamePrimitive(SpiteIR::Type* left, SpiteIR::Type* right)
	{
		return left->size == right->size &&
			left->primitive.kind == right->primitive.kind &&
			left->primitive.isSigned == right->primitive.isSigned;
	}

	ScopeValue BuildPrimitiveCast(const ScopeValue& from, SpiteIR::Type* to)
	{
		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Allocate alloc = BuildAllocate(to);
		SpiteIR::Instruction* cast = BuildCast(label, BuildRegisterOperand(from), AllocateToOperand(alloc));
		return { cast->cast.to.reg, cast->cast.to.type };
	}

	void HandlePrimitivePromotion(ScopeValue& left, ScopeValue& right)
	{
		if (left.type->kind == SpiteIR::TypeKind::ReferenceType)
		{
			ScopeValue value = BuildTypeDereference(GetCurrentLabel(), left);
			left.reg = value.reg;
			left.type = value.type;
		}

		if (right.type->kind == SpiteIR::TypeKind::ReferenceType)
		{
			ScopeValue value = BuildTypeDereference(GetCurrentLabel(), right);
			right.reg = value.reg;
			right.type = value.type;
		}

		Assert(left.type->kind == SpiteIR::TypeKind::PrimitiveType &&
				right.type->kind == SpiteIR::TypeKind::PrimitiveType);
		Assert(left.type->primitive.kind != SpiteIR::PrimitiveKind::Void &&
				right.type->primitive.kind != SpiteIR::PrimitiveKind::Void);		

		if (AreSamePrimitive(left.type, right.type)) return;

		SpiteIR::Type* castTo = context.ir->AllocateType();
		castTo->kind = SpiteIR::TypeKind::PrimitiveType;
		// Int to int 
		if (IsIntLikeType(left.type) && IsIntLikeType(right.type))
		{
			// Same size, but one type is signed and the other isn't
			if (left.type->size == right.type->size)
			{
				if (left.type->primitive.isSigned)
				{
					castTo->primitive.kind = right.type->primitive.kind;
					castTo->size = right.type->size;
					castTo->primitive.isSigned = false;

					ScopeValue casted = BuildPrimitiveCast(left, castTo);
					left.reg = casted.reg;
					left.type = casted.type;
				}
				else
				{
					castTo->primitive.kind = left.type->primitive.kind;
					castTo->size = left.type->size;
					castTo->primitive.isSigned = false;

					ScopeValue casted = BuildPrimitiveCast(right, castTo);
					right.reg = casted.reg;
					right.type = casted.type;
				}
			}
			// One type is larger than the other type, widen to the larger type, including signed conversions
			else if (left.type->size > right.type->size)
			{
				castTo->primitive.kind = left.type->primitive.kind;
				castTo->size = left.type->size;
				castTo->primitive.isSigned = left.type->primitive.isSigned;

				ScopeValue casted = BuildPrimitiveCast(right, castTo);
				right.reg = casted.reg;
				right.type = casted.type;
			}
			else
			{
				castTo->primitive.kind = right.type->primitive.kind;
				castTo->size = right.type->size;
				castTo->primitive.isSigned = right.type->primitive.isSigned;

				ScopeValue casted = BuildPrimitiveCast(left, castTo);
				left.reg = casted.reg;
				left.type = casted.type;
			}
		}
		// Both types are floating point, widen to larger type
		else if (IsFloatLikeType(left.type) && IsFloatLikeType(right.type))
		{
			if (left.type->size > right.type->size)
			{
				castTo->primitive.kind = left.type->primitive.kind;
				castTo->size = left.type->size;
				castTo->primitive.isSigned = left.type->primitive.isSigned;

				ScopeValue casted = BuildPrimitiveCast(right, castTo);
				right.reg = casted.reg;
				right.type = casted.type;
			}
			else
			{
				castTo->primitive.kind = right.type->primitive.kind;
				castTo->size = right.type->size;
				castTo->primitive.isSigned = right.type->primitive.isSigned;

				ScopeValue casted = BuildPrimitiveCast(left, castTo);
				left.reg = casted.reg;
				left.type = casted.type;
			}
		}
		// One type is floating point and one is int, cast int to floating point
		else
		{
			if (left.type->primitive.kind == SpiteIR::PrimitiveKind::Float)
			{
				castTo->primitive.kind = left.type->primitive.kind;
				castTo->size = left.type->size;
				castTo->primitive.isSigned = left.type->primitive.isSigned;

				ScopeValue casted = BuildPrimitiveCast(right, castTo);
				right.reg = casted.reg;
				right.type = casted.type;
			}
			else
			{
				castTo->primitive.kind = right.type->primitive.kind;
				castTo->size = right.type->size;
				castTo->primitive.isSigned = right.type->primitive.isSigned;

				ScopeValue casted = BuildPrimitiveCast(left, castTo);
				left.reg = casted.reg;
				left.type = casted.type;
			}
		}

	}

	ScopeValue BuildBinaryOp(ScopeValue leftVal, ScopeValue rightVal, SpiteIR::BinaryOpKind kind,
		SpiteIR::Label* label)
	{
		HandlePrimitivePromotion(leftVal, rightVal);

		SpiteIR::Type* returnType = nullptr;
		if (kind >= SpiteIR::BinaryOpKind::Equal) returnType = CreateBoolType(context.ir);
		else returnType = leftVal.type;
		
		SpiteIR::Allocate alloc = BuildAllocate(returnType);

		SpiteIR::Instruction* binOp = CreateInstruction(label);
		binOp->kind = SpiteIR::InstructionKind::BinOp;
		binOp->binOp.kind = kind;
		binOp->binOp.left = BuildRegisterOperand(leftVal);
		binOp->binOp.right = BuildRegisterOperand(rightVal);
		binOp->binOp.result = alloc.result;


		return { alloc.result, alloc.type };
	}

	ScopeValue BuildUnaryOp(ScopeValue opVal, SpiteIR::UnaryOpKind kind, SpiteIR::Label* label)
	{
		SpiteIR::Type* returnType = nullptr;
		if (kind == SpiteIR::UnaryOpKind::Not) returnType = CreateBoolType(context.ir);
		else returnType = opVal.type;

		SpiteIR::Allocate alloc = BuildAllocate(returnType);

		SpiteIR::Instruction* unOp = CreateInstruction(label);
		unOp->kind = SpiteIR::InstructionKind::UnOp;
		unOp->unOp.kind = kind;
		unOp->unOp.operand = BuildRegisterOperand(opVal);
		unOp->unOp.result = alloc.result;

		return { alloc.result, alloc.type };
	}

	ScopeValue BuildTypeReference(SpiteIR::Label* label, const ScopeValue& value)
	{
		if (value.type->kind == SpiteIR::TypeKind::ReferenceType) return value;
		SpiteIR::Type* refType = MakeReferenceType(value.type, context.ir);
		SpiteIR::Allocate alloc = BuildAllocate(refType);
		SpiteIR::Instruction* reference = BuildReference(label, AllocateToOperand(alloc),
			BuildRegisterOperand(value));
		return { alloc.result, alloc.type };
	}

	ScopeValue BuildTypeDereference(SpiteIR::Label* label, const ScopeValue& value)
	{
		if (value.type->kind != SpiteIR::TypeKind::ReferenceType) return value;
		SpiteIR::Type* valType = value.type->reference.type;
		SpiteIR::Allocate alloc = BuildAllocate(valType);
		SpiteIR::Instruction* reference = BuildDereference(label, AllocateToOperand(alloc),
			BuildRegisterOperand(value));
		return { alloc.result, alloc.type };
	}

	ScopeValue BuildStateDefaultValue(SpiteIR::State* state)
	{
		SpiteIR::Type* type = context.ir->AllocateType();
		type->kind = SpiteIR::TypeKind::StateType;
		type->size = state->size;
		type->stateType.state = state;

		SpiteIR::Label* label = GetCurrentLabel();
		SpiteIR::Allocate alloc = BuildAllocate(type);
		BuildDefaultValue(type, alloc.result, label);
		return BuildTypeReference(label, { alloc.result, alloc.type });
	}

	ScopeValue FindAndCallStateConstructor(SpiteIR::State* state, eastl::vector<Expr*>* params,
		Stmnt* stmnt, ScopeValue* thisValue = nullptr)
	{
		eastl::vector<ScopeValue> paramValues = eastl::vector<ScopeValue>();
		
		ScopeValue retValue;
		if (thisValue) retValue = *thisValue;
		else retValue = BuildStateDefaultValue(state);
		paramValues.push_back(retValue);

		for (Expr* param : *params)
		{
			paramValues.push_back(BuildExpr(param, stmnt));
		}

		SpiteIR::Function* conFunc = FindStateConstructor(state, &paramValues);

		eastl::vector<SpiteIR::Operand>* paramOps = context.ir->AllocateArray<SpiteIR::Operand>();
		paramOps->push_back(BuildRegisterOperand(BuildTypeReference(GetCurrentLabel(), paramValues.at(0))));
		for (size_t i = 1; i < conFunc->arguments.size(); i++)
		{
			SpiteIR::Type* argType = conFunc->arguments.at(i)->value->type;
			paramOps->push_back(BuildRegisterOperand(HandleAutoCast(paramValues.at(i), argType)));
		}

		BuildCall(conFunc, funcContext.curr, paramOps, GetCurrentLabel());
		return retValue;
	}

	SpiteIR::Function* FindStateConstructor(SpiteIR::State* state, eastl::vector<ScopeValue>* params)
	{
		eastl::vector<SpiteIR::Function*>& stateConstructors = state->constructors;
		SpiteIR::Function* opFunc = nullptr;
		for (SpiteIR::Function* func : stateConstructors)
		{
			Stmnt* funcStmnt = context.functionASTMap[func].node;
			if (params->size() < RequiredFunctionParamCount(funcStmnt)) continue;

			int match = 1;
			for (size_t i = 1; i < params->size(); i++)
			{
				SpiteIR::Type* argType = func->arguments.at(i)->value->type;
				SpiteIR::Type* paramType = params->at(i).type;
				int argMatch = IsIRTypeAssignable(argType, paramType);
				if (argMatch == 0)
				{
					match = 0;
					break;
				}
				else if (argMatch == 2)
				{
					match = 1;
				}
			}
			
			if (match == 0) continue;
			else if (match == 2 && !opFunc) opFunc = func;
			else if (match == 1)
			{
				opFunc = func;
				break;
			}
		}

		Assert(opFunc);
		return opFunc;
	}

	SpiteIR::Function* FindStateOperator(SpiteIR::State* state, UniqueType op, SpiteIR::Type* rhs = nullptr)
	{
		eastl::string opStr = OperatorToString(op);
		Assert(MapHas(state->operators, opStr));
		eastl::vector<SpiteIR::Function*>& stateOperators = state->operators[opStr];
		SpiteIR::Function* opFunc = nullptr;
		for (SpiteIR::Function* func : stateOperators)
		{
			if (rhs)
			{
				eastl::vector<SpiteIR::Argument*>& args = func->arguments;
				SpiteIR::Type* argType = args.at(1)->value->type;
				int match = IsIRTypeAssignable(argType, rhs);
				if (match == 2 && !opFunc) opFunc = func;
				else if (match == 1)
				{
					opFunc = func;
					break;
				}
			}
			else
			{
				opFunc = func;
				break;
			}
		}

		Assert(opFunc);
		return opFunc;
	}

	ScopeValue BuildStateOperatorCall(SpiteIR::State* state, const ScopeValue& of, UniqueType op,
		ScopeValue* rhs = nullptr)
	{
		SpiteIR::Type* rhsType = rhs ? rhs->type : nullptr;
		SpiteIR::Function* operatorFunc = FindStateOperator(state, op, rhsType);

		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		params->push_back(BuildRegisterOperand(HandleAutoCast(of, operatorFunc->arguments.at(0)->value->type)));
		if (rhs)
		{
			params->push_back(BuildRegisterOperand(HandleAutoCast(*rhs,
				operatorFunc->arguments.at(1)->value->type)));
		}

		SpiteIR::Allocate alloc = BuildAllocate(operatorFunc->returnType);
		SpiteIR::Instruction* call = BuildCall(operatorFunc, alloc.result, params, GetCurrentLabel());
		return { alloc.result, alloc.type };
	}

	ScopeValue BuildStateOperatorCallForType(const ScopeValue& of, UniqueType op, ScopeValue* rhs = nullptr)
	{
		SpiteIR::State* state = of.type->kind == SpiteIR::TypeKind::ReferenceType ?
			of.type->reference.type->stateType.state : of.type->stateType.state;

		return BuildStateOperatorCall(state, of, op, rhs);
	}

	ScopeValue BuildFunctionTypeCall(Expr* expr, Stmnt* stmnt)
	{
		ScopeValue functionValue = BuildExpr(expr->functionCallExpr.function, stmnt);
		ScopeValue funcValue = BuildTypeDereference(GetCurrentLabel(), functionValue);
		Assert(funcValue.type->kind == SpiteIR::TypeKind::FunctionType &&
			expr->functionCallExpr.params->size() == funcValue.type->function.params->size());

		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		SpiteIR::Type* funcType = funcValue.type;

		for (size_t i = 0; i < funcType->function.params->size(); i++)
		{
			SpiteIR::Type* argType = funcType->function.params->at(i);
			Expr* param = expr->functionCallExpr.params->at(i);
			ScopeValue value = BuildExpr(param, stmnt);
			params->push_back(BuildRegisterOperand(HandleAutoCast(value, argType)));
		}

		SpiteIR::Allocate alloc = BuildAllocate(funcType->function.returnType);
		ScopeValue ret = { alloc.result, alloc.type };
		SpiteIR::Instruction* callPtr = BuildCallPtr(BuildRegisterOperand(funcValue), ret.reg, params, GetCurrentLabel());
		return ret;
	}

	inline bool IsIntLiteral(UniqueType type)
	{
		switch (type)
		{
		case UniqueType::IntLiteral:
		case UniqueType::HexLiteral:
		case UniqueType::TrueLiteral:
		case UniqueType::FalseLiteral:
			return true;
		default:
			return false;
		}
	}

	inline intmax_t IntLiteralToInt(Token* lit)
	{
		switch (lit->uniqueType)
		{
		case UniqueType::IntLiteral:
			return IntLiteralStringToInt(lit->val);
			break;
		case UniqueType::HexLiteral:
			return std::stoul(lit->val.ToString().c_str(), nullptr, 16);
		case UniqueType::TrueLiteral:
			return 1;
		case UniqueType::FalseLiteral:
			return 0;
		default:
			return 0;
		}
	}

	inline bool IsFloatLiteral(UniqueType type)
	{
		return type == UniqueType::FloatLiteral;
	}

	inline double FloatLiteralToFloat(Token* lit)
	{
		return std::stof(lit->val.ToString().c_str());
	}

	inline bool IsStringLiteral(UniqueType type)
	{
		return type == UniqueType::StringLiteral;
	}

	ScopeValue BuildPrimitiveConstructor(Expr* expr, Stmnt* stmnt)
	{
		Expr* primExpr = expr->functionCallExpr.function;
		Assert(primExpr->typeID == ExprID::PrimitiveExpr);
		eastl::vector<Expr*>* params = expr->functionCallExpr.params;
		SpiteIR::Type* primType = ToIRType(symbolTable->CreatePrimitive(primExpr->primitiveExpr.primitive->uniqueType));

		return CreatePrimitiveForParams(primType, params, stmnt);
	}

	ScopeValue CreatePrimitiveForParams(SpiteIR::Type* primType, eastl::vector<Expr*>* params, Stmnt* stmnt)
	{
		SpiteIR::Allocate alloc = BuildAllocate(primType);

		if (!params->size())
		{
			return BuildDefaultValue(primType, alloc.result, GetCurrentLabel());
		}

		if (IsStringType(primType))
		{
			ScopeValue defaultStr = BuildDefaultValue(primType, alloc.result, GetCurrentLabel());
			SpiteIR::State* stringState = GetStateForType(primType);
			return FindAndCallStateConstructor(stringState, params, stmnt, &defaultStr);
		}

		Expr* param = params->at(0);
		UniqueType litType = param->literalExpr.val->uniqueType;
		if (param->typeID == ExprID::LiteralExpr && !IsStringLiteral(litType))
		{
			SpiteIR::Operand operand = SpiteIR::Operand();
			operand.type = primType;
			operand.kind = SpiteIR::OperandKind::Literal;
			SpiteIR::Literal& literal = operand.literal;
			literal.kind = primType->primitive.kind;

			if (IsIntLiteral(litType))
			{
				intmax_t i = IntLiteralToInt(param->literalExpr.val);

				switch (primType->primitive.kind)
				{
				case SpiteIR::PrimitiveKind::Bool:
				case SpiteIR::PrimitiveKind::Byte:
					literal.byteLiteral = i;
					break;
				case SpiteIR::PrimitiveKind::I16:
					literal.i16Literal = i;
					break;
				case SpiteIR::PrimitiveKind::I32:
					literal.i32Literal = i;
					break;
				case SpiteIR::PrimitiveKind::I64:
					literal.i64Literal = i;
					break;
				case SpiteIR::PrimitiveKind::Int:
					literal.intLiteral = i;
					break;
				case SpiteIR::PrimitiveKind::F32:
				case SpiteIR::PrimitiveKind::Float:
				case SpiteIR::PrimitiveKind::String:
					AddError(param->start, "LowerDefinitions:CreatePrimitiveForParams Invalid primitive type for literal expression");
					break;
				}
			}
			else if (IsFloatLiteral(litType))
			{
				double f = FloatLiteralToFloat(param->literalExpr.val);
				switch (primType->primitive.kind)
				{
				case SpiteIR::PrimitiveKind::F32:
					literal.f32Literal = f;
					break;
				case SpiteIR::PrimitiveKind::Float:
					literal.floatLiteral = f;
					break;
				case SpiteIR::PrimitiveKind::Bool:
				case SpiteIR::PrimitiveKind::Byte:
				case SpiteIR::PrimitiveKind::I16:
				case SpiteIR::PrimitiveKind::I32:
				case SpiteIR::PrimitiveKind::I64:
				case SpiteIR::PrimitiveKind::Int:				
				case SpiteIR::PrimitiveKind::String:
					AddError(param->start, "LowerDefinitions:CreatePrimitiveForParams Invalid primitive type for literal expression");
					break;
				}
			}

			SpiteIR::Instruction* store = BuildStore(GetCurrentLabel(), AllocateToOperand(alloc), operand);
			return { alloc.result, alloc.type };
		}

		ScopeValue value = BuildExpr(param, stmnt);
		SpiteIR::Instruction* cast = BuildCast(GetCurrentLabel(), BuildRegisterOperand(value), AllocateToOperand(alloc));
		return { alloc.result, alloc.type };
	}

	ScopeValue BuildFunctionCall(Expr* expr, Stmnt* stmnt)
	{
		if (expr->functionCallExpr.callKind == FunctionCallKind::UnknownCall)
		{
			Assert(expr->functionCallExpr.callKind != FunctionCallKind::UnknownCall);
		}
		auto& funcCall = expr->functionCallExpr;
		SpiteIR::Function* irFunction = nullptr;

		eastl::vector<SpiteIR::Operand>* params = context.ir->AllocateArray<SpiteIR::Operand>();
		switch (funcCall.callKind)
		{
		case FunctionCall:
			irFunction = FindFunctionForFunctionCall(expr);
			break;
		case ConstructorCall:
			irFunction = FindFunctionForConstructor(expr);
			break;
		case MemberMethodCall:
			irFunction = FindFunctionForMemberCall(expr, stmnt, params);
			break;
		case UniformMethodCall:
			irFunction = FindFunctionForUniformCall(expr, stmnt);
			break;
		case PrimitiveCall:
			return BuildPrimitiveConstructor(expr, stmnt);
		case FunctionTypeCall:
			return BuildFunctionTypeCall(expr, stmnt);
		case UnresolvedGenericCall:
		{
			ScopeValue resolvedValue = ResolveGenericFunctionCall(expr, stmnt);
			if (resolvedValue.reg == FunctionRegister)
			{
				irFunction = resolvedValue.function;
				break;
			}

			return resolvedValue;
		}
		case ExternalCall:
			irFunction = FindExternalFunctionForFunctionCall(expr);
			break;
		default:
			break;
		}

		if (!irFunction) return { InvalidRegister, nullptr };

		ScopeValue ret = InvalidScopeValue;
		if (funcCall.callKind == ConstructorCall)
		{
			ScopeValue stateValue = BuildExpr(funcCall.function, stmnt);
			Assert(stateValue.reg == StateRegister);

			SpiteIR::State* irState = stateValue.state;
			SpiteIR::Operand ref = BuildRegisterOperand(BuildStateDefaultValue(irState));
			params->push_back(ref);
			ret = { ref.reg, ref.type };
		}
		
		eastl::vector<Expr*>* exprParams = expr->functionCallExpr.params;
		size_t argOffset = params->size();
		for (size_t i = 0; i < exprParams->size(); i++)
		{
			Expr* param = exprParams->at(i);
			ScopeValue value = BuildExpr(param, stmnt);
			SpiteIR::Type* argType = irFunction->arguments.at(argOffset + i)->value->type;
			params->push_back(BuildRegisterOperand(HandleAutoCast(value, argType)));
		}

		// Assign default parameters
		if (params->size() < irFunction->arguments.size())
		{
			ASTContainer funcContainer = context.functionASTMap[irFunction];
			Stmnt* funcStmnt = funcContainer.node;
			eastl::vector<Stmnt*>* funcStmntParams = GetFunctionParams(funcStmnt);

			eastl::vector<Expr*>* prevTemplates = currTemplates;
			eastl::vector<Token*> prevGenerics = currGenerics;

			currTemplates = funcContainer.templates;
			SetCurrentGenerics(funcStmnt);

			for (size_t i = params->size(); i < irFunction->arguments.size(); i++)
			{
				Stmnt* stmntParam = funcStmntParams->at(i);
				Assert(stmntParam->definition.assignment);

				SpiteIR::Type* argType = irFunction->arguments.at(i)->value->type;
				ScopeValue value = BuildExpr(stmntParam->definition.assignment, stmnt);
				params->push_back(BuildRegisterOperand(HandleAutoCast(value, argType)));
			}

			currTemplates = prevTemplates;
			currGenerics = prevGenerics;
		}

		SpiteIR::Label* label = GetCurrentLabel();
		if (!ret.type)
		{
			SpiteIR::Allocate alloc = BuildAllocate(irFunction->returnType);
			ret = { alloc.result, alloc.type };
			SpiteIR::Instruction* call = BuildCall(irFunction, ret.reg, params, label);
		}
		else
		{
			SpiteIR::Instruction* call = BuildCall(irFunction, funcContext.curr, params, label);
		}
		return ret;
	}

	Stmnt* FindFunctionStmnt(Expr* expr)
	{
		switch (expr->typeID)
		{
		case IdentifierExpr:
			return context.globalTable->FindScopedFunction(expr->identifierExpr.identifier, symbolTable);
		case SelectorExpr:
			return context.globalTable->FindStatementForPackage(expr->selectorExpr.on->identifierExpr.identifier,
				expr->selectorExpr.select->identifierExpr.identifier);
		case TemplateExpr:
			return FindFunctionStmnt(expr->templateExpr.expr);
		default:
			break;
		}

		return nullptr;
	}

	bool FunctionExists(const StringView& packageName,
		const eastl::string& functionName)
	{
		if (MapHas(context.packageMap, packageName))
		{
			SpiteIR::Package* package = context.packageMap[packageName];
			return MapHas(package->functions, functionName);
		}

		return false;
	}

	SpiteIR::Function* FindFunction(const StringView& packageName,
		const eastl::string& functionName)
	{
		Assert(MapHas(context.packageMap, packageName));
		SpiteIR::Package* package = context.packageMap[packageName];
		Assert(MapHas(package->functions, functionName));
		SpiteIR::Function* function = package->functions[functionName];

		return function;
	}

	SpiteIR::Function* FindFunctionForMemberCall(Expr* expr, Stmnt* stmnt, eastl::vector<SpiteIR::Operand>* params)
	{
		Expr* functionExpr = expr->functionCallExpr.function;
		Expr* caller = GetCallerExprMethodCall(functionExpr);
		Stmnt* methodStmnt = expr->functionCallExpr.functionStmnt;
		StringView& packageName = methodStmnt->package->val;
		eastl::string methodName;

		ScopeValue thisValue = DereferenceToSinglePointer(BuildExpr(caller, stmnt));
		if (thisValue.type->kind != SpiteIR::TypeKind::PointerType)
			thisValue = BuildTypeReference(GetCurrentLabel(), thisValue);

		SpiteIR::State* state = GetStateForType(GetDereferencedType(thisValue.type));
		Assert(state);
		SpiteIR::Operand ref = BuildRegisterOperand(thisValue);
		params->push_back(ref);

		if (functionExpr->typeID == ExprID::TemplateExpr)
		{
			eastl::vector<Expr*>* templates = functionExpr->templateExpr.templateArgs;
			eastl::vector<Expr*> expandedTemplates = ExpandTemplates(templates);
			methodName = BuildTemplatedMethodName(state, methodStmnt, &expandedTemplates);
		}
		else
		{
			methodName = BuildMethodName(state, methodStmnt);
		}

		return FindFunction(packageName, methodName);
	}

	SpiteIR::Function* FindFunctionForUniformCall(Expr* expr, Stmnt* stmnt)
	{
		Expr* functionExpr = expr->functionCallExpr.function;
		Expr* caller = GetCallerExprMethodCall(functionExpr);
		Stmnt* methodStmnt = expr->functionCallExpr.functionStmnt;
		StringView& packageName = methodStmnt->package->val;
		eastl::string methodName;

		ScopeValue stateValue = BuildExpr(caller, stmnt);
		SpiteIR::State* state = stateValue.state;

		if (functionExpr->typeID == ExprID::TemplateExpr)
		{
			eastl::vector<Expr*>* templates = functionExpr->templateExpr.templateArgs;
			eastl::vector<Expr*> expandedTemplates = ExpandTemplates(templates);
			methodName = BuildTemplatedMethodName(state, methodStmnt, &expandedTemplates);
		}
		else
		{
			methodName = BuildMethodName(state, methodStmnt);
		}

		return FindFunction(packageName, methodName);
	}

	SpiteIR::Function* FindFunctionForConstructor(Expr* expr)
	{
		Expr* caller = expr->functionCallExpr.function;
		Stmnt* constructorStmnt = expr->functionCallExpr.functionStmnt;
		Stmnt* stateStmnt = constructorStmnt->nodeID == StmntID::StateStmnt ? constructorStmnt :
			context.globalTable->FindScopedState(constructorStmnt->constructor.stateName, symbolTable);

		StringView& packageName = stateStmnt->package->val;
		eastl::vector<Token*>* generics = stateStmnt->state.generics ?
			stateStmnt->state.generics->generics.names : nullptr;
		eastl::vector<Expr*> templates;
		if (caller->typeID == ExprID::TemplateExpr)
		{
			templates = ExpandTemplates(caller->templateExpr.templateArgs);
		}

		eastl::string constructorName = constructorStmnt->nodeID == StmntID::StateStmnt ?
			BuildDefaultConstructorName(constructorStmnt, &templates) :
			BuildConstructorName(constructorStmnt, generics, &templates);

		return FindFunction(packageName, constructorName);
	}

	SpiteIR::Function* FindExternalFunctionForFunctionCall(Expr* expr)
	{
		Expr* caller = expr->functionCallExpr.function;
		Stmnt* stmnt = FindFunctionStmnt(caller);

		eastl::string functionName = stmnt->externFunction.callName->val.ToString();
		StringView& packageName = stmnt->package->val;
		Assert(MapHas(context.packageMap, packageName));
		SpiteIR::Package* package = context.packageMap[packageName];
		Assert(MapHas(package->functions, functionName));
		SpiteIR::Function* function = package->functions[functionName];
		return function;
	}

	SpiteIR::Function* FindFunctionForFunctionCall(Expr* expr)
	{
		Assert(expr);
		Expr* caller = expr->functionCallExpr.function;
		Stmnt* func = expr->functionCallExpr.functionStmnt;
		StringView& packageName = func->package->val;
		eastl::string functionName;

		if (caller->typeID == ExprID::TemplateExpr)
		{
			eastl::vector<Expr*>* templates = caller->templateExpr.templateArgs;
			eastl::vector<Expr*> expandedTemplates = ExpandTemplates(templates);
			functionName = BuildTemplatedFunctionName(func, &expandedTemplates);
		}
		else
		{
			functionName = BuildFunctionName(func);
		}

		return FindFunction(packageName, functionName);
	}

	ScopeValue ResolveGenericFunctionCall(Expr* expr, Stmnt* stmnt)
	{
		Expr* caller = ExpandTemplate(expr->functionCallExpr.function);

		if (caller->typeID == ExprID::TypeExpr)
		{
			eastl::vector<Expr*>* params = expr->functionCallExpr.params;
			SpiteIR::Type* type = ToIRType(caller->typeExpr.type);
			if (type->kind == SpiteIR::TypeKind::PrimitiveType)
			{
				return CreatePrimitiveForParams(type, params, stmnt);
			}
			else
			{
				SpiteIR::State* state = GetStateForType(type);
				if (state)
				{
					return FindAndCallStateConstructor(state, params, stmnt);
				}
			}
		}

		funcContext.SetFlag(FunctionContextFlag::ReturnFunctionScopeValue);
		ScopeValue value = BuildExpr(caller, stmnt);
		funcContext.ClearFlag(FunctionContextFlag::ReturnFunctionScopeValue);

		if (value.reg == FunctionRegister)
		{
			return value;
		}

		return InvalidScopeValue;
	}

	SpiteIR::Operand AllocateToOperand(SpiteIR::Allocate& alloc)
	{
		return BuildRegisterOperand({ alloc.result, alloc.type });
	}

	SpiteIR::Allocate BuildAllocate(SpiteIR::Type* type)
	{
		SpiteIR::Allocate alloc = { funcContext.curr, type };
		funcContext.function->block->allocations.push_back(alloc);
		funcContext.IncrementRegister(type);
		return alloc;
	}

	SpiteIR::Allocate BuildAllocateForType(Type* type)
	{
		SpiteIR::Type* irType = ToIRType(type);
		return BuildAllocate(irType);
	}

	SpiteIR::Instruction* BuildStore(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = CreateInstruction(label);
		store->kind = SpiteIR::InstructionKind::Store;
		store->store.dst = dst;
		store->store.src = src;
		return store;
	}

	SpiteIR::Instruction* BuildStorePtr(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = BuildStore(label, dst, src);
		store->kind = SpiteIR::InstructionKind::StorePtr;
		return store;
	}

	SpiteIR::Instruction* BuildStoreFunc(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = BuildStore(label, dst, src);
		store->kind = SpiteIR::InstructionKind::StoreFunc;
		return store;
	}

	SpiteIR::Type* GetMoveType(SpiteIR::Type* type)
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PointerType:
			return type->pointer.type;
		case SpiteIR::TypeKind::ReferenceType:
			return type->reference.type;
		default:
			break;
		}

		return type;
	}

	SpiteIR::Instruction* BuildMove(SpiteIR::Label* label, SpiteIR::Operand dst,
		const SpiteIR::Operand& src)
	{
		dst.type = GetMoveType(dst.type);
		SpiteIR::Instruction* store = BuildStore(label, dst, src);
		store->kind = SpiteIR::InstructionKind::Move;
		return store;
	}

	SpiteIR::Instruction* BuildDereference(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = BuildStore(label, dst, src);
		store->kind = SpiteIR::InstructionKind::Dereference;
		return store;
	}

	SpiteIR::Instruction* BuildReference(SpiteIR::Label* label, const SpiteIR::Operand& dst,
		const SpiteIR::Operand& src)
	{
		SpiteIR::Instruction* store = BuildStore(label, dst, src);
		store->kind = SpiteIR::InstructionKind::Reference;
		return store;
	}

	SpiteIR::Instruction* BuildLoad(SpiteIR::Label* label, const SpiteIR::Operand dst,
		const SpiteIR::Operand& src, const SpiteIR::Operand& offset, SpiteIR::Type* indexType)
	{
		SpiteIR::Instruction* load = CreateInstruction(label);
		load->kind = SpiteIR::InstructionKind::Load;
		load->load.dst = dst;
		load->load.src = src;
		load->load.offset = offset;
		load->load.indexType = indexType;
		return load;
	}

	SpiteIR::Instruction* BuildLoadPtrOffset(SpiteIR::Label* label, const SpiteIR::Operand dst,
		const SpiteIR::Operand& src, const SpiteIR::Operand& offset, SpiteIR::Type* indexType)
	{
		SpiteIR::Instruction* load = BuildLoad(label, dst, src, offset, indexType);
		load->kind = SpiteIR::InstructionKind::LoadPtrOffset;
		return load;
	}

	SpiteIR::Instruction* BuildLoadGlobal(SpiteIR::Label* label, const SpiteIR::Operand dst, size_t src)
	{
		SpiteIR::Instruction* load = CreateInstruction(label);
		load->kind = SpiteIR::InstructionKind::LoadGlobal;
		load->loadGlobal.dst = dst;
		load->loadGlobal.src = src;
		return load;
	}

	SpiteIR::Instruction* BuildCast(SpiteIR::Label* label, const SpiteIR::Operand from,
		const SpiteIR::Operand& to)
	{
		SpiteIR::Instruction* cast = CreateInstruction(label);
		cast->kind = SpiteIR::InstructionKind::Cast;
		cast->cast.from = from;
		cast->cast.to = to;
		return cast;
	}

	SpiteIR::Operand BuildRegisterOperand(const ScopeValue& value)
	{
		SpiteIR::Operand operand = SpiteIR::Operand();
		operand.type = value.type;
		operand.kind = SpiteIR::OperandKind::Register;
		operand.reg = value.reg;
		return operand;
	}

	SpiteIR::InstructionKind GetCallKind(SpiteIR::Function* function)
	{
		if (function->metadata.externFunc) return SpiteIR::InstructionKind::ExternCall;

		return SpiteIR::InstructionKind::Call;
	}

	SpiteIR::Instruction* BuildCall(SpiteIR::Function* function, size_t returnReg,
		eastl::vector<SpiteIR::Operand>* params, SpiteIR::Label* label)
	{
		SpiteIR::Instruction* call = CreateInstruction(label);
		call->kind = GetCallKind(function);
		call->call.function = function;
		call->call.params = params;
		call->call.result = returnReg;
		return call;
	}

	SpiteIR::Instruction* BuildCallPtr(const SpiteIR::Operand& funcPtr, size_t returnReg,
		eastl::vector<SpiteIR::Operand>* params, SpiteIR::Label* label)
	{
		SpiteIR::Instruction* callPtr = CreateInstruction(label);
		callPtr->kind = SpiteIR::InstructionKind::CallPtr;
		callPtr->callPtr.funcPtr = funcPtr;
		callPtr->callPtr.params = params;
		callPtr->callPtr.result = returnReg;
		return callPtr;
	}

	SpiteIR::Instruction* BuildSwitch(SpiteIR::Label* label, const SpiteIR::Operand& test,
		eastl::hash_map<intmax_t, SpiteIR::Label*>* cases, SpiteIR::Label* defaultCase)
	{
		Assert(!label->terminator);
		SpiteIR::Instruction* switch_ = CreateTerminator(label);
		switch_->kind = SpiteIR::InstructionKind::Switch;
		switch_->switch_.test = test;
		switch_->switch_.cases = cases;
		switch_->switch_.defaultCase = defaultCase;
		return switch_;
	}

	SpiteIR::Instruction* BuildJump(SpiteIR::Label* label, SpiteIR::Label* to = nullptr)
	{
		Assert(!label->terminator);
		SpiteIR::Instruction* jump = CreateTerminator(label);
		jump->kind = SpiteIR::InstructionKind::Jump;
		jump->jump.label = to;
		return jump;
	}

	SpiteIR::Instruction* BuildBranch(SpiteIR::Label* label, const SpiteIR::Operand& test,
		SpiteIR::Label* true_ = nullptr, SpiteIR::Label* false_ = nullptr)
	{
		Assert(!label->terminator);
		SpiteIR::Instruction* branch = CreateTerminator(label);
		branch->kind = SpiteIR::InstructionKind::Branch;
		branch->branch.test = test;
		branch->branch.true_ = true_;
		branch->branch.false_ = false_;
		return branch;
	}

	SpiteIR::Instruction* BuildReturnOp(SpiteIR::Label* label, const SpiteIR::Operand& operand)
	{
		Assert(!label->terminator);
		SpiteIR::Instruction* ret = CreateTerminator(label);
		ret->kind = SpiteIR::InstructionKind::Return;
		ret->return_.operand = operand;
		return ret;
	}

	SpiteIR::Instruction* BuildAssert(SpiteIR::Label* label, const SpiteIR::Operand& test, 
		const SpiteIR::Operand& message)
	{
		SpiteIR::Instruction* assert = CreateInstruction(label);
		assert->kind = SpiteIR::InstructionKind::Assert;
		assert->assert.test = test;
		assert->assert.message = message;
		return assert;
	}

	SpiteIR::Instruction* BuildLog(SpiteIR::Label* label, eastl::vector<SpiteIR::Operand>* operands)
	{
		SpiteIR::Instruction* log = CreateInstruction(label);
		log->kind = SpiteIR::InstructionKind::Log;
		log->log.operands = operands;
		return log;
	}
};