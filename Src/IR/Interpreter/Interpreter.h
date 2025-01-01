#pragma once
#include "EASTL/deque.h"
#include "../IR.h"
#include "../../Utils/Utils.h"
#include "../../Log/Logger.h"
#include "ExternCall.h"

#ifdef WIN32
const size_t os = 0;
const size_t arch = 0;
#else UNIX
const size_t os = 1;
const size_t arch = 0;
#endif 

extern std::filesystem::path execDir;
extern Config config;

struct Interpreter
{
	char* stack;
	char* stackFrameStart;

	char* global = nullptr;

	Interpreter(size_t stackSize)
	{
		stack = new char[stackSize];
		stackFrameStart = stack;
		CreateDynCallVM();
	}

	~Interpreter()
	{
		delete stack;
		delete global;
	}

	inline void SetGlobalInt(SpiteIR::Package* package, const eastl::string& name, intmax_t value)
	{
		size_t index = package->globalVariableLookup[name];
		SpiteIR::GlobalVariable* var = package->globalVariables[index];
		intmax_t* val = (intmax_t*)(void*)(global + var->index);
		*val = value;
	}

	inline void SetGlobalString(SpiteIR::Package* package, const eastl::string& name,
		const eastl::string* value)
	{
		size_t index = package->globalVariableLookup[name];
		SpiteIR::GlobalVariable* var = package->globalVariables[index];

		size_t* sizeDst = (size_t*)(void*)(global + var->index);
		*sizeDst = value->size();
		char** strDst = (char**)(sizeDst + 1);
		*strDst = (char*)value->c_str();
	}

	void InitializeRuntimeValues(SpiteIR::IR* ir)
	{
		SpiteIR::Package* runtime = ir->runtime;

		SetGlobalInt(runtime, "__os", os);
		SetGlobalInt(runtime, "__targetOs", config.os);

		SetGlobalInt(runtime, "__arch", arch);
		SetGlobalInt(runtime, "__targetArch", config.arch);

		SetGlobalString(runtime, "__exec_dir", new eastl::string(execDir.string().c_str()));
	}

	void Initialize(SpiteIR::IR* ir)
	{
		delete global;
		global = new char[ir->globalSize];
		for (SpiteIR::Package* package : ir->packages)
		{
			if (package->initializer) InterpretFunction(package->initializer, 0);
		}
		InitializeRuntimeValues(ir);
	}

	void* Interpret(SpiteIR::IR* ir)
	{
		SpiteIR::Function* entry = ir->entry;
		Initialize(ir);
		return InterpretFunction(entry, 0);
	}

	inline void InterpretLabel(SpiteIR::Label* label)
	{
		SpiteIR::Label* currentLabel = label;
		while (currentLabel)
		{
			eastl::vector<SpiteIR::Instruction*>& instructions = currentLabel->values;
			SpiteIR::Instruction* terminator = currentLabel->terminator;
			currentLabel = nullptr;
			for (SpiteIR::Instruction* inst : instructions) InterpretInstruction(*inst, currentLabel);
			InterpretInstruction(*terminator, currentLabel);
		}
	}

	inline void InterpretBlock(SpiteIR::Block* block)
	{
		SpiteIR::Label*& entry = block->labels.front();
		//InterpretAllocations(block->allocations);
		InterpretLabel(entry);
	}

	inline void InterpretAllocations(eastl::vector<SpiteIR::Allocate>& allocInsts)
	{
		size_t amount = 0;
		for (SpiteIR::Allocate& alloc : allocInsts) amount += alloc.type->size;
	}

	inline void MoveParams(eastl::vector<SpiteIR::Operand>* params,
		eastl::vector<SpiteIR::Argument*>& args, char* frame)
	{
		size_t offset = 0;
		for (size_t i = 0; i < params->size(); i++)
		{
			SpiteIR::Operand& param = params->at(i);
			SpiteIR::Argument* arg = args.at(i);
			CopyValue(param.reg, arg->value->type, stackFrameStart + offset, frame);
			offset += param.type->size;
		}
	}

	void* InterpretFunction(SpiteIR::Function* func, size_t start, eastl::vector<SpiteIR::Operand>* params = nullptr)
	{
		char* prevStackStart = stackFrameStart;
		stackFrameStart = stackFrameStart + start;

		if (params) MoveParams(params, func->arguments, prevStackStart);

		InterpretBlock(func->block);
		stackFrameStart = prevStackStart;
		return stackFrameStart;
	}

	inline void CopyValue(size_t src, SpiteIR::Type* type, void* dst, char* frame)
	{
		memcpy(dst, frame + src, type->size);
	}

	inline void CopyRegValue(SpiteIR::Operand& src, SpiteIR::Operand& dst, char* frame)
	{
		void* dstPtr = stackFrameStart + dst.reg;
		memcpy(dstPtr, frame + src.reg, dst.type->size);
	}

	inline void InterpretInstruction(SpiteIR::Instruction& inst, SpiteIR::Label*& label)
	{
		switch (inst.kind)
		{
		case SpiteIR::InstructionKind::Return:
			InterpretReturn(inst);
			break;
		case SpiteIR::InstructionKind::Jump:
			InterpretJump(inst, label);
			break;
		case SpiteIR::InstructionKind::Branch:
			InterpretBranch(inst, label);
			break;
		case SpiteIR::InstructionKind::Switch:
			InterpretSwitch(inst, label);
			break;
		case SpiteIR::InstructionKind::ExternCall:
			InterpretExternCall(inst);
			break;
		case SpiteIR::InstructionKind::Call:
			InterpretCall(inst);
			break;
		case SpiteIR::InstructionKind::CallPtr:
			InterpretCallPtr(inst);
			break;
		case SpiteIR::InstructionKind::Load:
			InterpretLoad(inst);
			break;
		case SpiteIR::InstructionKind::LoadPtrOffset:
			InterpretLoadPtrOffset(inst);
			break;
		case SpiteIR::InstructionKind::LoadGlobal:
			InterpretLoadGlobal(inst);
			break;
		case SpiteIR::InstructionKind::Store:
			InterpretStore(inst);
			break;
		case SpiteIR::InstructionKind::StorePtr:
			InterpretStorePtr(inst);
			break;
		case SpiteIR::InstructionKind::Move:
			InterpretMove(inst);
			break;
		case SpiteIR::InstructionKind::Reference:
			InterpretReference(inst);
			break;
		case SpiteIR::InstructionKind::Dereference:
			InterpretDereference(inst);
			break;
		case SpiteIR::InstructionKind::Cast:
			InterpretCast(inst);
			break;
		case SpiteIR::InstructionKind::BinOp:
			InterpretBinaryOp(inst);
			break;
		case SpiteIR::InstructionKind::UnOp:
			InterpretUnaryOp(inst);
			break;
		case SpiteIR::InstructionKind::Assert:
			InterpretAssert(inst);
			break;
		case SpiteIR::InstructionKind::Log:
			InterpretLog(inst);
			break;
		default:
			break;
		}
	}

	inline void InterpretReturn(SpiteIR::Instruction& inst)
	{
		switch (inst.return_.operand.kind)
		{
		case SpiteIR::OperandKind::Register:
			CopyValue(inst.return_.operand.reg, inst.return_.operand.type, stackFrameStart,
				stackFrameStart);
			break;
		case SpiteIR::OperandKind::Literal:
			break;
		case SpiteIR::OperandKind::StructLiteral:
			break;
		default:
			break;
		}
	}

	inline void InterpretJump(SpiteIR::Instruction& jumpInst, SpiteIR::Label*& label)
	{
		label = jumpInst.jump.label;
	}

	inline void InterpretBranch(SpiteIR::Instruction& branchInst, SpiteIR::Label*& label)
	{
		if (*(bool*)(stackFrameStart + branchInst.branch.test.reg))
			label = branchInst.branch.true_;
		else
			label = branchInst.branch.false_;
	}

	inline void InterpretSwitch(SpiteIR::Instruction& switchInst, SpiteIR::Label*& label)
	{
		intmax_t switchValue = *(intmax_t*)(void*)(stackFrameStart + switchInst.switch_.test.reg);
		if (MapHas(*switchInst.switch_.cases, switchValue))
			label = switchInst.switch_.cases->at(switchValue);
		else
			label = switchInst.switch_.defaultCase;
	}

	inline void InterpretLoad(SpiteIR::Instruction& loadInst)
	{
		intmax_t offset;
		if (loadInst.load.offset.kind == SpiteIR::OperandKind::Literal)
			offset = loadInst.load.offset.literal.intLiteral * loadInst.load.indexType->size;
		else
			offset = *(intmax_t*)(void*)(stackFrameStart + loadInst.load.offset.reg) * loadInst.load.indexType->size;

		char* start = (char*)(void*)(stackFrameStart + loadInst.load.src.reg);
		char* indexed = start + offset;
		*(size_t*)(void*)(stackFrameStart + loadInst.load.dst.reg) = (size_t)indexed;
	}

	inline void InterpretLoadPtrOffset(SpiteIR::Instruction& loadInst)
	{
		intmax_t offset;
		if (loadInst.load.offset.kind == SpiteIR::OperandKind::Literal)
			offset = loadInst.load.offset.literal.intLiteral * loadInst.load.indexType->size;
		else
			offset = *(intmax_t*)(void*)(stackFrameStart + loadInst.load.offset.reg) * loadInst.load.indexType->size;

		char* start = (char*)*(size_t*)(void*)(stackFrameStart + loadInst.load.src.reg);
		char* indexed = start + offset;
		*(size_t*)(void*)(stackFrameStart + loadInst.load.dst.reg) = (size_t)indexed;
	}

	inline void InterpretLoadGlobal(SpiteIR::Instruction& loadGlobalInst)
	{
		size_t globalPtr = (size_t)(void*)(global + loadGlobalInst.loadGlobal.src);
		*(size_t*)(void*)(stackFrameStart + loadGlobalInst.loadGlobal.dst.reg) = globalPtr;
	}

	inline void InterpretStore(SpiteIR::Instruction& storeInst)
	{
		SpiteIR::Operand& src = storeInst.store.src;
		StoreOperand(src, stackFrameStart + storeInst.store.dst.reg);
	}

	inline void StoreOperand(SpiteIR::Operand& src, void* dst)
	{
		switch (src.kind)
		{
		case SpiteIR::OperandKind::Register:
		{
			CopyValue(src.reg, src.type, dst, stackFrameStart);
			break;
		}
		case SpiteIR::OperandKind::Literal:
		{
			switch (src.literal.kind)
			{
			case SpiteIR::PrimitiveKind::Bool:
			case SpiteIR::PrimitiveKind::Byte:
				*(char*)dst = src.literal.byteLiteral;
				break;
			case SpiteIR::PrimitiveKind::I16:
				*(int16_t*)dst = src.literal.i16Literal;
				break;
			case SpiteIR::PrimitiveKind::I32:
				*(int32_t*)dst = src.literal.i32Literal;
				break;
			case SpiteIR::PrimitiveKind::I64:
				*(int64_t*)dst = src.literal.i64Literal;
				break;
			case SpiteIR::PrimitiveKind::Int:
				*(intmax_t*)dst = src.literal.intLiteral;
				break;
			case SpiteIR::PrimitiveKind::F32:
				*(float*)dst = src.literal.f32Literal;
				break;
			case SpiteIR::PrimitiveKind::Float:
				*(double*)dst = src.literal.floatLiteral;
				break;
			case SpiteIR::PrimitiveKind::String:
			{
				size_t* sizeDst = (size_t*)dst;
				*sizeDst = src.literal.stringLiteral->size();
				char** strDst = (char**)(sizeDst + 1);
				*strDst = (char*)src.literal.stringLiteral->c_str();
			}
			break;
			default:
				break;
			}
			break;
		}
		case SpiteIR::OperandKind::StructLiteral:
		{
			size_t offset = 0;
			for (SpiteIR::Operand& op : *src.structLiteral)
			{
				StoreOperand(op, (char*)dst + offset);
				offset += op.type->size;
			}
			break;
		}
		case SpiteIR::OperandKind::Function:
		{
			*(SpiteIR::Function**)dst = src.function;
			break;
		}
		case SpiteIR::OperandKind::TypeData:
		{
			*(SpiteIR::Type**)dst = src.type;
			break;
		}
		default:
			break;
		}
	}

	inline void InterpretStorePtr(SpiteIR::Instruction& storeInst)
	{
		char* ptr = (char*)*(size_t*)(void*)(stackFrameStart + storeInst.store.dst.reg);
		char* src = stackFrameStart + storeInst.store.src.reg;
		memcpy(ptr, src, storeInst.store.src.type->size);
	}

	inline void InterpretMove(SpiteIR::Instruction& storeInst)
	{
		void* dst = *(size_t**)(void*)(stackFrameStart + storeInst.store.dst.reg);
		void* src = *(size_t**)(void*)(stackFrameStart + storeInst.store.src.reg);
		memcpy(dst, src, storeInst.store.dst.type->size);
	}

	inline void InterpretReference(SpiteIR::Instruction& storeInst)
	{
		void* ref = (stackFrameStart + storeInst.store.src.reg);
		*(size_t*)(stackFrameStart + storeInst.store.dst.reg) = (size_t)ref;
	}

	inline void InterpretDereference(SpiteIR::Instruction& storeInst)
	{
		char* ptr = (char*)*(size_t*)(void*)(stackFrameStart + storeInst.store.src.reg);
		char* dst = stackFrameStart + storeInst.store.dst.reg;
		memcpy(dst, ptr, storeInst.store.dst.type->size);
	}

	inline void InterpretCast(SpiteIR::Instruction& castInst)
	{
		switch (castInst.cast.from.type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
		{
			if (castInst.cast.to.type->kind == SpiteIR::TypeKind::PrimitiveType)
			{
				switch (castInst.cast.from.type->primitive.kind)
				{
				case SpiteIR::PrimitiveKind::Bool:
				case SpiteIR::PrimitiveKind::Byte:
				case SpiteIR::PrimitiveKind::I16:
				case SpiteIR::PrimitiveKind::I32:
				case SpiteIR::PrimitiveKind::I64:
				case SpiteIR::PrimitiveKind::Int:
				{
					if (castInst.cast.from.type->primitive.isSigned)
					{
						switch (castInst.cast.from.type->size)
						{
						case 1:
							CastPrimitive<char>(castInst.cast.from, castInst.cast.to);
							break;
						case 2:
							CastPrimitive<int16_t>(castInst.cast.from, castInst.cast.to);
							break;
						case 4:
							CastPrimitive<int32_t>(castInst.cast.from, castInst.cast.to);
							break;
						case 8:
							CastPrimitive<int64_t>(castInst.cast.from, castInst.cast.to);
							break;
						case 16:
							CastPrimitive<intmax_t>(castInst.cast.from, castInst.cast.to);
							break;
						default:
							break;
						}
					}
					else
					{
						switch (castInst.cast.from.type->size)
						{
						case 1:
							CastPrimitive<uint8_t>(castInst.cast.from, castInst.cast.to);
							break;
						case 2:
							CastPrimitive<uint16_t>(castInst.cast.from, castInst.cast.to);
							break;
						case 4:
							CastPrimitive<uint32_t>(castInst.cast.from, castInst.cast.to);
							break;
						case 8:
							CastPrimitive<uint64_t>(castInst.cast.from, castInst.cast.to);
							break;
						case 16:
							CastPrimitive<uintmax_t>(castInst.cast.from, castInst.cast.to);
							break;
						default:
							break;
						}
					}
					break;
				}
				case SpiteIR::PrimitiveKind::F32:
				case SpiteIR::PrimitiveKind::Float:
				{
					switch (castInst.cast.from.type->size)
					{
					case 4:
						CastPrimitive<float>(castInst.cast.from, castInst.cast.to);
						break;
					case 8:
						CastPrimitive<double>(castInst.cast.from, castInst.cast.to);
						break;
					default:
						break;
					}
				}
				default:
					break;
				}
			}
			else if (castInst.cast.to.type->kind == SpiteIR::TypeKind::PointerType)
			{
				CopyRegValue(castInst.cast.from, castInst.cast.to, stackFrameStart);
			}
			return;
		}
		case SpiteIR::TypeKind::ReferenceType:
		case SpiteIR::TypeKind::PointerType:
		{
			CopyRegValue(castInst.cast.from, castInst.cast.to, stackFrameStart);
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
	}

	template<typename Left = int, typename Right = int>
	inline void TypeCast(SpiteIR::Operand& from, SpiteIR::Operand& to)
	{
		Left left = *(Left*)(void*)(stackFrameStart + from.reg);
		Right* right = (Right*)(void*)(stackFrameStart + to.reg);
		*right = (Right)left;
	}

	template<typename Left = int>
	inline void CastPrimitive(SpiteIR::Operand& from, SpiteIR::Operand& to)
	{
		Assert(from.kind == SpiteIR::OperandKind::Register);
		Assert(to.kind == SpiteIR::OperandKind::Register);

		switch (to.type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Bool:
		case SpiteIR::PrimitiveKind::Byte:
		case SpiteIR::PrimitiveKind::I16:
		case SpiteIR::PrimitiveKind::I32:
		case SpiteIR::PrimitiveKind::I64:
		case SpiteIR::PrimitiveKind::Int:
		{
			if (to.type->primitive.isSigned)
			{
				switch (to.type->size)
				{
				case 1:
					TypeCast<Left, char>(from, to);
					break;
				case 2:
					TypeCast<Left, int16_t>(from, to);
					break;
				case 4:
					TypeCast<Left, int32_t>(from, to);
					break;
				case 8:
					TypeCast<Left, int64_t>(from, to);
					break;
				case 16:
					TypeCast<Left, intmax_t>(from, to);
					break;
				default:
					break;
				}
			}
			else
			{
				switch (to.type->size)
				{
				case 1:
					TypeCast<Left, uint8_t>(from, to);
					break;
				case 2:
					TypeCast<Left, uint16_t>(from, to);
					break;
				case 4:
					TypeCast<Left, uint32_t>(from, to);
					break;
				case 8:
					TypeCast<Left, uint64_t>(from, to);
					break;
				case 16:
					TypeCast<Left, uintmax_t>(from, to);
					break;
				default:
					break;
				}
			}
			break;
		}
		case SpiteIR::PrimitiveKind::F32:
		case SpiteIR::PrimitiveKind::Float:
		{
			switch (to.type->size)
			{
			case 4:
				TypeCast<Left, float>(from, to);
				break;
			case 8:
				TypeCast<Left, double>(from, to);
				break;
			default:
				break;
			}
		}
		default:
			break;
		}
	}

	inline void InterpretExternCall(SpiteIR::Instruction& callInst)
	{
		InterpretExternFunction(callInst.call.function, callInst.call.result, callInst.call.params);
	}

	inline void InterpretExternFunction(SpiteIR::Function* func, size_t dst,
		eastl::vector<SpiteIR::Operand>* params)
	{
		eastl::vector<void*> paramPtrs;
		for (SpiteIR::Operand& param : *params)
		{
			Assert(param.kind == SpiteIR::OperandKind::Register);
			paramPtrs.push_back((void*)(stackFrameStart + param.reg));
		}

		CallExternalFunction(func, paramPtrs, stackFrameStart + dst);
	}

	inline void InterpretCall(SpiteIR::Instruction& callInst)
	{
		InterpretFunction(callInst.call.function, callInst.call.result, callInst.call.params);
	}

	inline void InterpretCallPtr(SpiteIR::Instruction& callPtrInst)
	{
		size_t reg = callPtrInst.callPtr.funcPtr.reg;
		SpiteIR::Function* func = *(SpiteIR::Function**)(void*)(stackFrameStart + reg);
		if (func->metadata.externFunc)
			InterpretExternFunction(func, callPtrInst.callPtr.result, callPtrInst.callPtr.params);
		else InterpretFunction(func, callPtrInst.callPtr.result, callPtrInst.callPtr.params);
	}

#define binaryBoolOpTypeMacro(left, right, result, op, lType, rType)		\
{																			\
	*(bool*)(void*)(stackFrameStart + result) =								\
	*(lType*)(void*)(stackFrameStart + left.reg) op							\
	*(rType*)(void*)(stackFrameStart + right.reg);							\
}															

#define binaryOpTypeMacro(left, right, result, op, lType, rType)			\
{																			\
	*(lType*)(void*)(stackFrameStart + result) =							\
	*(lType*)(void*)(stackFrameStart + left.reg) op							\
	*(rType*)(void*)(stackFrameStart + right.reg);							\
}															

#define binaryOpMacroI(left, right, result, op, assignMacro)				\
	if (left.type->primitive.kind <= SpiteIR::PrimitiveKind::Int)			\
	{																		\
		if (left.type->primitive.isSigned)									\
		{																	\
			switch (left.type->size)										\
			{																\
			case 1:															\
				assignMacro(left, right, result, op, char, char);			\
				break;														\
			case 2:															\
				assignMacro(left, right, result, op, int16_t, int16_t);		\
				break;														\
			case 4:															\
				assignMacro(left, right, result, op, int32_t, int32_t);		\
				break;														\
			case 8:															\
				assignMacro(left, right, result, op, int64_t, int64_t);		\
				break;														\
			case 16:														\
				assignMacro(left, right, result, op, intmax_t, intmax_t);	\
				break;														\
			default:														\
				break;														\
			}																\
		}																	\
		else																\
		{																	\
			switch (left.type->size)										\
			{																\
			case 1:															\
				assignMacro(left, right, result, op, uint8_t, uint8_t);		\
				break;														\
			case 2:															\
				assignMacro(left, right, result, op, uint16_t, uint16_t);	\
				break;														\
			case 4:															\
				assignMacro(left, right, result, op, uint32_t, uint32_t);	\
				break;														\
			case 8:															\
				assignMacro(left, right, result, op, uint64_t, uint64_t);	\
				break;														\
			case 16:														\
				assignMacro(left, right, result, op, uintmax_t, uintmax_t);	\
				break;														\
			default:														\
				break;														\
			}																\
		}																	\
	}																		\

#define binaryOpMacroFP(left, right, result, op, assignMacro)				\
	else if (left.type->primitive.kind <= SpiteIR::PrimitiveKind::Float)	\
	{																		\
		switch (left.type->size)											\
		{																	\
		case 4:																\
			assignMacro(left, right, result, op, float, float);				\
			break;															\
		case 8:																\
			assignMacro(left, right, result, op, double, double);			\
			break;															\
		default:															\
			break;															\
		}																	\
	}																		\

	inline void InterpretBinaryOp(SpiteIR::Instruction& binOpInst)
	{
		SpiteIR::Operand& left = binOpInst.binOp.left;
		SpiteIR::Operand& right = binOpInst.binOp.right;
		size_t result = binOpInst.binOp.result;

		switch (binOpInst.binOp.kind)
		{
		case SpiteIR::BinaryOpKind::Add:
			binaryOpMacroI(left, right, result, +, binaryOpTypeMacro)
				binaryOpMacroFP(left, right, result, +, binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Subtract:
			binaryOpMacroI(left, right, result, -, binaryOpTypeMacro)
				binaryOpMacroFP(left, right, result, -, binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Multiply:
			binaryOpMacroI(left, right, result, *, binaryOpTypeMacro)
				binaryOpMacroFP(left, right, result, *, binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Divide:
			binaryOpMacroI(left, right, result, / , binaryOpTypeMacro)
				binaryOpMacroFP(left, right, result, / , binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Modulo:
			binaryOpMacroI(left, right, result, %, binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::And:
			binaryOpMacroI(left, right, result, &, binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Or:
			binaryOpMacroI(left, right, result, | , binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Xor:
			binaryOpMacroI(left, right, result, ^, binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::ShiftLeft:
			binaryOpMacroI(left, right, result, << , binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::ShiftRight:
			binaryOpMacroI(left, right, result, >> , binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::AndNot:
			binaryOpMacroI(left, right, result, &~, binaryOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Equal:
			binaryOpMacroI(left, right, result, == , binaryBoolOpTypeMacro)
				binaryOpMacroFP(left, right, result, == , binaryBoolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::NotEql:
			binaryOpMacroI(left, right, result, != , binaryBoolOpTypeMacro)
				binaryOpMacroFP(left, right, result, != , binaryBoolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Less:
			binaryOpMacroI(left, right, result, < , binaryBoolOpTypeMacro)
				binaryOpMacroFP(left, right, result, < , binaryBoolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Greater:
			binaryOpMacroI(left, right, result, > , binaryBoolOpTypeMacro)
				binaryOpMacroFP(left, right, result, > , binaryBoolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::LessEqual:
			binaryOpMacroI(left, right, result, <= , binaryBoolOpTypeMacro)
				binaryOpMacroFP(left, right, result, <= , binaryBoolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::GreaterEqual:
			binaryOpMacroI(left, right, result, >= , binaryBoolOpTypeMacro)
				binaryOpMacroFP(left, right, result, >= , binaryBoolOpTypeMacro)
				break;
		default:
			Logger::FatalError("Interpreter:InterpretBinaryOp Invalid operation");
			break;
		}
	}

#define unaryBoolOpTypeMacro(left, result, op, lType)						\
{																			\
	*(bool*)(void*)(stackFrameStart + result) =								\
	op *(lType*)(void*)(stackFrameStart + left.reg); 						\
}															

#define unaryOpTypeMacro(left, result, op, lType)							\
{																			\
	*(lType*)(void*)(stackFrameStart + result) =							\
	op *(lType*)(void*)(stackFrameStart + left.reg); 						\
}		

#define unaryOpMacroI(left, result, op, assignMacro)						\
	if (left.type->primitive.kind <= SpiteIR::PrimitiveKind::Int)			\
	{																		\
		if (left.type->primitive.isSigned)									\
		{																	\
			switch (left.type->size)										\
			{																\
			case 1:															\
				assignMacro(left, result, op, char);						\
				break;														\
			case 2:															\
				assignMacro(left, result, op, int16_t);						\
				break;														\
			case 4:															\
				assignMacro(left, result, op, int32_t);						\
				break;														\
			case 8:															\
				assignMacro(left, result, op, int64_t);						\
				break;														\
			case 16:														\
				assignMacro(left, result, op, intmax_t);					\
				break;														\
			default:														\
				break;														\
			}																\
		}																	\
		else																\
		{																	\
			switch (left.type->size)										\
			{																\
			case 1:															\
				assignMacro(left, result, op, uint8_t);						\
				break;														\
			case 2:															\
				assignMacro(left, result, op, uint16_t);					\
				break;														\
			case 4:															\
				assignMacro(left, result, op, uint32_t);					\
				break;														\
			case 8:															\
				assignMacro(left, result, op, uint64_t);					\
				break;														\
			case 16:														\
				assignMacro(left, result, op, uintmax_t);					\
				break;														\
			default:														\
				break;														\
			}																\
		}																	\
	}																		\

#define unaryOpMacroFP(left, result, op, assignMacro)						\
	else if (left.type->primitive.kind <= SpiteIR::PrimitiveKind::Float)	\
	{																		\
		switch (left.type->size)											\
		{																	\
		case 4:																\
			assignMacro(left, result, op, float);							\
			break;															\
		case 8:																\
			assignMacro(left, result, op, double);							\
			break;															\
		default:															\
			break;															\
		}																	\
	}		

	inline void InterpretUnaryOp(SpiteIR::Instruction& unOpInst)
	{
		SpiteIR::Operand& left = unOpInst.unOp.operand;
		size_t result = unOpInst.unOp.result;

		switch (unOpInst.unOp.kind)
		{
		case SpiteIR::UnaryOpKind::Subtract:
			unaryOpMacroI(left, result, -, unaryOpTypeMacro)
				unaryOpMacroFP(left, result, -, unaryOpTypeMacro)
				break;
		case SpiteIR::UnaryOpKind::Not:
			unaryOpMacroI(left, result, !, unaryBoolOpTypeMacro)
				unaryOpMacroFP(left, result, !, unaryOpTypeMacro)
				break;
		case SpiteIR::UnaryOpKind::XOr:
			unaryOpMacroI(left, result, ~, unaryOpTypeMacro)
				break;
		default:
			break;
		}
	}

	inline void InterpretAssert(SpiteIR::Instruction& assertInst)
	{
		bool test = *(bool*)(void*)(stackFrameStart + assertInst.assert.test.reg);
		if (!test)
		{
			eastl::string posString = assertInst.metadata->expressionPosition.ToString();
			if (assertInst.assert.message.kind == SpiteIR::OperandKind::Register)
			{
				void* stackValue = (stackFrameStart + assertInst.assert.message.reg);
				eastl::string errMsg = LogValue(stackValue, assertInst.assert.message.type) + " : " +
					posString;
				Logger::FatalAssert(errMsg);
			}

			Logger::FatalAssert(posString);
		}
	}

	inline void InterpretLog(SpiteIR::Instruction& logInst)
	{
		eastl::string out = "";
		for (SpiteIR::Operand& operand : *logInst.log.operands)
		{
			void* stackValue = (stackFrameStart + operand.reg);
			out += LogValue(stackValue, operand.type);
		}
		Logger::Log(out);
	}

	inline eastl::string LogValue(void* start, SpiteIR::Type* type,
		eastl::hash_set<size_t> logged = eastl::hash_set<size_t>())
	{
		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
		{
			switch (type->primitive.kind)
			{
			case SpiteIR::PrimitiveKind::Void:
				return "void";
			case SpiteIR::PrimitiveKind::Bool:
				if (*(bool*)start) return "true";
				else return "false";
			case SpiteIR::PrimitiveKind::Byte:
			case SpiteIR::PrimitiveKind::I16:
			case SpiteIR::PrimitiveKind::I32:
			case SpiteIR::PrimitiveKind::I64:
			case SpiteIR::PrimitiveKind::Int:
			{
				if (type->primitive.isSigned)
				{
					switch (type->size)
					{
					case 1:
					{
						char* b = (char*)start;
						return eastl::to_string(*b) + " ('" + eastl::string((char*)start, 1) + "')";
					}
					case 2:
						return eastl::to_string(*(int16_t*)start);
					case 4:
						return eastl::to_string(*(int32_t*)start);
					case 8:
						return eastl::to_string(*(int64_t*)start);
					case 16:
						return eastl::to_string(*(intmax_t*)start);
					default:
						break;
					}
				}
				else
				{
					switch (type->size)
					{
					case 1:
						return eastl::to_string(*(uint8_t*)start);
					case 2:
						return eastl::to_string(*(uint16_t*)start);
					case 4:
						return eastl::to_string(*(uint32_t*)start);
					case 8:
						return eastl::to_string(*(uint64_t*)start);
					case 16:
						return eastl::to_string(*(uintmax_t*)start);
					default:
						break;
					}
				}
				break;
			}
			case SpiteIR::PrimitiveKind::F32:
			case SpiteIR::PrimitiveKind::Float:
			{
				switch (type->size)
				{
				case 4:
					return eastl::to_string(*(float*)start);
				case 8:
					return eastl::to_string(*(double*)start);
				default:
					break;
				}
			}
			case SpiteIR::PrimitiveKind::String:
			{
				size_t count = *(size_t*)start;
				char* str = *(char**)((size_t*)start + 1);
				return "\"" + eastl::string(str, count) + "\"";
			}
			default:
				break;
			}
			break;
		}
		case SpiteIR::TypeKind::StateType:
		{
			eastl::string out = type->stateType.state->name + " {";
			for (SpiteIR::Member& member : type->stateType.state->members)
			{
				out += " " + member.value->name + ": ";
				void* memberStart = ((char*)start) + member.offset;
				out += LogValue(memberStart, member.value->type, logged);
				out += ",";
			}
			out.back() = ' ';
			out += "}";
			return out;
		}
		case SpiteIR::TypeKind::UnionType:
		{
			eastl::string out = "union: ";

			SpiteIR::Type byteType = SpiteIR::Type();
			byteType.kind = SpiteIR::TypeKind::PrimitiveType;
			byteType.primitive.kind = SpiteIR::PrimitiveKind::Byte;
			byteType.size = 1;

			SpiteIR::Type unionAsArr = SpiteIR::Type();
			unionAsArr.kind = SpiteIR::TypeKind::FixedArrayType;
			unionAsArr.fixedArray.count = type->size;
			unionAsArr.fixedArray.type = &byteType;

			out += LogValue(start, &unionAsArr, logged);
			return out;
		}
		case SpiteIR::TypeKind::StructureType:
		{
			eastl::string out = "{";
			for (SpiteIR::Member& inner : *type->structureType.members)
			{
				void* memberStart = ((char*)start) + inner.offset;
				out += " " + LogValue(memberStart, inner.value->type, logged);
				out += ",";
			}
			out.back() = ' ';
			out += "}";
			return out;
		}
		case SpiteIR::TypeKind::PointerType:
		{
			void* ptr = (void*)*(size_t*)start;
			size_t ptrVal = (size_t)ptr;
			if (ptrVal && MapHas(logged, ptrVal)) return "{ recursive type }";
			logged.insert(ptrVal);
			eastl::string out = "Ptr @" + eastl::to_string(ptrVal) + " ";
			if (ptrVal) return out + LogValue(ptr, type->pointer.type, logged);
			else return out + "null";
		}
		case SpiteIR::TypeKind::ReferenceType:
		{
			void* ptr = (void*)*(size_t*)start;
			size_t ptrVal = (size_t)ptr;
			if (ptrVal && MapHas(logged, ptrVal)) return "{ recursive type }";
			logged.insert(ptrVal);
			eastl::string out = "Ref @" + eastl::to_string(ptrVal) + " ";
			if (ptrVal) return out + LogValue(ptr, type->reference.type, logged);
			else return out + "nullref (error)";
		}
		case SpiteIR::TypeKind::DynamicArrayType:
		{
			SpiteIR::Type* itemType = type->dynamicArray.type;
			size_t itemSize = itemType->size;
			size_t count = *(size_t*)start;
			if (count == 0) return "[]";

			size_t capacity = *((size_t*)start + 2);
			void* data = (void*)*((size_t*)start + 1);

			eastl::string out = "[";
			for (size_t i = 0; i < count; i++)
			{
				size_t offset = i * itemSize;
				void* itemStart = ((char*)data) + offset;
				out += " " + LogValue(itemStart, itemType, logged) + ",";
			}
			out.back() = ' ';
			out += "]";
			return out;
		}
		case SpiteIR::TypeKind::FixedArrayType:
		{
			SpiteIR::Type* itemType = type->fixedArray.type;
			size_t itemSize = itemType->size;
			size_t count = type->fixedArray.count;

			eastl::string out = "fixed [";
			for (size_t i = 0; i < count; i++)
			{
				size_t offset = i * itemSize;
				void* itemStart = ((char*)start) + offset;
				out += " " + LogValue(itemStart, itemType, logged) + ",";
			}
			out.back() = ' ';
			out += "]";
			return out;
		}
		case SpiteIR::TypeKind::FunctionType:
		{
			void* ptr = (void*)*(size_t*)start;
			size_t ptrVal = (size_t)ptr;
			return "func (" + eastl::to_string(ptrVal) + ")";
		}
		default:
			break;
		}

		return "";
	}
};