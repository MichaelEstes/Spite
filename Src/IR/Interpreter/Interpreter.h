#pragma once
#include "EASTL/deque.h"
#include "../IR.h"
#include "../../Utils/Utils.h"
#include "../../Log/Logger.h"
#include "ExternCall.h"

struct Interpreter
{
	char* stack;
	eastl::deque<char*> stackFrameStartQueue;
	eastl::deque<char*> stackFrameTopQueue;
	char* stackFrameStart;
	char* stackTop;

	Interpreter(size_t stackSize)
	{
		stack = new char[stackSize];
		stackFrameStart = stack;
		stackTop = stack;
		IncrementStackFrame();
		CreateDynCallVM();
	}

	void InterpretLabel(SpiteIR::Label* label)
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

	void InterpretBlock(SpiteIR::Block* block)
	{
		SpiteIR::Label*& entry = block->labels.front();
		InterpretAllocations(block->allocations);
		InterpretLabel(entry);
	}

	inline void MoveParams(eastl::vector<SpiteIR::Operand>* params,
		eastl::vector<SpiteIR::Argument*>& args, char* frame)
	{
		size_t offset = 0;
		for (size_t i = 0; i < params->size(); i++)
		{
			SpiteIR::Operand& param = params->at(i);
			SpiteIR::Argument* arg = args.at(i);
			CopyValue(param.reg, arg->value->type, stackTop + offset, frame);
			offset += param.type->size;
		}
	}

	void* InterpretFunction(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>* params = nullptr)
	{
		char* prevStackStart = stackFrameStart;
		IncrementStackFrame();

		if (params)
		{
			MoveParams(params, func->arguments, prevStackStart);
		}

		InterpretBlock(func->block);
		return DecrementStackFrame();
	}

	void* Interpret(SpiteIR::IR* ir)
	{
		SpiteIR::Function* entry = ir->entry;
		return InterpretFunction(entry);
	}

	inline void IncrementStackFrame()
	{
		stackFrameStartQueue.push_back(stackTop);
		stackFrameTopQueue.push_back(stackTop);
		stackFrameStart = stackTop;
	}

	inline char* DecrementStackFrame()
	{
		stackFrameStartQueue.pop_back();
		stackFrameTopQueue.pop_back();
		stackFrameStart = stackFrameStartQueue.back();
		stackTop = stackFrameTopQueue.back();
		return stackFrameStart;
	}

	inline void IncrementStackPointer(size_t amount)
	{
		stackTop += amount;
		stackFrameTopQueue.back() = stackTop;
	}

	void CopyValue(size_t src, SpiteIR::Type* type, void* dst, char* frame)
	{
		for (size_t i = 0; i < type->size; i++)
		{
			((char*)dst)[i] = (frame + src)[i];
		}
	}

	void CopyRegValue(SpiteIR::Operand& src, SpiteIR::Operand& dst, char* frame)
	{
		void* dstPtr = stackFrameStart + dst.reg;
		for (size_t i = 0; i < dst.type->size; i++)
		{
			((char*)dstPtr)[i] = (frame + src.reg)[i];
		}
	}

	void InterpretInstruction(SpiteIR::Instruction& inst, SpiteIR::Label*& label)
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
		case SpiteIR::InstructionKind::Store:
			InterpretStore(inst);
			break;
		case SpiteIR::InstructionKind::StorePtr:
			InterpretStorePtr(inst);
			break;
		case SpiteIR::InstructionKind::Move:
			InterpretMove(inst);
			break;
		case SpiteIR::InstructionKind::StoreFunc:
			InterpretStoreFunc(inst);
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
		case SpiteIR::InstructionKind::Switch:
			break;
		case SpiteIR::InstructionKind::BinOp:
			InterpretBinaryOp(inst);
			break;
		case SpiteIR::InstructionKind::UnOp:
			InterpretUnaryOp(inst);
			break;
		case SpiteIR::InstructionKind::Log:
			InterpretLog(inst);
			break;
		default:
			break;
		}
	}

	void InterpretReturn(SpiteIR::Instruction& inst)
	{
		switch (inst.return_.operand.kind)
		{
		case SpiteIR::OperandKind::Register:
			CopyValue(inst.return_.operand.reg, inst.return_.operand.type,
				stackFrameStart, stackFrameStart);
			break;
		case SpiteIR::OperandKind::Literal:
			break;
		case SpiteIR::OperandKind::StructLiteral:
			break;
		default:
			break;
		}
	}

	void InterpretJump(SpiteIR::Instruction& jumpInst, SpiteIR::Label*& label)
	{
		label = jumpInst.jump.label;
	}

	void InterpretBranch(SpiteIR::Instruction& branchInst, SpiteIR::Label*& label)
	{
		if (*(bool*)(stackFrameStart + branchInst.branch.test.reg))
			label = branchInst.branch.true_;
		else
			label = branchInst.branch.false_;
	}

	void InterpretAllocations(eastl::vector<SpiteIR::Allocate>& allocInsts)
	{
		size_t amount = 0;
		for (SpiteIR::Allocate& alloc : allocInsts) amount += alloc.type->size;
		IncrementStackPointer(amount);
	}

	void InterpretLoad(SpiteIR::Instruction& loadInst)
	{
		intmax_t offset = *(intmax_t*)(void*)(stackFrameStart + loadInst.load.offset.reg) * loadInst.load.dst.type->size;
		char* start = (char*)*(size_t*)(void*)(stackFrameStart + loadInst.load.src.reg);
		char* indexed = start + offset;
		*(size_t*)(void*)(stackFrameStart + loadInst.load.dst.reg) = (size_t)indexed;
	}

	void InterpretLoadPtrOffset(SpiteIR::Instruction& loadInst)
	{
		intmax_t offset = *(intmax_t*)(void*)(stackFrameStart + loadInst.load.offset.reg);
		char* start = (char*)*(size_t*)(void*)(stackFrameStart + loadInst.load.src.reg);
		char* indexed = start + offset;
		*(size_t*)(void*)(stackFrameStart + loadInst.load.dst.reg) = (size_t)indexed;
	}

	void InterpretStore(SpiteIR::Instruction& storeInst)
	{
		SpiteIR::Operand& src = storeInst.store.src;

		switch (src.kind)
		{
		case SpiteIR::OperandKind::Register:
		{
			CopyRegValue(src, storeInst.store.dst, stackFrameStart);
			break;
		}
		case SpiteIR::OperandKind::Literal:
		{
			void* dst = stackFrameStart + storeInst.store.dst.reg;
			switch (src.literal.kind)
			{
			case SpiteIR::PrimitiveKind::Byte:
				*(char*)dst = src.literal.byteLiteral;
			case SpiteIR::PrimitiveKind::Int:
				*(int64_t*)dst = src.literal.intLiteral;
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
			break;
		default:
			break;
		}
	}

	void InterpretStorePtr(SpiteIR::Instruction& storeInst)
	{
		char* ptr = (char*)*(size_t*)(void*)(stackFrameStart + storeInst.store.dst.reg);
		char* src = stackFrameStart + storeInst.store.src.reg;
		for (size_t i = 0; i < storeInst.store.src.type->size; i++)
		{
			ptr[i] = src[i];
		}
	}

	void InterpretStoreFunc(SpiteIR::Instruction& storeInst)
	{
		Assert(storeInst.store.src.kind == SpiteIR::OperandKind::Function);
		size_t* ptr = (size_t*)(void*)(stackFrameStart + storeInst.store.dst.reg);
		*ptr = (size_t)storeInst.store.src.function;
	}

	void InterpretMove(SpiteIR::Instruction& storeInst)
	{
		char* dst = (char*)*(size_t*)(void*)(stackFrameStart + storeInst.store.dst.reg);
		char* src = (char*)*(size_t*)(void*)(stackFrameStart + storeInst.store.src.reg);
		for (size_t i = 0; i < storeInst.store.dst.type->size; i++)
		{
			dst[i] = src[i];
		}
	}

	void InterpretReference(SpiteIR::Instruction& storeInst)
	{
		void* ref = (stackFrameStart + storeInst.store.src.reg);
		*(size_t*)(stackFrameStart + storeInst.store.dst.reg) = (size_t)ref;
	}

	void InterpretDereference(SpiteIR::Instruction& storeInst)
	{
		char* ptr = (char*)*(size_t*)(void*)(stackFrameStart + storeInst.store.src.reg);
		char* dst = stackFrameStart + storeInst.store.dst.reg;
		for (size_t i = 0; i < storeInst.store.dst.type->size; i++)
		{
			dst[i] = ptr[i];
		}
	}

	void InterpretCast(SpiteIR::Instruction& castInst)
	{
		switch (castInst.cast.from.type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
		{
			if (castInst.cast.to.type->kind == SpiteIR::TypeKind::PrimitiveType)
			{
				switch (castInst.cast.from.type->primitive.kind)
				{
				case SpiteIR::PrimitiveKind::Int:
				case SpiteIR::PrimitiveKind::Bool:
				case SpiteIR::PrimitiveKind::Byte:
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
		case SpiteIR::TypeKind::PointerType:
		{
			CopyRegValue(castInst.cast.from, castInst.cast.to, stackFrameStart);
		}
		case SpiteIR::TypeKind::StateType:
		case SpiteIR::TypeKind::StructureType:
		case SpiteIR::TypeKind::DynamicArrayType:
		case SpiteIR::TypeKind::FixedArrayType:
		case SpiteIR::TypeKind::FunctionType:
			return;
		default:
			break;
		}
	}

	template<typename Left = int, typename Right = int>
	void BitCast(SpiteIR::Operand& from, SpiteIR::Operand& to)
	{
		Left left = *(Left*)(void*)(stackFrameStart + from.reg);
		Right* right = (Right*)(void*)(stackFrameStart + to.reg);
		*right = (Right)left;
	}

	template<typename Left = int>
	void CastPrimitive(SpiteIR::Operand& from, SpiteIR::Operand& to)
	{
		Assert(from.kind == SpiteIR::OperandKind::Register);
		Assert(to.kind == SpiteIR::OperandKind::Register);

		switch (to.type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Int:
		case SpiteIR::PrimitiveKind::Bool:
		case SpiteIR::PrimitiveKind::Byte:
		{
			if (to.type->primitive.isSigned)
			{
				switch (to.type->size)
				{
				case 1:
					BitCast<Left, char>(from, to);
					break;
				case 2:
					BitCast<Left, int16_t>(from, to);
					break;
				case 4:
					BitCast<Left, int32_t>(from, to);
					break;
				case 8:
					BitCast<Left, int64_t>(from, to);
					break;
				case 16:
					BitCast<Left, intmax_t>(from, to);
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
					BitCast<Left, uint8_t>(from, to);
					break;
				case 2:
					BitCast<Left, uint16_t>(from, to);
					break;
				case 4:
					BitCast<Left, uint32_t>(from, to);
					break;
				case 8:
					BitCast<Left, uint64_t>(from, to);
					break;
				case 16:
					BitCast<Left, uintmax_t>(from, to);
					break;
				default:
					break;
				}
			}
			break;
		}
		case SpiteIR::PrimitiveKind::Float:
		{
			switch (to.type->size)
			{
			case 4:
				BitCast<Left, float>(from, to);
				break;
			case 8:
				BitCast<Left, double>(from, to);
				break;
			default:
				break;
			}
		}
		default:
			break;
		}
	}

	void InterpretExternCall(SpiteIR::Instruction& callInst)
	{
		eastl::vector<void*> params;
		for (SpiteIR::Operand& param : *callInst.call.params)
		{
			Assert(param.kind == SpiteIR::OperandKind::Register);
			params.push_back((void*)(stackFrameStart + param.reg));
		}

		CallExternalFunction(callInst.call.function, params, stackFrameStart + callInst.call.result);
	}

	void InterpretCall(SpiteIR::Instruction& callInst)
	{
		InterpretFunction(callInst.call.function, callInst.call.params);
		CopyValue(stackTop - stackFrameStart, callInst.call.function->returnType,
			stackFrameStart + callInst.call.result, stackFrameStart);
	}

	void InterpretCallPtr(SpiteIR::Instruction& callPtrInst)
	{
		size_t reg = callPtrInst.callPtr.funcPtr.reg;
		SpiteIR::Function* func = *(SpiteIR::Function**)(void*)(stackFrameStart + reg);
		InterpretFunction(func, callPtrInst.callPtr.params);
		CopyValue(stackTop - stackFrameStart, callPtrInst.callPtr.funcPtr.type->function.returnType,
			stackFrameStart + callPtrInst.callPtr.result, stackFrameStart);
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
	if (left.type->primitive.kind ==										\
					SpiteIR::PrimitiveKind::Int ||							\
		left.type->primitive.kind ==										\
					SpiteIR::PrimitiveKind::Bool ||							\
		left.type->primitive.kind ==										\
					SpiteIR::PrimitiveKind::Byte)							\
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
	else if (left.type->primitive.kind ==									\
				SpiteIR::PrimitiveKind::Float)								\
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

	void InterpretBinaryOp(SpiteIR::Instruction& binOpInst)
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
		case SpiteIR::BinaryOpKind::LogicAnd:
			binaryOpMacroI(left, right, result, &&, binaryBoolOpTypeMacro)
				binaryOpMacroFP(left, right, result, &&, binaryBoolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::LogicOr:
			binaryOpMacroI(left, right, result, || , binaryBoolOpTypeMacro)
				binaryOpMacroFP(left, right, result, || , binaryBoolOpTypeMacro)
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
	if (left.type->primitive.kind ==										\
					SpiteIR::PrimitiveKind::Int ||							\
		left.type->primitive.kind ==										\
					SpiteIR::PrimitiveKind::Bool ||							\
		left.type->primitive.kind ==										\
					SpiteIR::PrimitiveKind::Byte)							\
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
	else if (left.type->primitive.kind ==									\
				SpiteIR::PrimitiveKind::Float)								\
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

	void InterpretUnaryOp(SpiteIR::Instruction& unOpInst)
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

	void InterpretLog(SpiteIR::Instruction& logInst)
	{
		eastl::string out = "";
		for (SpiteIR::Operand& operand : *logInst.log.operands)
		{
			void* stackValue = (stackFrameStart + operand.reg);
			out += LogValue(stackValue, operand.type);
		}
		Logger::Log(out);
	}

	eastl::string LogValue(void* start, SpiteIR::Type* type)
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
			case SpiteIR::PrimitiveKind::Int:
			{
				if (type->primitive.isSigned)
				{
					switch (type->size)
					{
					case 1:
						return eastl::to_string(*(char*)start);
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
				return eastl::string(str, count);
			}
			default:
				break;
			}
			break;
		}
		case SpiteIR::TypeKind::StateType:
		{
			eastl::string out = type->stateType.state->name + " {";
			for (SpiteIR::Member* member : type->stateType.state->members)
			{
				out += " " + member->value->name + ": ";
				void* memberStart = ((char*)start) + member->offset;
				out += LogValue(memberStart, member->value->type);
				out += ",";
			}
			out.back() = ' ';
			out += "}";
			return out;
		}
		case SpiteIR::TypeKind::StructureType:
			break;
		case SpiteIR::TypeKind::PointerType:
		{
			void* ptr = (void*)*(size_t*)start;
			size_t ptrVal = (size_t)ptr;
			eastl::string out = "Ptr @" + eastl::to_string(ptrVal) + " ";
			if (ptrVal) return out + LogValue(ptr, type->pointer.type);
			else return out + "null";
		}
		case SpiteIR::TypeKind::ReferenceType:
		{
			void* ptr = (void*)*(size_t*)start;
			size_t ptrVal = (size_t)ptr;
			eastl::string out = "Ref @" + eastl::to_string(ptrVal) + " ";
			if (ptrVal) return out + LogValue(ptr, type->pointer.type);
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
				out += " " + LogValue(itemStart, itemType) + ",";
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

			eastl::string out = "[";
			for (size_t i = 0; i < count; i++)
			{
				size_t offset = i * itemSize;
				void* itemStart = ((char*)start) + offset;
				out += " " + LogValue(itemStart, itemType) + ",";
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