#pragma once
#include "EASTL/deque.h"
#include "../IR.h"
#include "../../Utils/Utils.h"

struct Interpreter
{
	char* stack;
	eastl::deque<char*> stackFrameQueue;
	char* stackFrameTop;
	char* stackTop;

	Interpreter(size_t stackSize)
	{
		stack = new char[stackSize];
		stackFrameTop = stack;
		stackTop = stack;
	}

	void* InterpretBlock(SpiteIR::Block* block)
	{
		void* last = IncrementStackFrame();
		for (SpiteIR::Instruction& inst : block->values)
		{
			last = InterpretInstruction(inst);
		}
		return last;
	}

	void* Interpret(SpiteIR::IR* ir)
	{
		SpiteIR::Function* entry = ir->entry;
		InterpretBlock(entry->blocks.front());
		return stack;
	}

	void* IncrementStackFrame()
	{
		stackFrameQueue.push_back(stackTop);
		stackFrameTop = stackTop;
		return stackTop;
	}

	void* DecrementStackFrame()
	{
		char* last = stackFrameQueue.back();
		stackFrameQueue.pop_back();
		stackFrameTop = last;
		stackTop = last;
		return stackTop;
	}

	void IncrementStackPointer(size_t amount)
	{
		stackTop += amount;
	}

	void* InterpretInstruction(SpiteIR::Instruction& inst)
	{
		switch (inst.kind)
		{
		case SpiteIR::InstructionKind::Return:
			break;
		case SpiteIR::InstructionKind::Compare:
			break;
		case SpiteIR::InstructionKind::Jump:
			break;
		case SpiteIR::InstructionKind::Branch:
			break;
		case SpiteIR::InstructionKind::Call:
			return InterpretCall(inst);
		case SpiteIR::InstructionKind::Allocate:
			return InterpretAllocate(inst);
		case SpiteIR::InstructionKind::HeapAllocate:
			break;
		case SpiteIR::InstructionKind::Load:
			break;
		case SpiteIR::InstructionKind::Store:
			return InterpretStore(inst);
		case SpiteIR::InstructionKind::Free:
			break;
		case SpiteIR::InstructionKind::Cast:
			break;
		case SpiteIR::InstructionKind::Switch:
			break;
		case SpiteIR::InstructionKind::BinOp:
			return InterpretBinaryOp(inst);
		case SpiteIR::InstructionKind::UnOp:
			break;
		default:
			break;
		}

		return nullptr;
	}

	void* InterpretAllocate(SpiteIR::Instruction& allocateInst)
	{
		void* allocated = stackTop;
		IncrementStackPointer(allocateInst.allocate.type->size);
		return allocated;
	}

	void* InterpretStore(SpiteIR::Instruction& storeInst)
	{
		void* dst = stackFrameTop + storeInst.store.dst;
		SpiteIR::Operand& src = storeInst.store.src;

		switch (src.kind)
		{
		case SpiteIR::OperandKind::Register:
			break;
		case SpiteIR::OperandKind::Literal:
			switch (src.literal.kind)
			{
			case SpiteIR::PrimitiveKind::Byte:
				*(char*)dst = src.literal.byteLiteral;
			case SpiteIR::PrimitiveKind::Int:
				*(size_t*)dst = src.literal.intLiteral;
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
		case SpiteIR::OperandKind::StructLiteral:
			break;
		default:
			break;
		}

		return dst;
	}

	void* InterpretCall(SpiteIR::Instruction& callInst)
	{
		auto& call = callInst.call;

		if (call.function)
		{
			return call.function;
		}

		return stackTop;
	}

#define binaryOpTypeMacro(inst, op, castType)				\
{															\
	void* left = stackFrameTop + inst.binOp.left.reg;		\
	void* right = stackFrameTop + inst.binOp.right.reg;		\
	*(castType*)(void*)stackTop =							\
			*(castType*)left op *(castType*)right;			\
	IncrementStackPointer(inst.binOp.left.type->size);		\
}															

#define binaryOpMacro(inst, op)								\
{															\
	switch (inst.binOp.left.type->primitive.kind)			\
	{														\
	case SpiteIR::PrimitiveKind::Int:						\
		if (inst.binOp.left.type->primitive.isSigned)		\
		{													\
			switch (inst.binOp.left.type->size)				\
			{												\
			case 1:											\
			case 8:											\
				binaryOpTypeMacro(inst, op, char);			\
				break;										\
			case 16:										\
				binaryOpTypeMacro(inst, op, int16_t);		\
				break;										\
			case 32:										\
				binaryOpTypeMacro(inst, op, int32_t);		\
				break;										\
			case 64:										\
				binaryOpTypeMacro(inst, op, int64_t);		\
				break;										\
			case 128:										\
				binaryOpTypeMacro(inst, op, intmax_t);		\
				break;										\
			default:										\
				break;										\
			}												\
		}													\
		else												\
		{													\
			switch (inst.binOp.left.type->size)				\
			{												\
			case 1:											\
			case 8:											\
				binaryOpTypeMacro(inst, op, unsigned char);	\
				break;										\
			case 16:										\
				binaryOpTypeMacro(inst, op, uint16_t);		\
				break;										\
			case 32:										\
				binaryOpTypeMacro(inst, op, uint32_t);		\
				break;										\
			case 64:										\
				binaryOpTypeMacro(inst, op, uint64_t);		\
				break;										\
			case 128:										\
				binaryOpTypeMacro(inst, op, uintmax_t);		\
				break;										\
			default:										\
				break;										\
			}												\
		}													\
		break;												\
	case SpiteIR::PrimitiveKind::Float:						\
		switch (inst.binOp.left.type->size)					\
		{													\
		case 32:											\
			binaryOpTypeMacro(inst, op, float);				\
			break;											\
		case 64:											\
			binaryOpTypeMacro(inst, op, double);			\
			break;											\
		default:											\
			break;											\
		}													\
		break;												\
	default:												\
		break;												\
	}														\
}

	void* InterpretBinaryOp(SpiteIR::Instruction& binOpInst)
	{
		Assert(binOpInst.binOp.left.kind == SpiteIR::OperandKind::Register);
		Assert(binOpInst.binOp.right.kind == SpiteIR::OperandKind::Register);

		switch (binOpInst.binOp.kind)
		{
		case SpiteIR::BinaryOpKind::Add:
			binaryOpMacro(binOpInst, +);
			break;
		case SpiteIR::BinaryOpKind::Subtract:
			binaryOpMacro(binOpInst, -);
			break;
		case SpiteIR::BinaryOpKind::Multiply:
			binaryOpMacro(binOpInst, *);
			break;
		case SpiteIR::BinaryOpKind::Divide:
			break;
		case SpiteIR::BinaryOpKind::Modulo:
			break;
		case SpiteIR::BinaryOpKind::And:
			break;
		case SpiteIR::BinaryOpKind::Or:
			break;
		case SpiteIR::BinaryOpKind::Xor:
			break;
		case SpiteIR::BinaryOpKind::ShiftLeft:
			break;
		case SpiteIR::BinaryOpKind::ShiftRight:
			break;
		case SpiteIR::BinaryOpKind::AndNot:
			break;
		case SpiteIR::BinaryOpKind::LogicAnd:
			break;
		case SpiteIR::BinaryOpKind::LogicOr:
			break;
		case SpiteIR::BinaryOpKind::Equal:
			break;
		case SpiteIR::BinaryOpKind::NotEql:
			break;
		case SpiteIR::BinaryOpKind::Less:
			break;
		case SpiteIR::BinaryOpKind::Greater:
			break;
		case SpiteIR::BinaryOpKind::LessEqual:
			break;
		case SpiteIR::BinaryOpKind::GreaterEqual:
			break;
		default:
			break;
		}
	}

	void* InterpretUnaryOp(SpiteIR::Instruction& unOpInst)
	{

	}
};