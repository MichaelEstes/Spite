#pragma once
#include "EASTL/deque.h"
#include "../IR.h"

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

	void* InterpretBlock(SpiteIR::Block* block)
	{
		void* last = IncrementStackFrame();
		for (SpiteIR::Instruction& inst : block->values)
		{
			last = InterpretInstruction(inst);
		}
		return last;
	}

	void* IncrementStackPointer(size_t amount)
	{
		stackTop += amount;
		return stackTop;
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
			break;
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
		case SpiteIR::InstructionKind::SimpleOp:
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
				char** strDst = (char**)sizeDst + 1;
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
};