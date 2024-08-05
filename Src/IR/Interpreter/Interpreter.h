#pragma once
#include "EASTL/deque.h"
#include "../IR.h"
#include "../../Utils/Utils.h"
#include "../../Log/Logger.h"

struct Interpreter
{
	char* stack;
	eastl::deque<char*> stackFrameQueue;
	eastl::hash_map<eastl::string, char*> labelToFrame;
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
		labelToFrame[block->name] = stackTop;
		void* last = IncrementStackFrame();
		for (SpiteIR::Instruction& inst : block->values)
		{
			last = InterpretInstruction(inst);
		}
		return last;
	}

	void* InterpretFunction(SpiteIR::Function* func)
	{
		return InterpretBlock(func->blocks["entry"]);
	}

	void* Interpret(SpiteIR::IR* ir)
	{
		SpiteIR::Function* entry = ir->entry;
		return InterpretFunction(entry);
	}

	char* IncrementStackFrame()
	{
		stackFrameQueue.push_back(stackTop);
		stackFrameTop = stackTop;
		return stackTop;
	}

	char* DecrementStackFrame()
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

#define boolOpTypeMacro(inst, op, castType)					\
{															\
	void* left = stackFrameTop + inst.binOp.left.reg;		\
	void* right = stackFrameTop + inst.binOp.right.reg;		\
	*(char*)(void*)stackTop =								\
			*(castType*)left op *(castType*)right;			\
	IncrementStackPointer(inst.binOp.left.type->size);		\
}															

#define binaryOpTypeMacro(inst, op, castType)				\
{															\
	void* left = stackFrameTop + inst.binOp.left.reg;		\
	void* right = stackFrameTop + inst.binOp.right.reg;		\
	*(castType*)(void*)stackTop =							\
			*(castType*)left op *(castType*)right;			\
	IncrementStackPointer(inst.binOp.left.type->size);		\
}															

#define binaryOpMacroI(inst, op, assignMacro)				\
	if (inst.binOp.left.type->primitive.kind ==				\
					SpiteIR::PrimitiveKind::Int)			\
	{														\
		if (inst.binOp.left.type->primitive.isSigned)		\
		{													\
			switch (inst.binOp.left.type->size)				\
			{												\
			case 1:											\
				assignMacro(inst, op, char);				\
				break;										\
			case 2:											\
				assignMacro(inst, op, int16_t);				\
				break;										\
			case 4:											\
				assignMacro(inst, op, int32_t);				\
				break;										\
			case 8:											\
				assignMacro(inst, op, int64_t);				\
				break;										\
			case 16:										\
				assignMacro(inst, op, intmax_t);			\
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
				assignMacro(inst, op, unsigned char);		\
				break;										\
			case 2:											\
				assignMacro(inst, op, uint16_t);			\
				break;										\
			case 4:											\
				assignMacro(inst, op, uint32_t);			\
				break;										\
			case 8:											\
				assignMacro(inst, op, uint64_t);			\
				break;										\
			case 16:										\
				assignMacro(inst, op, uintmax_t);			\
				break;										\
			default:										\
				break;										\
			}												\
		}													\
	}														\

#define binaryOpMacroFP(inst, op, assignMacro)				\
	else if (inst.binOp.left.type->primitive.kind ==		\
				SpiteIR::PrimitiveKind::Float)				\
	{														\
		switch (inst.binOp.left.type->size)					\
		{													\
		case 4:												\
			assignMacro(inst, op, float);					\
			break;											\
		case 8:												\
			assignMacro(inst, op, double);					\
			break;											\
		default:											\
			break;											\
		}													\
	}														\


	void* InterpretBinaryOp(SpiteIR::Instruction& binOpInst)
	{
		Assert(binOpInst.binOp.left.kind == SpiteIR::OperandKind::Register);
		Assert(binOpInst.binOp.right.kind == SpiteIR::OperandKind::Register);

		switch (binOpInst.binOp.kind)
		{
		case SpiteIR::BinaryOpKind::Add:
			binaryOpMacroI(binOpInst, +, binaryOpTypeMacro)
			binaryOpMacroFP(binOpInst, +, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Subtract:
			binaryOpMacroI(binOpInst, -, binaryOpTypeMacro)
			binaryOpMacroFP(binOpInst, -, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Multiply:
			binaryOpMacroI(binOpInst, *, binaryOpTypeMacro)
			binaryOpMacroFP(binOpInst, *, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Divide:
			binaryOpMacroI(binOpInst, /, binaryOpTypeMacro)
			binaryOpMacroFP(binOpInst, /, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Modulo:
			binaryOpMacroI(binOpInst, %, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::And:
			binaryOpMacroI(binOpInst, &, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Or:
			binaryOpMacroI(binOpInst, |, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Xor:
			binaryOpMacroI(binOpInst, ^, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::ShiftLeft:
			binaryOpMacroI(binOpInst, <<, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::ShiftRight:
			binaryOpMacroI(binOpInst, >>, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::AndNot:
			binaryOpMacroI(binOpInst, &~, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::LogicAnd:
			binaryOpMacroI(binOpInst, &&, boolOpTypeMacro)
			binaryOpMacroFP(binOpInst, &&, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::LogicOr:
			binaryOpMacroI(binOpInst, ||, boolOpTypeMacro)
			binaryOpMacroFP(binOpInst, ||, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Equal:
			binaryOpMacroI(binOpInst, ==, boolOpTypeMacro)
			binaryOpMacroFP(binOpInst, ==, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::NotEql:
			binaryOpMacroI(binOpInst, !=, boolOpTypeMacro)
			binaryOpMacroFP(binOpInst, !=, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Less:
			binaryOpMacroI(binOpInst, <, boolOpTypeMacro)
			binaryOpMacroFP(binOpInst, <, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Greater:
			binaryOpMacroI(binOpInst, >, boolOpTypeMacro)
			binaryOpMacroFP(binOpInst, >, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::LessEqual:
			binaryOpMacroI(binOpInst, <=, boolOpTypeMacro)
			binaryOpMacroFP(binOpInst, <=, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::GreaterEqual:
			binaryOpMacroI(binOpInst, >=, boolOpTypeMacro)
			binaryOpMacroFP(binOpInst, >=, boolOpTypeMacro)
			break;
		default:
			Logger::FatalError("Interpreter:InterpretBinaryOp Invalid operation");
			break;
		}

		return stackTop;
	}

	void* InterpretUnaryOp(SpiteIR::Instruction& unOpInst)
	{

		return stackTop;
	}
};