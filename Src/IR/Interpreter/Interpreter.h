#pragma once
#include "EASTL/deque.h"
#include "../IR.h"
#include "../../Utils/Utils.h"
#include "../../Log/Logger.h"

struct Interpreter
{
	char* stack;
	eastl::deque<char*> stackFrameStartQueue;
	eastl::deque<char*> stackFrameTopQueue;
	char* stackFrameStart;
	char* stackTop;

	int branchCount = 0;

	Interpreter(size_t stackSize)
	{
		stack = new char[stackSize];
		stackFrameStart = stack;
		stackTop = stack;
		IncrementStackFrame();
	}

	void InterpretLabel(SpiteIR::Label* label)
	{
		SpiteIR::Label* currentLabel = label;
		while (currentLabel)
		{
			eastl::vector<SpiteIR::Instruction*>& instructions = currentLabel->values;
			SpiteIR::Instruction* terminator = currentLabel->terminator;
			currentLabel = nullptr;
			for (SpiteIR::Instruction* inst : instructions)
			{
				InterpretInstruction(*inst, currentLabel);
			}
			InterpretInstruction(*terminator, currentLabel);
		}
	}

	void InterpretBlock(SpiteIR::Block* block)
	{
		SpiteIR::Label*& entry = block->labels.front();
		for (SpiteIR::Instruction* inst : block->allocations)
		{
			Assert(inst->kind == SpiteIR::InstructionKind::Allocate);
			InterpretAllocate(*inst);
		}
		InterpretLabel(entry);
	}

	inline void MoveParams(eastl::vector<SpiteIR::Operand>* params, char* frame)
	{
		size_t offset = 0;
		for (SpiteIR::Operand& param : *params)
		{
			CopyRegValue(param, stackTop + offset, frame);
			offset += param.type->size;
		}
	}

	void* InterpretFunction(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>* params = nullptr)
	{
		char* prevStackStart = stackFrameStart;
		IncrementStackFrame();

		if (params)
		{
			MoveParams(params, prevStackStart);
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

	void CopyRegValue(SpiteIR::Operand& src, void* dst, char* frame)
	{
		for (size_t i = 0; i < src.type->size; i++)
		{
			((char*)dst)[i] = (frame + src.reg)[i];
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
		case SpiteIR::InstructionKind::Call:
			InterpretCall(inst);
			break;
		case SpiteIR::InstructionKind::Allocate:
			InterpretAllocate(inst);
			break;
		case SpiteIR::InstructionKind::HeapAllocate:
			break;
		case SpiteIR::InstructionKind::Load:
			InterpretLoad(inst);
			break;
		case SpiteIR::InstructionKind::Store:
			InterpretStore(inst);
			break;
		case SpiteIR::InstructionKind::Free:
			break;
		case SpiteIR::InstructionKind::Cast:
			break;
		case SpiteIR::InstructionKind::Switch:
			break;
		case SpiteIR::InstructionKind::BinOp:
			InterpretBinaryOp(inst);
			break;
		case SpiteIR::InstructionKind::UnOp:
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
			CopyRegValue(inst.return_.operand, stackFrameStart, stackFrameStart);
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
		Assert(jumpInst.jump.label);
		label = jumpInst.jump.label;
	} 

	void InterpretLoad(SpiteIR::Instruction& loadInst)
	{
		Assert(loadInst.load.offset.kind == SpiteIR::OperandKind::Register);
		Assert(loadInst.load.offset.type->kind == SpiteIR::TypeKind::PrimitiveType);

		int offset = *(int*)(void*)(stackFrameStart + loadInst.load.offset.reg) * loadInst.load.dst.type->size;
		CopyValue(loadInst.load.src.reg + offset, loadInst.load.dst.type,
			stackFrameStart + loadInst.load.dst.reg, stackFrameStart);
	}

	void InterpretBranch(SpiteIR::Instruction& branchInst, SpiteIR::Label*& label)
	{
		Assert(branchInst.branch.true_ && branchInst.branch.false_ &&
				branchInst.branch.test.kind == SpiteIR::OperandKind::Register &&
				branchInst.branch.test.type->kind == SpiteIR::TypeKind::PrimitiveType && 
				branchInst.branch.test.type->primitive.kind == SpiteIR::PrimitiveKind::Bool);

		if(*(bool*)(stackFrameStart + branchInst.branch.test.reg)) 
			label = branchInst.branch.true_;
		else 
			label = branchInst.branch.false_;

		branchCount += 1;
	}

	void InterpretAllocate(SpiteIR::Instruction& allocateInst)
	{
		IncrementStackPointer(allocateInst.allocate.type->size);
	}

	void InterpretStore(SpiteIR::Instruction& storeInst)
	{
		void* dst = stackFrameStart + storeInst.store.dst;
		SpiteIR::Operand& src = storeInst.store.src;

		switch (src.kind)
		{
		case SpiteIR::OperandKind::Register:
		{

			CopyRegValue(src, dst, stackFrameStart);
			break;
		}
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
	}

	void InterpretCall(SpiteIR::Instruction& callInst)
	{
		InterpretFunction(callInst.call.function, callInst.call.params);
		CopyValue(stackTop - stackFrameStart, callInst.call.function->returnType,
			stackFrameStart + callInst.call.result, stackFrameStart);
	}

#define boolOpTypeMacro(left, right, result, op, lType, rType)				\
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
		Assert(binOpInst.binOp.left.kind == SpiteIR::OperandKind::Register);
		Assert(binOpInst.binOp.right.kind == SpiteIR::OperandKind::Register);

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
			binaryOpMacroI(left, right, result, /, binaryOpTypeMacro)
			binaryOpMacroFP(left, right, result, /, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Modulo:
			binaryOpMacroI(left, right, result, %, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::And:
			binaryOpMacroI(left, right, result, &, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Or:
			binaryOpMacroI(left, right, result, |, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Xor:
			binaryOpMacroI(left, right, result, ^, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::ShiftLeft:
			binaryOpMacroI(left, right, result, <<, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::ShiftRight:
			binaryOpMacroI(left, right, result, >>, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::AndNot:
			binaryOpMacroI(left, right, result, &~, binaryOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::LogicAnd:
			binaryOpMacroI(left, right, result, &&, boolOpTypeMacro)
			binaryOpMacroFP(left, right, result, &&, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::LogicOr:
			binaryOpMacroI(left, right, result, ||, boolOpTypeMacro)
			binaryOpMacroFP(left, right, result, ||, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Equal:
			binaryOpMacroI(left, right, result, ==, boolOpTypeMacro)
			binaryOpMacroFP(left, right, result, ==, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::NotEql:
			binaryOpMacroI(left, right, result, !=, boolOpTypeMacro)
			binaryOpMacroFP(left, right, result, !=, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Less:
			binaryOpMacroI(left, right, result, <, boolOpTypeMacro)
			binaryOpMacroFP(left, right, result, <, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::Greater:
			binaryOpMacroI(left, right, result, >, boolOpTypeMacro)
			binaryOpMacroFP(left, right, result, >, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::LessEqual:
			binaryOpMacroI(left, right, result, <=, boolOpTypeMacro)
			binaryOpMacroFP(left, right, result, <=, boolOpTypeMacro)
			break;
		case SpiteIR::BinaryOpKind::GreaterEqual:
			binaryOpMacroI(left, right, result, >=, boolOpTypeMacro)
			binaryOpMacroFP(left, right, result, >=, boolOpTypeMacro)
			break;
		default:
			Logger::FatalError("Interpreter:InterpretBinaryOp Invalid operation");
			break;
		}
	}

	void InterpretUnaryOp(SpiteIR::Instruction& unOpInst)
	{

	}
};