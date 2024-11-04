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
		for (SpiteIR::Instruction* inst : block->allocations) InterpretAllocate(*inst);
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
		case SpiteIR::InstructionKind::ExternCall:
			InterpretExternCall(inst);
			break;
		case SpiteIR::InstructionKind::Call:
			InterpretCall(inst);
			break;
		case SpiteIR::InstructionKind::Allocate:
			InterpretAllocate(inst);
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
		label = jumpInst.jump.label;
	}

	void InterpretBranch(SpiteIR::Instruction& branchInst, SpiteIR::Label*& label)
	{
		if (*(bool*)(stackFrameStart + branchInst.branch.test.reg))
			label = branchInst.branch.true_;
		else
			label = branchInst.branch.false_;
	}

	void InterpretAllocate(SpiteIR::Instruction& allocateInst)
	{
		IncrementStackPointer(allocateInst.allocate.type->size);
	}

	void InterpretLoad(SpiteIR::Instruction& loadInst)
	{
		size_t offset = *(size_t*)(void*)(stackFrameStart + loadInst.load.offset.reg * loadInst.load.dst.type->size);
		CopyValue(loadInst.load.src.reg + offset, loadInst.load.dst.type,
			stackFrameStart + loadInst.load.dst.reg, stackFrameStart);
	}

	void InterpretLoadPtrOffset(SpiteIR::Instruction& loadInst)
	{
		size_t offset = *(size_t*)(void*)(stackFrameStart + loadInst.load.offset.reg);
		char* start = (char*)*(size_t*)(void*)(stackFrameStart + loadInst.load.src.reg);
		char* indexed = start + offset;
		*(size_t*)(void*)(stackFrameStart + loadInst.load.dst.reg) = (size_t)indexed;
	}

	void InterpretStore(SpiteIR::Instruction& storeInst)
	{
		void* dst = stackFrameStart + storeInst.store.dst.reg;
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

	void InterpretStorePtr(SpiteIR::Instruction& storeInst)
	{
		char* ptr = (char*)*(size_t*)(void*)(stackFrameStart + storeInst.store.dst.reg);
		char* src = stackFrameStart + storeInst.store.src.reg;
		for (size_t i = 0; i < storeInst.store.src.type->size; i++)
		{
			ptr[i] = src[i];
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
			else
			{

			}
			return;
		}
		case SpiteIR::TypeKind::StateType:
		case SpiteIR::TypeKind::StructureType:
		case SpiteIR::TypeKind::PointerType:
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
			binaryOpMacroI(left, right, result, &&, boolOpTypeMacro)
				binaryOpMacroFP(left, right, result, &&, boolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::LogicOr:
			binaryOpMacroI(left, right, result, || , boolOpTypeMacro)
				binaryOpMacroFP(left, right, result, || , boolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Equal:
			binaryOpMacroI(left, right, result, == , boolOpTypeMacro)
				binaryOpMacroFP(left, right, result, == , boolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::NotEql:
			binaryOpMacroI(left, right, result, != , boolOpTypeMacro)
				binaryOpMacroFP(left, right, result, != , boolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Less:
			binaryOpMacroI(left, right, result, < , boolOpTypeMacro)
				binaryOpMacroFP(left, right, result, < , boolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::Greater:
			binaryOpMacroI(left, right, result, > , boolOpTypeMacro)
				binaryOpMacroFP(left, right, result, > , boolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::LessEqual:
			binaryOpMacroI(left, right, result, <= , boolOpTypeMacro)
				binaryOpMacroFP(left, right, result, <= , boolOpTypeMacro)
				break;
		case SpiteIR::BinaryOpKind::GreaterEqual:
			binaryOpMacroI(left, right, result, >= , boolOpTypeMacro)
				binaryOpMacroFP(left, right, result, >= , boolOpTypeMacro)
				break;
		default:
			Logger::FatalError("Interpreter:InterpretBinaryOp Invalid operation");
			break;
		}
	}

	void InterpretUnaryOp(SpiteIR::Instruction& unOpInst)
	{

	}

	void InterpretLog(SpiteIR::Instruction& logInst)
	{
		void* stackValue = (stackFrameStart + logInst.log.operand.reg);
		eastl::string out = LogValue(stackValue, logInst.log.operand.type);
		Logger::Log(out);
	}

	void Print(const eastl::string& output, bool newline = true, int depth = 0)
	{
		Logger::Log(output, newline, depth);
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
			return "Ptr (" + eastl::to_string(ptrVal) + ") " + LogValue(ptr, type->pointer.type);
		}
		case SpiteIR::TypeKind::ReferenceType:
		{
			void* ptr = (void*)*(size_t*)start;
			return "Ref " + LogValue(ptr, type->reference.type);
		}
		case SpiteIR::TypeKind::DynamicArrayType:
		{
			SpiteIR::Type* itemType = type->dynamicArray.type;
			size_t itemSize = itemType->size;
			size_t count = *(size_t*)start;
			if (count == 0) return "[]";

			size_t capacity = *((size_t*)start + 2);
			void* data = (char*)*((size_t*)start + 1);

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