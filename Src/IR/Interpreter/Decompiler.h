#pragma once

#include "EASTL/hash_set.h"
#include "EASTL/deque.h"
#include "../../Log/Logger.h"
#include "../IR.h"

struct Decompiler
{
	eastl::string output;
	eastl::hash_set<SpiteIR::Function*> seenFunctions;
	eastl::deque<SpiteIR::Function*> functionQueue;
	
	eastl::hash_set<SpiteIR::Label*> seenLabels;

	Decompiler()
	{

	}

	void Print()
	{
		Logger::Info(output);
	}

	void Write(const eastl::string& line)
	{
		output += line;
		output += '\n';
	}

	eastl::string WriteType(SpiteIR::Type* type)
	{
		if (!type) return "void";

		switch (type->kind)
		{
		case SpiteIR::TypeKind::PrimitiveType:
		{
			eastl::string out = type->primitive.isSigned ? "" : "u";
			switch (type->primitive.kind)
			{
			case SpiteIR::PrimitiveKind::Void:
				out += "void";
				break;
			case SpiteIR::PrimitiveKind::Bool:
				out += "bool";
				break;
			case SpiteIR::PrimitiveKind::Byte:
				out += "byte";
				break;
			case SpiteIR::PrimitiveKind::Int:
				out += "int";
				break;
			case SpiteIR::PrimitiveKind::Float:
				out += "float";
				break;
			case SpiteIR::PrimitiveKind::String:
				out += "string";
				return out;
			default:
				break;
			}

			out += eastl::to_string(type->size);
			return out;
		}
		case SpiteIR::TypeKind::StateType:
			return type->stateType.state->name;
		case SpiteIR::TypeKind::StructureType:
		{
			eastl::string out = "{ ";
			for (SpiteIR::Type* member : *type->structureType.types)
			{
				out += WriteType(member);
				out += ",";
			}
			out.back() = ' ';
			out += "}";
			return out;
		}
		case SpiteIR::TypeKind::PointerType:
			return "*" + WriteType(type->pointer.type);
		case SpiteIR::TypeKind::ReferenceType:
			return "ref " + WriteType(type->reference.type);
		case SpiteIR::TypeKind::DynamicArrayType:
		{
			return "[]" + WriteType(type->dynamicArray.type);
		}
		case SpiteIR::TypeKind::FixedArrayType:
		{
			return "[" + eastl::to_string(type->fixedArray.count) + " x " + 
				WriteType(type->fixedArray.type) + "]";
		}
		case SpiteIR::TypeKind::FunctionType:
		default:
			break;
		}

		return "";
	}

	eastl::string WriteLiteral(SpiteIR::Literal& literal)
	{
		switch (literal.kind)
		{
		case SpiteIR::PrimitiveKind::Bool:
		case SpiteIR::PrimitiveKind::Byte:
			return eastl::to_string(literal.byteLiteral);
		case SpiteIR::PrimitiveKind::Int:
			return eastl::to_string(literal.intLiteral);
		case SpiteIR::PrimitiveKind::Float:
			return eastl::to_string(literal.floatLiteral);
		case SpiteIR::PrimitiveKind::String:
			return *literal.stringLiteral;
		default:
			break;
		}

		return "";
	}

	eastl::string WriteOperand(SpiteIR::Operand& operand)
	{
		if (operand.kind == SpiteIR::OperandKind::Void) return "void";

		eastl::string out = WriteType(operand.type) + " ";
		switch (operand.kind)
		{
		case SpiteIR::OperandKind::Register:
			out += "r" + eastl::to_string(operand.reg);
			break;
		case SpiteIR::OperandKind::Literal:
			out += WriteLiteral(operand.literal);
			break;
		case SpiteIR::OperandKind::StructLiteral:
			break;
		default:
			break;
		}

		return out;
	}

	void DecompileLabel(SpiteIR::Label* label)
	{
		if (seenLabels.find(label) == seenLabels.end())
		{
			seenLabels.insert(label);
			Write("\n" + label->name + ":");
			for (SpiteIR::Instruction* inst : label->values)
			{
				DecompileInstruction(*inst);
			}
			if (label->terminator) DecompileInstruction(*label->terminator);
			else Write("NON TERMINATED LABEL");
		}
	}

	void DecompileBlock(SpiteIR::Block* block)
	{
		SpiteIR::Label* entry = block->labels.front();
		for (SpiteIR::Instruction* inst : block->allocations)
		{
			DecompileInstruction(*inst);
		}
		DecompileLabel(entry);
	}


	void DecompileFunction(SpiteIR::Function* func)
	{	
		eastl::string decl = func->name;
		if (func->arguments.size())
		{
			eastl::string args = "(";
			for (SpiteIR::Argument* arg : func->arguments)
			{

				args += WriteType(arg->value->type);
				args += ",";
			}
			args.back() = ')';
			decl += args;
		}
		Write(decl);
		Write("{");
		DecompileBlock(func->block);
		Write("}\n");
	}

	void Decompile(SpiteIR::IR* ir)
	{
		SpiteIR::Function* entry = ir->entry;
		functionQueue.push_back(entry);

		while (functionQueue.size() > 0)
		{
			SpiteIR::Function* func = functionQueue.back();
			functionQueue.pop_back();
			DecompileFunction(func);
		}

		Print();
	}

	void DecompileInstruction(SpiteIR::Instruction& inst)
	{
		switch (inst.kind)
		{
		case SpiteIR::InstructionKind::Return:
			DecompileReturn(inst);
			break;
		case SpiteIR::InstructionKind::Jump:
			DecompileJump(inst);
			break;
		case SpiteIR::InstructionKind::Branch:
			DecompileBranch(inst);
			break;
		case SpiteIR::InstructionKind::ExternCall:
			DecompileExternCall(inst);
			break;
		case SpiteIR::InstructionKind::Call:
			DecompileCall(inst);
			break;
		case SpiteIR::InstructionKind::Allocate:
			DecompileAllocate(inst);
			break;
		case SpiteIR::InstructionKind::Load:
		case SpiteIR::InstructionKind::LoadPtrOffset:
			DecompileLoad(inst);
			break;
		case SpiteIR::InstructionKind::Store:
		case SpiteIR::InstructionKind::StorePtr:
		case SpiteIR::InstructionKind::Reference:
		case SpiteIR::InstructionKind::Dereference:
			DecompileStore(inst);
			break;
		case SpiteIR::InstructionKind::Cast:
			DecompileCast(inst);
			break;
		case SpiteIR::InstructionKind::Switch:
			break;
		case SpiteIR::InstructionKind::BinOp:
			DecompileBinaryOp(inst);
			break;
		case SpiteIR::InstructionKind::UnOp:
			break;
		case SpiteIR::InstructionKind::Log:
			DecompileLog(inst);
			break;
		default:
			break;
		}
	}

	void DecompileReturn(SpiteIR::Instruction& returnInst)
	{
		Write("return " + WriteOperand(returnInst.return_.operand));
	}

	void DecompileJump(SpiteIR::Instruction& jumpInst)
	{
		Write("jump " + jumpInst.jump.label->name);
		DecompileLabel(jumpInst.jump.label);
	}

	void DecompileBranch(SpiteIR::Instruction& branchInst)
	{
		Write("branch " +  WriteOperand(branchInst.branch.test) + " ? " + 
			branchInst.branch.true_->name + " : " + branchInst.branch.false_->name);
		DecompileLabel(branchInst.branch.true_);
		DecompileLabel(branchInst.branch.false_);
	}

	void DecompileAllocate(SpiteIR::Instruction& allocateInst)
	{
		Write("r" + eastl::to_string(allocateInst.allocate.result) + " = " + 
			"allocate " + WriteType(allocateInst.allocate.type));
	}

	void DecompileLoad(SpiteIR::Instruction& loadInst)
	{
		eastl::string loadString = loadInst.kind == SpiteIR::InstructionKind::Load ? "load " : "load* ";
		Write(WriteOperand(loadInst.load.dst) + " = " + loadString + 
			WriteOperand(loadInst.load.src) + " " + WriteOperand(loadInst.load.offset));
	}

	void DecompileStore(SpiteIR::Instruction& storeInst)
	{
		eastl::string storeString = "";
		switch (storeInst.kind)
		{
		case SpiteIR::InstructionKind::Store:
			storeString = " = store ";
			break;
		case SpiteIR::InstructionKind::StorePtr:
			storeString = " = store* ";
			break;
		case SpiteIR::InstructionKind::Reference:
			storeString = " = store@ ";
			break;
		case SpiteIR::InstructionKind::Dereference:
			storeString = " = store~ ";
			break;
		default:
			break;
		}
		Write("r" + eastl::to_string(storeInst.store.dst.reg) + storeString +
			WriteOperand(storeInst.store.src));
	}

	void DecompileCast(SpiteIR::Instruction& castInst)
	{
		Write("cast " + WriteOperand(castInst.cast.from) + " " + WriteOperand(castInst.cast.to));
	}

	void DecompileExternCall(SpiteIR::Instruction& callInst)
	{
		eastl::string callStr = "r" + eastl::to_string(callInst.call.result) + " = extern call " +
			WriteType(callInst.call.function->returnType) + " " + callInst.call.function->name + "(";
		for (SpiteIR::Operand& param : *callInst.call.params)
		{
			callStr += WriteOperand(param);
		}
		callStr += ")";

		Write(callStr);
	}

	void DecompileCall(SpiteIR::Instruction& callInst)
	{
		eastl::string callStr = "r" + eastl::to_string(callInst.call.result) + " = call " + 
			WriteType(callInst.call.function->returnType) + " " + callInst.call.function->name + "(";

		if (callInst.call.params->size())
		{
			for (SpiteIR::Operand& param : *callInst.call.params)
			{
				callStr += WriteOperand(param) + ",";
			}
			callStr.back() = ')';
		} else callStr += ")";

		Write(callStr);

		if (seenFunctions.find(callInst.call.function) == seenFunctions.end())
		{
			seenFunctions.insert(callInst.call.function);
			functionQueue.push_back(callInst.call.function);
		}
	}

	void DecompileBinaryOp(SpiteIR::Instruction& binOpInst)
	{
		eastl::string out = "r" + eastl::to_string(binOpInst.binOp.result) + " = ";
		switch (binOpInst.binOp.kind)
		{
		case SpiteIR::BinaryOpKind::Add:
			out += "add ";
			break;
		case SpiteIR::BinaryOpKind::Subtract:
			out += "sub ";
			break;
		case SpiteIR::BinaryOpKind::Multiply:
			out += "mul ";
			break;
		case SpiteIR::BinaryOpKind::Divide:
			out += "div ";
			break;
		case SpiteIR::BinaryOpKind::Modulo:
			out += "mod ";
			break;
		case SpiteIR::BinaryOpKind::And:
			out += "and ";
			break;
		case SpiteIR::BinaryOpKind::Or:
			out += "or ";
			break;
		case SpiteIR::BinaryOpKind::Xor:
			out += "xor ";
			break;
		case SpiteIR::BinaryOpKind::ShiftLeft:
			out += "shl ";
			break;
		case SpiteIR::BinaryOpKind::ShiftRight:
			out += "shr ";
			break;
		case SpiteIR::BinaryOpKind::AndNot:
			out += "anot ";
			break;
		case SpiteIR::BinaryOpKind::LogicAnd:
			out += "land ";
			break;
		case SpiteIR::BinaryOpKind::LogicOr:
			out += "lor ";
			break;
		case SpiteIR::BinaryOpKind::Equal:
			out += "eq ";
			break;
		case SpiteIR::BinaryOpKind::NotEql:
			out += "neq ";
			break;
		case SpiteIR::BinaryOpKind::Less:
			out += "lt ";
			break;
		case SpiteIR::BinaryOpKind::Greater:
			out += "gt ";
			break;
		case SpiteIR::BinaryOpKind::LessEqual:
			out += "leq ";
			break;
		case SpiteIR::BinaryOpKind::GreaterEqual:
			out += "geq ";
			break;
		default:
			break;
		}

		out += WriteOperand(binOpInst.binOp.left) + " " + WriteOperand(binOpInst.binOp.right);
		Write(out);
	}

	void DecompileUnaryOp(SpiteIR::Instruction& unOpInst)
	{

	}

	void DecompileLog(SpiteIR::Instruction& logInst)
	{
		Write("log " + WriteOperand(logInst.log.operand));
	}
};