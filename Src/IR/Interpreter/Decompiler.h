#pragma once

#include "EASTL/hash_set.h"
#include "../../Log/Logger.h"
#include "../IR.h"

struct Decompiler
{
	eastl::string output;
	eastl::hash_set<SpiteIR::Function*> seenFunctions;
	eastl::hash_set<SpiteIR::Label*> seenLabels;

	Decompiler()
	{

	}

	void Print()
	{
		Logger::Info(output);
	}

	void Write(const eastl::string line)
	{
		output += line;
		output += '\n';
	}

	eastl::string WriteType(SpiteIR::Type* type)
	{
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
		case SpiteIR::TypeKind::StructureType:
		case SpiteIR::TypeKind::PointerType:
		case SpiteIR::TypeKind::DynamicArrayType:
		case SpiteIR::TypeKind::FixedArrayType:
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
			Write(label->name + ":");
			for (SpiteIR::Instruction* inst : label->values)
			{
				DecompileInstruction(*inst);
			}
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
		if (seenFunctions.find(func) == seenFunctions.end())
		{
			seenFunctions.insert(func);
			Write(func->name);
			Write("{");
			DecompileBlock(func->block);
			Write("}");
		}
	}

	void Decompile(SpiteIR::IR* ir)
	{
		SpiteIR::Function* entry = ir->entry;
		DecompileFunction(entry);
		Print();
	}

	void DecompileInstruction(SpiteIR::Instruction& inst)
	{
		switch (inst.kind)
		{
		case SpiteIR::InstructionKind::Return:
			break;
		case SpiteIR::InstructionKind::Compare:
			break;
		case SpiteIR::InstructionKind::Jump:
			DecompileJump(inst);
			break;
		case SpiteIR::InstructionKind::Branch:
			DecompileBranch(inst);
			break;
		case SpiteIR::InstructionKind::Call:
			DecompileCall(inst);
			break;
		case SpiteIR::InstructionKind::Allocate:
			DecompileAllocate(inst);
			break;
		case SpiteIR::InstructionKind::HeapAllocate:
			break;
		case SpiteIR::InstructionKind::Load:
			break;
		case SpiteIR::InstructionKind::Store:
			DecompileStore(inst);
			break;
		case SpiteIR::InstructionKind::Free:
			break;
		case SpiteIR::InstructionKind::Cast:
			break;
		case SpiteIR::InstructionKind::Switch:
			break;
		case SpiteIR::InstructionKind::BinOp:
			DecompileBinaryOp(inst);
			break;
		case SpiteIR::InstructionKind::UnOp:
			break;
		default:
			break;
		}
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

	void DecompileStore(SpiteIR::Instruction& storeInst)
	{
		Write("r" + eastl::to_string(storeInst.store.dst) + " = store " + 
			WriteOperand(storeInst.store.src));
	}

	void DecompileCall(SpiteIR::Instruction& callInst)
	{
		auto& call = callInst.call;

		if (call.function)
		{
			return;
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
};