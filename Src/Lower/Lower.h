#pragma once
#include "../Intermediate/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerDeclarations.h"

struct Lower
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	SpiteIR::IR* ir;

	eastl::hash_map<eastl::string, SpiteIR::Package*> packageMap;
	eastl::hash_map<eastl::string, SpiteIR::Package*> stateMap;

	Lower(GlobalTable* globalTable)
	{
		this->globalTable = globalTable;
		this->ir = new SpiteIR::IR(globalTable->GetSize());
		symbolTable = nullptr;
	}

	SpiteIR::IR* BuildIR()
	{
		LowerDeclarations lowerDecl = LowerDeclarations(globalTable, ir, packageMap, stateMap);
		for (auto& [key, value] : globalTable->packageToSymbolTable)
		{
			SpiteIR::Package* package = ir->AddPackage();

			package->file = *symbolTable->package->pos.file;
			package->name = BuildPackageName(symbolTable->package);
			package->parent = ir;

			packageMap[package->file] = package;

			symbolTable = value;
			lowerDecl.BuildDeclarations(package, value);
		}

		for (auto& [key, value] : globalTable->packageToSymbolTable)
		{
			symbolTable = value;
			//BuildPackage(value);
		}

		return ir;
	}

	void FillValueFromExpr(SpiteIR::Value* value, Expr* expr)
	{
		value->kind = SpiteIR::ValueKind::None;
		if (!expr || expr->typeID == InvalidExpr) return;

		switch (expr->typeID)
		{
		case LiteralExpr:
			value->kind = SpiteIR::ValueKind::Constant;
			value->constant = ir->AllocateConstant();
			value->constant->value = expr->literalExpr.val->ToString();
			switch (expr->literalExpr.type)
			{
			case IntLiteral:
				value->constant->kind = SpiteIR::LiteralKind::IntLiteral;
				break;
			case FloatLiteral:
				value->constant->kind = SpiteIR::LiteralKind::FloatLiteral;
				break;
			case HexLiteral:
				value->constant->kind = SpiteIR::LiteralKind::HexLiteral;
				break;
			case StringLiteral:
				value->constant->kind = SpiteIR::LiteralKind::StringLiteral;
				break;
			case TrueLiteral:
			case FalseLiteral:
				value->constant->kind = SpiteIR::LiteralKind::BoolLiteral;
				break;
			default:
				break;
			}
			break;
		case IdentifierExpr:
			break;
		case PrimitiveExpr:
			break;
		case SelectorExpr:
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			break;
		case NewExpr:
			break;
		case FixedExpr:
			break;
		case AnonTypeExpr:
			break;
		case ExplicitTypeExpr:
			break;
		case AsExpr:
			break;
		case DereferenceExpr:
			break;
		case ReferenceExpr:
			break;
		case BinaryExpr:
			break;
		case UnaryExpr:
			break;
		case GroupedExpr:
			break;
		case GenericsExpr:
			break;
		case TypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		default:
			break;
		}

		return;
	}

	SpiteIR::Value* BuildValueSetForExpr(SpiteIR::Parent parent, const eastl::string& name,
		Expr* expr, eastl::vector<SpiteIR::Value*>& values)
	{
		if (!expr) return nullptr;

		switch (expr->typeID)
		{
		case InvalidExpr:
			//Error
			break;
		case LiteralExpr:
			break;
		case IdentifierExpr:
			break;
		case PrimitiveExpr:
			break;
		case SelectorExpr:
			break;
		case IndexExpr:
			break;
		case FunctionCallExpr:
			break;
		case NewExpr:
			break;
		case FixedExpr:
			break;
		case AnonTypeExpr:
			break;
		case ExplicitTypeExpr:
			break;
		case AsExpr:
			break;
		case DereferenceExpr:
			break;
		case ReferenceExpr:
			break;
		case BinaryExpr:
			return BuildBinaryOp(parent, name, expr, values);
		case UnaryExpr:
			break;
		case GroupedExpr:
			break;
		case GenericsExpr:
			break;
		case TypeExpr:
			break;
		case FunctionTypeDeclExpr:
			break;
		case CompileExpr:
			break;
		default:
			break;
		}
	}

	SpiteIR::Value* BuildBinaryOp(SpiteIR::Parent parent, const eastl::string& name,
		Expr* expr, eastl::vector<SpiteIR::Value*>& values)
	{
		Expr* left = expr->binaryExpr.left;
		Expr* right = expr->binaryExpr.right;
		UniqueType op = expr->binaryExpr.opType;

		SpiteIR::Value* leftValue = BuildValueSetForExpr(parent, name, left, values);
		SpiteIR::Value* rightValue = BuildValueSetForExpr(parent, name, right, values);

		SpiteIR::Value* value = ir->AllocateValue();
		value->parent = parent;
		value->pos = expr->start->pos;
		value->kind = SpiteIR::ValueKind::Instruction;
		value->instruction = ir->AllocateInstruction();
		value->instruction->kind = SpiteIR::InstructionKind::Binary;
		value->instruction->binary.op = ToBinaryOp(op);
		value->instruction->binary.left = leftValue;
		value->instruction->binary.right = rightValue;
		value->name = name + eastl::to_string(values.size());
		values.push_back(value);
		return value;
	}

	SpiteIR::Type* GetBinaryOpType(SpiteIR::Value* leftValue, SpiteIR::Value* rightValue)
	{
		SpiteIR::Type* leftType = leftValue->type;
		SpiteIR::Type* rightType = leftValue->type;

		switch (leftType->kind)
		{
		default:
			break;

		}
	}

	SpiteIR::BinaryOp ToBinaryOp(UniqueType type)
	{
		switch (type)
		{
		case Add:
			return SpiteIR::BinaryOp::Add;
		case Subtract:
			return SpiteIR::BinaryOp::Subtract;
		case Multiply:
			return SpiteIR::BinaryOp::Multiply;
		case Divide:
			return SpiteIR::BinaryOp::Divide;
		case Modulo:
			return SpiteIR::BinaryOp::Modulo;
		case And:
			return SpiteIR::BinaryOp::And;
		case Or:
			return SpiteIR::BinaryOp::Or;
		case Xor:
			return SpiteIR::BinaryOp::Xor;
		case Shiftl:
			return SpiteIR::BinaryOp::ShiftLeft;
		case Shiftr:
			return SpiteIR::BinaryOp::ShiftRight;
		case AndNot:
			return SpiteIR::BinaryOp::AndNot;
		case LogicAnd:
			return SpiteIR::BinaryOp::LogicAnd;
		case LogicOr:
			return SpiteIR::BinaryOp::LogicOr;
		case Equal:
			return SpiteIR::BinaryOp::Equal;
		case NotEql:
			return SpiteIR::BinaryOp::NotEql;
		case Less:
			return SpiteIR::BinaryOp::Less;
		case Greater:
			return SpiteIR::BinaryOp::Greater;
		case LessEqual:
			return SpiteIR::BinaryOp::LessEqual;
		case GreaterEqual:
			return SpiteIR::BinaryOp::GreaterEqual;
		default:
			// error
			return SpiteIR::BinaryOp::Add;
		}
	}

	void BuildWhereFunction(SpiteIR::Package* package, Stmnt* generics, eastl::string& name)
	{

	}
};