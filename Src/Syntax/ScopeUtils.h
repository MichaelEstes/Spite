#pragma once
#include "EASTL/deque.h"
#include "GlobalTable.h"

struct ScopeUtils
{
	GlobalTable* globalTable;
	SymbolTable* symbolTable;
	eastl::deque<eastl::hash_map<StringView, Stmnt*, StringViewHash>> scopeQueue;

	ScopeUtils(GlobalTable* globalTable, SymbolTable* symbolTable)
	{
		this->globalTable = globalTable;
		this->symbolTable = symbolTable;
	}

	void AddScope()
	{
		scopeQueue.emplace_back();
	}

	void PopScope()
	{
		scopeQueue.pop_back();
	}

	bool HasInTopScope(StringView& name)
	{
		eastl::hash_map<StringView, Stmnt*, StringViewHash>& back = scopeQueue.back();
		return MapHas(back, name);
	}

	void AddToTopScope(const StringView& name, Stmnt* stmnt)
	{
		eastl::hash_map<StringView, Stmnt*, StringViewHash>& back = scopeQueue.back();
		back[name] = stmnt;
	}

	bool IsConstantIntExpr(Expr* expr, bool checkIdent = true)
	{
		switch (expr->typeID)
		{
		case LiteralExpr:
			return expr->literalExpr.val->uniqueType == UniqueType::IntLiteral ||
					expr->literalExpr.val->uniqueType == UniqueType::HexLiteral;
		// Need to add reassignment check
		case IdentifierExpr:
		{
			//Only allow one degree of seperation to be considered a constant
			// Constant --
			// a := 1
			// arr := [a]int
			// Not Constant --
			// a := 1; b := a
			// arr := [b]int
			if (!checkIdent) return false;
			Stmnt* def = FindInScope(expr->identifierExpr.identifier->val);
			if (!def || !def->definition.assignment) return false;
			return IsConstantIntExpr(def->definition.assignment, false);
		}
		case BinaryExpr:
			return IsConstantIntExpr(expr->binaryExpr.left) && IsConstantIntExpr(expr->binaryExpr.right);
		case UnaryExpr:
			return IsConstantIntExpr(expr->unaryExpr.expr);
		case GroupedExpr:
			return IsConstantIntExpr(expr->groupedExpr.expr);
		default:
			break;
		}

		return false;
	}

	size_t EvaluateConstantIntExpr(Expr* expr)
	{
		switch (expr->typeID)
		{
		case LiteralExpr:
		{
			StringView& str = expr->literalExpr.val->val;
			if (expr->literalExpr.val->uniqueType == UniqueType::IntLiteral)
			{
				return IntLiteralStringToInt(str);
			}
			else if (expr->literalExpr.val->uniqueType == UniqueType::HexLiteral)
			{
				return std::stoi(str.ToString().c_str());
			}

			break;
		}
		case IdentifierExpr:
		{
			Stmnt* def = FindInScope(expr->identifierExpr.identifier->val);
			if (def && def->definition.assignment)
			{
				return EvaluateConstantIntExpr(def->definition.assignment);
			}
		}
		case BinaryExpr:
		{
			int left = EvaluateConstantIntExpr(expr->binaryExpr.left);
			int right = EvaluateConstantIntExpr(expr->binaryExpr.right);
			switch (expr->binaryExpr.op->uniqueType)
			{
			case UniqueType::Add:
				return left + right;
			case UniqueType::Subtract:
				return left - right;
			case UniqueType::Multiply:
				return left * right;
			case UniqueType::Divide:
				return left / right;
			case UniqueType::Modulo:
				return left % right;
			case UniqueType::And:
				return left & right;
			case UniqueType::Or:
				return left | right;
			case UniqueType::Xor:
				return left ^ right;
			case UniqueType::Shiftl:
				return left << right;
			case UniqueType::Shiftr:
				return left >> right;
			case UniqueType::AndNot:
				return left & ~right;
			case UniqueType::LogicAnd:
				return left && right;
			case UniqueType::LogicOr:
				return left || right;
			case UniqueType::Equal:
				return left == right;
			case UniqueType::Less:
				return left < right;
			case UniqueType::Greater:
				return left > right;
			case UniqueType::NotEql:
				return left != right;
			case UniqueType::LessEqual:
				return left <= right;
			case UniqueType::GreaterEqual:
				return left >= right;
			default:
				break;
			}

			break;
		}
		case UnaryExpr:
		{
			int value = EvaluateConstantIntExpr(expr->unaryExpr.expr);
			switch (expr->unaryExpr.op->uniqueType)
			{
			case UniqueType::Subtract:
				return -value;
			case UniqueType::Not:
				return !value;
			case UniqueType::Xor:
				return ~value;
			default:
				break;
			}

			break;
		}
		case GroupedExpr:
			return EvaluateConstantIntExpr(expr->groupedExpr.expr);
		default:
			break;
		}

		return 0;
	}

	inline bool IsPackageExpr(Expr* expr)
	{
		return expr->typeID == ExprID::SelectorExpr && expr->selectorExpr.on->typeID == ExprID::IdentifierExpr &&
			globalTable->IsPackage(expr->selectorExpr.on->identifierExpr.identifier->val);
	}

	inline Stmnt* FindInScope(StringView& val)
	{
		for (auto it = scopeQueue.rbegin(); it != scopeQueue.rend(); it++)
		{
			if (auto entry = it->find(val); entry != it->end())
			{
				return entry->second;
			}
		}

		return nullptr;
	}

	inline Stmnt* FindForName(Token* name)
	{
		Stmnt* stmnt = FindInScope(name->val);
		if (stmnt) return stmnt;

		return globalTable->FindScopedValue(name, symbolTable);
	}
};