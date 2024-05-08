#pragma once

#include "EASTL/hash_map.h"
#include "EASTL/vector.h"

#include "../Containers/StringView.h"
#include "../Containers/Arena.h"
#include "../Config/Config.h"
#include "SyntaxUtils.h"

extern Config config;
struct ExprHash;

struct StateSymbol
{
	Stmnt* state = nullptr;

	eastl::vector<Stmnt*> constructors;
	eastl::vector<Stmnt*> methods;
	eastl::vector<Stmnt*> operators;
	Stmnt* destructor = nullptr;
};

struct SymbolTable
{
	Stmnt* package;
	eastl::vector<Stmnt*> imports;
	eastl::hash_map<StringView, StateSymbol, StringViewHash> stateMap;
	eastl::hash_map<StringView, Stmnt*, StringViewHash> functionMap;
	eastl::hash_map<StringView, Stmnt*, StringViewHash> globalValMap;
	eastl::vector<Stmnt*> onCompiles;
	Arena* arena;

	SymbolTable(size_t initialSize)
	{
		arena = new Arena(initialSize);
	}

	~SymbolTable()
	{
		delete arena;
	}

	inline Stmnt* CreateStmnt(Token* start, StmntID nodeID, Stmnt* scopeOf)
	{
		return arena->Emplace<Stmnt>(nodeID, start, scopeOf);
	}

	inline Stmnt* InvalidStmnt()
	{
		return arena->Emplace<Stmnt>();
	}

	template<typename T>
	inline eastl::vector<T>* CreateVectorPtr()
	{
		return arena->Emplace<eastl::vector<T>>();
	}

	inline Type* CreateTypePtr(TypeID typeID)
	{
		return arena->Emplace<Type>(typeID);
	}

	inline Expr* CreateExpr(Token* start, ExprID exprID)
	{
		return arena->Emplace<Expr>(exprID, start);
	}

	StateSymbol& FindOrCreateState(const StringView& name)
	{
		if (stateMap.find(name) != stateMap.end())
		{
			return stateMap[name];
		}
		else
		{
			stateMap[name] = StateSymbol();
			return stateMap[name];
		}
	}

	void AddState(Stmnt* state)
	{
		StateSymbol& symbol = FindOrCreateState(state->state.name->val);
		symbol.state = state;
	}

	void AddConstructor(Stmnt* constructor)
	{
		StateSymbol& symbol = FindOrCreateState(constructor->constructor.stateName->val);
		symbol.constructors.push_back(constructor);
	}

	void AddMethod(Stmnt* method)
	{
		StateSymbol& symbol = FindOrCreateState(method->method.stateName->val);
		symbol.methods.push_back(method);
	}

	void AddOperator(Stmnt* stateOperator)
	{
		StateSymbol& symbol = FindOrCreateState(stateOperator->stateOperator.stateName->val);
		symbol.operators.push_back(stateOperator);
	}

	void SetDestructor(Stmnt* destructor)
	{
		StateSymbol& symbol = FindOrCreateState(destructor->destructor.stateName->val);
		symbol.destructor = destructor;
	}

	void AddFunction(Stmnt* function)
	{
		functionMap[function->function.name->val] = function;
	}

	void AddGlobalVal(Stmnt* globalVal)
	{
		globalValMap[globalVal->definition.name->val] = globalVal;
	}

	void AddOnCompile(Stmnt* compile)
	{
		onCompiles.push_back(compile);
	}

	inline StateSymbol* FindStateSymbol(StringView& val)
	{
		if (auto entry = stateMap.find(val); entry != stateMap.end())
		{
			return &entry->second;
		}

		return nullptr;
	}

	inline Stmnt* FindStateOrFunction(StringView& val)
	{
		Stmnt* node = FindState(val);
		if (!node) node = FindFunction(val);
		return node;
	}

	inline Stmnt* FindState(StringView& val)
	{
		if (auto entry = stateMap.find(val); entry != stateMap.end())
		{
			return entry->second.state;
		}

		return nullptr;
	}

	inline Stmnt* FindFunction(StringView& val)
	{
		if (auto entry = functionMap.find(val); entry != functionMap.end())
		{
			return entry->second;
		}

		return nullptr;
	}

	inline Stmnt* FindStateMethod(StateSymbol* of, StringView& val)
	{
		eastl::vector<Stmnt*>& methods = of->methods;
		for (Stmnt* node : methods)
		{
			if (node->method.name->val == val) return node;
		}

		return nullptr;
	}

	inline Stmnt* FindTypeMember(eastl::vector<Stmnt*>* members, StringView& val)
	{
		for (Stmnt* node : *members)
		{
			if (node->definition.name->val == val) return node;
		}

		return nullptr;
	}

	inline Stmnt* FindStateMember(Stmnt* of, StringView& val)
	{
		return FindTypeMember(of->state.members, val);
	}

	Type* CreatePrimitive(UniqueType primType)
	{
		Type* type = CreateTypePtr(TypeID::PrimitiveType);
		type->primitiveType.type = primType;
		switch (primType)
		{
		case UniqueType::Void:
			type->primitiveType.size = 0;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Bool:
			type->primitiveType.size = 1;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Byte:
			type->primitiveType.size = 8;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Ubyte:
			type->primitiveType.size = 8;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Int:
			type->primitiveType.size = config.targetArchBitWidth;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int16:
			type->primitiveType.size = 16;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int32:
			type->primitiveType.size = 32;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int64:
			type->primitiveType.size = 64;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int128:
			type->primitiveType.size = 128;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Uint:
			type->primitiveType.size = config.targetArchBitWidth;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint16:
			type->primitiveType.size = 16;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint32:
			type->primitiveType.size = 32;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint64:
			type->primitiveType.size = 64;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint128:
			type->primitiveType.size = 128;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Float:
			type->primitiveType.size = config.targetArchBitWidth;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Float32:
			type->primitiveType.size = 32;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Float64:
			type->primitiveType.size = 64;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::String:
			type->primitiveType.size = config.targetArchBitWidth * 2;
			type->primitiveType.isSigned = false;
			break;
		default:
			break;
		}

		return type;
	}

	void BuildStateGeneric(Stmnt* state, Expr* genericExpr)
	{
		StringView genericsStr = CreateGenericStateString(state->state.name->val, genericExpr);
		StateSymbol* stateSymbol = FindStateSymbol(state->state.name->val);
	}

	StringView CreateGenericStateString(StringView& stateName, Expr* genericExpr)
	{
		auto& generics = genericExpr->genericsExpr;
		eastl::string str = stateName.ToString();

		str += generics.open->val.ToString();
		size_t size = generics.templateArgs->size();
		for (int i = 0; i < size; i++)
		{
			Expr* expr = generics.templateArgs->at(i);
			str += ToString(expr);
			if (i < size - 1) str += ",";
		}
		str += generics.close->val.ToString();

		StringView val = StringView(str);
		return val;
	}
};