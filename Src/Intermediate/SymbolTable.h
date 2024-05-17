#pragma once

#include "EASTL/hash_map.h"
#include "EASTL/hash_set.h"
#include "EASTL/vector.h"

#include "../Containers/StringView.h"
#include "../Containers/Arena.h"
#include "../Config/Config.h"
#include "SyntaxUtils.h"

extern Config config;
struct ExprHash;

struct ImportHash
{
	StringViewHash stringHasher;
	size_t operator()(const Stmnt* stmnt) const
	{
		return stringHasher(stmnt->importStmnt.packageName->val);
	}
};

struct ImportEqual
{
	bool operator()(const Stmnt* l, const Stmnt* r) const
	{
		return l->importStmnt.packageName->val == r->importStmnt.packageName->val;
	}
};

struct MethodHash
{
	StringViewHash stringHasher;
	TypeHash typeHasher;
	size_t operator()(const Stmnt* stmnt) const
	{
		size_t hash = 0;
		Stmnt* decl = nullptr;
		Type* returnType = nullptr;
		StringView* stateName = nullptr;
		StringView* name = nullptr;
		switch (stmnt->nodeID)
		{
		case Method:
			decl = stmnt->method.decl;
			returnType = stmnt->method.returnType;
			stateName = &stmnt->method.stateName->val;
			name = &stmnt->method.name->val;
			break;
		case StateOperator:
			decl = stmnt->stateOperator.decl;
			returnType = stmnt->stateOperator.returnType;
			stateName = &stmnt->stateOperator.stateName->val;
			name = &stmnt->method.name->val;
			break;
		case Constructor:
			decl = stmnt->constructor.decl;
			stateName = &stmnt->stateOperator.stateName->val;
			break;
		default:
			break;
		}

		for (Stmnt* param : *decl->functionDecl.parameters)
		{
			hash += typeHasher(param->definition.type);
		}
		hash += stringHasher(*stateName);
		if (returnType) hash += typeHasher(returnType);
		if (name) hash += stringHasher(*name);

		return hash;
	}
};

struct MethodEqual
{
	StringViewHash stringHasher;
	TypeHash typeHasher;
	bool operator()(const Stmnt* l, const Stmnt* r) const
	{
		if (l->nodeID != r->nodeID) return false;

		Stmnt* lDecl = nullptr;
		Type* lReturnType = nullptr;
		StringView* lStateName = nullptr;
		StringView* lName = nullptr;

		Stmnt* rDecl = nullptr;
		Type* rReturnType = nullptr;
		StringView* rStateName = nullptr;
		StringView* rName = nullptr;

		switch (l->nodeID)
		{
		case Method:
			lDecl = l->method.decl;
			lReturnType = l->method.returnType;
			lStateName = &l->method.stateName->val;
			lName = &l->method.name->val;

			rDecl = r->method.decl;
			rReturnType = r->method.returnType;
			rStateName = &r->method.stateName->val;
			rName = &r->method.name->val;
			break;
		case StateOperator:
			lDecl = l->stateOperator.decl;
			lReturnType = l->stateOperator.returnType;
			lStateName = &l->stateOperator.stateName->val;
			lName = &l->method.name->val;

			rDecl = r->stateOperator.decl;
			rReturnType = r->stateOperator.returnType;
			rStateName = &r->stateOperator.stateName->val;
			rName = &r->method.name->val;
			break;
		case Constructor:
			lDecl = l->constructor.decl;
			lStateName = &l->stateOperator.stateName->val;

			rDecl = r->constructor.decl;
			rStateName = &r->stateOperator.stateName->val;
			break;
		default:
			break;
		}

		if (lDecl->functionDecl.parameters->size() != rDecl->functionDecl.parameters->size()) return false;
		for (size_t i = 0; i < lDecl->functionDecl.parameters->size(); i++)
		{
			Type* lType = lDecl->functionDecl.parameters->at(i)->definition.type;
			Type* rType = rDecl->functionDecl.parameters->at(i)->definition.type;
			if (*lType != *rType) return false;
		}
		if (*lStateName != *rStateName) return false;

		if (lReturnType && *lReturnType != *rReturnType) return false;
		if (lName && *lName != *rName) return false;

	}
};

struct StateSymbol
{
	Stmnt* state = nullptr;

	eastl::hash_set<Stmnt*, MethodHash, MethodEqual> constructors;
	eastl::hash_set<Stmnt*, MethodHash, MethodEqual> methods;
	eastl::hash_set<Stmnt*, MethodHash, MethodEqual> operators;
	Stmnt* destructor = nullptr;
};

struct SymbolTable
{
	Token* package;
	eastl::hash_set<Stmnt*, ImportHash, ImportEqual> imports;
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

	void Print()
	{
		eastl::string toPrint = "";
		if (package)
		{
			toPrint += "package " + package->ToString() + '\n';
		}

		for (Stmnt* node : imports)
		{
			toPrint += ToString(node);
			toPrint += '\n';
		}

		for (Stmnt* node : onCompiles)
		{
			toPrint += ToString(node);
			toPrint += '\n';
		}

		for (auto& [key, value] : globalValMap)
		{
			toPrint += ToString(value);
			toPrint += '\n';
		}

		for (auto& [key, value] : stateMap)
		{
			toPrint += ToString(value.state);
			toPrint += '\n';

			for (Stmnt* node : value.constructors)
			{
				toPrint += ToString(node);
				toPrint += '\n';
			}

			for (Stmnt* node : value.methods)
			{
				toPrint += ToString(node);
				toPrint += '\n';
			}

			for (Stmnt* node : value.operators)
			{
				toPrint += ToString(node);
				toPrint += '\n';
			}

			if (value.destructor)
			{
				toPrint += ToString(value.destructor);
				toPrint += '\n';
			}
		}

		for (auto& [key, value] : functionMap)
		{
			toPrint += ToString(value);
			toPrint += '\n';
		}

		Logger::Info(toPrint);
	}


	inline void Merge(SymbolTable* toMerge)
	{
		for (Stmnt* import : toMerge->imports)
		{
			imports.insert(import);
		}

		for (auto& [key, value] : toMerge->stateMap)
		{
			AddState(value.state);
			for (Stmnt* cons : value.constructors) AddConstructor(cons);
			for (Stmnt* method : value.methods) AddMethod(method);
			for (Stmnt* op : value.operators) AddMethod(op);
		}

		for (auto& [key, value] : toMerge->functionMap)
		{
			AddFunction(value);
		}

		for (auto& [key, value] : toMerge->globalValMap)
		{
			AddGlobalVal(value);
		}

		for (Stmnt* compile : toMerge->onCompiles) AddOnCompile(compile);
	}

	inline Stmnt* CreateStmnt(Token* start, StmntID nodeID, Token* package, Stmnt* scopeOf)
	{
		return arena->Emplace<Stmnt>(nodeID, start, package, scopeOf);
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

	void AddImport(Stmnt * import)
	{
		imports.insert(import);
	}

	void AddState(Stmnt* state)
	{
		StateSymbol& symbol = FindOrCreateState(state->state.name->val);
		if (symbol.state) AddError(state->start, "SymbolTable:AddState State already declared");
		symbol.state = state;
	}

	void AddConstructor(Stmnt* constructor)
	{
		StateSymbol& symbol = FindOrCreateState(constructor->constructor.stateName->val);
		if (symbol.constructors.find(constructor) != symbol.constructors.end()) 
			AddError(constructor->start, "SymbolTable:AddConstructor Constructor with identical signature already declared");
		symbol.constructors.insert(constructor);
	}

	void AddMethod(Stmnt* method)
	{
		StateSymbol& symbol = FindOrCreateState(method->method.stateName->val);
		if (symbol.methods.find(method) != symbol.methods.end())
			AddError(method->start, "SymbolTable:AddMethod Method with identical signature already declared");
		symbol.methods.insert(method);
	}

	void AddOperator(Stmnt* stateOperator)
	{
		StateSymbol& symbol = FindOrCreateState(stateOperator->stateOperator.stateName->val);
		if (symbol.operators.find(stateOperator) != symbol.operators.end())
			AddError(stateOperator->start, "SymbolTable:AddOperator Operator with identical signature already declared");
		symbol.operators.insert(stateOperator);
	}

	void SetDestructor(Stmnt* destructor)
	{
		StateSymbol& symbol = FindOrCreateState(destructor->destructor.stateName->val);
		if(symbol.destructor) AddError(destructor->start, "SymbolTable:SetDestructor Destructor already declared");
		symbol.destructor = destructor;
	}

	void AddFunction(Stmnt* function)
	{
		auto& name = function->function.name->val;
		if (functionMap.find(name) != functionMap.end())
			AddError(function->start, "SymbolTable:AddFunction Function name already declared");
		functionMap[function->function.name->val] = function;
	}

	void AddGlobalVal(Stmnt* globalVal)
	{
		auto& name = globalVal->definition.name->val;
		if (globalValMap.find(name) != globalValMap.end())
			AddError(globalVal->start, "SymbolTable:AddGlobalVal Package scoped values with name already declared");
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
};