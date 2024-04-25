#pragma once

#include "EASTL/hash_map.h"
#include "EASTL/vector.h"

#include "../Containers/StringView.h"
#include "../Containers/Arena.h"
#include "../Config/Config.h"
#include "Node.h"

extern Config config;

struct StateSymbol
{
	Node* state = nullptr;

	eastl::vector<Node*> constructors;
	eastl::vector<Node*> methods;
	eastl::vector<Node*> operators;
	Node* destructor = nullptr;
};

struct TypeHash
{
	StringViewHash inplaceStrHasher;

	size_t operator()(const Type* type) const
	{
		switch (type->typeID)
		{
		case PrimitiveType:
			return type->primitiveType.type;
		case NamedType:
		{
			size_t hash = 0;
			auto& namedType = type->namedType;
			hash += inplaceStrHasher(namedType.typeName->val);
			return hash;
		}
		case ExplicitType:
		{
			size_t hash = 0;
			for (Node* node : *type->explicitType.declarations)
			{
				Type* defType = node->definition.type;
				hash += this->operator()(defType);
			}
			return hash;
		}
		case PointerType:
		{
			size_t hash = '*';
			return hash + this->operator()(type->pointerType.type);
		}
		case ValueType:
		{
			size_t hash = '~';
			return hash + this->operator()(type->valueType.type);
		}
		case ArrayType:
		{
			size_t hash = '[' + ']';
			return hash + this->operator()(type->arrayType.type);
		}
		case GenericsType:
		{
			size_t hash = 0;
			auto& genericType = type->genericsType;
			for (Type* genType : *genericType.generics->genericsExpr.types)
			{
				hash += this->operator()(genType);
			}
			return hash + this->operator()(genericType.type);
		}
		case FunctionType:
		{
			size_t hash = 0;
			auto& functionType = type->functionType;
			hash += this->operator()(functionType.returnType);
			for (Type* param : *functionType.paramTypes)
			{
				hash += this->operator()(param);
			}
			return hash;
		}
		case ImportedType:
		{
			size_t hash = 0;
			auto& importedType = type->importedType;
			hash += inplaceStrHasher(importedType.packageName->val);
			hash += inplaceStrHasher(importedType.typeName->val);
			return hash;
		}
		default:
			break;
		}

		Logger::FatalError("SymbolTable:TypeHash Unable to create hash for Type");
		return 0;
	}
};

struct SymbolTable
{
	Node* package;
	eastl::vector<Node*> imports;
	eastl::hash_map<StringView, StateSymbol, StringViewHash> stateMap;
	eastl::hash_map<StringView, Node*, StringViewHash> functionMap;
	eastl::hash_map<StringView, Node*, StringViewHash> globalValMap;
	eastl::vector<Node*> onCompiles;
	Arena* arena;

	SymbolTable(size_t initialSize)
	{
		arena = new Arena(initialSize);
	}

	~SymbolTable()
	{
		delete arena;
	}

	inline Node* CreateNode(Token* start, NodeID nodeID, Node* scopeOf)
	{
		return arena->Emplace<Node>(nodeID, start, scopeOf);
	}

	inline Node* InvalidNode()
	{
		return arena->Emplace<Node>();
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
	
	void AddState(Node* state)
	{
		StateSymbol& symbol = FindOrCreateState(state->state.name->val);
		symbol.state = state;
	}

	void AddConstructor(Node* constructor)
	{
		StateSymbol& symbol = FindOrCreateState(constructor->constructor.stateName->val);
		symbol.constructors.push_back(constructor);
	}

	void AddMethod(Node* method)
	{
		StateSymbol& symbol = FindOrCreateState(method->method.stateName->val);
		symbol.methods.push_back(method);
	}

	void AddOperator(Node* stateOperator)
	{
		StateSymbol& symbol = FindOrCreateState(stateOperator->stateOperator.stateName->val);
		symbol.operators.push_back(stateOperator);
	}

	void SetDestructor(Node* destructor)
	{
		StateSymbol& symbol = FindOrCreateState(destructor->destructor.stateName->val);
		symbol.destructor = destructor;
	}

	void AddFunction(Node* function)
	{
		functionMap[function->function.name->val] = function;
	}

	void AddGlobalVal(Node* globalVal)
	{
		globalValMap[globalVal->definition.name->val] = globalVal;
	}

	void AddOnCompile(Node* compile)
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

	inline Node* FindStateOrFunction(StringView& val)
	{
		Node* node = FindState(val);
		if (!node) node = FindFunction(val);
		return node;
	}

	inline Node* FindState(StringView& val)
	{
		if (auto entry = stateMap.find(val); entry != stateMap.end())
		{
			return entry->second.state;
		}

		return nullptr;
	}

	inline Node* FindFunction(StringView& val)
	{
		if (auto entry = functionMap.find(val); entry != functionMap.end())
		{
			return entry->second;
		}

		return nullptr;
	}

	inline Node* FindStateMethod(StateSymbol* of, StringView& val)
	{
		eastl::vector<Node*>& methods = of->methods;
		for (Node* node : methods)
		{
			if (node->method.name->val == val) return node;
		}

		return nullptr;
	}

	inline Node* FindTypeMember(eastl::vector<Node*>* members, StringView& val)
	{
		for (Node* node : *members)
		{
			if (node->definition.name->val == val) return node;
		}

		return nullptr;
	}

	inline Node* FindStateMember(Node* of, StringView& val)
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
};