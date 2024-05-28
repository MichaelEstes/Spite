#pragma once
#include "GlobalTable.h"
#include "IR.h"

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
		for (auto& [key, value] : globalTable->packageToSymbolTable)
		{
			symbolTable = value;
			BuildPackage(value);
		}

		return ir;
	}

	void BuildPackage(SymbolTable* symbolTable)
	{
		SpiteIR::Package* package = ir->AddPackage();

		package->file = *symbolTable->package->pos.file;
		package->name = symbolTable->package->val.ToString();
		package->parent = ir;

		packageMap[package->file] = package;

		for (auto& [key, value] : symbolTable->globalValMap)
		{
			BuildGlobalVariable(package, value);
		}

		for (auto& [key, value] : symbolTable->stateMap)
		{
			BuildStateSymbol(package, value);
		}
	}

	void BuildGlobalVariable(SpiteIR::Package* package, Stmnt* globalVarStmnt)
	{
		auto& def = globalVarStmnt->definition;
		SpiteIR::Value* globalVar = ir->AllocateValue();
		globalVar->parent = SpiteIR::Parent(package);
		globalVar->pos = globalVarStmnt->start->pos;
		globalVar->name = package->name + ":" + def.name->val.ToString();
		globalVar->type = TypeToIRType(def.type, globalVar);
		BuildContatntFromExpr(globalVar, def.assignment);
		if (globalVar->kind != SpiteIR::ValueKind::Constant)
		{
			AddError(globalVarStmnt->start, "Global variable assignments must be constant");
		}
		package->globalVariables[globalVar->name] = globalVar;
	}

	void BuildContatntFromExpr(SpiteIR::Value* value, Expr* expr)
	{
		value->kind = SpiteIR::ValueKind::None;
		if (!expr) return;

		switch (expr->typeID)
		{
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

	void BuildStateSymbol(SpiteIR::Package* package, StateSymbol& stateSymbol)
	{
		if (stateSymbol.state->state.generics)
		{
			BuildGenericState(package, stateSymbol);
			return;
		}

		BuildState(package, stateSymbol);
	}

	void BuildState(SpiteIR::Package* package, StateSymbol& stateSymbol)
	{
		Stmnt* stateStmnt = stateSymbol.state;
		eastl::string name = stateStmnt->state.name->val.ToString();
		SpiteIR::State* state = EmplaceState(package, stateStmnt, name);

		for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
		{
			Stmnt* memberStmnt = stateStmnt->state.members->at(i);
			BuildMemberForState(state, memberStmnt->start->pos, memberStmnt->definition.name->ToString(),
				memberStmnt->definition.type, memberStmnt->definition.assignment);
		}

		for (Stmnt* method : stateSymbol.methods)
		{

		}

		for (Stmnt* con : stateSymbol.constructors)
		{

		}

		for (Stmnt* op : stateSymbol.operators)
		{

		}

		if (stateSymbol.destructor)
		{

		}


	}

	void BuildGenericState(SpiteIR::Package* package, StateSymbol& stateSymbol)
	{
		Stmnt* stateStmnt = stateSymbol.state;
		auto& generics = stateStmnt->state.generics->generics;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			eastl::string name = stateStmnt->state.name->val.ToString() + BuildGenericsString(templates);
			SpiteIR::State* state = EmplaceState(package, stateStmnt, name);

			for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
			{
				Stmnt* memberStmnt = stateStmnt->state.members->at(i);
				Type* type = ReplaceTypeWithTemplateType(memberStmnt->definition.type, generics.names, templates);
				BuildMemberForState(state, memberStmnt->start->pos, memberStmnt->definition.name->ToString(),
					type, memberStmnt->definition.assignment);
			}

			if (generics.whereStmnt)
			{
				BuildWhereFunction(package, stateStmnt->state.generics, name);
			}

			for (Stmnt* method : stateSymbol.methods)
			{

			}

			for (Stmnt* con : stateSymbol.constructors)
			{

			}

			for (Stmnt* op : stateSymbol.operators)
			{

			}

			if (stateSymbol.destructor)
			{

			}
		}
	}

	Type* ReplaceTypeWithTemplateType(Type* type, eastl::vector<Token*>* generics, eastl::vector<Expr*>* templates)
	{
		Type* returnType = type;
		if (type->typeID == TypeID::NamedType)
		{
			for (size_t i = 0; i < generics->size(); i++)
			{
				Token* genericName = generics->at(i);
				if (type->namedType.typeName->val == genericName->val)
				{
					Expr* templ = templates->at(i);
					if (templ && templ->typeID == ExprID::TypeExpr)
					{
						returnType = templ->typeExpr.type;
						break;
					}
					else AddError(genericName, "Lower:BuildGenericState Invalid expression used as type template");
				}
			}
		}

		return returnType;
	}

	SpiteIR::State* EmplaceState(SpiteIR::Package* package, Stmnt* stateStmnt, eastl::string& name)
	{
		SpiteIR::State* state = ir->AllocateState();

		state->parent = package;
		state->pos = stateStmnt->start->pos;
		state->name = package->name + ":" + name;
		state->metadata.flags = (int)*stateStmnt->state.insetFlags->flags;

		package->states[state->name] = state;
		return state;
	}

	void BuildMemberForState(SpiteIR::State* state, Position& pos, const eastl::string& name, Type* type,
		Expr* assignment)
	{
		SpiteIR::Member* member = ir->AllocateMember();
		member->parent = state;
		member->pos = pos;
		member->type = TypeToIRType(type, member);
		member->name = name;
		state->members.push_back(member);
	}

	eastl::string BuildGenericsString(eastl::vector<Expr*>* generics)
	{
		eastl::string str = "<";

		size_t size = generics->size();
		for (size_t i = 0; i < size; i++)
		{
			str += ToString(generics->at(i)) + ",";
		}
		str.back() = '>';

		return str;
	}

	void BuildWhereFunction(SpiteIR::Package* package, Stmnt* generics, eastl::string& name)
	{

	}

	template<typename P>
	SpiteIR::Type* TypeToIRType(Type* type, P* parent)
	{
		SpiteIR::Type* irType = ir->AllocateType();

		irType->parent = SpiteIR::Parent(parent);

		switch (type->typeID)
		{
		case InvalidType:
		case UnknownType:
		case ImplicitType:
			AddError("Lower:TypeToIRType Invalid type for conversion");
			break;
		case PrimitiveType:
		{
			irType->kind = SpiteIR::TypeKind::PrimitiveType;
			auto& prim = irType->primitive;
			prim.size = type->primitiveType.size;
			prim.isSigned = type->primitiveType.isSigned;
			switch (type->primitiveType.type)
			{
			case Void:
				prim.kind = SpiteIR::PrimitiveKind::Void;
				break;
			case Bool:
			case Byte:
			case Ubyte:
			case Int:
			case Int16:
			case Int32:
			case Int64:
			case Int128:
			case Uint:
			case Uint16:
			case Uint32:
			case Uint64:
			case Uint128:
				prim.kind = SpiteIR::PrimitiveKind::Int;
				break;
			case Float:
			case Float32:
			case Float64:
			case Float128:
				prim.kind = SpiteIR::PrimitiveKind::Float;
			case String:
				prim.kind = SpiteIR::PrimitiveKind::String;
				break;
			default:
				break;
			}
			break;
		}
		case ImportedType:
		case NamedType:
		{
			irType->kind = SpiteIR::TypeKind::NamedType;
			Stmnt* state = globalTable->FindStateForType(type, symbolTable);
			irType->namedType.name = ir->AllocateString();
			*irType->namedType.name = state->package->val.ToString() + ":" + state->state.name->val.ToString();
			break;
		}
		case ExplicitType:
		{
			irType->kind = SpiteIR::TypeKind::AnonymousType;
			irType->anonymousType.members = ir->AllocateArray<SpiteIR::AnonymousTypeMember*>();
			for (size_t i = 0; i < type->explicitType.declarations->size(); i++)
			{
				Stmnt* stmnt = type->explicitType.declarations->at(i);
				auto& def = stmnt->definition;
				SpiteIR::AnonymousTypeMember* member = ir->AllocateAnonymousTypeMember();
				member->parent = irType;
				member->pos = stmnt->start->pos;
				member->name = def.name->val.ToString();
				member->type = TypeToIRType(def.type, member);
				irType->anonymousType.members->push_back(member);
			}
			break;
		}
		case PointerType:
			irType->kind = SpiteIR::TypeKind::PointerType;
			irType->pointer.type = TypeToIRType(type->pointerType.type, irType);
			break;
		case ValueType:
			irType->kind = SpiteIR::TypeKind::ValueType;
			irType->value.type = TypeToIRType(type->valueType.type, irType);
			break;
		case ArrayType:
			irType->kind = SpiteIR::TypeKind::ArrayType;
			irType->array.type = TypeToIRType(type->arrayType.type, irType);
			break;
		case GenericsType:
			break;
		case FunctionType:
		{
			irType->kind = SpiteIR::TypeKind::FunctionType;
			irType->function.params = ir->AllocateArray<SpiteIR::Type*>();
			irType->function.returnType = TypeToIRType(type->functionType.returnType, irType);
			for (Type* param : *type->functionType.paramTypes)
			{
				irType->function.params->push_back(TypeToIRType(param, irType));
			}
			break;
		}
		default:
			break;
		}

		return irType;
	}
};