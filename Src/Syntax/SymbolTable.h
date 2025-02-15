#pragma once

#include "EASTL/hash_map.h"
#include "EASTL/hash_set.h"
#include "EASTL/vector.h"

#include "../Containers/StringView.h"
#include "../Containers/Arena.h"
#include "../Config/Config.h"
#include "Stmnt.h"
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

inline size_t HashGenerics(Stmnt* generics, const StringViewHash& stringHasher)
{
	size_t hash = 0;
	for (Token* gen : *generics->generics.names)
	{
		hash += stringHasher(gen->val);
	}
	return hash;
}

inline bool EqualGenerics(Stmnt* left, Stmnt* right)
{
	if (left->generics.names->size() != right->generics.names->size()) return false;
	
	for (size_t i = 0; i < left->generics.names->size(); i++)
	{
		Token* leftTok = left->generics.names->at(i);
		Token* rightTok = right->generics.names->at(i);
		if (leftTok->val != rightTok->val) return false;
	}

	return true;
}

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
		{
			decl = stmnt->method.decl;
			returnType = stmnt->method.returnType;
			stateName = &stmnt->method.stateName->val;
			name = &stmnt->method.name->val;
			if (stmnt->method.generics)
			{
				hash += HashGenerics(stmnt->method.generics, stringHasher);
			}
			break;
		}
		case StateOperator:
			decl = stmnt->stateOperator.decl;
			returnType = stmnt->stateOperator.returnType;
			stateName = &stmnt->stateOperator.stateName->val;
			name = &stmnt->method.name->val;
			break;
		case Constructor:
			decl = stmnt->constructor.decl;
			stateName = &stmnt->constructor.stateName->val;
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
			if (!l->method.generics != !r->method.generics) return false;
			if (l->method.generics && r->method.generics)
			{
				if (!EqualGenerics(l->method.generics, r->method.generics)) return false;
			}
			break;
		case StateOperator:
			lDecl = l->stateOperator.decl;
			lReturnType = l->stateOperator.returnType;
			lStateName = &l->stateOperator.stateName->val;
			lName = &l->stateOperator.op->val;

			rDecl = r->stateOperator.decl;
			rReturnType = r->stateOperator.returnType;
			rStateName = &r->stateOperator.stateName->val;
			rName = &r->stateOperator.op->val;
			break;
		case Constructor:
			lDecl = l->constructor.decl;
			lStateName = &l->constructor.stateName->val;

			rDecl = r->constructor.decl;
			rStateName = &r->constructor.stateName->val;
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

		return true;
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

inline Stmnt* GetDeclForFunc(Stmnt* func)
{
	switch (func->nodeID)
	{
	case FunctionStmnt:
		return func->function.decl;
	case Method:
		return func->method.decl;
	case StateOperator:
		return func->stateOperator.decl;
	case Constructor:
		return func->constructor.decl;
	case Destructor:
		return func->destructor.decl;
	default:
		break;
	}

	return nullptr;
}

inline eastl::vector<Stmnt*>* GetFunctionParams(Stmnt* func)
{
	if (func->nodeID == StmntID::ExternFunctionDecl) return func->externFunction.parameters;
	Stmnt* decl = GetDeclForFunc(func);
	return decl->functionDecl.parameters;
}

inline size_t RequiredFunctionParamCount(Stmnt* func)
{
	eastl::vector<Stmnt*>* params = GetFunctionParams(func);
	size_t count = 0;
	for (Stmnt* param : *params)
	{
		if (param->definition.assignment) return count;
		count += 1;
	}

	return count;
}

inline size_t RequiredGenericsCount(Stmnt* generics)
{
	size_t size = generics->generics.defaultValues->size();
	for (size_t i = 0; i < size; i++)
	{
		Expr* defaultValue = generics->generics.defaultValues->at(i);
		if (defaultValue) return i;
	}

	return size;
}

inline Type* GetReturnType(Stmnt* node)
{
	switch (node->nodeID)
	{
	case StmntID::FunctionStmnt:
		return node->function.returnType;
	case StmntID::Method:
		return node->method.returnType;
	case StmntID::StateOperator:
		return node->stateOperator.returnType;
	case StmntID::AnonFunction:
		return node->anonFunction.returnType;
	case StmntID::CompileStmnt:
		return node->compileStmnt.returnType;
	case StmntID::ExternFunctionDecl:
		return node->externFunction.returnType;
	default:
		break;
	}

	return nullptr;
}

inline Stmnt* GetGenerics(Stmnt* node)
{
	switch (node->nodeID)
	{
	case FunctionStmnt:
		return node->function.generics;
	case StateStmnt:
		return node->state.generics;
	case Method:
		return node->method.generics;
	default:
		return nullptr;
	}
}

inline bool IsStateFunction(Stmnt* stmnt)
{
	switch (stmnt->nodeID)
	{
	case Method:
	case StateOperator:
	case Destructor:
	case Constructor:
		return true;
	default:
		return false;
	}
}

inline Token* GetStateName(Stmnt* stmnt)
{
	switch (stmnt->nodeID)
	{
	case Method:
		return stmnt->method.stateName;
	case StateOperator:
		return stmnt->stateOperator.stateName;
	case Destructor:
		return stmnt->destructor.stateName;
	case Constructor:
		return stmnt->constructor.stateName;
	default:
		return nullptr;
	}
}

inline bool IsGeneric(Token* ident, Stmnt* stmnt)
{
	Stmnt* generics = GetGenerics(stmnt);
	if (!generics) return false;

	for (Token* gen : *generics->generics.names)
	{
		if (ident->val == gen->val) return true;
	}

	return false;
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

inline bool HasEnumMember(Stmnt* of, StringView& val)
{
	for (Token* name : *of->enumStmnt.names)
	{
		if (name->val == val) return true;
	}

	return false;
}

inline Stmnt* FindStateMethod(StateSymbol* of, const StringView& val)
{
	auto& methods = of->methods;
	for (Stmnt* node : methods)
	{
		if (node->method.name->val == val) return node;
	}

	return nullptr;
}

inline bool IsBooleanOperator(Token* op)
{
	switch (op->uniqueType) {
	case UniqueType::LogicOr:
	case UniqueType::LogicAnd:
	case UniqueType::Equal:
	case UniqueType::NotEql:
	case UniqueType::Less:
	case UniqueType::Greater:
	case UniqueType::LessEqual:
	case UniqueType::GreaterEqual:
		return true;
	default:
		return false;
	}
}

struct SymbolTable
{
	Token* package;
	eastl::vector<Stmnt*> imports;
	eastl::hash_map<StringView, StateSymbol, StringViewHash> stateMap;
	eastl::hash_map<StringView, Stmnt*, StringViewHash> enumMap;
	eastl::hash_map<StringView, Stmnt*, StringViewHash> functionMap;
	eastl::vector<Stmnt*> globalVals;
	eastl::hash_map<StringView, size_t, StringViewHash> globalValMap;
	eastl::hash_map<StringView, Stmnt*, StringViewHash> externFunctionMap;
	eastl::vector<Stmnt*> onCompiles;
	Arena* arena;

	SymbolTable(size_t initialSize)
	{
		package = nullptr;
		arena = new Arena(initialSize);
	}

	~SymbolTable()
	{
		delete arena;
	}

	inline size_t GetSize()
	{
		return imports.size() + stateMap.size() + functionMap.size() 
			+ globalValMap.size() + onCompiles.size();
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

		for (Stmnt* var : globalVals)
		{
			toPrint += ToString(var);
			toPrint += '\n';
		}

		for (auto& [key, value] : enumMap)
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
			imports.push_back(import);
		}

		for (auto& [key, value] : toMerge->stateMap)
		{
			if (value.state) AddState(value.state);
			for (Stmnt* cons : value.constructors) AddConstructor(cons);
			for (Stmnt* method : value.methods) AddMethod(method);
			for (Stmnt* op : value.operators) AddOperator(op);
		}

		for (auto& [key, value] : toMerge->enumMap)
		{
			AddEnum(value);
		}

		for (auto& [key, value] : toMerge->functionMap)
		{
			AddFunction(value);
		}

		for (Stmnt* value : toMerge->globalVals)
		{
			AddGlobalVal(value);
		}

		for (auto& [key, value] : toMerge->externFunctionMap)
		{
			AddExternFunc(value);
		}

		for (Stmnt* compile : toMerge->onCompiles) AddOnCompile(compile);
	}

	void SetGenericThis(StateSymbol& stateSymbol)
	{
		Type* templated = CreateTypePtr(TypeID::TemplatedType);
		eastl::vector<Expr*>* generics = CreateVectorPtr<Expr>();
		for (Token* gen : *stateSymbol.state->state.generics->generics.names)
		{
			Expr* genExpr = CreateExpr(gen, ExprID::IdentifierExpr);
			genExpr->identifierExpr.identifier = gen;
			generics->push_back(genExpr);
		}

		Expr* templates = CreateExpr(
			generics->at(0)->identifierExpr.identifier, ExprID::TemplateExpr);
		templates->templateExpr.expr = nullptr;
		templates->templateExpr.templateArgs = generics;

		Type* stateType = CreateTypePtr(TypeID::ImportedType);
		stateType->importedType.packageName = stateSymbol.state->package;
		stateType->importedType.typeName = stateSymbol.state->state.name;

		templated->templatedType.templates = templates;
		templated->templatedType.type = stateType;

		for (Stmnt* constructor : stateSymbol.constructors)
		{
			eastl::vector<Stmnt*>* params = constructor->constructor.decl->functionDecl.parameters;
			params->at(0)->definition.type = templated;
		}

		for (Stmnt* method : stateSymbol.methods)
		{
			eastl::vector<Stmnt*>* params = method->method.decl->functionDecl.parameters;
			params->at(0)->definition.type = templated;
		}

		for (Stmnt* op : stateSymbol.operators)
		{
			eastl::vector<Stmnt*>* params = op->stateOperator.decl->functionDecl.parameters;
			params->at(0)->definition.type = templated;
		}
	}

	void Finalize()
	{
		for (auto& [key, stateSymbol] : stateMap)
		{
			Stmnt* state = stateSymbol.state;
			if (state && state->state.generics)
			{
				SetGenericThis(stateSymbol);
			}
		}
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
	inline eastl::vector<T*>* CreateVectorPtr()
	{
		return arena->EmplaceContainer<eastl::vector<T*>>();
	}

	template<typename T>
	inline eastl::vector<T>* CreateVector()
	{
		return arena->EmplaceContainer<eastl::vector<T>>();
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

	void AddImport(Stmnt* import)
	{
		imports.push_back(import);
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

	void AddEnum(Stmnt* enumStmnt)
	{
		auto& name = enumStmnt->enumStmnt.name->val;
		if (enumMap.find(name) != enumMap.end())
			AddError(enumStmnt->start, "SymbolTable:AddEnum enum name already declared for: " + name);
		enumMap[name] = enumStmnt;
	}

	void AddFunction(Stmnt* function)
	{
		auto& name = function->function.name->val;
		if (functionMap.find(name) != functionMap.end())
			AddError(function->start, "SymbolTable:AddFunction Function name already declared for: " + name);
		functionMap[name] = function;
	}

	void AddGlobalVal(Stmnt* globalVal)
	{
		auto& name = globalVal->definition.name->val;
		if (globalValMap.find(name) != globalValMap.end())
			AddError(globalVal->start, "SymbolTable:AddGlobalVal Package scoped values with name already declared");

		globalVals.push_back(globalVal);
		globalValMap[name] = globalVals.size() - 1;
	}

	void AddOnCompile(Stmnt* compile)
	{
		onCompiles.push_back(compile);
	}

	void AddExternFunc(Stmnt* externFunc)
	{
		auto& name = externFunc->externFunction.callName->val;
		if (externFunctionMap.find(name) != externFunctionMap.end())
			AddError(externFunc->start, "SymbolTable:AddExternFunc External function declaration with call name already exists");
		externFunctionMap[name] = externFunc;
	}

	inline StateSymbol* FindStateSymbol(StringView& val)
	{
		if (auto entry = stateMap.find(val); entry != stateMap.end())
		{
			return &entry->second;
		}

		return nullptr;
	}

	inline Stmnt* FindStatement(StringView& val)
	{
		Stmnt* found = FindState(val); 
		if (!found) found = FindFunction(val);
		if (!found) found = FindGlobalVariable(val);
		if (!found) found = FindEnum(val);
		if (!found) found = FindExternalFunction(val);
		return found;
	}

	inline Stmnt* FindState(StringView& val)
	{
		if (auto entry = stateMap.find(val); entry != stateMap.end())
		{
			return entry->second.state;
		}

		return nullptr;
	}

	inline Stmnt* FindEnum(StringView& val)
	{
		if (auto entry = enumMap.find(val); entry != enumMap.end())
		{
			return entry->second;
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

	inline Stmnt* FindGlobalVariable(StringView& val)
	{
		if (auto entry = globalValMap.find(val); entry != globalValMap.end())
		{
			return globalVals[entry->second];
		}

		return nullptr;
	}

	inline Stmnt* FindExternalFunction(StringView& val)
	{
		if (auto entry = externFunctionMap.find(val); entry != externFunctionMap.end())
		{
			return entry->second;
		}

		return nullptr;
	}

	Expr* TypeToExpr(Type* type)
	{
		switch (type->typeID)
		{
		case NamedType:
		{
			Expr* identExpr = CreateExpr(type->namedType.typeName, ExprID::IdentifierExpr);
			identExpr->identifierExpr.identifier = type->namedType.typeName;
			return identExpr;
		}
		case ImportedType:
		{
			Expr* selectExpr = CreateExpr(type->importedType.packageName, ExprID::SelectorExpr);
			Expr* identLeft = CreateExpr(type->importedType.packageName, ExprID::IdentifierExpr);
			Expr* identRight = CreateExpr(type->importedType.typeName, ExprID::IdentifierExpr);

			identLeft->identifierExpr.identifier = type->importedType.packageName;
			identRight->identifierExpr.identifier = type->importedType.typeName;

			selectExpr->selectorExpr.on = identLeft;
			selectExpr->selectorExpr.select = identRight;
			return selectExpr;
		}
		case TemplatedType:
		{
			Expr* templatedExpr = TypeToExpr(type->templatedType.type);
			Expr* templatesExpr = CreateExpr(templatedExpr->start, TemplateExpr);
			templatesExpr->templateExpr.expr = templatedExpr;
			templatesExpr->templateExpr.templateArgs = type->templatedType.templates->templateExpr.templateArgs;
			return templatesExpr;
		}
		default:
			break;
		}

		return nullptr;
	}

	Expr* TypeExprToExpr(Expr* expr)
	{
		if (expr->typeID != ExprID::TypeExpr) return expr;

		Type* type = expr->typeExpr.type;
		return TypeToExpr(type);
	}

	Type* CloneType(Type* type)
	{
		if (!type) return type;
		Type* cloned = CreateTypePtr(TypeID::InvalidType);
		*cloned = *type;

		switch (type->typeID)
		{
		case ExplicitType:
		{
			cloned->explicitType.declarations = CreateVectorPtr<Stmnt>();
			for (Stmnt* decl : *type->explicitType.declarations)
			{
				cloned->explicitType.declarations->push_back(CloneStmnt(decl));
			}
			break;
		}
		case ImplicitType:
		{
			cloned->implicitType.identifiers = CreateVectorPtr<Token>();
			for (Token* tok : *type->implicitType.identifiers)
			{
				cloned->implicitType.identifiers->push_back(tok);
			}
			break;
		}
		case PointerType:
			cloned->pointerType.type = CloneType(type->pointerType.type);
			break;
		case ValueType:
			cloned->valueType.type = CloneType(type->valueType.type);
			break;
		case ArrayType:
			cloned->arrayType.type = CloneType(type->arrayType.type);
			cloned->arrayType.size = CloneExpr(type->arrayType.size);
			break;
		case TemplatedType:
			cloned->templatedType.templates = CloneExpr(type->templatedType.templates);
			cloned->templatedType.type = CloneType(type->templatedType.type);
			break;
		case FunctionType:
		{
			cloned->functionType.returnType = CloneType(type->functionType.returnType);
			cloned->functionType.paramTypes = CreateVectorPtr<Type>();
			for (Type* param : *type->functionType.paramTypes)
			{
				cloned->functionType.paramTypes->push_back(CloneType(param));
			}
			break;
		}
		case UnionType:
		{
			cloned->unionType.declarations = CreateVectorPtr<Stmnt>();
			for (Stmnt* decl : *type->unionType.declarations)
			{
				cloned->unionType.declarations->push_back(CloneStmnt(decl));
			}
			break;
		}
		case AnonymousType:
		{
			cloned->anonType.types = CreateVectorPtr<Type>();
			for (Type* anonType : *type->anonType.types)
			{
				cloned->anonType.types->push_back(CloneType(anonType));
			}
			break;
		}
		default:
			break;
		}

		return cloned;
	}

	Stmnt* CloneStmnt(Stmnt* stmnt)
	{
		if (!stmnt) return stmnt;
		Stmnt* cloned = CreateStmnt(stmnt->start, StmntID::InvalidStmnt, stmnt->package, stmnt->scope);
		*cloned = *stmnt;

		switch (stmnt->nodeID)
		{
		case ExpressionStmnt:
			cloned->expressionStmnt.expression = CloneExpr(stmnt->expressionStmnt.expression);
			break;
		case Definition:
			cloned->definition.type = CloneType(stmnt->definition.type);
			cloned->definition.assignment = CloneExpr(stmnt->definition.assignment);
			break;
		case InlineDefinition:
			cloned->inlineDefinition.type = CloneType(stmnt->inlineDefinition.type);
			cloned->inlineDefinition.assignment = CloneExpr(stmnt->inlineDefinition.assignment);
			break;
		case ExternFunctionDecl:
		{
			cloned->externFunction.returnType = CloneType(stmnt->externFunction.returnType);
			cloned->externFunction.parameters = CreateVectorPtr<Stmnt>();
			cloned->externFunction.links = CreateVectorPtr<Stmnt>();

			for (Stmnt* param : *stmnt->externFunction.parameters)
			{
				cloned->externFunction.parameters->push_back(CloneStmnt(param));
			}

			for (Stmnt* link : *stmnt->externFunction.links)
			{
				cloned->externFunction.links->push_back(CloneStmnt(link));
			}

			break;
		}
		case FunctionStmnt:
		{
			cloned->function.returnType = CloneType(stmnt->function.returnType);
			cloned->function.generics = CloneStmnt(stmnt->function.generics);
			cloned->function.decl = CloneStmnt(stmnt->function.decl);
			break;
		}
		case AnonFunction:
			cloned->anonFunction.returnType = CloneType(stmnt->anonFunction.returnType);
			cloned->anonFunction.decl = CloneStmnt(stmnt->anonFunction.decl);
			break;
		case FunctionDecl:
		{
			cloned->functionDecl.parameters = CreateVectorPtr<Stmnt>();
			for (Stmnt* param : *stmnt->functionDecl.parameters)
			{
				cloned->functionDecl.parameters->push_back(CloneStmnt(param));
			}
			break;
		}
		case StateStmnt:
		{
			cloned->state.generics = CloneStmnt(stmnt->state.generics);
			cloned->state.members = CreateVectorPtr<Stmnt>();
			for (Stmnt* member : *stmnt->state.members)
			{
				cloned->state.members->push_back(CloneStmnt(member));
			}
			break;
		}
		case GenericsDecl:
		{
			cloned->generics.defaultValues = CreateVectorPtr<Expr>();
			for (Expr* value : *stmnt->generics.defaultValues)
			{
				cloned->generics.defaultValues->push_back(CloneExpr(value));
			}
			cloned->generics.whereStmnt = CloneStmnt(cloned->generics.whereStmnt);
			break;
		}
		case WhereStmnt:
			cloned->whereStmnt.decl = CloneStmnt(stmnt->whereStmnt.decl);
			break;
		case Method:
			cloned->method.returnType = CloneType(stmnt->method.returnType);
			cloned->method.generics = CloneStmnt(stmnt->method.generics);
			cloned->method.decl = CloneStmnt(stmnt->method.decl);
			break;
		case StateOperator:
			cloned->stateOperator.returnType = CloneType(stmnt->stateOperator.returnType);
			cloned->stateOperator.decl = CloneStmnt(stmnt->stateOperator.decl);
			break;
		case Destructor:
			cloned->destructor.decl = CloneStmnt(stmnt->destructor.decl);
			break;
		case Constructor:
			cloned->constructor.decl = CloneStmnt(stmnt->constructor.decl);
			break;
		case EnumStmnt:
		{
			cloned->enumStmnt.type = CloneType(stmnt->enumStmnt.type);
			cloned->enumStmnt.valueExprs = CreateVectorPtr<Expr>();
			for (Expr* value : *stmnt->enumStmnt.valueExprs)
			{
				cloned->enumStmnt.valueExprs->push_back(CloneExpr(value));
			}
			break;
		}
		case Conditional:
			cloned->conditional.condition = CloneExpr(stmnt->conditional.condition);
			break;
		case AssignmentStmnt:
			cloned->assignmentStmnt.assignTo = CloneExpr(stmnt->assignmentStmnt.assignTo);
			cloned->assignmentStmnt.assignment = CloneExpr(stmnt->assignmentStmnt.assignment);
			break;
		case IfStmnt:
		{
			cloned->ifStmnt.condition = CloneStmnt(stmnt->ifStmnt.condition);
			cloned->ifStmnt.elifs = CreateVectorPtr<Stmnt>();
			for (Stmnt* elif : *stmnt->ifStmnt.elifs)
			{
				cloned->ifStmnt.elifs->push_back(CloneStmnt(elif));
			}			
			break;
		}
		case ForStmnt:
		{
			cloned->forStmnt.toIterate = CloneExpr(stmnt->forStmnt.toIterate);
			if (stmnt->forStmnt.isDeclaration)
			{
				cloned->forStmnt.iterated.declaration = CloneStmnt(stmnt->forStmnt.iterated.declaration);
			}
			break;
		}
		case WhileStmnt:
			cloned->whileStmnt.conditional = CloneStmnt(stmnt->whileStmnt.conditional);
			break;
		case SwitchStmnt:
		{
			cloned->switchStmnt.switchOn = CloneExpr(stmnt->switchStmnt.switchOn);
			cloned->switchStmnt.cases = CreateVectorPtr<Stmnt>();
			for (Stmnt* case_ : *stmnt->switchStmnt.cases)
			{
				cloned->switchStmnt.cases->push_back(CloneStmnt(case_));
			}
			break;
		}
		case DeleteStmnt:
			cloned->deleteStmnt.primaryExpr = CloneExpr(stmnt->deleteStmnt.primaryExpr);
			break;
		case DeferStmnt:
		{
			if (stmnt->deferStmnt.deferIf)
			{
				cloned->deferStmnt.conditional = CloneStmnt(stmnt->deferStmnt.conditional);
			}
			break;
		}
		case ReturnStmnt:
			cloned->returnStmnt.expr = CloneExpr(stmnt->returnStmnt.expr);
			break;
		case CompileStmnt:
			cloned->compileStmnt.returnType = CloneType(stmnt->compileStmnt.returnType);
			break;
		case LogStmnt:
		{
			cloned->logStmnt.exprs = CreateVectorPtr<Expr>();
			for (Expr* expr : *stmnt->logStmnt.exprs)
			{
				cloned->logStmnt.exprs->push_back(CloneExpr(expr));
			}
			break;
		}
		case AssertStmnt:
			cloned->assertStmnt.expr = CloneExpr(stmnt->assertStmnt.expr);
			cloned->assertStmnt.message = CloneExpr(stmnt->assertStmnt.message);
			break;
		default:
			break;
		}

		return cloned;
	}

	Expr* CloneExpr(Expr* expr)
	{
		if (!expr) return expr;
		Expr* cloned = CreateExpr(expr->start, ExprID::InvalidExpr);
		*cloned = *expr;

		switch (expr->typeID)
		{
		case SelectorExpr:
			cloned->selectorExpr.on = CloneExpr(expr->selectorExpr.on);
			cloned->selectorExpr.select = CloneExpr(expr->selectorExpr.select);
			break;
		case IndexExpr:
			cloned->indexExpr.of = CloneExpr(expr->indexExpr.of);
			cloned->indexExpr.index = CloneExpr(expr->indexExpr.index);
			break;
		case FunctionCallExpr:
		{
			cloned->functionCallExpr.function = CloneExpr(expr->functionCallExpr.function);
			cloned->functionCallExpr.params = CreateVectorPtr<Expr>();
			for (Expr* param : *expr->functionCallExpr.params)
			{
				cloned->functionCallExpr.params->push_back(CloneExpr(param));
			}
			break;
		}
		case NewExpr:
			cloned->newExpr.primaryExpr = CloneExpr(expr->newExpr.primaryExpr);
			cloned->newExpr.atExpr = CloneExpr(expr->newExpr.atExpr);
			break;
		case FixedExpr:
			cloned->fixedExpr.atExpr = CloneExpr(expr->fixedExpr.atExpr);
			break;
		case TypeLiteralExpr:
		{
			cloned->typeLiteralExpr.values = CreateVectorPtr<Expr>();
			for (Expr* value : *expr->typeLiteralExpr.values)
			{
				cloned->typeLiteralExpr.values->push_back(CloneExpr(value));
			}
			break;
		}
		case ExplicitTypeExpr:
		{
			cloned->explicitTypeExpr.values = CreateVectorPtr<Stmnt>();
			for (Stmnt* value : *expr->explicitTypeExpr.values)
			{
				cloned->explicitTypeExpr.values->push_back(CloneStmnt(value));
			}
			break;
		}
		case AsExpr:
			cloned->asExpr.of = CloneExpr(expr->asExpr.of);
			cloned->asExpr.to = CloneType(expr->asExpr.to);
			break;
		case DereferenceExpr:
			cloned->dereferenceExpr.of = CloneExpr(expr->dereferenceExpr.of);
			break;
		case ReferenceExpr:
			cloned->referenceExpr.of = CloneExpr(expr->referenceExpr.of);
			break;
		case BinaryExpr:
			cloned->binaryExpr.left = CloneExpr(expr->binaryExpr.left);
			cloned->binaryExpr.right = CloneExpr(expr->binaryExpr.right);
			break;
		case UnaryExpr:
			cloned->unaryExpr.expr = CloneExpr(expr->unaryExpr.expr);
			break;
		case GroupedExpr:
			cloned->groupedExpr.expr = CloneExpr(expr->groupedExpr.expr);
			break;
		case TemplateExpr:
		{
			cloned->templateExpr.expr = CloneExpr(expr->templateExpr.expr);
			cloned->templateExpr.templateArgs = CreateVectorPtr<Expr>();
			for(Expr* arg : *expr->templateExpr.templateArgs)
			{ 
				cloned->templateExpr.templateArgs->push_back(CloneExpr(arg));
			}
			break;
		}
		case TypeExpr:
			cloned->typeExpr.type = CloneType(expr->typeExpr.type);
			break;
		case FunctionTypeDeclExpr:
			cloned->functionTypeDeclExpr.anonFunction = CloneStmnt(expr->functionTypeDeclExpr.anonFunction);
			break;
		case CompileExpr:
			cloned->compileExpr.compile = CloneStmnt(expr->compileExpr.compile);
			break;
		case SizeOfExpr:
			cloned->sizeOfExpr.expr = CloneExpr(expr->sizeOfExpr.expr);
			break;
		case AlignOfExpr:
			cloned->alignOfExpr.expr = CloneExpr(expr->alignOfExpr.expr);
			break;
		case TypeOfExpr:
			cloned->typeOfExpr.expr = CloneExpr(expr->typeOfExpr.expr);
			break;
		default:
			break;
		}

		return cloned;
	}

	Expr* CreateIntLiteralExpr(intmax_t i, Token* start)
	{
		Expr* expr = CreateExpr(start, ExprID::LiteralExpr);
		eastl::string* val = arena->Emplace<eastl::string>(eastl::to_string(i));
		Token* token = arena->Emplace<Token>();
		token->pos = start->pos;
		token->uniqueType = UniqueType::IntLiteral;
		token->type = TokenType::Literal;
		token->val = StringView(val->c_str());
		expr->literalExpr.val = token;
		return expr;
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
			type->primitiveType.size = 1;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Ubyte:
			type->primitiveType.size = 1;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Int:
			type->primitiveType.size = config.targetArchByteWidth;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int16:
			type->primitiveType.size = 2;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int32:
			type->primitiveType.size = 4;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int64:
			type->primitiveType.size = 8;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Int128:
			type->primitiveType.size = 16;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Uint:
			type->primitiveType.size = config.targetArchByteWidth;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint16:
			type->primitiveType.size = 2;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint32:
			type->primitiveType.size = 4;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint64:
			type->primitiveType.size = 8;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Uint128:
			type->primitiveType.size = 16;
			type->primitiveType.isSigned = false;
			break;
		case UniqueType::Float:
			type->primitiveType.size = config.targetArchByteWidth;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Float32:
			type->primitiveType.size = 4;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::Float64:
			type->primitiveType.size = 8;
			type->primitiveType.isSigned = true;
			break;
		case UniqueType::String:
			type->primitiveType.size = config.targetArchByteWidth * 2;
			type->primitiveType.isSigned = true;
			break;
		default:
			break;
		}

		return type;
	}
};