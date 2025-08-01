#pragma once
#include "EASTL/tuple.h"

#include "../Syntax/GlobalTable.h"
#include "../IR/IR.h"
#include "LowerUtils.h"
#include "LowerDefinitions.h"
#include "LowerContext.h"

struct LowerDeclarations
{
	LowerContext& context;
	eastl::hash_map<eastl::vector<Stmnt*>*, eastl::vector<SpiteIR::PlatformLib>*> linkMap;
	SymbolTable* symbolTable;
	ScopeUtils scopeUtils;

	LowerDeclarations(LowerContext& context) : context(context), scopeUtils(context.globalTable, nullptr)
	{}

	ScopeUtils& GetScopeUtils()
	{
		return scopeUtils;
	}

	size_t GetSizeOf(Expr* expr, eastl::vector<Token*>* generics = nullptr,
		eastl::vector<Expr*>* templates = nullptr, Stmnt* stmnt = nullptr)
	{
		if (expr->sizeOfExpr.expr->typeID == ExprID::TypeExpr)
		{
			SpiteIR::Type* type = TypeToIRType(context.ir, expr->sizeOfExpr.expr->typeExpr.type,
				this, generics, templates);
			return type->size;
		}

		if (expr->sizeOfExpr.expr->typeID == ExprID::IdentifierExpr)
		{
			StringView& ident = expr->sizeOfExpr.expr->identifierExpr.identifier->val;
			Stmnt* def = scopeUtils.FindInScope(ident);
			if (def)
			{
				SpiteIR::Type* type = TypeToIRType(context.ir, def->definition.type, this, 
					generics, templates);
				return type->size;
			}
		}

		return 0;
	}

	size_t GetAlignOf(Expr* expr, eastl::vector<Token*>* generics = nullptr,
		eastl::vector<Expr*>* templates = nullptr, Stmnt* stmnt = nullptr)
	{
		if (expr->alignOfExpr.expr->typeID == ExprID::TypeExpr)
		{
			SpiteIR::Type* type = TypeToIRType(context.ir, expr->alignOfExpr.expr->typeExpr.type,
				this, generics, templates);
			return type->alignment;
		}

		if (expr->alignOfExpr.expr->typeID == ExprID::IdentifierExpr)
		{
			StringView& ident = expr->alignOfExpr.expr->identifierExpr.identifier->val;
			Stmnt* def = scopeUtils.FindInScope(ident);
			if (def)
			{
				SpiteIR::Type* type = TypeToIRType(context.ir, def->definition.type, this,
					generics, templates);
				return type->alignment;
			}
		}

		return 0;
	}

	void BuildDeclarations()
	{
		for (auto& [key, value] : context.globalTable->packageToSymbolTable)
		{
			BuildPackageDeclarations(value);
		}
	}

	void ResolveStructuredTypeAndAlign(SpiteIR::Type* type)
	{

		for (SpiteIR::Member* member : *type->structureType.members)
		{
			SpiteIR::Type* memberType = member->value.type;
			if (!memberType->alignment) ResolveTypeSizeAndAlign(memberType);
		}

		if (type->kind == SpiteIR::TypeKind::UnionType)
		{
			for (SpiteIR::Member* member : *type->structureType.members)
			{
				SpiteIR::Type* memberType = member->value.type;
				if (memberType->size > type->size) type->size = memberType->size;
				if (memberType->alignment > type->alignment) type->alignment = memberType->alignment;
			}
		}
		else
		{
			SetStructuredTypeSizeAndAlign(type, this);
		}
	}

	void ResolveTypeSizeAndAlign(SpiteIR::Type* type)
	{
		if (type->kind == SpiteIR::TypeKind::StructureType || type->kind == SpiteIR::TypeKind::UnionType)
		{
			ResolveStructuredTypeAndAlign(type);
		}
		else if (type->kind == SpiteIR::TypeKind::StateType)
		{
			BuildStateSize(type->stateType.state);
			type->size = type->stateType.state->size;
			type->alignment = type->stateType.state->alignment;
			type->byValue = type->stateType.state->IsValueType();
		}
		else if (type->kind == SpiteIR::TypeKind::FixedArrayType)
		{
			SpiteIR::Type* innerType = type->fixedArray.type;
			ResolveTypeSizeAndAlign(innerType);
			type->size = (innerType->size * type->fixedArray.count);
			type->alignment = innerType->alignment;
		}
	}

	void Resolve()
	{
		while (context.toResolveStateType.size() > 0)
		{
			eastl::tuple<eastl::string, SpiteIR::Type*> val = context.toResolveStateType.back();
			eastl::string& typeName = eastl::get<0>(val);
			SpiteIR::Type* type = eastl::get<1>(val);
			type->stateType.state = FindState(this, typeName, type, false);
			if (!type->stateType.state)
			{
				Logger::FatalError("LowerDeclarations:Resolve Unable to resolve state type: " + typeName);
			}
			context.toResolveStateType.pop_back();
		}

		for (SpiteIR::Type* type : context.toResolveSizeAndAlignment)
		{
			ResolveTypeSizeAndAlign(type);
		}

		for (SpiteIR::Package* package : context.ir->packages)
		{
			for (auto& [key, state] : package->states)
			{
				BuildStateSize(state);
			}
		}

		for (SpiteIR::Package* package : context.ir->packages)
		{
			SymbolTable* symbolTable = context.packageToSymbolTableMap[package];
			for (Stmnt* value : symbolTable->globalVals)
			{
				BuildGlobalVariableDeclaration(package, value);
			}
		}
	}

	SpiteIR::Package* BuildPackageDeclarations(SymbolTable* symbolTable)
	{
		StringView& packageName = symbolTable->package->val;
		if (MapHas(context.packageMap, packageName)) return context.packageMap[packageName];

		SpiteIR::Package* package = context.ir->AddPackage();

		package->file = *symbolTable->package->pos.file;
		package->name = BuildPackageName(symbolTable->package);
		package->parent = context.ir;

		context.packageToSymbolTableMap[package] = symbolTable;
		context.packageMap[packageName] = package;

		for (Stmnt* key : symbolTable->imports)
		{
			SymbolTable* symbolTable = context.globalTable->FindSymbolTable(key->importStmnt.packageName->val);
			SpiteIR::Package* imported = BuildPackageDeclarations(symbolTable);
			package->imports.push_back(imported);
		}

		Logger::Debug("LowerDeclarations:BuildPackageDeclarations Lowering declarations for package: " + packageName);
		this->symbolTable = symbolTable;
		scopeUtils.symbolTable = symbolTable;

		scopeUtils.AddScope();

		for (Stmnt* globalVal : symbolTable->globalVals)
		{
			auto& definition = globalVal->definition;
			StringView& name = definition.name->val;
			scopeUtils.AddToTopScope(name, globalVal);
		}

		for (auto& [key, enumStmnt] : symbolTable->enumMap)
		{
			BuildEnumDeclaration(enumStmnt);
		}

		for (auto& [key, value] : symbolTable->stateMap)
		{
			BuildStateDeclarations(package, value);
		}

		for (auto& [key, value] : symbolTable->functionMap)
		{
			BuildFunctionDeclaration(package, value);
		}

		for (auto& [key, value] : symbolTable->externFunctionMap)
		{
			BuildExternalFunctionDeclarations(package, value);
		}

		for (auto& [key, value] : symbolTable->functionMap)
		{
			BuildFunctionDeclaration(package, value);
		}

		for (auto& [key, value] : symbolTable->externFunctionMap)
		{
			BuildExternalFunctionDeclarations(package, value);
		}

		scopeUtils.PopScope();

		return package;
	}

	void BuildEnumDeclaration(Stmnt* enumStmnt)
	{

		eastl::vector<Expr*>* values = enumStmnt->enumStmnt.valueExprs;
		eastl::hash_map<StringView, intmax_t, StringViewHash>& enumValues = context.enumMap[enumStmnt];

		if (!IsInt(enumStmnt->enumStmnt.type))
		{
			AddError(enumStmnt->start, "Checker:CheckEnum: Enums can only be of integer types");
		}

		intmax_t curr = 0;
		for (size_t i = 0; i < enumStmnt->enumStmnt.names->size(); i++)
		{
			StringView& name = enumStmnt->enumStmnt.names->at(i)->val;
			Expr* expr = enumStmnt->enumStmnt.valueExprs->at(i);
			if (expr)
			{
				if (!IsConstantIntExpr(expr, scopeUtils))
				{
					AddError(enumStmnt->start, "Checker:CheckEnum: Enum member values must evalute to a constant literal integer type");
				}
				else
				{
					curr = EvaluateConstantIntExpr(expr, this);
				}
			}

			enumValues[name] = curr;
			curr += 1;
		}
	}

	bool BuildStateSize(SpiteIR::State* state, SpiteIR::State* outer = nullptr)
	{
		if (state->size) return true;
		if (!outer) outer = state;
		else if (state == outer)
		{
			Logger::FatalError("LowerDeclarations:BuildStateSize Unable to calculate state size due to cyclical reference in: " + state->name);
		}

		for (SpiteIR::Member* member : state->members)
		{
			SpiteIR::Type* memberType = member->value.type;
			if (!memberType->alignment) ResolveTypeSizeAndAlign(memberType);
			if (memberType->kind == SpiteIR::TypeKind::StateType)
			{
				BuildStateSize(memberType->stateType.state, outer);
				memberType->size = memberType->stateType.state->size;
				memberType->alignment = memberType->stateType.state->alignment;
			}
		}

		SizeAndAlignment sa = CalculateSizeAndAlignForMembers(&state->members);

		if (!sa.alignment)
		{
			CalculateSizeAndAlignForMembers(&state->members);
			Logger::FatalError("Unable to resolve state size and alignment: " + state->name);
		}

		state->size = sa.size;
		state->alignment = sa.alignment;
		return state->alignment;
	}

	void BuildStateDeclarations(SpiteIR::Package* package, StateSymbol& stateSymbol)
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
		SpiteIR::State* state = EmplaceState(package, stateSymbol, stateStmnt, BuildStateName(stateStmnt));

		for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
		{
			Stmnt* memberStmnt = stateStmnt->state.members->at(i);
			BuildMemberForState(state, memberStmnt, i);
		}

		context.stateASTMap[state] = { stateStmnt, nullptr };
		BuildMethodsForState(package, stateSymbol, state);
	}

	void BuildGenericState(SpiteIR::Package* package, StateSymbol& stateSymbol)
	{
		Stmnt* stateStmnt = stateSymbol.state;
		auto& generics = stateStmnt->state.generics->generics;
		eastl::vector<Token*>* genericNames = generics.names;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			SpiteIR::State* state = EmplaceState(package, stateSymbol, stateStmnt,
				BuildTemplatedStateName(stateStmnt, templates));

			for (size_t i = 0; i < stateStmnt->state.members->size(); i++)
			{
				Stmnt* memberStmnt = stateStmnt->state.members->at(i);
				BuildMemberForState(state, memberStmnt, i, genericNames, templates);
			}

			context.stateASTMap[state] = { stateStmnt, templates };
			BuildMethodsForState(package, stateSymbol, state, genericNames, templates);
		}
	}

	SpiteIR::State* EmplaceState(SpiteIR::Package* package, StateSymbol& stateSymbol,
		Stmnt* stateStmnt, const eastl::string& name)
	{
		SpiteIR::State* state = context.ir->AllocateState();

		state->parent = package;
		state->name = name;
		state->flags = stateStmnt->state.insetFlags;

		package->states[state->name] = state;
		context.stateMap[state->name] = state;
		return state;
	}

	void BuildMemberForState(SpiteIR::State* state, Stmnt* memberStmnt, size_t index,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Member* member = context.ir->AllocateMember();
		member->value.type = TypeToIRType(context.ir, memberStmnt->definition.type, this, generics, templates);
		member->value.name = memberStmnt->definition.name->val.ToString();
		state->members.push_back(member);
	}

	void BuildMethodsForState(SpiteIR::Package* package, StateSymbol& stateSymbol, SpiteIR::State* state,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		for (Stmnt* con : stateSymbol.constructors)
		{
			BuildConstructorDeclaration(package, state, con, generics, templates);
		}
		BuildDefaultConstructor(package, state, stateSymbol.state, templates);


		for (Stmnt* method : stateSymbol.methods)
		{
			BuildMethodDeclaration(package, state, method, generics, templates);
		}

		for (Stmnt* op : stateSymbol.operators)
		{
			BuildOperatorDeclaration(package, state, op, generics, templates);
		}

		if (stateSymbol.destructor)
		{
			BuildDestructor(package, state, stateSymbol.destructor, templates);
		}
	}

	void BuildMethodThisArgument(SpiteIR::State* state, SpiteIR::Function* method, SpiteIR::IR* ir)
	{
		SpiteIR::Argument* arg = ir->AllocateArgument();
		arg->value.name = "this";
		arg->parent = method;

		SpiteIR::Type* thisType = ir->AllocateType();
		thisType->kind = SpiteIR::TypeKind::StateType;
		thisType->stateType.state = state;
		if (state->alignment)
		{
			thisType->size = state->size;
			thisType->alignment = state->alignment;
		}
		else context.toResolveSizeAndAlignment.insert(thisType);
		arg->value.type = MakeReferenceType(thisType, context.ir);

		method->arguments.push_back(arg);
	}

	void BuildDefaultConstructor(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* stateStmnt,
		eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* con = context.ir->AllocateFunction(package);
		con->name = BuildDefaultConstructorName(stateStmnt, templates);
		con->returnType = CreateVoidType(context.ir);
		SetConstructorFlag(con);

		BuildMethodThisArgument(state, con, context.ir);
		state->defaultConstructor = con;
		context.functionMap[con->name] = con;
		package->functions[con->name] = con;
		context.functionASTMap[con] = { stateStmnt, templates };
	}

	void BuildConstructorDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* conStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* con = context.ir->AllocateFunction(package);
		con->name = BuildConstructorName(conStmnt, generics, templates);
		con->returnType = CreateVoidType(context.ir);
		SetConstructorFlag(con);
		SetInlineFlag(con, conStmnt);

		// First argument is this argument
		BuildMethodThisArgument(state, con, context.ir);
		for (size_t i = 1; i < conStmnt->constructor.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = conStmnt->constructor.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(con, param, i, generics, templates);
		}

		state->constructors.push_back(con);
		context.functionMap[con->name] = con;
		context.functionASTMap[con] = { conStmnt, templates };
		package->functions[con->name] = con;
	}

	void BuildOperatorDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* opStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* op = context.ir->AllocateFunction(package);
		op->name = BuildOperatorMethodName(opStmnt, state->name, generics, templates);
		op->returnType = TypeToIRType(context.ir, opStmnt->stateOperator.returnType, this,
			generics, templates);
		SetMethodFlag(op);
		SetInlineFlag(op, opStmnt);

		// First argument is this argument
		BuildMethodThisArgument(state, op, context.ir);
		for (size_t i = 1; i < opStmnt->stateOperator.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = opStmnt->stateOperator.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(op, param, i, generics, templates);
		}

		eastl::string opStr = OperatorToString(opStmnt->stateOperator.op->uniqueType);
		state->operators[opStr].push_back(op);
		context.functionMap[op->name] = op;
		context.functionASTMap[op] = { opStmnt, templates };
		package->functions[op->name] = op;
	}

	void BuildMethodDeclaration(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* methodStmnt,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		if (methodStmnt->method.generics)
		{
			BuildGenericMethod(package, state, methodStmnt, generics, templates);
			return;
		}

		BuildMethod(package, state, methodStmnt, methodStmnt->method.decl,
			BuildMethodName(state, methodStmnt), generics, templates);
	}

	void BuildGenericMethod(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* methodStmnt,
		eastl::vector<Token*>* stateGenerics = nullptr, eastl::vector<Expr*>* stateTemplates = nullptr)
	{
		auto& generics = methodStmnt->method.generics->generics;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			eastl::vector<Token*>* stateAndMethodGenerics = symbolTable->CreateVectorPtr<Token>();
			if (stateGenerics) for (Token* stateGeneric : *stateGenerics) stateAndMethodGenerics->push_back(stateGeneric);
			for (Token* methodGeneric : *generics.names) stateAndMethodGenerics->push_back(methodGeneric);

			eastl::vector<Expr*>* stateAndMethodTemplates = symbolTable->CreateVectorPtr<Expr>();
			if (stateTemplates) for (Expr* stateTemplate : *stateTemplates) stateAndMethodTemplates->push_back(stateTemplate);
			for (Expr* methodTemplate : *templates) stateAndMethodTemplates->push_back(methodTemplate);

			BuildMethod(package, state, methodStmnt, methodStmnt->method.decl,
				BuildTemplatedMethodName(state, methodStmnt, templates),
				stateAndMethodGenerics, stateAndMethodTemplates);
		}
	}

	void BuildMethod(SpiteIR::Package* package, SpiteIR::State* state,
		Stmnt* methodStmnt, Stmnt* methodDeclStmnt, const eastl::string& name,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* method = context.ir->AllocateFunction(package);
		method->name = name;
		method->returnType = TypeToIRType(context.ir, methodStmnt->method.returnType, this, generics, templates);
		SetMethodFlag(method);
		SetInlineFlag(method, methodStmnt);

		// First argument is this argument
		BuildMethodThisArgument(state, method, context.ir);
		for (size_t i = 1; i < methodStmnt->method.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = methodStmnt->method.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(method, param, i, generics, templates);
		}

		state->methods.push_back(method);
		context.functionMap[method->name] = method;
		context.functionASTMap[method] = { methodStmnt, templates };
		package->functions[method->name] = method;
	}

	void BuildDestructor(SpiteIR::Package* package, SpiteIR::State* state, Stmnt* destructorStmnt,
		eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* destructor = context.ir->AllocateFunction(package);
		destructor->name = BuildDestructorName(state);
		destructor->returnType = CreateVoidType(context.ir);
		SetMethodFlag(destructor);

		// First argument is this argument
		BuildMethodThisArgument(state, destructor, context.ir);

		state->destructor = destructor;
		context.functionMap[destructor->name] = destructor;
		context.functionASTMap[destructor] = { destructorStmnt, templates };
		package->functions[destructor->name] = destructor;
	}

	void BuildFunctionDeclaration(SpiteIR::Package* package, Stmnt* funcStmnt)
	{
		if (funcStmnt->function.generics)
		{
			BuildGenericFunction(package, funcStmnt);
			return;
		}

		BuildFunction(package, funcStmnt, BuildFunctionName(funcStmnt));
	}

	void BuildGenericFunction(SpiteIR::Package* package, Stmnt* funcStmnt)
	{
		auto& generics = funcStmnt->function.generics->generics;

		for (eastl::vector<Expr*>* templates : *generics.templatesToExpand)
		{
			BuildFunction(package, funcStmnt, BuildTemplatedFunctionName(funcStmnt, templates),
				generics.names, templates);
		}
	}

	void BuildFunction(SpiteIR::Package* package, Stmnt* funcStmnt, const eastl::string& name,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{
		SpiteIR::Function* function = context.ir->AllocateFunction(package);
		function->name = name;
		function->returnType = TypeToIRType(context.ir, funcStmnt->function.returnType, this, generics, templates);
		SetInlineFlag(function, funcStmnt);

		for (size_t i = 0; i < funcStmnt->function.decl->functionDecl.parameters->size(); i++)
		{
			Stmnt* param = funcStmnt->function.decl->functionDecl.parameters->at(i);
			BuildArgumentForFunction(function, param, i, generics, templates);
		}

		context.functionMap[function->name] = function;
		context.functionASTMap[function] = { funcStmnt, templates };
		package->functions[function->name] = function;
	}

	void BuildArgumentForFunction(SpiteIR::Function* function, Stmnt* param, size_t index,
		eastl::vector<Token*>* generics = nullptr, eastl::vector<Expr*>* templates = nullptr)
	{

		SpiteIR::Type* argType = TypeToIRType(context.ir, param->definition.type, this, generics, templates);
		if (!argType->byValue) argType = MakeReferenceType(argType, context.ir);

		SpiteIR::Argument* arg = context.ir->AllocateArgument();
		arg->value.type = argType;
		arg->value.name = param->definition.name->val.ToString();
		arg->parent = function;
		function->arguments.push_back(arg);
	}

	void BuildGlobalVariableDeclaration(SpiteIR::Package* package, Stmnt* globalVarStmnt)
	{
		auto& def = globalVarStmnt->definition;
		SpiteIR::GlobalVariable* globalVar = context.ir->AllocateGlobalVariable();
		globalVar->parent = package;
		globalVar->name = BuildGlobalVariableName(globalVarStmnt);
		globalVar->type = TypeToIRType(context.ir, def.type, this);
		globalVar->index = context.ir->globalSize;
		context.ir->IncremementGlobalSize(globalVar->type);
		package->globalVariables.push_back(globalVar);
		package->globalVariableLookup[globalVar->name] = package->globalVariables.size() - 1;
		context.globalVarASTMap[globalVar] = globalVarStmnt;
	}

	eastl::vector<SpiteIR::PlatformLib>* GetPlatformLibs(eastl::vector<Stmnt*>* links)
	{
		if (MapHas(linkMap, links)) return linkMap[links];

		eastl::vector<SpiteIR::PlatformLib>* platformLibs = context.ir->AllocateArray<SpiteIR::PlatformLib>();

		for (Stmnt* link : *links)
		{
			platformLibs->push_back({ link->linkDecl.platform->val.ToString(),
				link->linkDecl.lib->val.ToString() });
		}

		linkMap[links] = platformLibs;
		return platformLibs;
	}

	void BuildExternalFunctionDeclarations(SpiteIR::Package* package, Stmnt* externFunc)
	{
		SpiteIR::Function* function = context.ir->AllocateFunction(package);
		function->name = externFunc->externFunction.callName->val.ToString();
		function->returnType = TypeToIRType(context.ir, externFunc->externFunction.returnType, this);

		for (size_t i = 0; i < externFunc->externFunction.parameters->size(); i++)
		{
			Stmnt* param = externFunc->externFunction.parameters->at(i);
			BuildArgumentForFunction(function, param, i);
		}

		function->metadata.externFunc = context.ir->AllocateExternFunction();
		function->metadata.externFunc->libs = GetPlatformLibs(externFunc->externFunction.links);
		function->metadata.externFunc->externName = externFunc->externFunction.externName->val.ToString();
		context.functionMap[function->name] = function;
		context.functionASTMap[function] = { externFunc, nullptr };
		package->functions[function->name] = function;
	}

	void SetInlineFlag(SpiteIR::Function* function, Stmnt* funcStmnt)
	{
		Stmnt* decl = GetDeclForFunc(funcStmnt);
		if (decl->functionDecl.body.statement)
		{
			function->metadata.flags |= SpiteIR::FunctionFlags::Inline;
		}
	}

	void SetMethodFlag(SpiteIR::Function* function)
	{
		function->metadata.flags |= SpiteIR::FunctionFlags::IsMethod;
	}

	void SetConstructorFlag(SpiteIR::Function* function)
	{
		function->metadata.flags |= SpiteIR::FunctionFlags::IsConstructor;
	}
};