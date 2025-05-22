#pragma once
#include "../Syntax/GlobalTable.h"
#include "DeclarationChecker.h"
#include "DefinitionChecker.h"
#include "DeferredChecker.h"
#include "GenericInference.h"

struct Checker
{
	GlobalTable* globalTable;
	DeferredContainer deferred;
	eastl::hash_set<SymbolTable*> checkedPackages;

	Checker(GlobalTable* globalTable)
	{
		this->globalTable = globalTable;
	}

	void Check()
	{
		DeclarationChecker declChecker = DeclarationChecker(globalTable, globalTable->runtimeTable, deferred);
		declChecker.Check();

		CheckPackageDeclarations(globalTable->entryTable);

		checkedPackages.clear();

		DefinitionChecker defChecker = DefinitionChecker(globalTable, globalTable->runtimeTable, deferred);
		defChecker.Check();

		CheckPackageDefinitions(globalTable->entryTable);
	
		CheckDeferred();
	}

	void CheckPackageDeclarations(SymbolTable* symbolTable)
	{
		if (MapHas(checkedPackages, symbolTable)) return;
		checkedPackages.insert(symbolTable);
		CheckImports(symbolTable, &Checker::CheckPackageDeclarations);
		DeclarationChecker declChecker = DeclarationChecker(globalTable, symbolTable, deferred);
		declChecker.Check();
	}

	void CheckPackageDefinitions(SymbolTable* symbolTable)
	{
		if (MapHas(checkedPackages, symbolTable)) return;
		checkedPackages.insert(symbolTable);
		CheckImports(symbolTable, &Checker::CheckPackageDefinitions);
		DefinitionChecker defChecker = DefinitionChecker(globalTable, symbolTable, deferred);
		defChecker.Check();
	}

	void CheckImports(SymbolTable* symbolTable, void(Checker::*check)(SymbolTable*))
	{
		for (Stmnt* key : symbolTable->imports)
		{
			CheckImport(key, check);
		}
	}

	void CheckImport(Stmnt* imported, void(Checker::*check)(SymbolTable*))
	{
		Token* packageName = imported->importStmnt.packageName;
		SymbolTable* table = globalTable->FindSymbolTable(packageName->val);
		if (!table)
		{
			AddError(packageName, "PackageChecker:CheckImport No package found with name: " + packageName->ToString());
			return;
		}

		(this->*check)(table);
	}

	void CheckDeferred()
	{
		ExpandDeferredTemplates();
		ExpandForwardedTemplates();
	}

	eastl::vector<Expr*>* CloneTemplateArgs(eastl::vector<Expr*>* args, Token* package)
	{
		eastl::vector<Expr*>* copy = globalTable->FindSymbolTable(package->val)->CreateVectorPtr<Expr>();
		for (Expr* expr : *args)
		{
			copy->push_back(globalTable->runtimeTable->CloneExpr(expr));
		}
		return copy;
	}

	void ExpandDeferredTemplates()
	{
		for (auto& [from, def] : deferred.deferredTemplates)
		{
			eastl::vector<Token*>* names = from->generics.names;
			auto toExpand = from->generics.templatesToExpand;
			for (auto& defInst : def)
			{
				Stmnt* to = defInst.forwardTo;
				eastl::vector<Expr*>* forwardedTemplates = defInst.templatesToForward;
				for (eastl::vector<Expr*>* replaceWith : *toExpand)
				{
					eastl::hash_set<Stmnt*> seen;
					ForwardTemplates(from, to, forwardedTemplates, replaceWith, seen);
				}
			}
		}
	}

	Stmnt* FindGenericsForToken(Stmnt* stmnt, Token* token)
	{
		Stmnt* generics = GetGenerics(stmnt);
		if (generics)
		{
			for (Token* name : *generics->generics.names)
			{
				if (name->val == token->val) return generics;
			}
		}

		if (IsStateFunction(stmnt))
		{
			SymbolTable* symbolTable = globalTable->FindSymbolTable(stmnt->package->val);
			Stmnt* stateStmnt = globalTable->FindStateForStmnt(stmnt, symbolTable);
			Stmnt* stateGenerics = GetGenerics(stateStmnt);
			if (stateGenerics)
			{
				for (Token* name : *stateGenerics->generics.names)
				{
					if (name->val == token->val) return stateGenerics;
				}
			}
		}

		return nullptr;
	}

	size_t FindGenericIndex(Stmnt* generics, Token* name)
	{
		for (size_t i = 0; i < generics->generics.names->size(); i++)
		{
			Token* genericName = generics->generics.names->at(i);
			if (genericName->val == name->val) return i;
		}

		return (size_t)-1;
	}

	bool TemplatesContainGeneric(eastl::vector<Expr*>* templates, Stmnt* generics)
	{
		for (Expr* templ : *templates)
		{
			Token* templTok = GetTokenForTemplate(templ);
			if (!templTok) continue;

			for (Token* genericName : *generics->generics.names)
			{
				if (templTok->val == genericName->val) return true;
			}
		}

		return false;
	}

	void ExpandForwardedTemplates()
	{
		for (auto& [from, def] : deferred.deferredForwardedTemplates)
		{
			SymbolTable* symbolTable = globalTable->FindSymbolTable(from->package->val);
			CheckerContext context = CheckerContext(globalTable, symbolTable);
			TypeInferer typeInferer = TypeInferer(context);

			for (auto& defInst : def)
			{
				Token* genericName = defInst.genericName;
				eastl::vector<Expr*>* toForward = defInst.templatesToForward;
				Stmnt* generics = FindGenericsForToken(from, genericName);
				if (!generics)
				{
					AddError(defInst.genericName, "Checker:ExpandForwardedTemplated Unable to find matching generic name for: " + defInst.genericName->val);
					continue;
				}

				size_t index = FindGenericIndex(generics, genericName);

				for (eastl::vector<Expr*>* templatesToExpand : *generics->generics.templatesToExpand)
				{
					Expr* forwardTo = templatesToExpand->at(index);
					Stmnt* stmnt = typeInferer.GetDeclarationStmntForExpr(forwardTo);
					Stmnt* toGenerics = GetGenerics(stmnt);
					if (!toGenerics || toGenerics->generics.names->size() != toForward->size())
					{
						AddError(genericName, "Checker:ExpandForwardedTemplated Incorrect number of templates for generics");
						continue;
					}

					if (TemplatesContainGeneric(toForward, generics))
					{
						eastl::hash_set<eastl::vector<Expr*>*, ExprArrHash, ExprArrEqual>* expandSet = generics->generics.templatesToExpand;
						for (eastl::vector<Expr*>* toExpand : *expandSet)
						{
							eastl::vector<Expr*>* templates = symbolTable->CreateVectorPtr<Expr>();

							for (Expr* forward : *toForward)
							{
								Token* name = GetTokenForTemplate(forward);
								if (name)
								{
									size_t forwardIndex = FindGenericIndex(generics, name);
									if (forwardIndex != (size_t)-1)
									{
										templates->push_back(templatesToExpand->at(forwardIndex));
										continue;
									}
								}

								templates->push_back(forward);
							}

							toGenerics->generics.templatesToExpand->insert(templates);
						}
					}
					else
					{
						toGenerics->generics.templatesToExpand->insert(toForward);
					}
				}
			}
		}
	}

	void ForwardTemplates(Stmnt* from, Stmnt* to, eastl::vector<Expr*>* templatesToReplace,
		eastl::vector<Expr*>* templatesReplaceWith, eastl::hash_set<Stmnt*>& seen, bool cycle = false)
	{
		seen.insert(from);
		eastl::vector<Expr*>* copyArgs = CloneTemplateArgs(templatesToReplace, to->package);
		eastl::vector<Token*>* names = from->generics.names;

		for (size_t i = 0; i < copyArgs->size(); i++)
		{
			Expr* templ = copyArgs->at(i);
			bool wasInferred = false;
			Expr* inferred = InferGenericExpr(from, templ, templatesReplaceWith, &wasInferred);
			if (wasInferred)
			{
				copyArgs->at(i) = inferred;
			}
		}

		to->generics.templatesToExpand->insert(copyArgs);

		if (cycle) return;
		if (deferred.deferredTemplates.find(to) != deferred.deferredTemplates.end())
		{
			eastl::vector<DeferredTemplateInstantiation> deferredTemplates = deferred.deferredTemplates.at(to);
			for (auto& deferredTempl : deferredTemplates)
			{
				Stmnt* nestedTo = deferredTempl.forwardTo;
				eastl::vector<Expr*>* forwardedTemplates = deferredTempl.templatesToForward;
				ForwardTemplates(to, nestedTo, forwardedTemplates, copyArgs, seen, MapHas(seen, nestedTo));
			}
		}
	}
};