#pragma once
#include "../Syntax/GlobalTable.h"
#include "DeclarationChecker.h"
#include "DefinitionChecker.h"
#include "DeferredChecker.h"

struct Checker
{
	GlobalTable* globalTable;
	DeferredContainer deferred;
	eastl::hash_set<SymbolTable*> checkedPackages;
	eastl::hash_map<Stmnt*, eastl::hash_set<Stmnt*>> forwarded;

	Checker(GlobalTable* globalTable)
	{
		this->globalTable = globalTable;
	}

	void Check()
	{
		DeclarationChecker declChecker = DeclarationChecker(globalTable, globalTable->runtimeTable, deferred);
		declChecker.Check();

		for (auto& [key, value] : globalTable->packageToSymbolTable)
		{
			CheckPackageDeclarations(value);
		}

		checkedPackages.clear();

		DefinitionChecker defChecker = DefinitionChecker(globalTable, globalTable->runtimeTable, deferred);
		defChecker.Check();

		for (auto& [key, value] : globalTable->packageToSymbolTable)
		{
			CheckPackageDefinitions(value);
		}

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

	eastl::vector<Expr*>* CopyTemplateArgs(eastl::vector<Expr*>* args, Token* package)
	{
		eastl::vector<Expr*>* copy = globalTable->FindSymbolTable(package->val)->CreateVectorPtr<Expr>();
		*copy = *args;
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
					ForwardTemplates(from, to, forwardedTemplates, replaceWith);
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
		eastl::vector<Expr*>* templatesReplaceWith)
	{
		eastl::vector<Expr*>* copyArgs = CopyTemplateArgs(templatesToReplace, to->package);
		eastl::vector<Token*>* names = from->generics.names;
		bool replacedArgs = false;
		for (size_t i = 0; i < names->size(); i++)
		{
			Token* name = names->at(i);
			for (size_t j = 0; j < copyArgs->size(); j++)
			{
				Expr* templ = copyArgs->at(j);
				Token* arg = GetTokenForTemplate(templ);
				Expr* replaceWith = templatesReplaceWith->at(i);
				if (arg && arg->val == name->val)
				{
					copyArgs->at(j) = replaceWith;
					replacedArgs = true;
					break;
				}
			}
		}
		
		// This check skips templates from inferred generic types that contain
		// their own generic types
		if (replacedArgs)
		{
			to->generics.templatesToExpand->insert(copyArgs);
		}

		if (deferred.deferredTemplates.find(to) != deferred.deferredTemplates.end())
		{
			eastl::vector<DeferredTemplateInstantiation> deferredTemplates = deferred.deferredTemplates.at(to);
			for (auto& deferredTempl : deferredTemplates)
			{
				Stmnt* nestedTo = deferredTempl.forwardTo;
				eastl::vector<Expr*>* forwardedTemplates = deferredTempl.templatesToForward;
				if (MapHas(forwarded[to], nestedTo)) continue;
				forwarded[to].insert(nestedTo);
				ForwardTemplates(to, nestedTo, forwardedTemplates, copyArgs);
			}
		}
	}
};