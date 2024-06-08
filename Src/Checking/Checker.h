#pragma once
#include "../Intermediate/GlobalTable.h"
#include "PackageChecker.h"
#include "DeferredChecker.h"

struct Checker
{
	GlobalTable* globalTable;
	DeferredContainer deferred;

	Checker(GlobalTable* globalTable)
	{
		this->globalTable = globalTable;
	}

	void Check()
	{
		for (auto& [key, value] : globalTable->packageToSymbolTable)
		{
			PackageChecker packageChecker = PackageChecker(globalTable, value, deferred);
			packageChecker.Check();
		}

		CheckDeferred();
	}

	void CheckDeferred()
	{
		ExpandDeferredTemplates();
	}

	Token* GetTokenForTemplate(Expr* expr)
	{
		if (expr->typeID == ExprID::TypeExpr && expr->typeExpr.type->typeID == TypeID::NamedType)
		{
			return expr->typeExpr.type->namedType.typeName;
		}
		else if (expr->typeID == ExprID::IdentifierExpr)
		{
			return expr->identifierExpr.identifier;
		}

		return nullptr;
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

	void ForwardTemplates(Stmnt* from, Stmnt* to, eastl::vector<Expr*>* templatesToReplace, 
		eastl::vector<Expr*>* templatesReplaceWith)
	{
		eastl::vector<Expr*>* copyArgs = CopyTemplateArgs(templatesToReplace, to->package);
		eastl::vector<Token*>* names = from->generics.names;

		for (size_t i = 0; i < names->size(); i++)
		{
			Token* name = names->at(i);
			for (size_t j = 0; j < copyArgs->size(); j++)
			{
				Expr* templ = copyArgs->at(j);
				Token* arg = GetTokenForTemplate(templ);
				if (arg && arg->val == name->val)
				{
					copyArgs->at(j) = templatesReplaceWith->at(i);
				}
			}
		}

		to->generics.templatesToExpand->insert(copyArgs);

		if (deferred.deferredTemplates.find(to) != deferred.deferredTemplates.end())
		{
			eastl::vector<DeferredTemplateInstantiation> deferredTemplates = deferred.deferredTemplates.at(to);

			for (auto& deferredTempl : deferredTemplates)
			{
				Stmnt* nestedTo = deferredTempl.forwardTo;
				eastl::vector<Expr*>* forwardedTemplates = deferredTempl.templatesToForward;
				ForwardTemplates(to, nestedTo, forwardedTemplates, copyArgs);
			}
		}
	}
};