#pragma once

#include "EASTL/hash_map.h"
#include "../Syntax/Stmnt.h"

struct DeferredTemplateInstantiation
{
	Stmnt* forwardTo;
	eastl::vector<Expr*>* templatesToForward;
};

struct DeferredContainer
{
	eastl::hash_map<Stmnt*, eastl::vector<DeferredTemplateInstantiation>> deferredTemplates;
};
