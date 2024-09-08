#pragma once

#include "EASTL/string.h"
#include "dyncall/dyncall.h"

void CallExternalFunction(const eastl::string& name)
{
	double r;
	DCCallVM* vm = dcNewCallVM(4096);
	dcMode(vm, DC_CALL_C_DEFAULT);
	dcReset(vm);
	dcArgFloat(vm, 4.2373);
	r = dcCallFloat(vm, (DCpointer)&sqrtf);
	dcFree(vm);
}