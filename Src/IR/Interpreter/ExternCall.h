#pragma once

#include "EASTL/string.h"
#include "dyncall/dyncall.h"

typedef void (*func_ptr)();

#ifdef WIN32

#include <windows.h>
func_ptr FindFunction(const eastl::string& name)
{
    HMODULE hModule = LoadLibraryA("msvcrt.dll");;
    if (!hModule) return nullptr;

    func_ptr func = (func_ptr)GetProcAddress(hModule, name.c_str());
    return func;
}

#elif UNIX

#include <dlfcn.h>
func_ptr FindFunction(const eastl::string& name)
{

}

#endif 


void CallExternalFunction(const eastl::string& name)
{
    func_ptr func = FindFunction(name);
	double r;
	DCCallVM* vm = dcNewCallVM(4096);
	dcMode(vm, DC_CALL_C_DEFAULT);
	dcReset(vm);
	dcArgFloat(vm, 4.2373);
	r = dcCallFloat(vm, (DCpointer)func);
	dcFree(vm);
}
