#pragma once

#include <mutex>

#include "dyncall.h"
#include "dynload.h"
#include "dyncall_callback.h"
#include "../IR.h"
#include "InterpreterUtils.h"

typedef void (*func_ptr)();
struct Interpreter;

struct DynCall
{
	DCCallVM* dynCallVM;
	eastl::hash_map<eastl::string, DLLib*> libCache;
	eastl::vector<DCaggr*> dcaggrs;
};

struct DCCallbackData
{
	Interpreter* interpreter;
	SpiteIR::Function* func;
};

func_ptr FindDCFunction(const eastl::string& name, eastl::string* lib);
DynCall CreateDynCallVM();
void DestroyDynCallVM(DynCall& dynCall);
char TypeToDCSigChar(SpiteIR::Type* type);
char DCCallbackFunc(DCCallback* callback, DCArgs* args, DCValue* result, void* userdata);
void BuildDCArg(SpiteIR::Type* type, void* value, DynCall& dyncall, Interpreter* interpreter);
eastl::string* FindLibForPlatform(eastl::vector<SpiteIR::PlatformLib>* platformToLib);
void CallDCFunc(SpiteIR::Type* type, void* func, char* dst, DynCall& dyncall);
void CallExternalFunction(SpiteIR::Function* function, eastl::vector<void*>& params, char* dst,
	DynCall& dyncall, Interpreter* interpreter);
