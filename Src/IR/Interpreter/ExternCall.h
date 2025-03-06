#pragma once

#include <mutex>

#include "dyncall.h"
#include "dynload.h"
#include "dyncall_callback.h"
#include "../IR.h"
#include "../../Log/Logger.h"
#include "InterpreterUtils.h"

typedef void (*func_ptr)();
struct Interpreter;

struct DCCallbackData
{
	Interpreter* interpreter;
	SpiteIR::Function* func;
};

inline eastl::hash_map<SpiteIR::Function*, func_ptr> funcCache;
inline std::mutex funcCacheMutex;
inline eastl::hash_map<eastl::string, DLLib*> libCache;
inline std::mutex libCacheMutex;

func_ptr FindDCFunction(const eastl::string& name, eastl::string* lib);
DCCallVM* CreateDynCallVM();
void DestroyDynCallVM(DCCallVM* dynCallVM);
char TypeToDCSigChar(SpiteIR::Type* type);
char DCCallbackFunc(DCCallback* callback, DCArgs* args, DCValue* result, void* userdata);
void BuildDCArg(SpiteIR::Type* type, void* value, DCCallVM* dynCallVM, Interpreter* interpreter);
eastl::string* FindLibForPlatform(eastl::vector<SpiteIR::PlatformLib>* platformToLib);
inline void CopyDCReturnValue(size_t size, const void* ptr, char* dst, DCCallVM* dynCallVM);
void CallDCFunc(SpiteIR::Type* type, void* func, char* dst, DCCallVM* dynCallVM);
void CallExternalFunction(SpiteIR::Function* function, eastl::vector<void*>& params, char* dst, 
	DCCallVM* dynCallVM, Interpreter* interpreter);
