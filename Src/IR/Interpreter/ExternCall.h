#pragma once

#include "EASTL/string.h"
#include "dyncall/dyncall.h"
#include "../IR.h"
#include "../../Log/Logger.h"

typedef void (*func_ptr)();

DCCallVM* dynCallVM;

eastl::hash_map<SpiteIR::Function*, func_ptr> funcCache;

#ifdef WIN32

const char* platform = "windows";

#include <windows.h>
func_ptr FindFunction(const eastl::string& name, const eastl::string& lib)
{
	HMODULE hModule = LoadLibraryA(lib.c_str());
	if (!hModule) return nullptr;

	func_ptr func = (func_ptr)GetProcAddress(hModule, name.c_str());
	return func;
}

#else UNIX

const char* platform = "linux";

#include <dlfcn.h>
func_ptr FindFunction(const eastl::string& name)
{

}

#endif 


void CreateDynCallVM()
{
	if (!dynCallVM)
	{
		dynCallVM = dcNewCallVM(4096);
		dcMode(dynCallVM, DC_CALL_C_DEFAULT);
	}
}

void DestroyDynCallVM()
{
	dcFree(dynCallVM);
	dynCallVM = nullptr;
}

//void CallExternalFunction(const eastl::string& name)
//{
//	func_ptr func = FindFunction(name);
//	double r;
//	DCCallVM* vm = dcNewCallVM(4096);
//	dcMode(vm, DC_CALL_C_DEFAULT);
//	dcReset(vm);
//	dcArgFloat(vm, 4.2373);
//	r = dcCallFloat(vm, (DCpointer)func);
//	dcFree(vm);
//}

void BuildDCArg(SpiteIR::Type* type, void* value)
{
	switch (type->kind)
	{
	case SpiteIR::TypeKind::PrimitiveType:
	{
		switch (type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Bool:
			dcArgBool(dynCallVM, *(bool*)value);
			return;
		case SpiteIR::PrimitiveKind::Byte:
		case SpiteIR::PrimitiveKind::Int:
			switch (type->size)
			{
			case 1:
				dcArgChar(dynCallVM, *(char*)value);
				return;
			case 2:
				dcArgShort(dynCallVM, *(int16_t*)value);
				return;
			case 4:
				dcArgInt(dynCallVM, *(int32_t*)value);
				return;
			case 8:
			case 16:
				dcArgLongLong(dynCallVM, *(int64_t*)value);
				return;
			default:
				return;
			}
		case SpiteIR::PrimitiveKind::Float:
			switch (type->size)
			{
			case 4:
				dcArgFloat(dynCallVM, *(float*)value);
				return;
			case 8:
				dcArgDouble(dynCallVM, *(double*)value);
				return;
			default:
				return;
			}
			return;
		case SpiteIR::PrimitiveKind::String:
			return;
		}

		return;
	}

	case SpiteIR::TypeKind::StateType:
		return;
	case SpiteIR::TypeKind::StructureType:
		return;
	case SpiteIR::TypeKind::PointerType:
		dcArgPointer(dynCallVM, *(void**)value);
		return;
	case SpiteIR::TypeKind::DynamicArrayType:
		return;
	case SpiteIR::TypeKind::FixedArrayType:
		return;
	case SpiteIR::TypeKind::FunctionType:
		return;
	default:
		return;
	}
}

eastl::string* FindLibForPlatform(eastl::vector<SpiteIR::PlatformLib>* platformToLib)
{
	for (auto& platformLib : *platformToLib)
	{
		if (platformLib.platform == platform) return &platformLib.lib;
	}

	return nullptr;
}

void CopyValue(size_t size, const void* ptr, char* dst)
{
	for (size_t i = 0; i < size; i++)
	{
		dst[i] = ((char*)ptr)[i];
	}
}

void CallDCFunc(SpiteIR::Type* type, func_ptr func, char* dst)
{
	switch (type->kind)
	{
	case SpiteIR::TypeKind::PrimitiveType:
	{
		switch (type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Void:
		{
			dcCallVoid(dynCallVM, func);
			break;
		}
		case SpiteIR::PrimitiveKind::Bool:
		{
			bool ret = dcCallBool(dynCallVM, func);
			CopyValue(type->size, &ret, dst);
			break;
		}
		case SpiteIR::PrimitiveKind::Byte:
		case SpiteIR::PrimitiveKind::Int:
			switch (type->size)
			{
			case 1:
			{
				char ret = dcCallChar(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				break;
			}
			case 2:
			{
				int16_t ret = dcCallShort(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				break;
			}
			case 4:
			{
				int32_t ret = dcCallInt(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				break;
			}
			case 8:
			case 16:
			{
				int64_t ret = dcCallLongLong(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				break;
			}
			default:
				break;
			}
		case SpiteIR::PrimitiveKind::Float:
			switch (type->size)
			{
			case 4:
			{
				float ret = dcCallFloat(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				break;
			}
			case 8:
			{
				double ret = dcCallDouble(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				break;
			}
			default:
				break;
			}
			break;
		case SpiteIR::PrimitiveKind::String:
			break;
		}

		break;
	}

	case SpiteIR::TypeKind::StateType:
		break;
	case SpiteIR::TypeKind::StructureType:
		break;
	case SpiteIR::TypeKind::PointerType:
	{
		void* ret = dcCallPointer(dynCallVM, func);
		CopyValue(type->size, &ret, dst);
		break;
	}
	case SpiteIR::TypeKind::DynamicArrayType:
		break;
	case SpiteIR::TypeKind::FixedArrayType:
		break;
	case SpiteIR::TypeKind::FunctionType:
		break;
	default:
		break;
	}

}

void CallExternalFunction(SpiteIR::Function* function, eastl::vector<void*>& params, char* dst)
{
	dcReset(dynCallVM);

	for (size_t i = 0; i < params.size(); i++)
	{
		SpiteIR::Type* type = function->arguments.at(i)->value->type;
		void* value = params.at(i);
		BuildDCArg(type, value);
	}

	func_ptr func = funcCache[function];
	if (!func)
	{
		eastl::string& name = function->metadata.externFunc->callName;
		eastl::string* lib = FindLibForPlatform(function->metadata.externFunc->libs);
		func = FindFunction(name, *lib);
		if (!func) Logger::FatalError("ExternalCall:CallExternalFunction Could not find function named '" +
				name + "' for platform '" + platform + "' in lib '" + *lib + "'");
		funcCache[function] = func;
	}

	if (function->name == "free")
	{
		int i = 2;
	}
	CallDCFunc(function->returnType, func, dst);
}
