#pragma once

#include "EASTL/string.h"
#include "dyncall.h"
#include "dynload.h"
#include "../IR.h"
#include "../../Log/Logger.h"

typedef void (*func_ptr)();

DCCallVM* dynCallVM;

eastl::hash_map<SpiteIR::Function*, func_ptr> funcCache;
eastl::hash_map<eastl::string, DLLib*> libCache;

func_ptr FindFunction(const eastl::string& name, eastl::string* lib)
{
	DLLib* dlLib = nullptr;
	if (lib)
	{
		dlLib = libCache[*lib];
		if (!dlLib)
		{
			dlLib = dlLoadLibrary(lib->c_str());
			libCache[*lib] = dlLib;
		}
	}
		
	func_ptr func = (func_ptr)dlFindSymbol(dlLib, name.c_str());
	return func;
}

#ifdef WIN32
const char* platform = "windows";
#elif __unix__
const char* platform = "linux";
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
	for (auto& [name, lib] : libCache)
	{
		dlFreeLibrary(lib);
	}
	libCache.clear();
	dcFree(dynCallVM);
	dynCallVM = nullptr;
}

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
		case SpiteIR::PrimitiveKind::I16:
		case SpiteIR::PrimitiveKind::I32:
		case SpiteIR::PrimitiveKind::I64:
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
				break;
			}
		case SpiteIR::PrimitiveKind::F32:
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
				break;
			}
			return;
		case SpiteIR::PrimitiveKind::String:
			break;
		}

		break;
	}
	case SpiteIR::TypeKind::PointerType:
		dcArgPointer(dynCallVM, *(void**)value);
		return;
	case SpiteIR::TypeKind::StateType:
	case SpiteIR::TypeKind::StructureType:
	case SpiteIR::TypeKind::DynamicArrayType:
	case SpiteIR::TypeKind::FixedArrayType:
	case SpiteIR::TypeKind::FunctionType:
		break;
	default:
		break;
	}

	Logger::FatalError("ExternCall:BuildDCArg Unable to create arg for type");
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

void CallDCFunc(SpiteIR::Type* type, void* func, char* dst)
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
			return;
		}
		case SpiteIR::PrimitiveKind::Bool:
		{
			bool ret = dcCallBool(dynCallVM, func);
			CopyValue(type->size, &ret, dst);
			return;
		}
		case SpiteIR::PrimitiveKind::Byte:
		case SpiteIR::PrimitiveKind::I16:
		case SpiteIR::PrimitiveKind::I32:
		case SpiteIR::PrimitiveKind::I64:
		case SpiteIR::PrimitiveKind::Int:
			switch (type->size)
			{
			case 1:
			{
				char ret = dcCallChar(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				return;
			}
			case 2:
			{
				int16_t ret = dcCallShort(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				return;
			}
			case 4:
			{
				int32_t ret = dcCallInt(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				return;
			}
			case 8:
			case 16:
			{
				int64_t ret = dcCallLongLong(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				return;
			}
			default:
				break;
			}
		case SpiteIR::PrimitiveKind::F32:
		case SpiteIR::PrimitiveKind::Float:
			switch (type->size)
			{
			case 4:
			{
				float ret = dcCallFloat(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				return;
			}
			case 8:
			{
				double ret = dcCallDouble(dynCallVM, func);
				CopyValue(type->size, &ret, dst);
				return;
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
	case SpiteIR::TypeKind::PointerType:
	{
		void* ret = dcCallPointer(dynCallVM, func);
		CopyValue(type->size, &ret, dst);
		return;
	}
	case SpiteIR::TypeKind::StructureType:
	case SpiteIR::TypeKind::StateType:
	case SpiteIR::TypeKind::DynamicArrayType:
	case SpiteIR::TypeKind::FixedArrayType:
	case SpiteIR::TypeKind::FunctionType:
		break;
	default:
		break;
	}

	Logger::FatalError("ExternCall:CallDCFunc Invalid agument type passed in to external call");
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
		eastl::string& name = function->metadata.externFunc->externName;
		eastl::string* lib = FindLibForPlatform(function->metadata.externFunc->libs);
		func = FindFunction(name, lib);
		if (!func)
		{
			if (lib)
				Logger::FatalError("ExternalCall:CallExternalFunction Could not find function named '" +
						name + "' for platform '" + platform + "' in lib '" + *lib + "'");
			else
				Logger::FatalError("ExternalCall:CallExternalFunction Could not find function named '" +
					name + "' for platform '" + platform + "'");
		}

		funcCache[function] = func;
	}

	CallDCFunc(function->returnType, (void*)func, dst);
}
