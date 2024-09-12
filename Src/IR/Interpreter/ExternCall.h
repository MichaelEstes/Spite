#pragma once

#include "EASTL/string.h"
#include "dyncall/dyncall.h"
#include "../IR.h"

typedef void (*func_ptr)();

DCCallVM* dynCallVM;

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


void CreateDynCallVM()
{
	if (!dynCallVM) dynCallVM = dcNewCallVM(4096);
}

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

void* CallExternalFunction(SpiteIR::Function* function, eastl::vector<void*>& params)
{
	dcReset(dynCallVM);

	for (size_t i = 0; i < params.size(); i++)
	{
		SpiteIR::Type* type = function->arguments.at(i)->value->type;
		void* value = params.at(i);
		BuildDCArg(type, value);
	}

	return nullptr;
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
				break;
			case 8:
				dcArgDouble(dynCallVM, *(double*)value);
				break;
			default:
				break;
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
		break;
	}
}
