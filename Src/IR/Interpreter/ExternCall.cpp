#include "ExternCall.h"
#include "Interpreter.h"

static std::mutex libMutex;
static eastl::hash_map<eastl::string, DLLib*> libCache = eastl::hash_map<eastl::string, DLLib*>();

func_ptr FindDCFunction(const eastl::string& name, eastl::string* lib)
{
	DLLib* dlLib = nullptr;

	if (lib)
	{
		eastl::string libName = *lib;
		if (libName.find(libExt) == eastl::string::npos) libName += libExt;
		dlLib = dlLoadLibrary(libName.c_str());

		/*libMutex.lock();
		eastl::string libName = eastl::string(*lib);
		if (MapHas(libCache, libName))
		{
			dlLib = libCache.at(libName);
		}
		else
		{
			eastl::string fullName = libName;
			if (libName.find(libExt) == eastl::string::npos) fullName += libExt;
			dlLib = dlLoadLibrary(fullName.c_str());
			libCache.emplace(libName, dlLib);
		}
		libMutex.unlock();*/
	}

	func_ptr func = (func_ptr)dlFindSymbol(dlLib, name.c_str());
	//dlFreeLibrary(dlLib);
	return func;
}

DCCallVM* CreateDynCallVM()
{
	DCCallVM* dynCallVM = dcNewCallVM(4096);
	dcMode(dynCallVM, DC_CALL_C_DEFAULT);
	return dynCallVM;
}

void DestroyDynCallVM(DCCallVM* dynCallVM)
{
	//for (auto& [name, lib] : libCache)
	//{
	//	dlFreeLibrary(lib);
	//}
	dcFree(dynCallVM);
}

char TypeToDCSigChar(SpiteIR::Type* type)
{
	switch (type->kind)
	{
	case SpiteIR::TypeKind::PrimitiveType:
	{
		switch (type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Void:
			return 'v';
		case SpiteIR::PrimitiveKind::Bool:
			return 'B';
		case SpiteIR::PrimitiveKind::Byte:
		case SpiteIR::PrimitiveKind::I16:
		case SpiteIR::PrimitiveKind::I32:
		case SpiteIR::PrimitiveKind::I64:
		case SpiteIR::PrimitiveKind::Int:
			switch (type->size)
			{
			case 1:
				return type->primitive.isSigned ? 'c' : 'C';
			case 2:
				return type->primitive.isSigned ? 's' : 'S';
			case 4:
				return type->primitive.isSigned ? 'i' : 'I';
			case 8:
			case 16:
				return type->primitive.isSigned ? 'l' : 'L';
			default:
				break;
			}
			break;
		case SpiteIR::PrimitiveKind::F32:
		case SpiteIR::PrimitiveKind::Float:
			switch (type->size)
			{
			case 4:
				return 'f';
			case 8:
				return 'd';
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
		return 'p';
	case SpiteIR::TypeKind::FunctionType:
	case SpiteIR::TypeKind::StateType:
	case SpiteIR::TypeKind::StructureType:
	case SpiteIR::TypeKind::DynamicArrayType:
	case SpiteIR::TypeKind::FixedArrayType:
		break;
	default:
		break;
	}

	Logger::FatalError("ExternCall:TypeToDCSigChar Unable to create signature for type");
	return 0;
}

SpiteIR::Operand DCArgToOperand(DCArgs* args, SpiteIR::Type* type)
{
	SpiteIR::Operand operand;
	operand.type = type;
	switch (type->kind)
	{
	case SpiteIR::TypeKind::PrimitiveType:
	{
		operand.kind = SpiteIR::OperandKind::Literal;
		switch (type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Void:
			operand.kind = SpiteIR::OperandKind::Void;
			return operand;
		case SpiteIR::PrimitiveKind::Bool:
			operand.literal.kind = SpiteIR::PrimitiveKind::Bool;
			operand.literal.byteLiteral = dcbArgBool(args);
			return operand;
		case SpiteIR::PrimitiveKind::Byte:
		case SpiteIR::PrimitiveKind::I16:
		case SpiteIR::PrimitiveKind::I32:
		case SpiteIR::PrimitiveKind::I64:
		case SpiteIR::PrimitiveKind::Int:
			switch (type->size)
			{
			case 1:
				operand.literal.kind = SpiteIR::PrimitiveKind::Byte;
				operand.literal.byteLiteral = type->primitive.isSigned ? 
					dcbArgChar(args) : dcbArgUChar(args);
				return operand;
			case 2:
				operand.literal.kind = SpiteIR::PrimitiveKind::I16;
				operand.literal.byteLiteral = type->primitive.isSigned ?
					dcbArgShort(args) : dcbArgUShort(args);
				return operand;
			case 4:
				operand.literal.kind = SpiteIR::PrimitiveKind::I32;
				operand.literal.byteLiteral = type->primitive.isSigned ?
					dcbArgInt(args) : dcbArgUInt(args);
				return operand;
			case 8:
			case 16:
				operand.literal.kind = SpiteIR::PrimitiveKind::I64;
				operand.literal.byteLiteral = type->primitive.isSigned ?
					dcbArgLongLong(args) : dcbArgULongLong(args);
				return operand;
			default:
				break;
			}
			break;
		case SpiteIR::PrimitiveKind::F32:
		case SpiteIR::PrimitiveKind::Float:
			switch (type->size)
			{
			case 4:
				operand.literal.kind = SpiteIR::PrimitiveKind::F32;
				operand.literal.byteLiteral = dcbArgFloat(args);
				return operand;
			case 8:
				operand.literal.kind = SpiteIR::PrimitiveKind::Float;
				operand.literal.byteLiteral = dcbArgDouble(args);
				return operand;
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
		operand.kind = SpiteIR::OperandKind::Literal;
		operand.literal.kind = SpiteIR::PrimitiveKind::Pointer;
		operand.literal.pointerLiteral = dcbArgPointer(args);
		return operand;
	case SpiteIR::TypeKind::FunctionType:
	case SpiteIR::TypeKind::StateType:
	case SpiteIR::TypeKind::StructureType:
	case SpiteIR::TypeKind::DynamicArrayType:
	case SpiteIR::TypeKind::FixedArrayType:
		break;
	default:
		break;
	}

	Logger::FatalError("ExternCall:DCArgToOperand Unable to create arg for type");
	return operand;
}

void SetResultValue(SpiteIR::Type* type, void* value, DCValue* result)
{
	switch (type->kind)
	{
	case SpiteIR::TypeKind::PrimitiveType:
	{
		switch (type->primitive.kind)
		{
		case SpiteIR::PrimitiveKind::Void:
			return;
		case SpiteIR::PrimitiveKind::Bool:
			result->B = *(bool*)value;
			return;
		case SpiteIR::PrimitiveKind::Byte:
		case SpiteIR::PrimitiveKind::I16:
		case SpiteIR::PrimitiveKind::I32:
		case SpiteIR::PrimitiveKind::I64:
		case SpiteIR::PrimitiveKind::Int:
			switch (type->size)
			{
			case 1:
				if (type->primitive.isSigned)
				{
					result->c = *(char*)value;
				}
				else
				{
					result->C = *(unsigned char*)value;
				}
				return;
			case 2:
				if (type->primitive.isSigned)
				{
					result->s = *(int16_t*)value;
				}
				else
				{
					result->S = *(uint16_t*)value;
				}
				return;
			case 4:
				if (type->primitive.isSigned)
				{
					result->i = *(int32_t*)value;
				}
				else
				{
					result->I = *(uint32_t*)value;
				}
				return;
			case 8:
			case 16:
				if (type->primitive.isSigned)
				{
					result->l = *(int64_t*)value;
				}
				else
				{
					result->L = *(uint64_t*)value;
				}
				return;
			default:
				break;
			}
			break;
		case SpiteIR::PrimitiveKind::F32:
		case SpiteIR::PrimitiveKind::Float:
			switch (type->size)
			{
			case 4:
				result->f = *(float*)value;
				return;
			case 8:
				result->d = *(double*)value;
				return;
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
		result->p = *(void**)value;
		return;
	case SpiteIR::TypeKind::FunctionType:
	case SpiteIR::TypeKind::StateType:
	case SpiteIR::TypeKind::StructureType:
	case SpiteIR::TypeKind::DynamicArrayType:
	case SpiteIR::TypeKind::FixedArrayType:
		break;
	default:
		break;
	}

	Logger::FatalError("ExternCall:SetResultValue Unable to set callback result for type");
}

char DCCallbackFunc(DCCallback* callback, DCArgs* args, DCValue* result, void* userdata)
{
	DCCallbackData* callbackData = (DCCallbackData*)userdata;
	SpiteIR::Function* func = callbackData->func;
	SpiteIR::Type* returnType = callbackData->func->returnType;
	eastl::vector<SpiteIR::Operand> params;

	for (SpiteIR::Argument* arg : func->arguments)
	{
		SpiteIR::Type* type = arg->value.type;
		params.push_back(DCArgToOperand(args, type));
	}

	void* returnValue = nullptr;
	int currentThreadID = CurrentThreadID();
	if (currentThreadID == callbackData->interpreter->threadID)
	{
		returnValue = callbackData->interpreter->InterpretCallbackFunction(
			func,
			params
		);
		SetResultValue(returnType, returnValue, result);
	}
	else
	{
		Interpreter interpreter = Interpreter(config.interpreterStackSize);
		interpreter.threadID = currentThreadID;
		returnValue = interpreter.InterpretCallbackFunction(
			func,
			params
		);
		SetResultValue(returnType, returnValue, result);
	}
	
	dcbFreeCallback(callback);
	delete callbackData;
	return TypeToDCSigChar(returnType);
}

void BuildDCArg(SpiteIR::Type* type, void* value, DCCallVM* dynCallVM, Interpreter* interpreter)
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
			break;
		case SpiteIR::PrimitiveKind::String:
			break;
		}

		break;
	}
	case SpiteIR::TypeKind::PointerType:
		dcArgPointer(dynCallVM, *(void**)value);
		return;
	case SpiteIR::TypeKind::FunctionType:
	{
		eastl::string sig = "";
		for (SpiteIR::Type* param : *type->function.params)
		{
			sig += TypeToDCSigChar(param);
		}
		sig += ')';
		sig += TypeToDCSigChar(type->function.returnType);

		DCCallbackData* data = new DCCallbackData();
		data->interpreter = interpreter;
		data->func = *(SpiteIR::Function**)value;

		DCCallback* callback = dcbNewCallback(sig.c_str(), &DCCallbackFunc, (void*)data);
		dcArgPointer(dynCallVM, callback);
		return;
	}
	case SpiteIR::TypeKind::StateType:
	case SpiteIR::TypeKind::StructureType:
	case SpiteIR::TypeKind::DynamicArrayType:
	case SpiteIR::TypeKind::FixedArrayType:
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

inline void CopyDCReturnValue(size_t size, const void* ptr, char* dst)
{
	memcpy(dst, ptr, size);
}

void CallDCFunc(SpiteIR::Type* type, void* func, char* dst, DCCallVM* dynCallVM)
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
			CopyDCReturnValue(type->size, &ret, dst);
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
				CopyDCReturnValue(type->size, &ret, dst);
				return;
			}
			case 2:
			{
				int16_t ret = dcCallShort(dynCallVM, func);
				CopyDCReturnValue(type->size, &ret, dst);
				return;
			}
			case 4:
			{
				int32_t ret = dcCallInt(dynCallVM, func);
				CopyDCReturnValue(type->size, &ret, dst);
				return;
			}
			case 8:
			case 16:
			{
				int64_t ret = dcCallLongLong(dynCallVM, func);
				CopyDCReturnValue(type->size, &ret, dst);
				return;
			}
			default:
				break;
			}
			break;
		case SpiteIR::PrimitiveKind::F32:
		case SpiteIR::PrimitiveKind::Float:
			switch (type->size)
			{
			case 4:
			{
				float ret = dcCallFloat(dynCallVM, func);
				CopyDCReturnValue(type->size, &ret, dst);
				return;
			}
			case 8:
			{
				double ret = dcCallDouble(dynCallVM, func);
				CopyDCReturnValue(type->size, &ret, dst);
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
		CopyDCReturnValue(type->size, &ret, dst);
		return;
	}
	case SpiteIR::TypeKind::FunctionType:
	case SpiteIR::TypeKind::StructureType:
	case SpiteIR::TypeKind::StateType:
	case SpiteIR::TypeKind::DynamicArrayType:
	case SpiteIR::TypeKind::FixedArrayType:
		break;
	default:
		break;
	}

	Logger::FatalError("ExternCall:CallDCFunc Invalid agument type passed in to external call");
}

void CallExternalFunction(SpiteIR::Function* function, eastl::vector<void*>& params, char* dst,
	DCCallVM* dynCallVM, Interpreter* interpreter)
{
	dcReset(dynCallVM);

	for (size_t i = 0; i < params.size(); i++)
	{
		SpiteIR::Type* type = function->arguments.at(i)->value.type;
		void* value = params.at(i);
		BuildDCArg(type, value, dynCallVM, interpreter);
	}

	eastl::string& name = function->metadata.externFunc->externName;
	eastl::string* lib = FindLibForPlatform(function->metadata.externFunc->libs);
	func_ptr func = FindDCFunction(name, lib);
	if (!func)
	{
		if (lib)
			Logger::FatalError("ExternalCall:CallExternalFunction Could not find function named '" +
				name + "' for platform '" + platform + "' in lib '" + *lib + libExt + "'");
		else
			Logger::FatalError("ExternalCall:CallExternalFunction Could not find function named '" +
				name + "' for platform '" + platform + "'");
	}

	CallDCFunc(function->returnType, (void*)func, dst, dynCallVM);
}