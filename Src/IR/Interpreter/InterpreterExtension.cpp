#include "InterpreterExtension.h"

#include "Interpreter.h"


void CallExtensionFunction(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>& params,
						   Interpreter* interpreter)
{
	interpreter->runningExtension = true;
	interpreter->InterpretCallbackFunction(func, params);
	interpreter->runningExtension = false;
}

void BuildParamsFromFunction(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>& params)
{
	for (auto arg : func->arguments)
	{
		SpiteIR::Operand op = SpiteIR::Operand();
		op.type = arg->value.type;
		op.kind = SpiteIR::OperandKind::Literal;
		op.literal.kind = SpiteIR::PrimitiveKind::Pointer;
		params.push_back(op);
	}
}

void RunInstructionExtensions(SpiteIR::Instruction& inst, SpiteIR::Label*& label, 
							  Interpreter* interpreter)
{
	size_t count = instExts.count;
	if (!count || interpreter->runningExtension) return;

	SpiteIR::Function** funcArray = (SpiteIR::Function**)instExts.memory;
	eastl::vector<SpiteIR::Operand> params = eastl::vector<SpiteIR::Operand>();
	BuildParamsFromFunction(funcArray[0], params);

	for (size_t i = 0; i < count; i++)
	{
		SpiteIR::Function* func = funcArray[i];
		params[0].literal.pointerLiteral = &inst;
		params[1].literal.pointerLiteral = label;
		CallExtensionFunction(func, params, interpreter);
	}
}

void RunFunctionExtensions(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>* params, Interpreter* interpreter)
{
	size_t count = funcExts.count;
	if (!count || interpreter->runningExtension) return;

	SpiteIR::Function** funcArray = (SpiteIR::Function**)funcExts.memory;
	eastl::vector<SpiteIR::Operand> _params = eastl::vector<SpiteIR::Operand>();
	BuildParamsFromFunction(funcArray[0], _params);

	for (size_t i = 0; i < count; i++)
	{
		SpiteIR::Function* _func = funcArray[i];
		_params[0].literal.pointerLiteral = func;
		_params[1].literal.pointerLiteral = params;
		CallExtensionFunction(_func, _params, interpreter);
	}
}

void RunBlockExtensions(SpiteIR::Block* block, Interpreter* interpreter)
{
	size_t count = blockExts.count;
	if (!count || interpreter->runningExtension) return;

	SpiteIR::Function** funcArray = (SpiteIR::Function**)blockExts.memory;
	eastl::vector<SpiteIR::Operand> params = eastl::vector<SpiteIR::Operand>();
	BuildParamsFromFunction(funcArray[0], params);

	for (size_t i = 0; i < count; i++)
	{
		SpiteIR::Function* func = funcArray[i];
		params[0].literal.pointerLiteral = block;
		CallExtensionFunction(func, params, interpreter);
	}
}

void RunLabelExtensions(SpiteIR::Label* label, Interpreter* interpreter)
{
	size_t count = labelExts.count;
	if (!count || interpreter->runningExtension) return;

	SpiteIR::Function** funcArray = (SpiteIR::Function**)labelExts.memory;
	eastl::vector<SpiteIR::Operand> params = eastl::vector<SpiteIR::Operand>();
	BuildParamsFromFunction(funcArray[0], params);

	for (size_t i = 0; i < count; i++)
	{
		SpiteIR::Function* func = funcArray[i];
		params[0].literal.pointerLiteral = label;
		CallExtensionFunction(func, params, interpreter);
	}
}