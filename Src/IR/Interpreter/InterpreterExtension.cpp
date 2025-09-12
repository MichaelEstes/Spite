#include "InterpreterExtension.h"

#ifdef _INTERPRETER_EXTS
#include "Extensions/Extensions.h"
#include "imgui.h"
#include "imgui_internal.h"
#include <chrono>
#endif

bool initialized = false;
float framerate = 2.0f;
uint64_t ticks = 0;

bool RegisterInterpreterExtension(
	InstructionCallback onInst, FunctionCallback onFunc,
	BlockCallback onBlock, LabelCallback onLabel, ExitCallback onExit,
	RenderCallback onRender)
{
	if (onInst) instExts.push_back(onInst);
	if (onFunc) funcExts.push_back(onFunc);
	if (onBlock) blockExts.push_back(onBlock);
	if (onLabel) labelExts.push_back(onLabel);
	if (onExit) exitExts.push_back(onExit);
	if (onRender) renderExts.push_back(onRender);

	return true;
}

void InitExtensions()
{
	#ifdef _INTERPRETER_EXTS
	InitUI();
	initialized = true;
	#endif
}

void ShutdownExtensions()
{
	#ifdef _INTERPRETER_EXTS
	UIShutdown();
	initialized = false;
	#endif
}

void RunInstructionExtensions(SpiteIR::Instruction& inst, SpiteIR::Label*& label, Interpreter* interpreter)
{
	for (auto& instExt : instExts)
	{
		instExt(inst, label, interpreter);
	}
}

void RunFunctionExtensions(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>* params, Interpreter* interpreter)
{
	for (auto funcExt : funcExts)
	{
		(*funcExt)(func, params, interpreter);
	}
}

void RunBlockExtensions(SpiteIR::Block* block, Interpreter* interpreter)
{
	for (auto blockExt : blockExts)
	{
		(*blockExt)(block, interpreter);
	}
}

void RunLabelExtensions(SpiteIR::Label* label, Interpreter* interpreter)
{
	for (auto labelExt : labelExts)
	{
		(*labelExt)(label, interpreter);
	}
}

void RunExitExtensions(Interpreter* interpreter)
{
	for (auto exitExt : exitExts)
	{
		(*exitExt)(interpreter);
	}
}

void RunUIExtensions(Interpreter* interpreter)
{
	#ifdef _INTERPRETER_EXTS
	if (!initialized) return;

	auto now = std::chrono::high_resolution_clock::now();
	auto epoch = std::chrono::time_point_cast<std::chrono::milliseconds>(now).time_since_epoch();
	uint64_t curr = std::chrono::duration_cast<std::chrono::milliseconds>(epoch).count();
	uint64_t delta = curr - ticks;
	if (delta > 1000 / framerate)
	{
		ticks = curr;
		
		bool render = RenderBegin();
		if (!render) return;
		
		for (auto renderExt : renderExts)
		{
			(*renderExt)(interpreter);
		}
		RenderEnd();
	}

	#endif
}