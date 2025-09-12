#pragma once

#include "../IR.h"
#include "../../Log/Logger.h"

struct Interpreter;

typedef void (*InstructionCallback)(SpiteIR::Instruction&, SpiteIR::Label*&, Interpreter*);
typedef void (*FunctionCallback)(SpiteIR::Function*, eastl::vector<SpiteIR::Operand>*, Interpreter*);
typedef void (*BlockCallback)(SpiteIR::Block*, Interpreter*);
typedef void (*LabelCallback)(SpiteIR::Label*, Interpreter*);
typedef void (*ExitCallback)(Interpreter*);
typedef void (*RenderCallback)(Interpreter*);

inline eastl::vector<InstructionCallback> instExts = eastl::vector<InstructionCallback>();
inline eastl::vector<FunctionCallback> funcExts = eastl::vector<FunctionCallback>();
inline eastl::vector<BlockCallback> blockExts = eastl::vector<BlockCallback>();
inline eastl::vector<LabelCallback> labelExts = eastl::vector<LabelCallback>();
inline eastl::vector<ExitCallback> exitExts = eastl::vector<ExitCallback>();
inline eastl::vector<RenderCallback> renderExts = eastl::vector<RenderCallback>();

bool RegisterInterpreterExtension(
	InstructionCallback onInst, FunctionCallback onFunc = nullptr,
	BlockCallback onBlock = nullptr, LabelCallback onLabel = nullptr,
	ExitCallback onExit = nullptr, RenderCallback onRender = nullptr
);

void InitExtensions();
void ShutdownExtensions();

void RunInstructionExtensions(SpiteIR::Instruction& inst, SpiteIR::Label*& label, Interpreter* interpreter);
void RunFunctionExtensions(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>* params, Interpreter* interpreter);
void RunBlockExtensions(SpiteIR::Block* block, Interpreter* interpreter);
void RunLabelExtensions(SpiteIR::Label* label, Interpreter* interpreter);
void RunExitExtensions(Interpreter* interpreter);

void RunUIExtensions(Interpreter* interpreter);
