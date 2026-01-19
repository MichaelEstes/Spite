#pragma once

#include "../IR.h"
#include "../../Log/Logger.h"
#include "../../Containers/Array.h"

struct Interpreter;

typedef void (*InstructionCallback)(SpiteIR::Instruction&, SpiteIR::Label*&, Interpreter*);
typedef void (*FunctionCallback)(SpiteIR::Function*, eastl::vector<SpiteIR::Operand>*, Interpreter*);
typedef void (*BlockCallback)(SpiteIR::Block*, Interpreter*);
typedef void (*LabelCallback)(SpiteIR::Label*, Interpreter*);
typedef void (*ExitCallback)(Interpreter*);
typedef void (*RenderCallback)(Interpreter*);

inline InteropArray funcExts = CreateInteropArray<SpiteIR::Function*>();
inline InteropArray blockExts = CreateInteropArray<SpiteIR::Function*>();
inline InteropArray labelExts = CreateInteropArray<SpiteIR::Function*>();
inline InteropArray instExts = CreateInteropArray<SpiteIR::Function*>();

void RunInstructionExtensions(SpiteIR::Instruction& inst, SpiteIR::Label*& label, Interpreter* interpreter);
void RunFunctionExtensions(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>* params, Interpreter* interpreter);
void RunBlockExtensions(SpiteIR::Block* block, Interpreter* interpreter);
void RunLabelExtensions(SpiteIR::Label* label, Interpreter* interpreter);
