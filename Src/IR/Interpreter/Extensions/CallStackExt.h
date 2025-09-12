#pragma once

#include <EASTL/hash_map.h>
#include <EASTL/vector.h>
#include "../InterpreterExtension.h"

struct Interpreter;

void OnFunction(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>* params, Interpreter* interpreter);
void OnRender(Interpreter* interpreter);