// Spite_Lang.h : Include file for standard system include files,
// or project specific include files.

#pragma once

#include <iostream>
#include <fstream>
#include <iostream>
#include <filesystem>

#include "Config/Config.h"
#include "Parsing/Parser.h"
#include "Log/Logger.h"
#include "Utils/Profiler.h"
#include "Utils/Utils.h"
#include "Config/BuildConfig.h"
#include "Checking/Checker.h"

#ifdef _INCLUDE_LLVM
	#include "Output/LLVM/LLVMBuilder.h"
#endif // _INCLUDE_LLVM

