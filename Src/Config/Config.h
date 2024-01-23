#pragma once
#include "EASTL/string.h"

enum OutputAs
{
	LLVM,
	C,
};

enum Target
{
	NOT_FOUND,
	WIN64,
};

struct Config
{
	eastl::string fileLoc;
	eastl::string entryPoint;
	bool keepComments;
	OutputAs outputAs;
	Target target;
};