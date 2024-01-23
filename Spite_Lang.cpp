// Spite_Lang.cpp : Defines the entry point for the application.
//
#include "Spite_Lang.h"
#include "./Src/Config/Config.h"
#include "./Src/Parsing/Parser.h"
#include "./Src/Log/Logger.h"
#include "./Src/Utils/Profiler.h"
#include "./Src/Utils/Utils.h"

typedef eastl::string string;

#define MSVC _MSC_VER
#if MSVC
void* __cdecl operator new[](size_t size, const char* name, int flags, unsigned debugFlags, const char* file, int line)
	{
		return new uint8_t[size];
	}
#else
void* operator new[](size_t size, const char* name, int flags, unsigned debugFlags, const char* file, int line)
	{
		return new uint8_t[size];
	}
#endif

void* operator new[](size_t size, size_t alignment, size_t alignmentOffset, const char* pName, int flags, unsigned debugFlags, const char* file, int line) { return new uint8_t[size]; }

std::ostream& operator << (std::ostream& os, const eastl::string& str) {
	for (auto ch : str)
		os << static_cast<char>(ch);
	return os;
}

namespace EA::StdC
{
	int Vsnprintf(char* pDestination, size_t n, const char* pFormat, va_list arguments)
	{
		return std::vsnprintf(pDestination, n, pFormat, arguments);
	}
}

int AssignConfig(string arg, string toMatch, string& toAssign, int& index, char** argv, int argc)
{
	if (arg == toMatch)
	{
		if (index + 1 < argc)
		{
			index = index + 1;
			toAssign = argv[index];

			return 1;
		}
		else
		{
			return -1;
		}
	}

	return 0;
}

Target GetTargetFromString(string& val)
{
	if (val == "win64")
	{
		return Target::WIN64;
	}

	return Target::NOT_FOUND;
}

OutputAs GetOutputAsFromString(string& val)
{
	if (val == "C" || val == "c")
	{
		return OutputAs::C;
	}

	return OutputAs::LLVM;
}

Config config = Config();

void ParseBuildFlags(int argc, char** argv)
{
	config.entryPoint = "Main";
	config.keepComments = false;
	config.outputAs = OutputAs::LLVM;

	string temp;
	int i = 0;
	while (i < argc)
	{
		string arg(argv[i]);

		if (AssignConfig(arg, "--file", config.fileLoc, i, argv, argc) == -1)
			Logger::Error("--file argument must be followed by the relative or absolute path to a .sp file");
		if (AssignConfig(arg, "--entry", config.entryPoint, i, argv, argc) == -1)
			Logger::Warning("--entry argument passed without value, will default entry point to Main function if exists");

		int targetVal = AssignConfig(arg, "--target", temp, i, argv, argc);
		if (targetVal == -1)
		{
			Logger::Error("--target argument must be followed by a supported target");
		}
		else if (targetVal == 1)
		{
			Target target = GetTargetFromString(temp);
			if (target == Target::NOT_FOUND)
				Logger::Error("Target argument supplied is not a supported target");
			config.target = target;
		}

		int outPutAsVal = AssignConfig(arg, "--outputAs", temp, i, argv, argc);
		if (outPutAsVal == -1)
		{
			Logger::Warning("--target argument passed without value, will use default output of LLVM");
		}
		else if (targetVal == 1)
		{
			config.outputAs = GetOutputAsFromString(temp);
		}

		if (arg == "--keepComments") config.keepComments = true;

		i++;
	}

	if (config.fileLoc.empty())
	{
		Logger::Error("--file argument must be present with a path to the entry file of your program");
	}
}

int main(int argc, char** argv)
{
	Profiler profiler = Profiler();
	ParseBuildFlags(argc, argv);

	Parser parser = Parser();
	if (!parser.Parse())
	{
		//return 1;
	}

	float elapsedCompileTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedCompileTime) + "/s to compile " + config.fileLoc);

	return 0;
}

