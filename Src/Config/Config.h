#pragma once
#include <initializer_list>
#include <utility>
#include "EASTL/string.h"
#include "EASTL/vector.h"

enum Output
{
	Llvm,
	C,
	Ir,
	OutputInvalid
};

inline Output StringToOutput(const eastl::string& str)
{
	if (str == "llvm") return Output::Llvm;
	else if (str == "c") return Output::C;
	else if (str == "ir") return Output::Ir;
	return Output::OutputInvalid;
}

enum Arch
{
	X64,
	X86,
	Arm32,
	Arm64,
	ArchInvalid
};

inline Arch StringToArch(const eastl::string& str)
{
	if (str == "x64") return Arch::X64;
	else if (str == "x86") return Arch::X86;
	else if (str == "arm32") return Arch::Arm32;
	else if (str == "arm64") return Arch::Arm64;
	return Arch::ArchInvalid;
}

enum Os
{
	Windows,
	Linux,
	Mac,
	Android,
	Ios,
	OsInvalid
};

inline Os StringToOs(const eastl::string& str)
{
	if (str == "windows") return Os::Windows;
	else if (str == "linux") return Os::Linux;
	else if (str == "mac") return Os::Mac;
	else if (str == "android") return Os::Android;
	else if (str == "ios") return Os::Ios;
	return Os::OsInvalid;
}

struct ArgInfo
{
	eastl::string name;
	eastl::string description;
	bool required;
	eastl::vector<eastl::string> options;
};

struct Config
{
	eastl::string file;
	eastl::string entry = "Main";
	Output output = Output::Llvm;
	Arch arch = Arch::X64;
	Os os = Os::Windows;
	bool comments = false;
	int targetArchBitWidth;
};

inline eastl::string GetNextArg(int& index, int argc, char** argv)
{
	if (index + 1 < argc)
	{
		index = index + 1;
		return eastl::string(argv[index]);
	}
	else return "";
}

inline Config ParseConfig(int argc, char** argv)
{
	Config config = Config();
	eastl::vector<ArgInfo> argInfos = { {"-file", "File path of the file to compile", true, {}}, {"-entry", "Name of the function in the file to run first, defaults to Main", false, {}}, {"-output", "Sets the compiler output format, defaults to llvm", false, {"llvm", "c", "ir"}}, {"-arch", "Sets the target architecture to build the binary for", false, {"x64", "x86", "arm32", "arm64"}}, {"-os", "Sets the target os to build the binary for, defaults to windows", false, {"windows", "linux", "mac", "android", "ios"}}, {"-comments", "If present comments will be retained", false, {}} };
	int i = 0;
	while (i < argc)
	{
		eastl::string arg(argv[i]);
		if (arg == "-file")
		{
			config.file = GetNextArg(i, argc, argv);
		}
		else if (arg == "-entry")
		{
			config.entry = GetNextArg(i, argc, argv);
		}
		else if (arg == "-output")
		{
			config.output = StringToOutput(GetNextArg(i, argc, argv));
		}
		else if (arg == "-arch")
		{
			config.arch = StringToArch(GetNextArg(i, argc, argv));
		}
		else if (arg == "-os")
		{
			config.os = StringToOs(GetNextArg(i, argc, argv));
		}
		else if (arg == "-comments")
		{
			config.comments = true;
		}

		i += 1;
	}

	switch (config.arch)
	{
	case X64:
		config.targetArchBitWidth = 64;
	case X86:
		config.targetArchBitWidth = 32;
	case Arm32:
		config.targetArchBitWidth = 32;
	case Arm64:
		config.targetArchBitWidth = 64;
	default:
		config.targetArchBitWidth = 64;
	}

	return config;
}

