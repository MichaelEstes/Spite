// Spite_Lang.cpp : Defines the entry point for the application.
//
#include "Spite_Lang.h"

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

Config config;

int main(int argc, char** argv)
{
	Profiler profiler = Profiler();

	/*BuildConfig("C:\\Users\\Flynn\\Documents\\Spite_Lang\\Src\\Config\\Args.txt",
		"C:\\Users\\Flynn\\Documents\\Spite_Lang\\Src\\Config\\NewConfig.h");*/

	config = ParseConfig(argc, argv);

	// TODO Build file information
	eastl::vector<string> files = eastl::vector<string>({ config.file });

	{
		GlobalTable globalTable = GlobalTable();
		eastl::vector<Parser> parsers = eastl::vector<Parser>();
		for (string& file : files)
		{
			Parser& parser = parsers.emplace_back(file);
			SymbolTable* symbolTable = parser.Parse(file);
			if (!symbolTable)
			{
				return 1;
			}
			globalTable.InsertTable(symbolTable);
		}

		{
			Profiler checkerProfiler = Profiler();
			Checker checker = Checker(&globalTable);
			checker.Check();

			if (Logger::HasErrors())
			{
				Logger::PrintErrors();
				return false;
			}
			size_t elapsedScanTime = checkerProfiler.End();
			Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to check syntax for " + config.file);
		}
	}

	/*{
		Profiler builderProfiler = Profiler();
		switch (config.output)
		{
		case Llvm:
		{
			LLVMBuilder builder = LLVMBuilder(symbolTable);
			builder.Build();
			break;
		}
		case C:
			break;
		case Ir:
			break;
		case OutputInvalid:
			break;
		default:
			break;
		}

		size_t elapsedScanTime = builderProfiler.End();
		Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to build output for " + config.file);
	}*/

	float elapsedCompileTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedCompileTime) + "/s to compile " + config.file);

	return 0;
}

