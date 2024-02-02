// Spite_Lang.cpp : Defines the entry point for the application.
//
#include "Spite_Lang.h"
#include "./Src/Config/Config.h"
#include "./Src/Parsing/Parser.h"
#include "./Src/Log/Logger.h"
#include "./Src/Utils/Profiler.h"
#include "./Src/Utils/Utils.h"
#include "Src/Config/BuildConfig.h"

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

	Parser parser = Parser();
	if (!parser.Parse())
	{
		//return 1;
	}

	float elapsedCompileTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedCompileTime) + "/s to compile " + config.file);

	return 0;
}

