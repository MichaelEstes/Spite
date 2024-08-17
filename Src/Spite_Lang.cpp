// Spite_Lang.cpp : Defines the entry point for the application.
//
#include "Spite_Lang.h"
#include "IR/IR.h"
#include "Lower/Lower.h"
#include "./IR/Interpreter/Interpreter.h"
#include "IR/Interpreter/Decompiler.h"

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

Stmnt* CheckEntryFunction(SymbolTable* symbolTable)
{
	StringView entryFuncName = StringView(config.entry.c_str());
	Stmnt* entryFunc = symbolTable->FindFunction(entryFuncName);
	if (!entryFunc)
	{
		Logger::FatalError("CheckEntryFunction: No entry function named " + config.entry + " found in file " + config.dir);
	}

	return entryFunc;
}

int main(int argc, char** argv)
{
	Profiler profiler = Profiler();

	config = ParseConfig(argc, argv);

	eastl::hash_set<string> files = eastl::hash_set<string>();
	
	if (config.dir.length() > 0)
	{
		for (auto& entry : std::filesystem::recursive_directory_iterator(config.dir.c_str()))
		{
			if (!entry.is_directory() && entry.path().extension() == ".sp")
			{
				string path(entry.path().string().c_str());
				files.insert(path);
			}
		}
	}

	/*BuildConfig("C:\\Users\\Flynn\\Documents\\Spite_Lang\\Src\\Config\\Args.txt",
		"C:\\Users\\Flynn\\Documents\\Spite_Lang\\Src\\Config\\NewConfig.h");*/

	string entry(std::filesystem::canonical(config.file.c_str()).string().c_str());
	files.erase(entry);

	SpiteIR::IR* ir = nullptr;

	{
		GlobalTable globalTable = GlobalTable();
		eastl::vector<Parser> parsers = eastl::vector<Parser>();
		for (const string& file : files)
		{
			Parser& parser = parsers.emplace_back(file);
			SymbolTable* symbolTable = parser.Parse();
			if (!symbolTable)
			{
				Logger::PrintErrors();
				return 1;
			}
			globalTable.InsertTable(symbolTable);
		}

		Parser& parser = parsers.emplace_back(entry);
		SymbolTable* entryTable = parser.Parse();
		if (!entryTable)
		{
			Logger::PrintErrors();
			return 1;
		}
		globalTable.InsertTable(entryTable);
		globalTable.entryTable = entryTable;
		globalTable.entryFunc = CheckEntryFunction(entryTable);
		if (Logger::HasErrors())
		{
			Logger::PrintErrors();
			return 1;
		}

		{
			Profiler checkerProfiler = Profiler();
			Checker checker = Checker(&globalTable);
			checker.Check();

			if (Logger::HasErrors())
			{
				Logger::PrintErrors();
				return 1;
			}

			Logger::Info("Took " + eastl::to_string(checkerProfiler.End()) + "/s to check syntax");
		}

		//globalTable.Print();
		Profiler lowerProfiler = Profiler();
		Lower lower = Lower(&globalTable);
		ir = lower.BuildIR(entryTable);
		if (Logger::HasErrors())
		{
			Logger::PrintErrors();
			return 1;
		}
		Logger::Info("Took " + eastl::to_string(lowerProfiler.End()) + "/s to lower syntax");
	}

	Profiler interpretProfiler = Profiler();
	Interpreter interpreter = Interpreter(2000000);
	int64_t value = *(int64_t*)interpreter.Interpret(ir);
	Logger::Info("Took " + eastl::to_string(interpretProfiler.End()) + "/s to interpret program");

	//interpretProfiler.Reset();
	//Decompiler decompiler = Decompiler();
	//decompiler.Decompile(ir);
	//Logger::Info("Took " + eastl::to_string(interpretProfiler.End()) + "/s to decompile program");


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

	Logger::Info("Took " + eastl::to_string(profiler.End()) + "/s to compile " + config.file);

	return 0;
}

