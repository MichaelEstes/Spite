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
std::filesystem::path execDir;

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

#ifdef WIN32

#include <windows.h>
std::filesystem::path GetExecutableDir()
{
	wchar_t path[MAX_PATH];
	GetModuleFileNameW(NULL, path, MAX_PATH);
	return std::filesystem::path{ path }.parent_path() / "";
}

#elif __unix__

#include <unistd.h>
std::filesystem::path GetExecutableDir()
{
	char path[PATH_MAX];
	size_t count = readlink("/proc/self/exe", path, PATH_MAX);
	path[count] = '\0';
	return std::filesystem::path{ path }.parent_path() / "";
}

#endif 

void FindAllSourceFilesInDir(eastl::hash_set<string>& files, const std::filesystem::path& dir)
{
	for (auto& entry : std::filesystem::recursive_directory_iterator(dir))
	{
		if (!entry.is_directory() && entry.path().extension() == ".sp")
		{
			string path(entry.path().string().c_str());
			files.insert(path);
		}
	}
}

int main(int argc, char** argv)
{
	Profiler profiler = Profiler();

	config = ParseConfig(argc, argv);

	std::filesystem::path dir;
	if (config.dir.length() > 0)
	{
		dir = std::filesystem::canonical(std::filesystem::path{ config.dir.c_str() });
	}
	else
	{
		dir = std::filesystem::current_path();
	}

	eastl::hash_set<string> files = eastl::hash_set<string>();	
	FindAllSourceFilesInDir(files, dir);

	execDir = GetExecutableDir();
	std::filesystem::path runTimeDir = execDir / "Runtime";
	FindAllSourceFilesInDir(files, runTimeDir);

	string entry(std::filesystem::canonical(config.file.c_str()).string().c_str());
	files.erase(entry);

	SpiteIR::IR* ir = nullptr;
	Interpreter interpreter = Interpreter(config.interpreterStackSize);

	{
		GlobalTable globalTable = GlobalTable();
		Arena parserArena = Arena((files.size() + 1) * sizeof(Parser));
		for (const string& file : files)
		{
			Parser* parser = parserArena.EmplaceContainer<Parser>(file);
			SymbolTable* symbolTable = parser->Parse();
			if (!symbolTable)
			{
				Logger::PrintErrors();
				return 1;
			}
			globalTable.InsertTable(symbolTable);
		}

		SymbolTable* entryTable = nullptr;
		if (!entry.empty())
		{
			Parser* parser = parserArena.EmplaceContainer<Parser>(entry);
			entryTable = parser->Parse();
			if (!entryTable)
			{
				Logger::PrintErrors();
				return 1;
			}
			globalTable.InsertTable(entryTable);
		}
		else
		{
			StringView entryName = StringView(config.entry.c_str());
			entryTable = globalTable.FindSymbolTable(entryName);
			if (!entryTable)
			{
				eastl::string entryError = "No entry file provided and no package found with name: " + config.entry;
				Logger::Error(entryError);
				return 1;
			}
		}

		globalTable.entryTable = entryTable;
		globalTable.entryFunc = CheckEntryFunction(entryTable);
		globalTable.SetRuntimeTable();
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

			Logger::Debug("Took " + eastl::to_string(checkerProfiler.End()) + "/s to check syntax");
		}

		Profiler lowerProfiler = Profiler();
		Lower lower = Lower(&globalTable, &interpreter);
		ir = lower.BuildIR(entryTable);
		if (Logger::HasErrors())
		{
			Logger::PrintErrors();
			return 1;
		}
		Logger::Debug("Took " + eastl::to_string(lowerProfiler.End()) + "/s to lower syntax");
	}

	{
		Profiler builderProfiler = Profiler();
		switch (config.output)
		{
		case Llvm:
		case Ir:
		{
			LLVMBuilder builder = LLVMBuilder(ir);
			builder.Build();
			break;
		}
		case Run:
		{
			Profiler interpretProfiler = Profiler();
			//Decompiler decompiler = Decompiler();
			//decompiler.Decompile(ir);
			//Logger::Debug("Took " + eastl::to_string(interpretProfiler.End()) + "/s to decompile program");
			//interpretProfiler.Reset();
			int value = *(int*)(void*)interpreter.Interpret(ir);
			Logger::Debug("Took " + eastl::to_string(interpretProfiler.End()) + "/s to interpret program");
			return value;
		}
		case C:
			break;
		case OutputInvalid:
			break;
		default:
			break;
		}

		Logger::Debug("Took " + eastl::to_string(builderProfiler.End()) + "/s to build output");
	}

	Logger::Debug("Took " + eastl::to_string(profiler.End()) + "/s to compile");
	return 0;
}

