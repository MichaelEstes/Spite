#include "../Log/Logger.h"
#include "Parser.h"
#include "Scanner.h"
#include "../Tokens/Tokens.h"
#include "../Intermediate/Syntax.h"
#include "../Utils/Profiler.h"
#include "../Checking/Checker.h"

#include "../Output/LLVM/LLVMBuilder.h"

extern Config config;

bool Parser::Parse()
{
	Tokens tokens = Tokens();
	Scanner scanner = Scanner();

	Profiler profiler = Profiler();
	size_t fileSize = scanner.Init();
	tokens.Init(fileSize);

	scanner.Scan(tokens);

	scanner.Finalize();
	tokens.Finalize();

	if (Logger::HasErrors())
	{
		Logger::PrintErrors();
		return false;
	}

	float elapsedScanTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to process " + config.file);
	profiler = Profiler();

	Syntax syntax = Syntax(tokens);
	syntax.BuildSyntax();
	if (Logger::HasErrors())
	{
		Logger::PrintErrors();
		return false;
	}

	elapsedScanTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to build syntax for " + config.file);

	profiler = Profiler();

	SymbolTable* symbolTable = syntax.symbolTable;

	Checker checker = Checker(symbolTable);
	checker.Check();

	if (Logger::HasErrors())
	{
		Logger::PrintErrors();
		return false;
	}

	syntax.Print();
	elapsedScanTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to check syntax for " + config.file);

	profiler = Profiler();
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

	elapsedScanTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to build output for " + config.file);

	return true;
}