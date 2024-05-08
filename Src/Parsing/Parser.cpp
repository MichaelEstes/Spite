#include "../Log/Logger.h"
#include "Parser.h"
#include "Scanner.h"
#include "../Intermediate/Syntax.h"
#include "../Utils/Profiler.h"

SymbolTable* Parser::Parse(eastl::string& file)
{
	Profiler profiler = Profiler();
	Tokens tokens = Tokens();
	Scanner scanner = Scanner(file);
	size_t fileSize = scanner.Init();
	tokens.Init(fileSize);

	scanner.Scan(tokens);

	scanner.Finalize();
	tokens.Finalize();

	if (Logger::HasErrors())
	{
		Logger::PrintErrors();
		return nullptr;
	}

	float elapsedScanTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to process " + file);
	profiler = Profiler();

	Syntax syntax = Syntax(tokens);
	syntax.BuildSyntax();
	if (Logger::HasErrors())
	{
		Logger::PrintErrors();
		return nullptr;
	}

	syntax.Print();

	elapsedScanTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to build syntax for " + config.file);
	return syntax.symbolTable;
}