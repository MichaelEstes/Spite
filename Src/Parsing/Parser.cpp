#include "../Log/Logger.h"
#include "Parser.h"
#include "../Syntax/Syntax.h"
#include "../Utils/Profiler.h"

SymbolTable* Parser::Parse()
{
	Profiler profiler = Profiler();
	size_t fileSize = scanner.Init(file);
	tokens.Init(fileSize);

	scanner.Scan(tokens);

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

	//syntax.Print();

	elapsedScanTime = profiler.End();
	Logger::Info("Took " + eastl::to_string(elapsedScanTime) + "/s to build syntax for " + config.file);
	return syntax.symbolTable;
}