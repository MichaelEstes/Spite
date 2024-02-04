#include "../Log/Logger.h"
#include "Parser.h"
#include "Scanner.h"
#include "../Tokens/Tokens.h"
#include "../Intermediate/Syntax.h"
#include "../Utils/Profiler.h"

extern Config config;

bool Parser::Parse()
{
	Tokens tokens = Tokens();
	Scanner scanner = Scanner();

	Profiler profiler = Profiler();
	size_t fileSize = scanner.Init();
	tokens.Init(fileSize);

	do
	{
		Token* next = scanner.Next(tokens);
	} while (!scanner.Finished());

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

	return true;
}