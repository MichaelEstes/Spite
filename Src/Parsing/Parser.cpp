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

	
	if (!tokens.Finalize())
	{
		AddError("Unable to tokenize file: " + this->file);
	}

	if (Logger::HasErrors())
	{
		return nullptr;
	}

	float elapsedScanTime = profiler.End();
	Logger::Debug("Took " + eastl::to_string(elapsedScanTime) + "/s to process " + file);
	profiler = Profiler();

	Syntax syntax = Syntax(tokens);
	syntax.BuildSyntax();
	if (Logger::HasErrors())
	{
		return nullptr;
	}

	//syntax.Print();

	elapsedScanTime = profiler.End();
	Logger::Debug("Took " + eastl::to_string(elapsedScanTime) + "/s to build syntax for " + file);
	return syntax.symbolTable;
}