#pragma once
#include <fstream>

#include "EASTL/string.h"

#include "../Config/Config.h"
#include "../Log/Logger.h"
#include "Position.h"
#include "../Tokens/Tokens.h"
#include "../Utils/Profiler.h"

extern Config config; 

class Scanner
{
public:

	Scanner()
	{
		fileLoc = config.fileLoc;
		pos = Position(&fileLoc);
		index = 0;
	}

	size_t Init()
	{
		std::ifstream file = std::ifstream(config.fileLoc.c_str(), std::fstream::in);
		if (file.fail())
		{
			Logger::FatalError("Unable to open src file: " + config.fileLoc);
		}

		Profiler profiler = Profiler();
		defer(Logger::Info("Took " + eastl::to_string(profiler.End()) + "/s to read file " + config.fileLoc));

		file.seekg(0, std::ios::end);
		size_t fileSize = file.tellg();
		contents.resize(fileSize + 1);
		contentCount = contents.size() - 1;
		file.seekg(0, std::ios::beg);
		file.read(&contents[0], contentCount);
		file.close();

		return fileSize;
	}

	inline Token* Next(Tokens& tokens)
	{
		Position start = pos;
		Token* out = nullptr;
		while ((out == nullptr || !out->IsCompleted()) && index < contentCount)
		{
			char* curr = &contents[index];
			tokens.Tokenize(out, curr, Peek(), start);
			UpdatePosition(*curr);
			index += 1;
		}

		return out;
	}

	inline bool Finished()
	{
		return index >= contentCount;
	}

	inline void Finalize()
	{
		contents.clear();
	}

	inline char* Peek()
	{
		return &contents[index + 1];
	}

private:
	eastl::string contents;
	size_t contentCount;
	size_t index;
	Position pos;
	eastl::string fileLoc;

	inline void UpdatePosition(char last)
	{
		if (last == '\n') {
			pos.line++;
			pos.columnOffset = -1;
		}

		pos.columnOffset++;
		pos.fileOffset++;
	}
};