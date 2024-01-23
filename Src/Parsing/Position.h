#pragma once
#include "memory"
#include "EASTL/string.h"

struct Position
{
	eastl::string* file;
	size_t fileOffset;
	size_t line;
	size_t columnOffset;

	Position()
	{
		file = nullptr;
		fileOffset = 0;
		line = 1;
		columnOffset = 0;
	}

	Position(eastl::string* file)
	{
		this->file = file;
		fileOffset = 0;
		line = 1;
		columnOffset = 0;
	}

	Position& operator=(const Position& copy)
	{
		file = copy.file;
		fileOffset = copy.fileOffset;
		line = copy.line;
		columnOffset = copy.columnOffset;

		return *this;
	}

	eastl::string ToString()
	{
		return "{ file: " + *file + ", line: " + eastl::to_string(line) + ", column : " + eastl::to_string(columnOffset) + " }";
	}
};