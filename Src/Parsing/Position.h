#pragma once
#include "EASTL/string.h"

struct Position
{
	const eastl::string* file;
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

	Position(const eastl::string* file)
	{
		this->file = file;
		fileOffset = 0;
		line = 1;
		columnOffset = 0;
	}

	Position(const Position& other)
	{
		*this = other;
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
		if (!file) return "";
		return "{ file: " + *file + ", line: " + eastl::to_string(line) + ", column : " + eastl::to_string(columnOffset) + " }";
	}
};