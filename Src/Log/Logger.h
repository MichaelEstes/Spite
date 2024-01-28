#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"
#include <stdlib.h>
#include <iostream>

#include "../Parsing/Position.h"

std::ostream& operator << (std::ostream& os, const eastl::string& str);

enum LogLevel
{
	INFO,
	WARNING,
	ERROR
};

struct Logger
{
public:
	struct LogMessage
	{
		Position pos;
		size_t tokenIndex;
		eastl::string msg;

		LogMessage(Position pos, size_t tokenIndex, const eastl::string& msg)
		{
			this->msg = msg;
			this->pos = pos;
			this->tokenIndex = tokenIndex;
		}
	};

	static eastl::string LogLevelStr(LogLevel logLevel)
	{
		switch (logLevel)
		{
		case INFO:
			return "INFO: ";
		case WARNING:
			return "WARNING: ";
		case ERROR:
			return "ERROR: ";
		default:
			break;
		}
	}

	static void Error(eastl::string msg)
	{
		std::cout << "ERROR: " << msg << '\n';
	}

	static void ErrorAt(eastl::string msg, Position pos)
	{
		std::cout << "ERROR: " << msg << " :: found at position " + pos.ToString() << '\n';
	}

	static void FatalError(eastl::string msg, int errorCode = 1)
	{
		std::cout << "ERROR: " << msg << '\n';
		std::exit(errorCode);
	}

	static void FatalErrorAt(eastl::string msg, Position pos, int errorCode = 1)
	{
		std::cout << "ERROR: " << msg << " :: found at position " + pos.ToString() << '\n';
		std::exit(errorCode);
	}

	static void Warning(eastl::string msg)
	{
		std::cout << "WARNING: " << msg << '\n';
	}

	static void Info(eastl::string msg)
	{
		std::cout << "INFO: " << msg << '\n';
	}

	static void AddMessage(LogLevel logLevel, Position pos, size_t tokenIndex, const eastl::string& msg)
	{
		switch (logLevel)
		{
		case INFO:
			break;
		case WARNING:
			break;
		case ERROR:
			errors.emplace_back(pos, tokenIndex, msg);
			break;
		default:
			break;
		}
	}

	static void AddError(Position pos, size_t tokenIndex, const eastl::string& msg)
	{
		AddMessage(ERROR, pos, tokenIndex, msg);
	}

	static bool HasErrors()
	{
		return (bool)errors.size();
	}

	static void PrintErrors()
	{
		for (LogMessage msg : errors)
		{
			ErrorAt(msg.msg, msg.pos);
		}
	}

	inline static eastl::vector<LogMessage> errors = eastl::vector<LogMessage>();
};