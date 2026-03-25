#pragma once
#include "EASTL/string.h"
#include "EASTL/vector.h"
#include <stdlib.h>
#include <iostream>

#include "../Parsing/Position.h"

std::ostream& operator << (std::ostream& os, const eastl::string& str);


struct Logger
{

enum LogLevel
{
	INFO,
	WARNING,
	ERR
};

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
		case ERR:
			return "ERROR: ";
		default:
			break;
		}
	}

	static void Error(const eastl::string& msg)
	{
		std::cout << "ERROR: " << msg << '\n' << std::flush;
	}

	static void ErrorAt(const eastl::string& msg, Position pos)
	{
		std::cout << "ERROR: " << msg << " \n found at position " + pos.ToString() << '\n' << std::flush;
	}

	static void FatalAssert(const eastl::string& msg, int errorCode = 1)
	{
		std::cout << "Assert failed: " << msg << '\n' << std::flush;
		std::exit(errorCode);
	}

	static void FatalError(const eastl::string& msg, int errorCode = 1)
	{
		std::cout << "ERROR: " << msg << '\n' << std::flush;
		std::exit(errorCode);
	}

	static void FatalErrorAt(const eastl::string& msg, Position& pos, int errorCode = 1)
	{
		PrintErrors();
		std::cout << "ERROR: " << msg << " \n found at position " + pos.ToString() << '\n' << std::flush;
		std::exit(errorCode);
	}

	static void Warning(const eastl::string& msg)
	{
		std::cout << "WARNING: " << msg << '\n' << std::flush;
	}

	static void Info(const eastl::string& msg)
	{
		std::cout << "INFO: " << msg << '\n' << std::flush;
	}

	static void Debug(const eastl::string& msg)
	{
#ifdef NDEBUG
#else
		std::cout << "DEBUG: " << msg << '\n' << std::flush;
#endif
	}

	static void AddMessage(LogLevel logLevel, Position pos, size_t tokenIndex, const eastl::string& msg)
	{
		switch (logLevel)
		{
		case INFO:
			break;
		case WARNING:
			break;
		case ERR:
			errors.emplace_back(pos, tokenIndex, msg);
			break;
		default:
			break;
		}
	}

	static void LogError(Position pos, size_t tokenIndex, const eastl::string& msg)
	{
		if (errors.size() > 0 && errors.back().tokenIndex == tokenIndex) FatalErrorAt(msg, pos);
		AddMessage(LogLevel::ERR, pos, tokenIndex, msg);
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

	inline static size_t errorRollback = 0;
	static void SetErrorRollback()
	{
		errorRollback = errors.size();
	}

	static void ErrorRollback()
	{
		while (errors.size() != errorRollback)
		{
			errors.pop_back();
		}
	}

	static bool HasErrorsToRollback()
	{
		return errors.size() != errorRollback;
	}
};
