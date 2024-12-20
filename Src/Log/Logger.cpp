#include "Logger.h"
#include "../Tokens/Token.h"

inline void AddError(Token* token, const eastl::string& msg)
{
	Logger::LogError(token->pos, token->index, msg);
}

inline void AddError(const eastl::string& msg)
{
	Logger::LogError(Position(), 0, msg);
}