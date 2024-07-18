#include "Logger.h"
#include "../Tokens/Token.h"

#ifdef NDEBUG
#define Assert(condition) void(0)
#else
#define Assert(condition)													\
do {																		\
	if (!condition)															\
	{																		\
		std::cout << "Assertion Failed - Function='" << __FUNCTION__ <<		\
		"' File='" << __FILE__ << "' Line='" << __LINE__ << "'\n";			\
		std::exit(1);														\
	}																		\
 } while(0)
#endif

inline void AddError(Token* token, const eastl::string& msg)
{
	Logger::LogError(token->pos, token->index, msg);
}

inline void AddError(const eastl::string& msg)
{
	Logger::FatalError(msg);
}