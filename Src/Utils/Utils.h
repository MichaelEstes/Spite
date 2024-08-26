#pragma once
#include <memory>
#include <stdlib.h>
#include <iostream>

#define ToBit(val) (1 << (val))

#ifdef NDEBUG
#define Assert(condition) void(0)
#else
#define Assert(condition)													\
do {																		\
	if (!(condition))														\
	{																		\
		std::cout << "Assertion Failed - Function='" << __FUNCTION__ <<		\
		"' File='" << __FILE__ << "' Line='" << __LINE__ << "'\n";			\
		std::exit(1);														\
	}																		\
 } while(0)
#endif

template<typename Func>
struct DeferCall {
    Func callback;
	DeferCall(Func&& callback) : callback(std::forward<Func>(callback)){}

	~DeferCall()
	{
		callback();
	}
};

template <typename Func> 
DeferCall<Func> createDefer(Func&& callback) { return DeferCall<Func>(std::forward<Func>(callback)); }

#define deferFuncNameUnique(l, r) l##r
#define deferFuncName deferFuncNameUnique(_deffered, __COUNTER__)
#define defer(statement) auto deferFuncName = createDefer([&]()->void{statement;})

template<typename Map, typename Value>
inline bool MapHas(Map& map, Value& value)
{
	return map.find(value) != map.end();
}