#pragma once
#include <memory>

#define ToBit(val) (1 << (val))


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