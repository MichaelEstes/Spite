#pragma once
#pragma once
#include <initializer_list>
#include <utility>

#include "EASTL/memory.h"
#include "EASTL/tuple.h"
#include "EASTL/vector.h"

const int offset = 32;
const int size = 128 - offset;

template<typename T, typename T2, typename T3>
struct TupleLookup
{
	eastl::vector<eastl::tuple<T, T2, T3>> lookup;

	TupleLookup(std::initializer_list<eastl::tuple<T, T2, T3>> list)
	{
		for (const auto& val : list)
		{
			lookup.push_back(val);
		}
	}

	T3 Lookup(T left, T2 right, T3 defaultVal)
	{
		for (auto& val : lookup)
		{
			T l = eastl::get<0>(lookup);
			T2 r = eastl::get<1>(lookup);

			if (left == l && r == right) return eastl::get<2>(lookup);
		}

		return defaultVal;
	}

};