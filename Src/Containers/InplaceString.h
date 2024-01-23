#pragma once
#include "EASTL/string.h"
struct InplaceString
{
	char* start;
	char* last;
	int count;

	InplaceString()
	{
		Clear();
	}

	InplaceString(InplaceString const& toCopy)
	{
		start = toCopy.start;
		last = toCopy.last;
		count = toCopy.count;
	}
	
	inline void Clear()
	{
		count = 0;
		start = nullptr;
		last = nullptr;
	}

	inline eastl::string ToString()
	{
		return eastl::string(start, count);
	}

	inline operator eastl::string() const
	{
		return eastl::string(start, count);
	}

	inline InplaceString& operator+=(char* next)
	{
		count += 1;
		last = next;
		return *this;
	}

	inline bool operator==(const eastl::string& comp)
	{
		if (comp.size() != count) return false;

		for (int i = 0; i < count; i++)
		{
			if (comp[i] != *(start + i)) return false;
		}

		return true;
	}

	inline char& operator [](unsigned int i) const
	{
		return *(start + i);
	}
};