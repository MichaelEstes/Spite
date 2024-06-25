#pragma once
#include "EASTL/string.h"
#include "llvm/ADT/StringRef.h"

struct StringView
{
	const char* start;
	const char* last;
	size_t count;

	StringView()
	{
		Clear();
	}

	StringView(StringView const& toCopy)
	{
		start = toCopy.start;
		last = toCopy.last;
		count = toCopy.count;
	}

	StringView(const char* toCopy)
	{
		count = 0;
		start = toCopy;
		const char* end = toCopy;
		while (*end)
		{
			count += 1;
			end += 1;
		}
		last = start + count;
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

	inline StringView& operator+=(char* next)
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

	inline const char& operator [](unsigned int i) const
	{
		return *(start + i);
	}

	size_t Hash()
	{
		size_t result = 0;
		for (int i = 0; i < count; i++)
			result += (i * 0xDEAD) ^ *(start + i);

		return result;
	}
};

inline bool operator==(const StringView& l, const StringView& r)
{
	if (l.count != r.count) return false;

	for (int i = 0; i < l.count; i++)
	{
		if (l[i] != r[i]) return false;
	}

	return true;
}

inline const eastl::string operator+(const eastl::string l, const StringView& r)
{
	return l + eastl::string(r.start, r.count);
}

inline const eastl::string operator+(const char* l, const StringView& r)
{
	const eastl::string str = eastl::string(l);

	return str + r;
}

struct StringViewHash
{
	size_t operator()(const StringView& str) const
	{
		size_t result = 0;
		for (size_t i = 0; i < str.count; i++)
			result += (i * 0xDEAD) ^ str[i];

		return result;
	}
};