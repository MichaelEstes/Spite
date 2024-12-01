#pragma once
#include "EASTL/string.h"
#include "llvm/ADT/StringRef.h"

struct StringView
{
	const char* start;
	const char* last;

	StringView()
	{
		Clear();
	}

	StringView(StringView const& toCopy)
	{
		start = toCopy.start;
		last = toCopy.last;
	}

	StringView(const char* toCopy)
	{
		start = toCopy;
		const char* end = toCopy;
		while (*end)
		{
			end += 1;
		}
		last = end - 1;
	}

	inline const size_t Count() const
	{
		return (last - start) + 1;
	}

	StringView Preceding(char ch)
	{
		StringView view = StringView(*this);
		while (*view.last != ch && view.last != view.start)
			view.last -= 1;
		

		view.last -= 1;
		return view;
	}

	inline void Clear()
	{
		start = nullptr;
		last = nullptr;
	}

	inline eastl::string ToString()
	{
		return eastl::string(start, last + 1);
	}

	inline StringView& operator+=(char* next)
	{
		last = next;
		return *this;
	}

	inline bool operator==(const eastl::string& comp)
	{
		size_t count = Count();
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
		size_t count = Count();
		size_t result = 0;
		for (int i = 0; i < count; i++)
			result += (i * 0xDEAD) ^ *(start + i);

		return result;
	}
};

inline bool operator==(const StringView& l, const StringView& r)
{
	size_t lCount = l.Count();
	size_t rCount = r.Count();

	if (lCount != rCount) return false;

	for (int i = 0; i < lCount; i++)
	{
		if (l[i] != r[i]) return false;
	}

	return true;
}

inline const eastl::string operator+(const eastl::string l, const StringView& r)
{
	return l + eastl::string(r.start, r.last);
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
		size_t count = str.Count();
		size_t result = 0;
		for (size_t i = 0; i < count; i++)
			result += (i * 0xDEAD) ^ str[i];

		return result;
	}
};