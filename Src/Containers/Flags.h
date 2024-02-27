#pragma once
#include "EASTL/memory.h"

const unsigned int bitCount = 8;

template<const unsigned int T = bitCount>
struct Flags
{
	unsigned char flags[((bitCount - T % bitCount) + T) / bitCount];
	unsigned int size;
	unsigned int count;

	Flags()
	{
		size = T;
		count = ((bitCount - T % bitCount) + T) / bitCount;
		ClearAll();
	}

	Flags(Flags const& toCopy)
	{
		size = toCopy.size;
		count = toCopy.count;
		for (unsigned int i = 0; i < count; i++) flags[count] = toCopy.flags[count];
	}

	void Assign(unsigned int i, bool val)
	{
		if (val)
		{
			Set(i);
		}
		else
		{
			Clear(i);
		}
	}

	void Set(unsigned int i)
	{
		int index = i / bitCount;
		int offset = i % bitCount;
		flags[index] = flags[index] | (1 << offset);
	}

	void Clear(unsigned int i)
	{
		int index = i / bitCount;
		int offset = i % bitCount;
		flags[index] = flags[index] & ~(1 << offset);
	}

	void Toggle(unsigned int i)
	{
		int index = i / bitCount;
		int offset = i % bitCount;
		flags[index] = flags[index] ^ (1 << offset);
	}

	void ClearAll()
	{
		for (unsigned int i = 0; i < count; i++) flags[i] = 0;
	}

	void SetAll()
	{
		for (unsigned int i = 0; i < count; i++) flags[i] = 1;
	}

	inline bool operator [](unsigned int i) const 
	{ 
		int index = i / bitCount;
		int offset = i % bitCount;
		int ret = (flags[index] >> offset) & 1;
		return ret;
	}

	inline Flags& operator=(const Flags& toCopy)
	{
		size = toCopy.size;
		count = toCopy.count;
		for (unsigned int i = 0; i < count; i++) flags[count] = toCopy.flags[count];
		return *this;
	}

};