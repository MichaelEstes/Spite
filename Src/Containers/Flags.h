#pragma once
#include "EASTL/memory.h"

const unsigned int bitsInByte = 8;

template<const unsigned int T = 64>
struct Flags
{
	unsigned char flags[(((bitsInByte - T % bitsInByte) + T) / bitsInByte) - 1];

	Flags()
	{
		ClearAll();
	}

	Flags(Flags const& toCopy)
	{
		unsigned int count = (((bitsInByte - T % bitsInByte) + T) / bitsInByte) - 1;
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
		int index = i / bitsInByte;
		int offset = i % bitsInByte;
		flags[index] = flags[index] | (1 << offset);
	}

	void Clear(unsigned int i)
	{
		int index = i / bitsInByte;
		int offset = i % bitsInByte;
		flags[index] = flags[index] & ~(1 << offset);
	}

	void Toggle(unsigned int i)
	{
		int index = i / bitsInByte;
		int offset = i % bitsInByte;
		flags[index] = flags[index] ^ (1 << offset);
	}

	void ClearAll()
	{
		unsigned int count = (((bitsInByte - T % bitsInByte) + T) / bitsInByte) - 1;
		for (unsigned int i = 0; i < count; i++) flags[i] = 0;
	}

	void SetAll()
	{
		unsigned int count = (((bitsInByte - T % bitsInByte) + T) / bitsInByte) - 1;
		for (unsigned int i = 0; i < count; i++) flags[i] = 1;
	}

	inline bool operator [](unsigned int i) const 
	{ 
		int index = i / bitsInByte;
		int offset = i % bitsInByte;
		int ret = (flags[index] >> offset) & 1;
		return ret;
	}
};