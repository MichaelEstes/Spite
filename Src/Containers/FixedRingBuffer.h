#pragma once
#include <stdint.h>
#include <algorithm>

template<typename T, const unsigned int Size = 12>
struct FixedRingBuffer
{
	T mem[Size];
	uint32_t count;

	FixedRingBuffer()
	{
		for (size_t i = 0; i < Size; i++)
		{
			mem[i] = T();
		}
	}

	void Add(T value)
	{
		for (size_t i = 1; i < Size; i++)
		{
			mem[i] = mem[i - 1];
		}
		
		mem[0] = value;
		count = (((count + 1) < (Size)) ? (count + 1) : (Size));
	}
};

