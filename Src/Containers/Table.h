#pragma once

#include "EASTL/vector.h"

const size_t defaultStartCapacity = 1024;
template <typename Key, typename Val>
struct Table
{
	Key* keys;
	size_t* indices;
	size_t count;
	size_t capacity;
	eastl::vector<Val> vals;

	Table()
	{
		capacity = defaultStartCapacity;
		keys = new Key[capacity];
		indices = new size_t[capacity];
		count = 0;
		ClearKeyArr();
	}

	Table(size_t startCapacity)
	{
		capacity = startCapacity;
		keys = new Key[capacity];
		indices = new size_t[capacity];
		count = 0;
		ClearKeyArr();
	}

	inline void ClearKeyArr()
	{
		for (int i = 0; i < capacity; i++)
			indices[i] = 0;
	}

	inline size_t GetIndex(Key& key)
	{
		return (key.Hash() % capacity) + 1;
	}

	void Insert(Key& key, Val& val)
	{
		size_t index = GetIndex(key);
		while (keys[index] && keys[index] == key) index += 1;
	}

	Val& Get(Key& key)
	{
		size_t index = GetIndex(key);
		
		return vals.at(index);
	}
};