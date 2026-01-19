#pragma once

struct InteropArray
{
	size_t count;
	size_t capacity;
	void* memory;
	size_t itemBytes;
};

template<typename T>
InteropArray CreateInteropArray()
{
	InteropArray arr = InteropArray();
	arr.count = 0;
	arr.capacity = 0;
	arr.memory = nullptr;
	arr.itemBytes = sizeof(T);

	return arr;
}