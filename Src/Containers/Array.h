#pragma once

template <typename T>
struct Array 
{
	size_t count;
	T* start;

	Array(size_t count)
	{
		this->count = count;
		this->start = new T[count];
	}

	T& Get(size_t index)
	{
		return start[index];
	}

	T& Find(bool (*pred)(T&))
	{
		for (size_t i = 0; i < count; i++)
		{
			T& value = Get(i);
			if (pred(value)) return value;
		}

		return (T&)nullptr;
	}

	T& begin() { return &start[0]; }
	T& end() { return &start[count]; }
};
