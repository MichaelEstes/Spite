#pragma once
#include <functional>
#include "EASTL/vector.h"

const size_t defaultChunkSize = 1024;

// Doesn't do any validation if the size of a type is bigger than the chunk size
struct Arena
{

	struct Chunk
	{
		void* start;
		void* end;
	};

	void* next;

	Chunk chunk;
	eastl::vector<Chunk> prevChunks;
	size_t chunkSize;
	std::function<size_t(size_t)> getNewChunkSize;

	Arena() : chunkSize(defaultChunkSize), getNewChunkSize([](size_t size) -> size_t { return size * 2; })
	{
		CreateChunk();
	}

	Arena(size_t chunkSize,
		std::function<size_t(size_t)> getNewChunkSize = [](size_t size) -> size_t { return size * 2; })
		: chunkSize(chunkSize), getNewChunkSize(getNewChunkSize)
	{
		CreateChunk();
	}

	void CreateChunk()
	{
		chunk.start = new char[chunkSize];
		chunk.end = (char*)chunk.start + chunkSize;
		next = chunk.start;
	}

	template<typename T, typename... Args>
	T* Emplace(Args&&... args)
	{
		size_t size = sizeof(T);
		if ((char*)next + size > chunk.end)
		{
			chunkSize = getNewChunkSize(chunkSize);
			AddChunk();
		}

		T* valPtr = (T*)next;
		::new(valPtr) T(eastl::forward<Args>(args)...);
		next = (char*)next + size;

		return valPtr;
	}

	void AddChunk()
	{
		prevChunks.push_back(chunk);
		CreateChunk();
	}

	~Arena()
	{
		delete[](char*)chunk.start;

		for (Chunk curr : prevChunks)
		{
			delete[](char*)curr.start;
		}
	}
};