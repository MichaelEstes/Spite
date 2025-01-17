#pragma once
#include <functional>
#include "EASTL/vector.h"
#include "EASTL/tuple.h"

const size_t defaultChunkSize = 1024;

template<class T>
void DestroyItem(void* item) {
	static_cast<T*>(item)->~T();
}

struct DestructorContainer
{
	void (*func)(void*);
	void* arg;

	DestructorContainer(void (*func)(void*), void* arg) : func(func), arg(arg) {}

	~DestructorContainer()
	{
		func(arg);
	}
};

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
	eastl::vector<DestructorContainer*> toDestroy;

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

	template<typename T, typename... Args>
	T* EmplaceContainer(Args&&... args)
	{
		T* valPtr = Emplace<T, Args...>(args...);
		DestructorContainer* destroy = new DestructorContainer((void(*)(void*))DestroyItem<T>, (void*)valPtr);
		toDestroy.push_back(destroy);
		return valPtr;
	}

	void AddChunk()
	{
		prevChunks.push_back(chunk);
		CreateChunk();
	}

	~Arena()
	{
		for (DestructorContainer* destroy : toDestroy)
		{
			delete destroy;
		}

		delete[](char*)chunk.start;

		for (Chunk curr : prevChunks)
		{
			delete[](char*)curr.start;
		}
	}
};