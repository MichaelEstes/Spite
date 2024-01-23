#pragma once
#include <time.h>
#include <chrono>

using namespace std::chrono;

struct Profiler
{
	high_resolution_clock::time_point time;

	Profiler()
	{
		time = high_resolution_clock::now();
	}

	float End()
	{
		return duration_cast<nanoseconds>(high_resolution_clock::now() - time).count() / 1000000000.0f;
	}
};