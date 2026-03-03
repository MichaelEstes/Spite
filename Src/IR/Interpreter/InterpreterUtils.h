#pragma once

#ifdef WIN32

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif

inline const size_t os = 0;
inline const size_t arch = 0;
inline const char* platform = "windows";
inline const char* libExt = ".dll";

#include <windows.h>
inline int CurrentThreadID()
{
	return GetCurrentThreadId();
}

#elif __unix__
inline const size_t os = 1;
inline const size_t arch = 0;
inline const char* platform = "linux";
inline const char* libExt = ".so";

#include <unistd.h>
inline int CurrentThreadID()
{
	return gettid();
}

#else
#endif 