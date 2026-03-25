#pragma once

#ifdef WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "Ws2_32.lib")
#endif

#include "../IR.h"
#include "../../Log/Logger.h"
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <cstdlib>
#include <functional>
#include <mutex>
#include <thread>

struct Interpreter;

enum class DebugStopReason
{
	Breakpoint,
	Pause,
	Step
};

struct DebugStoppedEvent
{
	Position* position;
	DebugStopReason reason = DebugStopReason::Pause;
	int threadID = -1;
};

struct DebugBreakpoint
{
	eastl::string file = "";
	size_t line = 0;
	size_t columnOffset = 0;
};

struct DebugInterface
{
	using EnterFn = void(*)(int, Interpreter*);
	using SafePointFn = void(*)(int, SpiteIR::Instruction& inst);
	using BreakpointFn = void(*)(int, Position*);
	using ExitFn = void(*)(int);

	EnterFn Enter = nullptr;
	SafePointFn SafePoint = nullptr;
	BreakpointFn Breakpoint = nullptr;
	ExitFn Exit = nullptr;
};


struct DebugBreakpointHash
{
	size_t operator()(const DebugBreakpoint& breakpointPos) const
	{
		eastl::hash<eastl::string> fileHasher;
		size_t hash = fileHasher(breakpointPos.file);
		hash ^= breakpointPos.line;
		return hash;
	}
};

inline bool operator==(const DebugBreakpoint& l, const DebugBreakpoint& r)
{
	return l.file == r.file && l.line == r.line;
}

struct Debugger
{
	using PublishStoppedCallback = void(*)(const DebugStoppedEvent&);

	PublishStoppedCallback publishStoppedCallback = nullptr;

	eastl::vector<DebugBreakpoint> breakpoints;

	std::mutex pauseMutex;
	std::condition_variable pauseCondition;
	std::atomic<bool> pauseRequested = false;
	std::atomic<int> lastStoppedThreadID = -1;

	const size_t invalidBreakpointIndex = (size_t)-1;

	Debugger(PublishStoppedCallback publishStopped)
		: publishStoppedCallback(publishStopped)
	{
	}

	void SetBreakpoint(DebugBreakpoint& breakpoint)
	{
		if (BreakpointIndex(breakpoint) == invalidBreakpointIndex)
		{
			breakpoints.push_back(breakpoint);
		}
	}

	void SetBreakpoint(const Position& pos)
	{
		DebugBreakpoint breakpoint;
		breakpoint.file = *pos.file;
		breakpoint.line = pos.line;
		breakpoint.columnOffset = pos.columnOffset;
		return SetBreakpoint(breakpoint);
	}

	void RemoveBreakpoint(DebugBreakpoint& breakpoint)
	{
		size_t breakpointIndex = BreakpointIndex(breakpoint);
		if (breakpointIndex == invalidBreakpointIndex) return;
		breakpoints.erase(breakpoints.begin() + breakpointIndex);
	}

	size_t BreakpointIndex(DebugBreakpoint& breakpoint)
	{
		for (size_t i = 0; i < breakpoints.size(); i++)
		{
			if (breakpoint == breakpoints.at(i)) return i;
		}

		return (size_t)-1;
	}

	bool HasBreakpointAt(const Position* position)
	{
		if (!position->file) return false;

		for (DebugBreakpoint& breakpoint : breakpoints)
		{
			if (*position->file == breakpoint.file &&
				position->line == breakpoint.line)
			{
				if (breakpoint.columnOffset == invalidBreakpointIndex) return true;
				else return position->columnOffset == breakpoint.columnOffset;
			}
		}

		return false;
	}
	
	void Pause()
	{
		pauseRequested.store(true, std::memory_order_release);
	}

	void Pause(int threadID,
		Position* position,
		DebugStopReason reason = DebugStopReason::Pause)
	{
		{
			std::lock_guard<std::mutex> lock(pauseMutex);
			pauseRequested.store(true, std::memory_order_release);
			lastStoppedThreadID.store(threadID, std::memory_order_release);
		}

		DebugStoppedEvent event;
		event.reason = reason;
		event.threadID = threadID;
		event.position = position;
		publishStoppedCallback(event);
		
		std::unique_lock<std::mutex> lock(pauseMutex);
		while (pauseRequested.load(std::memory_order_acquire))
		{
			pauseCondition.wait_for(lock, std::chrono::milliseconds(2));
		}
	}

	void Resume()
	{
		pauseRequested.store(false, std::memory_order_release);
		pauseCondition.notify_all();
	}

	void StepInto() { Resume(); }
	void StepOut() { Resume(); }
	void StepOver() { Resume(); }
};

struct DebugTcpServer
{
	std::thread serverThread;
	Debugger* controller = nullptr;
	SpiteIR::IR* ir = nullptr;
	eastl::hash_map<int, Interpreter*> interpreterMap;
	struct {
		const eastl::string continueKey = "continue";
		const eastl::string pauseKey = "pause";
		const eastl::string stepKey = "step";
		const eastl::string setBreakpointKey = "setbp";
		const eastl::string clearBreakpointKey = "clearbp";
		const eastl::string breakpointKey = "breakpoint";
		const eastl::string stackTraceKey = "stacktrace";
		const eastl::string endKey = "end";
		const eastl::string stoppedKey = "stopped";
		const eastl::string readyKey = "ready";
		const eastl::string enterKey = "enter";
		const eastl::string exitKey = "exit";
		const eastl::string errorKey = "error";
		const eastl::string ackKey = "ok";
	} commandKeys;

#ifdef WIN32
	SOCKET listenSocket = INVALID_SOCKET;
	SOCKET clientSocket = INVALID_SOCKET;
	std::mutex socketMutex;
#endif
	
	std::atomic<bool> running = false;

	void Start(Debugger* controller, SpiteIR::IR* ir)
	{
		if (running.load(std::memory_order_acquire)) return;
		this->controller = controller;
		this->ir = ir;
		running.store(true, std::memory_order_release);
		serverThread = std::thread([this]() { Run(); });
	}

	void Stop()
	{
		running.store(false, std::memory_order_release);

#ifdef WIN32
		{
			std::lock_guard<std::mutex> lock(socketMutex);
			if (clientSocket != INVALID_SOCKET) closesocket(clientSocket);
			clientSocket = INVALID_SOCKET;
			if (listenSocket != INVALID_SOCKET) closesocket(listenSocket);
			listenSocket = INVALID_SOCKET;
		}
#endif
		if (serverThread.joinable()) serverThread.join();
	}

	void PublishThreadEnter(int threadID, Interpreter* interpreter)
	{
		interpreterMap.emplace(threadID, interpreter);
		eastl::string msg = commandKeys.enterKey + "|" + eastl::to_string(threadID) + "\n";
		SendLine(msg);
	}

	eastl::string PositionToString(Position* pos)
	{
		return *pos->file + "|" +
			eastl::to_string(pos->line) + "|" +
			eastl::to_string(pos->columnOffset);
	}

	void PublishStopped(const DebugStoppedEvent& event)
	{
		eastl::string reason = commandKeys.pauseKey;
		if (event.reason == DebugStopReason::Breakpoint) reason = commandKeys.breakpointKey;
		else if (event.reason == DebugStopReason::Step) reason = commandKeys.stepKey;

		eastl::string msg = commandKeys.stoppedKey + "|" + reason + "|" +
			eastl::to_string(event.threadID) + "|" +
			PositionToString(event.position) + "\n";
		SendLine(msg);
	}

	void PublishThreadExit(int threadID)
	{
		interpreterMap.erase(threadID);
		eastl::string msg = commandKeys.exitKey + "|" + eastl::to_string(threadID) + "\n";
		SendLine(msg);
	}

	void PublishStacktrace(int threadID);

	eastl::vector<eastl::string> Split(const eastl::string& str, char ch)
	{
		eastl::vector<eastl::string> parts;
		eastl::string current;
		for (char c : str)
		{
			if (c == ch)
			{
				parts.push_back(current);
				current.clear();
			}
			else current.push_back(c);
		}
		parts.push_back(current);
		return parts;
	}

	void HandleLine(const eastl::string& rawLine)
	{
		//Logger::Info("Received: " + rawLine);
		eastl::string line = rawLine;
		if (!line.empty() && line.back() == '\r') line.pop_back();
		if (line.empty() || !controller) return;

		eastl::vector<eastl::string> commandMsg = Split(line, '|');
		if (commandMsg.empty()) return;

		eastl::string key = commandMsg[0];
		eastl::string ack = commandKeys.ackKey + "\n";
		if (key == commandKeys.continueKey)
		{
			controller->Resume();
			SendLine(ack);
			return;
		}
		else if (key == commandKeys.pauseKey)
		{
			controller->Pause();
			SendLine(ack);
			return;
		}
		else if (key == commandKeys.setBreakpointKey && commandMsg.size() > 2)
		{
			DebugBreakpoint breakpoint = DebugBreakpoint();
			breakpoint.file = eastl::string(commandMsg[1].c_str());
			breakpoint.line = (size_t)std::strtoull(commandMsg[2].c_str(), nullptr, 10);
			breakpoint.columnOffset = (size_t)std::strtoull(commandMsg[3].c_str(), nullptr, 10);
			controller->SetBreakpoint(breakpoint);
			SendLine(ack);
			return;
		}
		else if (key == commandKeys.clearBreakpointKey && commandMsg.size() > 2)
		{
			DebugBreakpoint breakpoint = DebugBreakpoint();
			breakpoint.file = eastl::string(commandMsg[1].c_str());
			breakpoint.line = (size_t)std::strtoull(commandMsg[2].c_str(), nullptr, 10);
			breakpoint.columnOffset = (size_t)std::strtoull(commandMsg[3].c_str(), nullptr, 10);
			controller->RemoveBreakpoint(breakpoint);
			SendLine(ack);
			return;
		}
		else if (key == commandKeys.stackTraceKey && commandMsg.size() > 1)
		{
			int threadID = std::stoi(commandMsg[1].c_str());
			PublishStacktrace(threadID);
			return;
		}

		SendLine(commandKeys.errorKey + "|unknown_command\n");
	}

	void SendLine(const eastl::string& text)
	{
#ifdef WIN32
		std::lock_guard<std::mutex> lock(socketMutex);
		if (clientSocket == INVALID_SOCKET) return;
		send(clientSocket, text.c_str(), (int)text.size(), 0);
#endif
	}

	void Run()
	{
#ifdef WIN32
		WSADATA wsaData;
		if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) return;

		listenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
		if (listenSocket == INVALID_SOCKET) return;

		sockaddr_in service = {};
		service.sin_family = AF_INET;
		service.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
		service.sin_port = htons(4712);

		if (bind(listenSocket, (SOCKADDR*)&service, sizeof(service)) == SOCKET_ERROR) return;
		if (listen(listenSocket, 1) == SOCKET_ERROR) return;

		while (running.load(std::memory_order_acquire))
		{
			SOCKET accepted = accept(listenSocket, (sockaddr*)nullptr, nullptr);
			if (accepted == INVALID_SOCKET) break;

			{
				std::lock_guard<std::mutex> lock(socketMutex);
				clientSocket = accepted;
			}
			SendLine(commandKeys.readyKey + "\n");

			eastl::string pending;
			char buffer[1024];
			while (running.load(std::memory_order_acquire))
			{
				int received = recv(accepted, buffer, sizeof(buffer), 0);
				if (received <= 0) break;
				pending.append(buffer, buffer + received);

				size_t nl = pending.find('\n');
				while (nl != eastl::string::npos)
				{
					eastl::string line = pending.substr(0, nl);
					pending.erase(0, nl + 1);
					HandleLine(line);
					nl = pending.find('\n');
				}
			}

			{
				std::lock_guard<std::mutex> lock(socketMutex);
				if (clientSocket != INVALID_SOCKET) closesocket(clientSocket);
				clientSocket = INVALID_SOCKET;
			}
		}

		if (listenSocket != INVALID_SOCKET) closesocket(listenSocket);
		listenSocket = INVALID_SOCKET;
		WSACleanup();
#endif
	}
};

inline Debugger* globalDebugger = nullptr;
inline DebugTcpServer globalDebugTcpServer;

inline void DebugPublishStopped(const DebugStoppedEvent& event)
{
	globalDebugTcpServer.PublishStopped(event);
}

inline void EnsureGlobalDebugger(SpiteIR::IR* ir)
{
	if (!globalDebugger)
	{
		globalDebugger = new Debugger(&DebugPublishStopped);
		globalDebugTcpServer.Start(globalDebugger, ir);
	}
}

inline void DebugNoopEnter(int, Interpreter*) {}
inline void DebugNoopSafePoint(int, SpiteIR::Instruction&) {}
inline void DebugNoopBreakpoint(int, Position*) {}
inline void DebugNoopExit(int) {}

inline void DebugSafePoint(int threadID, SpiteIR::Instruction& inst)
{
	Debugger* debugger = globalDebugger;

	Position* position = &inst.metadata->expressionPosition;
	if (debugger->HasBreakpointAt(position))
	{
		debugger->Pause(threadID, position, DebugStopReason::Breakpoint);
		return;
	}

	if (!debugger->pauseRequested.load(std::memory_order_acquire)) return;
	debugger->Pause(threadID, position, DebugStopReason::Pause);
}

inline void OnDebugBreakpoint(int threadID, Position* position)
{
	Debugger* debugger = globalDebugger;
	debugger->Pause(threadID, position, DebugStopReason::Breakpoint);
}

inline void OnDebugEnter(int threadID, Interpreter* interpreter)
{
	globalDebugTcpServer.PublishThreadEnter(threadID, interpreter);
}

inline void OnDebugExit(int threadID)
{
	globalDebugTcpServer.PublishThreadExit(threadID);
}

inline DebugInterface InactiveDebugInterface()
{
	DebugInterface hooks;
	hooks.Enter = &DebugNoopEnter;
	hooks.SafePoint = &DebugNoopSafePoint;
	hooks.Breakpoint = &DebugNoopBreakpoint;
	hooks.Exit = &DebugNoopExit;
	return hooks;
}

inline DebugInterface ActiveDebugInterface()
{
	DebugInterface hooks;
	hooks.Enter = &OnDebugEnter;
	hooks.SafePoint = &DebugSafePoint;
	hooks.Breakpoint = &OnDebugBreakpoint;
	hooks.Exit = &OnDebugExit;
	return hooks;
}
