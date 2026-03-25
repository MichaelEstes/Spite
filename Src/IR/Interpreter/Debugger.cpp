#include "Debugger.h"
#include "Interpreter.h"

void DebugTcpServer::PublishStacktrace(int threadID)
{
	Interpreter* interpreter = this->interpreterMap[threadID];

	eastl::string stacktraceMsg = commandKeys.stackTraceKey + "|" + eastl::to_string(threadID) + "\n";

	eastl::deque<SpiteIR::Function*>& callstack = interpreter->callStack;

	for (SpiteIR::Function* func : callstack) 
	{
		stacktraceMsg += func->name + "|" + PositionToString(func->metadata.position) + "\n";
	}

	stacktraceMsg += commandKeys.endKey + "\n";

	SendLine(stacktraceMsg);
}