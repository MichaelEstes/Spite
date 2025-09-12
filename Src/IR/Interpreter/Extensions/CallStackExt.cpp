#include "CallStackExt.h"
#include "../Interpreter.h"
#include "../../../Containers/FixedRingBuffer.h"
#include <imgui.h>

eastl::hash_map<int, FixedRingBuffer<eastl::string>> callStackMap = eastl::hash_map<int, FixedRingBuffer<eastl::string>>();

void OnFunction(SpiteIR::Function* func, eastl::vector<SpiteIR::Operand>* params, Interpreter* interpreter)
{
    int threadID = interpreter->threadID;
    if (!MapHas(callStackMap, threadID)) callStackMap.emplace(threadID, FixedRingBuffer<eastl::string>());
	
    auto& buf = callStackMap[threadID];
    buf.Add(func->name);
}

void OnRender(Interpreter* interpreter)
{
    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
    ImGuiIO& io = ImGui::GetIO(); (void)io;

    ImGui::Begin("Callstacks", nullptr, ImGuiWindowFlags_AlwaysAutoResize);

    if (ImGui::BeginTable("GridTable", 3, ImGuiTableFlags_SizingStretchSame)) // 3 columns
    {
        for (auto& [threadID, buf] : callStackMap)
        {
            ImGui::TableNextColumn();

            ImGui::BeginGroup();

            ImGui::TextColored(ImVec4(1, 1, 0, 1), "Thread %d", threadID);

            for (size_t i = 0; i < buf.count; i++)
            {
                auto& string = buf.mem[i];
                ImGui::Text(string.c_str());
            }

            ImGui::Dummy(ImVec2(0, 5));
            ImGui::Separator();

            ImGui::EndGroup();
        }

        ImGui::EndTable();
    }

    ImGui::End();
}

bool registered = RegisterInterpreterExtension(nullptr, &OnFunction, nullptr, nullptr, nullptr, &OnRender);