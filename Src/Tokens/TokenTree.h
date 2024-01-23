#pragma once
#include <initializer_list>
#include <utility>

#include "EASTL/memory.h"
#include "EASTL/tuple.h"

const int offset = 32;
const int size = 128 - offset;

template<typename T, typename T2, typename T3>
class TokenTree
{

public:
	class Node
	{
	public:
		char val;
		T2 type;
		T3 uniqueType;
		Node* children[size];
		bool completed;

		Node(char val)
		{
			this->val = val;
			for (int i = 0; i < size; i++) children[i] = nullptr;
			this->completed = false;
		}

		Node* GetChild(char c)
		{
			char index = c - offset;
			return children[index] != nullptr ? children[index] : nullptr;
		}

		bool NextCompleted(char c)
		{
			char index = c - offset;
			return children[index] != nullptr && children[index]->completed;
		}
	};

	TokenTree(std::initializer_list<eastl::tuple<T, T2, T3>> list)
	{
		this->root = new Node('\0');

		for (const auto& val : list)
		{
			T key = eastl::get<0>(val);
			T2 type = eastl::get<1>(val);
			T3 uniqueType = eastl::get<2>(val);

			Node* current = root;
			for (char c : key) {
				char index = c - offset;
				if (current->children[index] == nullptr) {
					current->children[index] = new Node(c);
				}
				current = current->children[index];
			}
			current->type = type;
			current->uniqueType = uniqueType;
			current->completed = true;
		}
	}

	~TokenTree()
	{
		delete root;
	}

	Node* GetRoot()
	{
		return root;
	}

private:
	Node* root;
};