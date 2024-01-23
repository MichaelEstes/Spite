#pragma once
#include <initializer_list>
#include <utility>

const int offset = 32;
const int size = 128 - offset;

template<typename T, typename T2>
class Trie
{

public:
	class Node
	{
	public:
		char val;
		T2 completeVal;
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

	Trie(std::initializer_list<std::pair<T, T2>> list)
	{
		this->root = new Node('\0');

		for (const auto& pair : list) 
		{
			T key = pair.first;
			T2 value = pair.second;

			Node* current = root;
			for (char c : key) {
				char index = c - offset;
				if (current->children[index] == nullptr) {
					current->children[index] = new Node(c);
				}
				current = current->children[index];
			}
			current->completeVal = value;
			current->completed = true;
		}
	}

	~Trie()
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