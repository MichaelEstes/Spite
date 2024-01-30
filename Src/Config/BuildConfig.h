#pragma once
#include <fstream>

#include "EASTL/string.h"
#include "EASTL/vector.h"
#include "../Log/Logger.h"
#include "../Containers/InplaceString.h"

enum ArgType
{
	StringArg,
	EnumArg,
};

struct Arg
{
	eastl::string name;
	eastl::string description;
	bool required;
	eastl::string defaultVal;
	eastl::vector<eastl::string> options;
	ArgType type;

	Arg()
	{
		name = "";
		description = "";
		required = false;
		defaultVal = "";
		options = eastl::vector<eastl::string>();
		type = StringArg;
	}

	Arg(const Arg& copy)
	{
		name = copy.name;
		description = copy.description;
		required = copy.required;
		defaultVal = copy.defaultVal;
		options = copy.options;
		type = copy.type;
	}

	bool IsValid()
	{
		return name != "" && description != "" && (type != ArgType::EnumArg ||
			(type == ArgType::EnumArg && options.size() > 0));
	}
};

size_t EatWhitespace(eastl::string& contents, size_t start)
{
	size_t index = start;
	while (index < contents.size() && std::isspace(contents[index])) index += 1;
	return index;
}

enum Context
{
	ParseLabel,
	ParseDesc,
	ParseReq,
	ParseDef,
	ParseOpt,
	ParseType,
	ParseInvalid
};

eastl::string ParseToChar(eastl::string& contents, size_t& index, char to)
{
	size_t start = index;
	while (index < contents.size() && contents[index] != to) index += 1;
	return eastl::string(&contents[start], index - start);
}

eastl::string ParseToSpace(eastl::string& contents, size_t& index)
{
	size_t start = index;
	while (index < contents.size() && !std::isspace(contents[index])) index += 1;
	return eastl::string(&contents[start], index - start);
}

Context ContextFromLabel(eastl::string& label)
{
	switch (label[0])
	{
	case 'd':
		if (label == "description") return Context::ParseDesc;
		else if (label == "default") return Context::ParseDef;
		break;
	case 'r':
		if (label == "required") return Context::ParseReq;
		break;
	case 'o':
		if (label == "options") return Context::ParseOpt;
		break;
	case 't':
		if (label == "type") return Context::ParseType;
		break;

	default:
		break;
	}

	return Context::ParseInvalid;
}

Arg ParseArg(eastl::string& contents, size_t& index)
{
	Arg arg = Arg();
	eastl::string curr;
	size_t start = index;

	// Parse name
	arg.name = ParseToSpace(contents, start);
	start = EatWhitespace(contents, start);

	Context context = ParseLabel;
	while (start < contents.size() && contents[start] != '-')
	{
		switch (context)
		{
		case ParseLabel:
		{
			eastl::string label = ParseToSpace(contents, start);
			context = ContextFromLabel(label);
			break;
		}
		case ParseDesc:
			arg.description = ParseToChar(contents, start, '\n');
			context = Context::ParseLabel;
			break;
		case ParseReq:
			arg.required = true;
			context = Context::ParseLabel;
			break;
		case ParseDef:
			arg.defaultVal = ParseToChar(contents, start, '\n');
			context = Context::ParseLabel;
			break;
		case ParseOpt:
		{
			size_t curr = start;
			while (contents[curr] != '\n')
			{
				while (contents[curr] != ',' && contents[curr] != '\n')
				{
					curr += 1;
				}
				arg.options.push_back(eastl::string(&contents[start], curr - start));
				if (contents[curr] == ',') curr += 1;
				start = curr;
			}
			context = Context::ParseLabel;
			break;
		}
		case ParseType:
		{
			eastl::string type = ParseToChar(contents, start, '\n');
			if (type == "enum") arg.type = ArgType::EnumArg;
			else arg.type = ArgType::StringArg;

			context = Context::ParseLabel;
			break;
		}
		default:
			break;
		}

		start = EatWhitespace(contents, start);
	}

	index = start;
	return arg;
}

int BuildConfig(const eastl::string& fileLoc, const eastl::string& outDir)
{
	std::ifstream file = std::ifstream(fileLoc.c_str(), std::fstream::in);
	if (file.fail())
	{
		Logger::FatalError("Unable to open src file: " + fileLoc);
	}

	eastl::string contents;
	file.seekg(0, std::ios::end);
	contents.resize(file.tellg());
	size_t contentCount = contents.size();
	file.seekg(0, std::ios::beg);
	file.read(&contents[0], contentCount);
	file.close();


	size_t index = EatWhitespace(contents, 0);

	eastl::vector<Arg> args = eastl::vector<Arg>();

	while (contents[index] == '-')
	{
		Arg arg = ParseArg(contents, index);
		if (arg.IsValid()) args.push_back(arg);
		else return 1;
	}

	return 0;
}

