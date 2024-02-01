#pragma once
#include <initializer_list>
#include <utility>
#include <fstream>

#include "EASTL/string.h"
#include "EASTL/vector.h"
#include "EASTL/tuple.h"
#include "../Log/Logger.h"
#include "../Containers/InplaceString.h"

enum ArgType
{
	StringArg,
	EnumArg,
	BoolArg,
};

struct ParsedArg
{
	eastl::string name;
	eastl::string description;
	bool required;
	eastl::string defaultVal;
	eastl::vector<eastl::string> options;
	ArgType type;

	ParsedArg()
	{
		name = "";
		description = "";
		required = false;
		defaultVal = "";
		options = eastl::vector<eastl::string>();
		type = StringArg;
	}

	ParsedArg(const ParsedArg& copy)
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

ParsedArg ParseArg(eastl::string& contents, size_t& index)
{
	ParsedArg arg = ParsedArg();
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
				start = EatWhitespace(contents, curr);
			}
			context = Context::ParseLabel;
			break;
		}
		case ParseType:
		{
			eastl::string type = ParseToChar(contents, start, '\n');
			if (type == "enum") arg.type = ArgType::EnumArg;
			else if (type == "bool") arg.type = ArgType::BoolArg;
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

inline void TrimPrec(eastl::string& str)
{
	size_t index = 0;
	while (index < str.size() && !std::isalpha(str[index])) index += 1;
	if (!(index < str.size())) return;
	str = eastl::string(&str[index], str.size() - index);
}

inline void MakeFirstUpper(eastl::string& str)
{
	str[0] = std::toupper(str[0]);
}

eastl::string TrimAndUpper(eastl::string str)
{
	TrimPrec(str);
	MakeFirstUpper(str);
	return str;
}

eastl::string BuildEnum(ParsedArg& arg)
{
	eastl::string enumStr = "enum ";
	eastl::string name = TrimAndUpper(arg.name);

	enumStr += name + "\n{\n";

	for (eastl::string option : arg.options)
	{
		eastl::string enumName = TrimAndUpper(option);
		enumStr += "\t" + enumName + ",\n";
	}
	enumStr += "\t" + name + "Invalid\n};\n\n" + name + " StringTo" + name + "(const eastl::string& str)\n{\n";

	bool elif = false;
	for (eastl::string option : arg.options)
	{
		if (elif) enumStr += "\telse if";
		else
		{
			enumStr += "\tif";
			elif = true;
		}
		enumStr += " (str == \"" + option + "\") return " + name + "::" + TrimAndUpper(option) + ";\n";
	}
	enumStr += "\treturn " + name + "::" + name + "Invalid;\n}\n\n";

	return enumStr;
}

template<typename T>
struct Arg
{
	eastl::string name;
	eastl::string description;
	T defaultVal;
	eastl::vector<eastl::string> options;
};

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

	eastl::vector<ParsedArg> args = eastl::vector<ParsedArg>();

	while (contents[index] == '-')
	{
		ParsedArg arg = ParseArg(contents, index);
		if (arg.IsValid()) args.push_back(arg);
		else return 1;
	}

	eastl::string filePath = outDir + "\\NewConfig.h";
	std::ofstream outfile(filePath.c_str());

	outfile << "//Auto generated\n";
	outfile << "#pragma once\n#include <initializer_list>\n#include <utility>\n#include \"EASTL/string.h\"\n#include \"EASTL/vector.h\"\n\n";

	for (ParsedArg arg : args)
	{
		if (arg.type == EnumArg)
		{
			outfile << BuildEnum(arg);
		}
	}

	outfile << "struct ArgInfo\n{\n\teastl::string name;\n\teastl::string description;\n\tbool required;\n\teastl::vector<eastl::string> options;\n};\n\n";

	outfile << "struct Config\n{\n";

	for (ParsedArg arg : args)
	{
		eastl::string name = arg.name;
		TrimPrec(name);
		eastl::string configVar = "\t";
		if (arg.type == EnumArg) configVar += TrimAndUpper(arg.name) + " ";
		else if (arg.type == BoolArg) configVar += "bool ";
		else configVar += "eastl::string ";
		configVar += name;
		if (arg.defaultVal != "")
		{
			configVar += " = ";
			if (arg.type == EnumArg) configVar += TrimAndUpper(arg.name) + "::" + TrimAndUpper(arg.defaultVal);
			else if (arg.type == BoolArg) configVar += arg.defaultVal;
			else configVar += "\"" + arg.defaultVal + "\"";
		}
		configVar += ";\n";
		outfile << configVar;
	}

	outfile << "};\n\nConfig ParseConfig(int argc, char** argv)\n{\n\tConfig config = Config();\n\teastl::vector<ArgInfo> argInfo = {";

	for (int i = 0; i < args.size(); i++)
	{
		ParsedArg arg = args[i];
		eastl::string argInfo = "{";
		argInfo += "\"" + arg.name + "\", \"" + arg.description + "\", "
			+ (arg.required ? "true" : "false") + ", {";
		for (int j = 0; j < arg.options.size(); j++)
		{
			eastl::string str = arg.options[j];
			argInfo += "\"" + str + "\"";
			if (j < arg.options.size() - 1) argInfo += ", ";
		}
		argInfo += "}}";
		if (i < args.size() - 1) argInfo += ", ";

		outfile << argInfo;
	}
	outfile << "};\n\tint i = 0;\n\twhile (i < argc)\n\t{\n\t\teastl::string arg(argv[i]);\n\t\tfor (ArgInfo argInfo : argInfos)\n\t\t{\n";

	bool elif = false;
	for (ParsedArg arg : args)
	{
		eastl::string argCheck = "";
		if (elif) argCheck += "\t\t\telse if ";
		else
		{
			argCheck += "\t\t\tif ";
			elif = true;
		}

		argCheck += "(arg == \"" + arg.name + "\")\n\t\t\t{\n";

		switch (arg.type)
		{
		case StringArg:
			break;
		case EnumArg:
			break;
		case BoolArg:
			break;
		default:
			break;

		}

		argCheck += "\t\t\t}\n";
		outfile << argCheck;
	}

	outfile << std::endl;
	outfile.close();

	Arg<eastl::string> arg = { "file", "desc", "str", {"one", "two", "three"} };

	return 0;
}

