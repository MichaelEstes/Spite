#pragma once
#include <iostream>

#include "EASTL/vector.h"
#include "EASTL/string.h"

#include "Token.h"
#include "TokenTree.h"

#include "../Parsing/Position.h"
#include "../Utils/Utils.h"
#include "../Utils/Profiler.h"
#include "../Config/Config.h"
#include "../Log/Logger.cpp"

extern Config config;

static TokenTree tokenTypeLookup = {
	{ "import", TokenType::Keyword, UniqueType::Import },
	{ "package", TokenType::Keyword, UniqueType::Package },
	{ "state", TokenType::Keyword, UniqueType::State },
	{ "enum", TokenType::Keyword, UniqueType::Enum },
	{ "extern", TokenType::Keyword, UniqueType::Extern },
	{ "assert", TokenType::Keyword, UniqueType::AssertTok },
	{ "log", TokenType::Keyword, UniqueType::LogTok },
	{ "#sizeof", TokenType::Keyword, UniqueType::SizeOfTok },
	{ "#alignof", TokenType::Keyword, UniqueType::AlignOfTok },
	{ "where", TokenType::Keyword, UniqueType::Where },
	{ "operator", TokenType::Keyword, UniqueType::OperatorOverload },
	{ "#compile", TokenType::Keyword, UniqueType::OnCompile },
	{ "#debug", TokenType::Keyword, UniqueType::OnCompileDebug },
	{ "#link", TokenType::Keyword, UniqueType::Link },

	{ "new", TokenType::Keyword, UniqueType::New },
	{ "delete", TokenType::Keyword, UniqueType::Delete },
	{ "fixed", TokenType::Keyword, UniqueType::Fixed },
	{ "as", TokenType::Keyword, UniqueType::As },
	//{ "ref", TokenType::Keyword, UniqueType::Ref },

	//{ "public", TokenType::AccessModifier, UniqueType::Public },
	//{ "private", TokenType::AccessModifier, UniqueType::Private },

	{ "any", TokenType::Keyword, UniqueType::Any },

	{ "void", TokenType::Primitive, UniqueType::Void },
	{ "bool", TokenType::Primitive, UniqueType::Bool },
	{ "byte", TokenType::Primitive, UniqueType::Byte },
	{ "ubyte", TokenType::Primitive, UniqueType::Ubyte },
	{ "int", TokenType::Primitive, UniqueType::Int },
	{ "int16", TokenType::Primitive, UniqueType::Int16 },
	{ "int32", TokenType::Primitive, UniqueType::Int32 },
	{ "int64", TokenType::Primitive, UniqueType::Int64 },
	{ "int128", TokenType::Primitive, UniqueType::Int128 },
	{ "uint", TokenType::Primitive, UniqueType::Uint },
	{ "uint16", TokenType::Primitive, UniqueType::Uint16 },
	{ "uint32", TokenType::Primitive, UniqueType::Uint32 },
	{ "uint64", TokenType::Primitive, UniqueType::Uint64 },
	{ "uint128", TokenType::Primitive, UniqueType::Uint128 },
	{ "float", TokenType::Primitive, UniqueType::Float },
	{ "float32", TokenType::Primitive, UniqueType::Float32 },
	{ "float64", TokenType::Primitive, UniqueType::Float64 },
	{ "float128", TokenType::Primitive, UniqueType::Float128 },
	{ "string", TokenType::Primitive, UniqueType::String },

	{ "true", TokenType::Literal, UniqueType::TrueLiteral },
	{ "false", TokenType::Literal, UniqueType::FalseLiteral },

	{ "if", TokenType::Flow, UniqueType::If },
	{ "else", TokenType::Flow, UniqueType::Else },
	{ "for", TokenType::Flow, UniqueType::For },
	{ "while", TokenType::Flow, UniqueType::While },
	{ "do", TokenType::Flow, UniqueType::Do },
	{ "switch", TokenType::Flow, UniqueType::Switch },
	{ "case", TokenType::Flow, UniqueType::Case },
	{ "default", TokenType::Flow, UniqueType::Default },
	{ "continue", TokenType::Flow, UniqueType::Continue },
	{ "break", TokenType::Flow, UniqueType::Break },
	{ "return", TokenType::Flow, UniqueType::Return },
	{ "defer", TokenType::Flow, UniqueType::Defer },

	//{ "at", TokenType::Decorator, UniqueType::At },
	{ "=>", TokenType::Decorator, UniqueType::FatArrow },
	{ "...", TokenType::Decorator, UniqueType::Ellipsis },
	{ "~", TokenType::Decorator, UniqueType::Tilde },
	{ "@", TokenType::Decorator, UniqueType::AtOp },
	{ "<-", TokenType::Decorator, UniqueType::Arrow },

	{ "+", TokenType::Operator, UniqueType::Add },
	{ "-", TokenType::Operator, UniqueType::Subtract },
	{ "*", TokenType::Operator, UniqueType::Multiply },
	{ "/", TokenType::Operator, UniqueType::Divide },
	{ "%", TokenType::Operator, UniqueType::Modulo },
	{ "&", TokenType::Operator, UniqueType::And },
	{ "|", TokenType::Operator, UniqueType::Or },
	{ "^", TokenType::Operator, UniqueType::Xor },
	{ "<<", TokenType::Operator, UniqueType::Shiftl },
	{ ">>", TokenType::Operator, UniqueType::Shiftr },
	{ "&^", TokenType::Operator, UniqueType::AndNot },
	{ "+=", TokenType::Operator, UniqueType::AddAssign },
	{ "-=", TokenType::Operator, UniqueType::SubtractAssign },
	{ "*=", TokenType::Operator, UniqueType::MultiplyAssign },
	{ "/=", TokenType::Operator, UniqueType::DivideAssign },
	{ "%=", TokenType::Operator, UniqueType::ModuloAssign },
	{ "&=", TokenType::Operator, UniqueType::AndAssign },
	{ "|=", TokenType::Operator, UniqueType::OrAssign },
	{ "^=", TokenType::Operator, UniqueType::XorAssign },
	{ "<<=", TokenType::Operator, UniqueType::ShiftlAssign },
	{ ">>=", TokenType::Operator, UniqueType::ShiftrAssign },
	{ "&^=", TokenType::Operator, UniqueType::AndNotAssign },
	{ "&&", TokenType::Operator, UniqueType::LogicAnd },
	{ "||", TokenType::Operator, UniqueType::LogicOr },
	{ "++", TokenType::Operator, UniqueType::Increment },
	{ "--", TokenType::Operator, UniqueType::Decrement },
	{ "==", TokenType::Operator, UniqueType::Equal },
	{ "<", TokenType::Operator, UniqueType::Less },
	{ ">", TokenType::Operator, UniqueType::Greater },
	{ "=", TokenType::Operator, UniqueType::Assign },
	{ "!", TokenType::Operator, UniqueType::Not },
	{ "!=", TokenType::Operator, UniqueType::NotEql },
	{ "<=", TokenType::Operator, UniqueType::LessEqual },
	{ ">=", TokenType::Operator, UniqueType::GreaterEqual },
	{ ":=", TokenType::Operator, UniqueType::ImplicitAssign },
	{ "in", TokenType::Operator, UniqueType::In },
	{ "..", TokenType::Operator, UniqueType::To },
	{ "[]", TokenType::Operator, UniqueType::Array },

	{ "(", TokenType::Seperator, UniqueType::Lparen },
	{ "[", TokenType::Seperator, UniqueType::Lbrack },
	{ "{", TokenType::Seperator, UniqueType::Lbrace },
	{ ",", TokenType::Seperator, UniqueType::Comma },
	{ ".", TokenType::Seperator, UniqueType::Period },
	{ ")", TokenType::Seperator, UniqueType::Rparen },
	{ "]", TokenType::Seperator, UniqueType::Rbrack },
	{ "}", TokenType::Seperator, UniqueType::Rbrace },
	{ ";", TokenType::Seperator, UniqueType::Semicolon },
	{ ":", TokenType::Seperator, UniqueType::Colon },
	{ "::", TokenType::Seperator, UniqueType::DoubleColon },
	{ "?", TokenType::Seperator, UniqueType::QuestionMark },
};

struct Tokens
{
	eastl::vector<Token> tokens;
	size_t count;
	eastl::vector<eastl::string*> escapedStrings;

	Tokens()
	{
		count = -1;
		tokens = eastl::vector<Token>();
		context = Context::None;

		identifierParser.Reset(this);
		operatorParser.Reset(this);
	}

	~Tokens()
	{
		for (eastl::string* str : escapedStrings)
		{
			delete str;
		}
	}

	void Init(size_t fileSize)
	{
		tokens.reserve(fileSize / 20);
	}

	void Tokenize(Token*& out, char* val, char* next, Position& pos)
	{
		if (context == Context::None)
		{
			SetContext(val, *next);
		}

		switch (this->context)
		{
		case Context::Number:
			numberParser.GetToken(val, next, pos, this, out);
			break;
		case Context::String:
			stringParser.GetToken(val, next, pos, this, out);
			break;
		case Context::Operator:
			operatorParser.GetToken(val, next, pos, this, out);
			break;
		case Context::Identifier:
			identifierParser.GetToken(val, next, pos, this, out);
			break;
		case Context::Comment:
			commentParser.GetToken(val, next, pos, this, out);
			break;
		default:
			break;
		}
	}

	void Finalize()
	{
		StringView val = StringView();
		CreateToken(val, tokens[count].pos, TokenType::EndOfFile, UniqueType::UniqueUnknown);
		//PrintTokens();
	}

	void PrintTokens()
	{
		for (int i = 0; i <= count; i++)
		{
			Token& curr = tokens[i];
			std::cout << curr.type << ":" << curr.uniqueType << " " << curr.ToString() << '\n';
		}
	}

	inline Token* First()
	{
		return &tokens[0];
	}

	inline Token* Last()
	{
		return &tokens[count];
	}

	inline Token* Next(Token* token)
	{
		return &tokens[token->index + 1];
	}

	inline Token* Prev(Token* token)
	{
		return &tokens[token->index - 1];
	}

	inline Token* At(size_t index)
	{
		return &tokens[index];
	}

	enum Context
	{
		None,
		Identifier,
		Operator,
		Number,
		String,
		Comment
	} context;

	void ResetContext()
	{
		context = Context::None;
	}

	void SetContext(char* valPtr, char next)
	{
		char val = *valPtr;
		if (val > 32)
		{
			if (isalpha(val))
			{
				context = Context::Identifier;
				identifierParser.val.start = valPtr;
			}
			else if (val == '/' && next == '/')
			{
				context = Context::Comment;
				commentParser.val.start = valPtr;
			}
			else if (IsSeperator(val))
			{
				context = Context::Operator;
				operatorParser.val.start = valPtr;
			}
			else if (isdigit(val))
			{
				context = Context::Number;
				numberParser.val.start = valPtr;
			}
			else if (val == '"' || val == '\'' || val == '`')
			{
				context = Context::String;
				stringParser.val.start = valPtr;
			}
			else
			{
				context = Context::Identifier;
				identifierParser.val.start = valPtr;
			}
		}
	}

	struct IdentifierParser
	{
		StringView val;
		TokenTree::TokenNode* currTokenType = nullptr;

		inline void GetToken(char* curr, char* next, Position& pos, Tokens* tokens, Token*& token)
		{
			val += curr;
			char currVal = *curr;
			char nextVal = *next;

			currTokenType = currTokenType != nullptr ? currTokenType->GetChild(currVal) : nullptr;

			bool currIsInvalid = tokens->IsInvalidIdentifierChar(currVal);

			if (!currIsInvalid && (nextVal < 33 || tokens->IsSeperator(nextVal)))
			{
				if (currTokenType != nullptr && currTokenType->completed)
				{
					token = tokens->CreateToken(val, pos, currTokenType->type, currTokenType->uniqueType);
				}
				else if (val[0] == '#')
				{
					token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::UniqueUnknown);
					AddError(token, "Illegal use of the '#' symbol, the '#' symbol can only be used in front specified keywords");
				}
				else
				{
					token = tokens->CreateToken(val, pos, TokenType::Identifier, UniqueType::Name);
				}
				Reset(tokens);
			}
			else if (currIsInvalid && !(val.Count() == 1 && currVal == '#'))
			{
				token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::UniqueUnknown);
				eastl::string errMsg = "Invalid character in identifer: ";
				AddError(token, errMsg + currVal);
				Reset(tokens);
			}
		}

		inline void Reset(Tokens* tokens)
		{
			val.Clear();
			currTokenType = tokenTypeLookup.GetRoot();
			tokens->ResetContext();
		}
	} identifierParser;

	struct OperatorParser
	{
		StringView val;
		TokenTree::TokenNode* currTokenType = nullptr;

		inline void GetToken(char* curr, char* next, Position& pos, Tokens* tokens, Token*& token)
		{
			val += curr;
			char currVal = *curr;
			char nextVal = *next;

			currTokenType = currTokenType != nullptr ? currTokenType->GetChild(currVal) : nullptr;
			bool valIsKnown = currTokenType != nullptr && currTokenType->completed;
			bool nextValIsKnown = currTokenType != nullptr && currTokenType->NextCompleted(nextVal);

			if (valIsKnown && !nextValIsKnown)
			{
				token = tokens->CreateToken(val, pos, currTokenType->type, currTokenType->uniqueType);
				Reset(tokens);
			}
			else if (currTokenType == nullptr)
			{
				token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::UniqueUnknown);
				AddError(token, "Unknown operator");
			}
		}

		inline void Reset(Tokens* tokens)
		{
			val.Clear();
			currTokenType = tokenTypeLookup.GetRoot();
			tokens->ResetContext();
		}
	} operatorParser;

	struct NumberParser
	{
		StringView val;
		// 0 is int, 1 is decimal, 2 is hex
		int numberType;
		bool seenDot;

		NumberParser()
		{
			numberType = 0;
			seenDot = false;
		}

		inline void GetToken(char* curr, char* next, Position& pos, Tokens* tokens, Token*& token)
		{
			val += curr;
			char currVal = *curr;
			char nextVal = *next;

			if (currVal == '0' && (nextVal == 'x' || nextVal == 'X') && val.Count() == 1)
			{
				numberType = 2;
				return;
			}
			else if (nextVal == '.')
			{
				if (seenDot)
				{
					token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::UniqueUnknown);
					AddError(token, "Unexpected '.' in number litteral");
					Reset(tokens);
					return;
				}
				seenDot = true;
				numberType = 1;
				return;
			}

			bool nextIsNumber = numberType == 2 ? isxdigit(nextVal) : isdigit(nextVal);
			if (!nextIsNumber)
			{
				switch (numberType)
				{
				case 0:
					token = tokens->CreateToken(val, pos, TokenType::Literal, UniqueType::IntLiteral);
					break;
				case 1:
					token = tokens->CreateToken(val, pos, TokenType::Literal, UniqueType::FloatLiteral);
					break;
				case 2:
					if (val == "0x" || val == "0X")
					{
						token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::UniqueUnknown);
						AddError(token, "Incomplete hex literal");
					}
					else
					{
						token = tokens->CreateToken(val, pos, TokenType::Literal, UniqueType::HexLiteral);
					}
					break;
				default:
					break;
				}

				Reset(tokens);
			}
		}

		inline void Reset(Tokens* tokens)
		{
			val.Clear();
			numberType = 0;
			seenDot = false;
			tokens->ResetContext();
		}
	} numberParser;

	struct StringParser
	{
		StringView val;
		bool escaped;
		bool hasEscapedChar;

		StringParser()
		{
			escaped = false;
			hasEscapedChar = false;
		}

		inline char GetEscapedChar(char c)
		{
			switch (c)
			{
			case 'n':
				return '\n';
			case '\'':
				return '\'';
			case '"':
				return '"';
			case 't':
				return '\t';
			case 'b':
				return '\b';
			case 'a':
				return '\a';
			case '\\':
				return '\\';
			default:
				break;
			}
		}

		inline void GetToken(char* curr, char* next, Position& pos, Tokens* tokens, Token*& token)
		{
			val += curr;
			char currVal = *curr;

			if (!escaped && currVal == '\\')
			{
				escaped = true;
				hasEscapedChar = true;
			}
			else if (!escaped && currVal == val[0] && val.Count() > 1)
			{
				val.start += 1;
				val.last -= 1;
				if (hasEscapedChar)
				{
					size_t count = val.Count();
					eastl::string* escapedStr = new eastl::string();
					escapedStr->reserve(count);
					for (size_t i = 0; i < count; i++)
					{
						char c = val[i];
						if (c == '\\')
						{
							i += 1;
							c = val[i];
							c = GetEscapedChar(c);
						}

						escapedStr->push_back(c);
					}

					tokens->escapedStrings.push_back(escapedStr);
					val.start = escapedStr->begin();
					val.last = escapedStr->end() - 1;
				}

				if (val.Count() == 1)
				{
					token = tokens->CreateToken(val, pos, TokenType::Literal, UniqueType::ByteLiteral);
				}
				else
				{
					token = tokens->CreateToken(val, pos, TokenType::Literal, UniqueType::StringLiteral);
				}

				Reset(tokens);
			}
			else
			{
				escaped = false;
			}
		}

		inline void Reset(Tokens* tokens)
		{
			val.Clear();
			escaped = false;
			hasEscapedChar = false;
			tokens->ResetContext();
		}
	} stringParser;

	struct CommentParser
	{
		StringView val;

		inline void GetToken(char* curr, char* next, Position& pos, Tokens* tokens, Token*& token)
		{
			val += curr;

			if (*curr == '\n')
			{
				if (config.comments)
				{
					token = tokens->CreateToken(val, pos, TokenType::Comment, UniqueType::UniqueUnknown);
				}
				else
				{
					token = &emptyToken;
				}
				Reset(tokens);
			}
		}

		inline void Reset(Tokens* tokens)
		{
			val.Clear();
			tokens->ResetContext();
		}
	} commentParser;

	bool IsSeperator(char val)
	{
		return WithinRange(val, 37, 38) ||
			WithinRange(val, 40, 47) ||
			WithinRange(val, 58, 64) ||
			WithinRange(val, 123, 126) ||
			val == '!' ||
			val == '[' ||
			val == ']' ||
			val == '^';
	}

	inline bool WithinRange(char val, char min, char max)
	{
		return val >= min && val <= max;
	}

	bool IsInvalidIdentifierChar(char val)
	{
		return val == '#' || val == '$' || val == '\'' ||
			val == '"' || val == '\\' || val == '`' || val == '|';
	}

	inline Token* CreateToken(StringView& val, Position& pos, TokenType type, UniqueType uniqueType)
	{
		count += 1;
		return &tokens.emplace_back(val, pos, type, uniqueType, count);
	}
};

