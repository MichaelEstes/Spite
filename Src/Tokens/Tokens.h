#pragma once
#include <iostream>

#include "EASTL/vector.h"
#include "EASTL/string.h"

#include "../Parsing/Position.h"
#include "../Log/Logger.h"
#include "TokenTree.h"
#include "../Utils/Utils.h"
#include "../Config/Config.h"
#include "../Containers/InplaceString.h"
#include "../Utils/Profiler.h"

extern Config config;

enum TokenType : int
{
	//Known predefined values
	Keyword = ToBit(1),
	AccessModifier = ToBit(2),
	Primitive = ToBit(3),
	Flow = ToBit(4),
	Decorator = ToBit(5),
	Operator = ToBit(6),
	Seperator = ToBit(7),
	Comment = ToBit(8),

	//Unknown programmer defined values
	Identifier = ToBit(9),
	Literal = ToBit(10),
	Invalid = ToBit(11),

	EndOfFile = ToBit(12),

	None = ToBit(13),
};

enum UniqueType
{
	Any,
	Name,

	IntLiteral,
	FloatLiteral,
	HexLiteral,
	StringLiteral,

	// TopLevelKeywords 
	Using,
	Package,
	State,
	Assert,
	Global,
	Where,
	Oncompile,
	Oncompiledebug,
	OperatorOverload,

	// AccessModifierKeywords 
	Public,
	Private,

	// PrimitiveKeywords
	Void,
	Bool,
	Byte,
	Ubyte,
	Int,
	Int16,
	Int32,
	Int64,
	Int128,
	Uint,
	Uint16,
	Uint32,
	Uint64,
	Uint128,
	Float,
	Float32,
	Float64,
	String,

	// FlowControlKeywords 
	If,
	Else,
	For,
	While,
	Do,
	Switch,
	Case,
	Default,
	Continue,
	Break,
	Return,
	Defer,

	// VariableDecorationKeywords 
	New,
	Fixed,
	Delete,
	Pointer,
	Rawpointer,
	As,
	At,

	// OperatorKeywords 
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	And,
	Or,
	Xor,
	Shiftl,
	Shiftr,
	AndNot,
	AddAssign,
	SubtractAssign,
	MultiplyAssign,
	DivideAssign,
	ModuloAssign,
	AndAssign,
	OrAssign,
	XorAssign,
	ShiftlAssign,
	ShiftrAssign,
	AndNotAssign,
	LogicAnd,
	LogicOr,
	Arrow,
	Increment,
	Decrement,
	Equal,
	Less,
	Greater,
	Assign,
	Not,
	NotEql,
	LessEqual,
	GreaterEqual,
	ImplicitAssign,
	Ellipsis,
	In,
	To,
	Array,
	FatArrow,
	Tilde,
	AtOp,

	// SEPERATORS 
	Lparen,
	Lbrack,
	Lbrace,
	Comma,
	Period,
	Rparen,
	Rbrack,
	Rbrace,
	Semicolon,
	Colon,
	DoubleColon,
	Ternary,
};

struct Token
{
	InplaceString val;
	Position pos;
	TokenType type;
	UniqueType uniqueType;
	size_t index;

	Token()
	{
		type = TokenType::None;
		uniqueType = UniqueType::Any;
		index = 0;
	}

	Token(InplaceString& val, Position& pos, TokenType type, UniqueType uniqueType, size_t index)
	{
		this->val = val;
		this->pos = pos;
		this->type = type;
		this->uniqueType = uniqueType;
		this->index = index;
	}

	inline bool IsCompleted()
	{
		return type != TokenType::None;
	}

	inline bool IsValid()
	{
		return type != TokenType::Invalid;
	}

	inline eastl::string ToString()
	{
		return val.ToString();
	}
};

class Tokens
{

public:

	Tokens()
	{
		count = -1;
		tokens = eastl::vector<Token>();
		context = Context::None;

		identifierParser.Reset(this);
		operatorParser.Reset(this);
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
		InplaceString val = InplaceString();
		CreateToken(val, tokens[count].pos, TokenType::EndOfFile, UniqueType::Any);

		Logger::Info("Created " + eastl::to_string(count) + " Tokens");

		//PrintTokens();
	}

	void PrintTokens()
	{
		for (int i = 0; i <= count; i++)
		{
			Token curr = tokens[i];
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

	eastl::vector<Token> tokens;
	size_t count;

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
		InplaceString val;
		TokenTree<eastl::string, TokenType, UniqueType>::Node* currTokenType;

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
					token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::Any);
					Logger::AddError(pos, token->index, "Illegal use of the '#' symbol, the '#' symbol can only be used in front of OnCompile and OnCompileDebug");
				}
				else
				{
					token = tokens->CreateToken(val, pos, TokenType::Identifier, UniqueType::Name);
				}
				Reset(tokens);
			}
			else if (currIsInvalid && !(val.count == 1 && currVal == '#'))
			{
				token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::Any);
				eastl::string errMsg = "Invalid character in identifer: ";
				Logger::AddError(pos, token->index, errMsg + currVal);
				Reset(tokens);
			}
		}

		inline void Reset(Tokens* tokens)
		{
			val.Clear();
			currTokenType = tokens->tokenTypeLookup.GetRoot();
			tokens->ResetContext();
		}
	} identifierParser;

	struct OperatorParser
	{
		InplaceString val;
		TokenTree<eastl::string, TokenType, UniqueType>::Node* currTokenType;

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
				token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::Any);
				Logger::AddError(pos, token->index, "Unknown operator");
			}
		}

		inline void Reset(Tokens* tokens)
		{
			val.Clear();
			currTokenType = tokens->tokenTypeLookup.GetRoot();
			tokens->ResetContext();
		}
	} operatorParser;

	struct NumberParser
	{
		InplaceString val;
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

			if (currVal == '0' && (nextVal == 'x' || nextVal == 'X') && val.count == 1)
			{
				numberType = 2;
				return;
			}
			else if (nextVal == '.')
			{
				if (seenDot)
				{
					token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::Any);
					Logger::AddError(pos, token->index, "Unexpected '.' in number litteral");
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
						token = tokens->CreateToken(val, pos, TokenType::Invalid, UniqueType::Any);
						Logger::AddError(pos, token->index, "Incomplete hex literal");
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
		InplaceString val;
		bool escaped;

		StringParser()
		{
			escaped = false;
		}

		inline void GetToken(char* curr, char* next, Position& pos, Tokens* tokens, Token*& token)
		{
			val += curr;
			char currVal = *curr;

			if (currVal == '\\')
			{
				escaped = true;
			}
			else if (currVal == val[0] && !escaped && val.count > 1)
			{
				token = tokens->CreateToken(val, pos, TokenType::Literal, UniqueType::StringLiteral);
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
			tokens->ResetContext();
		}
	} stringParser;

	struct CommentParser
	{
		InplaceString val;

		inline void GetToken(char* curr, char* next, Position& pos, Tokens* tokens, Token*& token)
		{
			val += curr;

			if (*next == '\n')
			{
				if (config.keepComments)
				{
					token = tokens->CreateToken(val, pos, TokenType::Comment, UniqueType::Any);
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
			val == '!' ||
			val == '[' ||
			val == ']' ||
			val == '{' ||
			val == '}' ||
			val == '~' ||
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

	inline Token* CreateToken(InplaceString& val, Position& pos, TokenType type, UniqueType uniqueType)
	{
		count += 1;
		return &tokens.emplace_back(val, pos, type, uniqueType, count);
	}

	TokenTree<eastl::string, TokenType, UniqueType> tokenTypeLookup = {
		{ "using", TokenType::Keyword, UniqueType::Using },
		{ "package", TokenType::Keyword, UniqueType::Package },
		{ "state", TokenType::Keyword, UniqueType::State },
		{ "global", TokenType::Keyword, UniqueType::Global },
		{ "Assert", TokenType::Keyword, UniqueType::Assert },
		{ "Where", TokenType::Keyword, UniqueType::Where },
		{ "#OnCompile", TokenType::Keyword, UniqueType::Oncompile },
		{ "#OnCompileDebug", TokenType::Keyword, UniqueType::Oncompiledebug },
		{ "operator", TokenType::Keyword, UniqueType::OperatorOverload },

		{ "public", TokenType::AccessModifier, UniqueType::Public },
		{ "private", TokenType::AccessModifier, UniqueType::Private },

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
		{ "string", TokenType::Primitive, UniqueType::String },

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

		{ "new", TokenType::Decorator, UniqueType::New },
		{ "fixed", TokenType::Decorator, UniqueType::Fixed },
		{ "delete", TokenType::Decorator, UniqueType::Delete },
		{ "~*", TokenType::Decorator, UniqueType::Rawpointer },
		{ "as", TokenType::Decorator, UniqueType::As },
		{ "at", TokenType::Decorator, UniqueType::At },

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
		{ "<-", TokenType::Operator, UniqueType::Arrow },
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
		{ "...", TokenType::Operator, UniqueType::Ellipsis },
		{ "in", TokenType::Operator, UniqueType::In },
		{ "..", TokenType::Operator, UniqueType::To },
		{ "[]", TokenType::Operator, UniqueType::Array },
		{ "=>", TokenType::Operator, UniqueType::FatArrow },
		{ "~", TokenType::Operator, UniqueType::Tilde },
		{ "@", TokenType::Operator, UniqueType::AtOp },

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
		{ "?", TokenType::Seperator, UniqueType::Ternary },
	};
};

