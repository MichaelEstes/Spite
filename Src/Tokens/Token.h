#pragma once

#include "EASTL/string.h"

#include "../Containers/StringView.h"
#include "../Parsing/Position.h"
#include "../Utils/Utils.h"

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
	TrueLiteral,
	FalseLiteral,

	// TopLevelKeywords 
	Import,
	Package,
	State,
	Assert,
	Extern,
	Global,
	Where,
	OnCompile,
	OnCompileDebug,
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
	Float128,
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
	As,
	At,
	FatArrow,
	Tilde,
	AtOp,

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
	StringView val;
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

	Token(StringView& val, Position& pos, TokenType type, UniqueType uniqueType, size_t index)
	{
		this->val = val;
		this->pos = pos;
		this->type = type;
		this->uniqueType = uniqueType;
		this->index = index;
	}

	Token(const char* val)
	{
		this->val = StringView(val);
		this->pos = Position();
		this->type = TokenType::Identifier;
		this->uniqueType = UniqueType::Name;
		this->index = 0;
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

static Token emptyToken = "";
static Token thisToken = "this";
