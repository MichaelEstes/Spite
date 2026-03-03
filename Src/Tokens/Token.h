#pragma once

#include "EASTL/string.h"

#include "../Containers/StringView.h"
#include "../Parsing/Position.h"
#include "../Utils/Utils.h"

enum UniqueType
{
	UniqueUnknown,
	Name,

	IntLiteral,
	FloatLiteral,
	HexLiteral,
	StringLiteral,
	ByteLiteral,
	TrueLiteral,
	FalseLiteral,

	// TopLevelKeywords 
	Import,
	Package,
	State,
	AssertTok,
	LogTok,
	SizeOfTok,
	AlignOfTok,
	TypeOfTok,
	TypeOfExactTok,
	OffsetOfTok,
	Extern,
	Enum,
	Where,
	OnCompile,
	OnCompileDebug,
	Link,
	Breakpoint,
	OperatorOverload,

	// AccessModifierKeywords 
	Public,
	Private,

	//Any type keyword
	Any,
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
	Ref,
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
	QuestionMark,
};

enum TokenType
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

struct Token
{
	StringView val;
	Position pos;
	size_t index;
	TokenType type;
	UniqueType uniqueType;

	Token(const StringView& val, const Position& pos, TokenType type, UniqueType uniqueType, size_t index)
	{
		this->val = val;
		this->pos = pos;
		this->index = index;
		this->type = type;
		this->uniqueType = uniqueType;
	}

	Token(const Token& copy)
	{
		this->val = copy.val;
		this->pos = copy.pos;
		this->index = copy.index;
		this->type = copy.type;
		this->uniqueType = copy.uniqueType;
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

inline Token CreateThisToken(Position& pos)
{
	StringView thisTok = StringView("this");
	return Token(
		thisTok,
		pos,
		TokenType::Identifier,
		UniqueType::Name,
		0
	);
}

static Token emptyToken = Token(
	StringView(""),
	Position(),
	TokenType::Identifier,
	UniqueType::Name,
	0
);