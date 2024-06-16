#pragma once
#include "EASTL/vector.h"
#include "EASTL/string.h"
#include "EASTL/hash_map.h"

#include "../Parsing/Position.h"
#include "../Containers/Arena.h"

namespace SpiteIR
{
	struct Block;
	struct Instruction;
	struct Value;
	struct Type;
	struct Argument;
	struct Function;
	struct AnonymousTypeMember;
	struct Member;
	struct State;
	struct Package;
	struct IR;

	typedef eastl::string string;
	template <typename Key, typename Value>
	using HashMap = eastl::hash_map<Key, Value>;
	template <typename Value>
	using Array = eastl::vector<Value>;

	enum class ParentKind
	{
		Invalid,
		Block,
		Instruction,
		Value,
		Type,
		Argument,
		Function,
		AnonymousTypeMember,
		Member,
		State,
		Package,
		IR,
	};

	struct Parent
	{
		ParentKind kind;

		union
		{
			Block* blockParent;
			Instruction* instructionParent;
			Value* valueParent;
			Type* typeParent;
			Argument* argumentParent;
			Function* functionParent;
			AnonymousTypeMember* anonymousTypeMemberParent;
			Member* memberParent;
			State* stateParent;
			Package* packageParent;
			IR* iRParent;
		};

		Parent()
		{
			this->kind = ParentKind::Invalid;
			this->iRParent = nullptr;
		}

		Parent(Block* blockParent) { this->kind = ParentKind::Block; this->blockParent = blockParent; }
		Parent(Instruction* instructionParent) { this->kind = ParentKind::Instruction; this->instructionParent = instructionParent; }
		Parent(Value* valueParent) { this->kind = ParentKind::Value; this->valueParent = valueParent; }
		Parent(Type* typeParent) { this->kind = ParentKind::Type; this->typeParent = typeParent; }
		Parent(Argument* argumentParent) { this->kind = ParentKind::Argument; this->argumentParent = argumentParent; }
		Parent(Function* functionParent) { this->kind = ParentKind::Function; this->functionParent = functionParent; }
		Parent(AnonymousTypeMember* anonymousTypeMemberParent) { this->kind = ParentKind::AnonymousTypeMember; this->anonymousTypeMemberParent = anonymousTypeMemberParent; }
		Parent(Member* memberParent) { this->kind = ParentKind::Member; this->memberParent = memberParent; }
		Parent(State* stateParent) { this->kind = ParentKind::State; this->stateParent = stateParent; }
		Parent(Package* packageParent) { this->kind = ParentKind::Package; this->packageParent = packageParent; }
		Parent(IR* iRParent) { this->kind = ParentKind::IR; this->iRParent = iRParent; }
	};

	enum class LiteralKind
	{
		IntLiteral,
		FloatLiteral,
		HexLiteral,
		StringLiteral,
		BoolLiteral,
	};

	enum class PrimitiveType
	{
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
	};

	enum class BinaryOp
	{
		Add,
		Subtract,
		Multiply,
		Divide,
		Modulo,
		And,
		Or,
		Xor,
		ShiftLeft,
		ShiftRight,
		AndNot,
		LogicAnd,
		LogicOr,
		Equal,
		NotEql,
		Less,
		Greater,
		LessEqual,
		GreaterEqual,
	};

	enum class UnaryOp
	{
		Subtract,
		Not,
		XOr,
	};

	struct Block
	{
		Function* parent;
		Position pos;

		string name;
		Array<Value> values;
	};

	struct Constant
	{
		LiteralKind kind;
		string value;
	};

	enum class OperandKind
	{
		Identifier,
		Literal,
		StructLiteral,
		Label,

	};

	struct Operand
	{
		Position pos;
		Type* type;

		OperandKind kind;

		union
		{
			struct
			{
				string* ident;
			} identifier;

			struct
			{
				string* value;
			} literal;

			struct
			{
				Array<Operand*>* members;
			} structLiteral;

			struct
			{
				Block* block;
			} label;
		};
	};

	

	struct Result
	{
		Type* type = nullptr;
		string name;
	};

	enum class InstructionKind
	{
		Return,
		Compare,
		Branch,
		Call,
		Initialize,
		Allocate,
		Load,
		Cast,
		Switch,
	};

	struct Return
	{
		Operand value;
	};

	struct Compare
	{
		Operand left;
		Operand right;
	};

	struct Branch
	{
		Compare* compare;
		Block* label;
	};

	struct Call
	{
		Function* call;
		Array<Operand>* params;
	};

	struct Load
	{
		Operand value;
	};

	struct Store
	{
		Operand value;
	};

	struct Cast
	{
		Operand from;
		Type* to;
	};

	struct Switch
	{

	};

	struct Instruction
	{
		InstructionKind kind;

		union 
		{
			Return return_;
			Compare compare;
			Branch branch;
			Call call;
			Load load; // x := y~ or x := y[2]
			Store store; // x := 0 or implicitTypeTest := { x := 0.0, y: float = 0.0, z: float }
			Cast cast;
			Switch switch_;
		};

	};

	struct Value
	{
		Parent parent;
		Position pos;

		Result result;
		Instruction instruction;
	};

	enum class TypeKind
	{
		PrimitiveType,
		NamedType,
		StructureType,
		PointerType,
		ValueType,
		ArrayType,
		FunctionType
	};

	enum class PrimitiveKind
	{
		Void,
		Int,
		Float,
		String
	};

	struct AnonymousTypeMember
	{
		Type* parent;
		Position pos;

		string name;
		Type* type;
	};

	struct Type
	{
		Parent parent;
		size_t size;
		TypeKind kind;

		union
		{
			struct
			{
				size_t size;
				bool isSigned;
				PrimitiveKind kind;
			} primitive;

			struct
			{
				string* name;
			} namedType;

			struct
			{
				Array<Type*>* types;
				Array<string>* names;
			} structureType;

			struct
			{
				Type* type;
			} pointer;

			struct
			{
				Type* type;
			} value;

			struct
			{
				Type* type;
			} array;

			struct
			{
				Type* returnType;
				Array<Type*>* params;
			} function;
		};
	};

	struct Argument
	{
		Function* parent;
		Position pos;

		size_t index;
		Type* type;
		string name;
		Array<Value*> values;
	};

	struct Function
	{
		Package* parent;
		Position pos;

		string name;
		Type* returnType;
		HashMap<string, Argument*> arguments;
	};

	struct Member
	{
		State* parent;
		Position pos;

		size_t index;
		Type* type;
		string name;
		Array<Value*> values;
	};

	struct State
	{
		Package* parent;
		Position pos;

		struct
		{
			int flags;
		} metadata;

		HashMap<string, Member*> members;
		Array<Function*> methods;
		Array<Function*> operators;
		Array<Function*> constructors;
		Function* destructor = nullptr;
		string name;
		size_t size;
	};

	struct CompileFunction
	{
		Package* parent;
		Position pos;

		Function compileFunc;
		// Node to replace with the node to insert return value into
		Parent node;
	};

	struct Package
	{
		IR* parent;
		string file;
		string name;
		Array<Package*> imports;
		HashMap<string, Value*> globalVariables;
		HashMap<string, State*> states;
		HashMap<string, Function*> functions;
		Array<CompileFunction*> debugFunctions;
		Array<CompileFunction*> compileFunctions;
	};

	struct IR
	{
		Array<Package*> packages;
		Arena arena;

		IR(size_t initialSize) : arena(initialSize * 256) {}

		inline Package* AddPackage()
		{
			Package* package = AllocatePackage();
			packages.push_back(package);
			return package;
		}

		inline Package* AllocatePackage()
		{
			return arena.Emplace<Package>();
		}

		inline State* AllocateState()
		{
			return arena.Emplace<State>();
		}

		inline Member* AllocateMember()
		{
			return arena.Emplace<Member>();
		}

		inline AnonymousTypeMember* AllocateAnonymousTypeMember()
		{
			return arena.Emplace<AnonymousTypeMember>();
		}

		inline Function* AllocateFunction()
		{
			return arena.Emplace<Function>();
		}

		inline Argument* AllocateArgument()
		{
			return arena.Emplace<Argument>();
		}

		inline CompileFunction* AllocateCompileFunction()
		{
			return arena.Emplace<CompileFunction>();
		}

		inline Type* AllocateType(Parent parent)
		{
			Type* type = arena.Emplace<Type>();
			type->parent = parent;
			return type;
		}

		inline Constant* AllocateConstant()
		{
			return arena.Emplace<Constant>();
		}

		inline Operand* AllocateOperand()
		{
			return arena.Emplace<Operand>();
		}

		inline Instruction* AllocateInstruction()
		{
			return arena.Emplace<Instruction>();
		}

		inline Value* AllocateValue()
		{
			return arena.Emplace<Value>();
		}

		template<typename T>
		inline Array<T>* AllocateArray()
		{
			return arena.Emplace<Array<T>>();
		}

		inline string* AllocateString()
		{
			return arena.Emplace<string>();
		}
	};
}
