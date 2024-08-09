#pragma once
#include "EASTL/vector.h"
#include "EASTL/string.h"
#include "EASTL/hash_map.h"

#include "../Containers/Arena.h"
#include "../Utils/Utils.h"

namespace SpiteIR
{
	struct Block;
	struct Instruction;
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
		Parent(Type* typeParent) { this->kind = ParentKind::Type; this->typeParent = typeParent; }
		Parent(Argument* argumentParent) { this->kind = ParentKind::Argument; this->argumentParent = argumentParent; }
		Parent(Function* functionParent) { this->kind = ParentKind::Function; this->functionParent = functionParent; }
		Parent(AnonymousTypeMember* anonymousTypeMemberParent) { this->kind = ParentKind::AnonymousTypeMember; this->anonymousTypeMemberParent = anonymousTypeMemberParent; }
		Parent(Member* memberParent) { this->kind = ParentKind::Member; this->memberParent = memberParent; }
		Parent(State* stateParent) { this->kind = ParentKind::State; this->stateParent = stateParent; }
		Parent(Package* packageParent) { this->kind = ParentKind::Package; this->packageParent = packageParent; }
		Parent(IR* iRParent) { this->kind = ParentKind::IR; this->iRParent = iRParent; }
	};

	enum class BinaryOpKind
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

	enum class UnaryOpKind
	{
		Subtract,
		Not,
		XOr,
	};

	enum class PrimitiveKind
	{
		Void,
		Bool,
		Byte,
		Int,
		Float,
		String
	};

	enum class OperandKind
	{
		None,
		Register,
		Literal,
		StructLiteral,
	};

	enum class InstructionKind
	{
		None,
		Return,
		Compare,
		Jump,
		Branch,
		Call,
		TailCall,
		Allocate,
		HeapAllocate,
		Load,
		Store,
		Free,
		Cast,
		Switch,
		BinOp,
		UnOp
	};

	enum class CompareKind
	{
		Equal,
		NotEqual,
		LessThan,
		GreaterThan,
		LessThanEqual,
		GreaterThanEqual,
	};

	enum class TypeKind
	{
		PrimitiveType,
		StateType,
		StructureType,
		PointerType,
		DynamicArrayType,
		FixedArrayType,
		FunctionType
	};

	struct Label
	{
		string name;
		Array<Instruction*> values;
	};

	struct Block
	{
		Parent parent;
		Array<Label*> labels;
	};

	struct Literal
	{
		PrimitiveKind kind;
		
		union
		{
			char byteLiteral;
			size_t intLiteral;
			double floatLiteral;
			string* stringLiteral;
		};
	};

	struct Operand
	{
		Type* type;
		OperandKind kind;

		union
		{
			size_t reg;
			Literal literal;
			Array<Operand>* structLiteral;
		};
	};

	struct Return
	{
		Operand operand;
	};

	struct Jump
	{
		Label* label;
	};

	struct Branch
	{
		Operand test;
		Label* true_;
		Label* false_;
	};

	struct Call
	{
		Function* function;
		Array<Operand>* params;
		size_t result;
	};

	struct Allocate
	{
		Type* type;
		size_t result;
	};

	struct Load
	{
		Operand src;
		size_t dst;
	};

	struct Store
	{
		Operand src;
		size_t dst;
	};

	struct Free
	{
		Operand operand;
	};

	struct Cast
	{
		Operand from;
		Type* to;
	};

	struct Switch
	{
		Operand test;
		HashMap<int, Block*>* cases;
	};

	struct BinaryOp
	{
		BinaryOpKind kind;
		Operand left;
		Operand right;
		size_t result;
	};

	struct UnaryOp
	{
		UnaryOpKind kind;
		Operand operand;
		size_t result;
	};

	struct Instruction
	{
		InstructionKind kind = InstructionKind::None;

		union 
		{
			Return return_;
			Jump jump;
			Branch branch;
			Call call;
			Allocate allocate;
			Load load; // x := y~ or x := y[2]
			Store store; // x := 0 or implicitTypeTest := { x := 0.0, y: float = 0.0, z: float }
			Free free;
			Cast cast;
			Switch switch_;
			BinaryOp binOp;
			UnaryOp unOp;
		};
	};

	struct Type
	{
		size_t size = 0;
		TypeKind kind;
		bool byValue = false;

		union
		{
			struct
			{
				bool isSigned;
				PrimitiveKind kind;
			} primitive;

			struct
			{
				State* state;
			} stateType;

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
			} dynamicArray;

			struct
			{
				size_t count;
				Type* type;
			} fixedArray;

			struct
			{
				Type* returnType;
				Array<Type*>* params;
			} function;
		};
	};

	struct Value
	{
		Parent parent;
		Type* type;
		Block* block = nullptr;
		string name;
	};

	struct Argument
	{
		Function* parent;
		size_t index;
		Value* value;
	}; 

	enum FunctionFlags : int
	{
		Inline = ToBit(1),
	};

	struct Function
	{
		Package* parent;

		struct
		{
			int flags;
		} metadata;

		string name;
		Type* returnType;
		HashMap<string, Argument*> arguments;
		Block* block;
	};

	struct Member
	{
		State* parent;
		size_t index;
		Value* value;
	};

	enum StateFlags: int
	{
		Size = ToBit(1),
		SOA = ToBit(2),
		Serialized = ToBit(3),
		NoAlign = ToBit(4),
	};

	struct State
	{
		Package* parent;
		size_t size = 0;

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
	};

	struct CompileFunction
	{
		Package* parent;
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
		Function* entry;
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

		inline Function* AllocateFunction()
		{
			return arena.Emplace<Function>();
		}

		inline Argument* AllocateArgument()
		{
			return arena.Emplace<Argument>();
		}

		inline Value* AllocateValue()
		{
			return arena.Emplace<Value>();
		}

		inline Block* AllocateBlock()
		{
			return arena.Emplace<Block>();
		}

		inline Label* AllocateLabel()
		{
			return arena.Emplace<Label>();
		}

		inline CompileFunction* AllocateCompileFunction()
		{
			return arena.Emplace<CompileFunction>();
		}

		inline Type* AllocateType()
		{
			return arena.Emplace<Type>();
		}

		inline Operand* AllocateOperand()
		{
			return arena.Emplace<Operand>();
		}

		inline Instruction* AllocateInstruction()
		{
			return arena.Emplace<Instruction>();
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
