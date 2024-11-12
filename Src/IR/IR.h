#pragma once
#include "EASTL/vector.h"
#include "EASTL/string.h"
#include "EASTL/hash_map.h"
#include "EASTL/bonus/tuple_vector.h"

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
		Void,
		Register,
		Literal,
		StructLiteral,
		Function,
	};

	enum class InstructionKind
	{
		None,
		Return,
		Jump,
		Branch,
		Call,
		CallPtr,
		ExternCall,
		TailCall,
		Load,
		LoadPtrOffset,
		Store,
		StorePtr,
		StoreFunc,
		Move,
		Reference,
		Dereference,
		Cast,
		Switch,
		BinOp,
		UnOp,
		Log
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
		ReferenceType,
		DynamicArrayType,
		FixedArrayType,
		FunctionType
	};

	struct Label
	{
		string name;
		Array<Instruction*> values;
		Instruction* terminator = nullptr;
	};

	struct Allocate
	{
		size_t result;
		Type* type;
	};

	struct Block
	{
		Parent parent;
		Array<Allocate> allocations;
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
			Function* function;
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

	struct CallPtr
	{
		Operand funcPtr;
		Array<Operand>* params;
		size_t result;
	};

	struct Load
	{
		Operand dst;
		Operand src;
		Operand offset;
	};

	struct Store
	{
		Operand src;
		Operand dst;
	};

	struct Free
	{
		Operand operand;
	};

	struct Cast
	{
		Operand from;
		Operand to;
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

	struct Log
	{
		Array<Operand>* operands;
	};

	struct InstructionMetadata
	{
		InstructionKind kind = InstructionKind::None;
	};

	struct Instruction
	{
		InstructionKind kind = InstructionKind::None;
		size_t metadata = 0;

		union 
		{
			Return return_;
			Jump jump;
			Branch branch;
			Call call;
			CallPtr callPtr;
			Load load;
			Store store;
			Free free;
			Cast cast;
			Switch switch_;
			BinaryOp binOp;
			UnaryOp unOp;
			Log log;
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
				//Nullable
				Array<string>* names;
			} structureType;

			struct
			{
				Type* type;
			} pointer;

			struct
			{
				Type* type;
			} reference;

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
		Value* value;
	}; 

	enum FunctionFlags : int
	{
		Inline = ToBit(1),
	};

	struct PlatformLib
	{
		string platform;
		string lib;
	};

	struct ExternFunction
	{
		Array<PlatformLib>* libs;
		string externName;
		string callName;
	};

	struct Function
	{
		Package* parent;

		struct
		{
			int flags;
			ExternFunction* externFunc = nullptr;
		} metadata;

		string name;
		Type* returnType;
		Array<Argument*> arguments;
		Block* block;
	};

	struct Member
	{
		State* parent;
		Value* value;
		size_t offset;
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

		Array<Member*> members;
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
		Package* runtime;
		Function* entry;
		Arena arena;
		Arena instructions;

		IR(size_t initialSize) : arena(initialSize * 256) {}

		inline Package* AddPackage()
		{
			Package* package = AllocatePackage();
			packages.push_back(package);
			return package;
		}

		inline Package* FindPackage(string name)
		{
			for (Package* package : packages) 
			{
				if (package->name == name) return package;
			}

			return nullptr;
		}

		inline void SetRuntimePackage(Package* package)
		{
			runtime = package;
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

		inline ExternFunction* AllocateExternFunction()
		{
			return arena.Emplace<ExternFunction>();
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
			return instructions.Emplace<Instruction>();
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
