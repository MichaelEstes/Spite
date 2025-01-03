#pragma once
#include "EASTL/vector.h"
#include "EASTL/string.h"
#include "EASTL/hash_map.h"
#include "EASTL/bonus/tuple_vector.h"

#include "../Containers/Arena.h"
#include "../Utils/Utils.h"

// Any changes to this namespace need to be reflected in Runtime/Compile/Meta.sp
namespace SpiteIR
{
	struct Block;
	struct Instruction;
	struct InstructionMetadata;
	struct Type;
	struct Argument;
	struct Function;
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
		I16,
		I32,
		I64,
		Int,
		F32,
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
		TypeData,
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
		Load,
		LoadPtrOffset,
		LoadGlobal,
		Store,
		StorePtr,
		Move,
		Reference,
		Dereference,
		Cast,
		Switch,
		BinOp,
		UnOp,
		Assert
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
		FunctionType,
		UnionType
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
		Function* parent;
		Array<Allocate> allocations;
		Array<Label*> labels;
	};

	struct Literal
	{
		PrimitiveKind kind;
		
		union
		{
			char byteLiteral;
			int16_t i16Literal;
			int32_t i32Literal;
			int64_t i64Literal;
			intmax_t intLiteral;
			float f32Literal;
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
		Type* indexType;
	};

	struct LoadGlobal
	{
		Operand dst;
		size_t src;
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
		HashMap<intmax_t, Label*>* cases;
		Label* defaultCase;
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

	struct Assert
	{
		Operand test;
		Operand message;
	};

	struct InstructionMetadata
	{
		Label* label;
		Block* block;
		Position statementPosition;
		Position expressionPosition;
	};

	struct Instruction
	{
		InstructionKind kind = InstructionKind::None;
		InstructionMetadata* metadata = nullptr;

		union 
		{
			Return return_;
			Jump jump;
			Branch branch;
			Call call;
			CallPtr callPtr;
			Load load;
			LoadGlobal loadGlobal;
			Store store;
			Free free;
			Cast cast;
			Switch switch_;
			BinaryOp binOp;
			UnaryOp unOp;
			Log log;
			Assert assert;
		};
	};

	struct Type
	{
		size_t size = 0;
		size_t alignment = 0;
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
				Array<Member>* members;
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
		Type* type;
		string name;
	};

	struct GlobalVariable
	{
		Package* parent;
		size_t index;
		Type* type;
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
	};

	struct Function
	{
		Package* parent;

		struct
		{
			ExternFunction* externFunc = nullptr;
			size_t flags = 0;
		} metadata;

		string name;
		Type* returnType;
		Array<Argument*> arguments;
		Block* block;

		inline bool IsInline()
		{
			return metadata.flags & FunctionFlags::Inline;
		}
	};

	struct Member
	{
		Value* value;
		size_t offset = 0;
	};

	enum StateFlags: int
	{
		Size = ToBit(1),
		SOA = ToBit(2),
		Serialized = ToBit(3),
		NoAlign = ToBit(4),
		ValueType = ToBit(5),
	};

	struct State
	{
		Package* parent;
		size_t size = 0;
		size_t alignment = 0;

		struct
		{
			size_t flags = 0;
		} metadata;

		Array<Member> members;
		Array<Function*> methods;
		HashMap<string, Array<Function*>> operators;
		Array<Function*> constructors;
		Function* defaultConstructor;
		Function* destructor = nullptr;
		string name;

		inline bool IsValueType()
		{
			return metadata.flags & StateFlags::ValueType;
		}
	};

	struct Package
	{
		IR* parent;
		string file;
		string name;
		Array<Package*> imports;
		Array<GlobalVariable*> globalVariables;
		HashMap<string, size_t> globalVariableLookup;
		HashMap<string, State*> states;
		HashMap<string, Function*> functions;
		Function* initializer = nullptr;
	};

	struct IR
	{
		Array<Package*> packages;
		Package* runtime;
		Function* entry;
		Arena arena;
		Arena instructions;
		size_t globalSize = 0;

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

		inline void IncremementGlobalSize(Type* type)
		{
			this->globalSize += type->size;
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

		inline Function* AllocateFunction(Package* parent)
		{
			Function* func = arena.Emplace<Function>();
			func->parent = parent;
			return func;
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

		inline GlobalVariable* AllocateGlobalVariable()
		{
			return arena.Emplace<GlobalVariable>();
		}

		inline Block* AllocateBlock(Function* parent)
		{
			Block* block = arena.Emplace<Block>();
			block->parent = parent;
			return block;
		}

		inline Label* AllocateLabel()
		{
			return arena.Emplace<Label>();
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

		inline InstructionMetadata* AllocateInstructionMetadata()
		{
			return arena.Emplace<InstructionMetadata>();
		}

		template<typename T>
		inline Array<T>* AllocateArray()
		{
			return arena.EmplaceScalar<Array<T>>();
		}

		template<typename Key, typename Value>
		inline HashMap<Key, Value>* AllocateHashMap()
		{
			return arena.EmplaceScalar<HashMap<Key, Value>>();
		}

		inline string* AllocateString()
		{
			return arena.EmplaceScalar<string>();
		}
	};
}
