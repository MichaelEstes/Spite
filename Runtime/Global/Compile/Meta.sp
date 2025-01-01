package _

enum _ParentKind: int32
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
}

state _Parent
{
	kind: _ParentKind,

	parent: ?{
		blockParent: *_Block,
		instructionParent: *_Instruction,
		typeParent: *_Type,
		argumentParent: *_Argument,
		functionParent: *_Function,
		memberParent: *_Member,
		stateParent: *_State,
		packageParent: *_Package,
		iRParent: *_IR,
	}
}

state _Label
{
	name: _Interop_String,
	values: _Interop_Vector<*_Instruction>,
	terminator: *_Instruction
}

state _Allocate
{
	result: uint,
	type: *_Type
}

state _Block
{
	parent: _Parent,
	allocations: _Interop_Vector<_Allocate>,
	labels:	_Interop_Vector<*_Label>
}

state _Literal
{
	kind: _PrimitiveKind,
	
	literal: ?{
		byteLiteral: byte,
		i16Literal: int16,
		i32Literal: int32,
		i64Literal: int64,
		intLiteral: int,
		f32Literal: float32,
		floatLiteral: float,
		_Interop_StringLiteral: *_Interop_String
	}
}

enum _OperandKind: int32
{
	Void,
	Register,
	Literal,
	StructLiteral,
	Function,
}

state _Operand
{
	type: *_Type,
	kind: _OperandKind,

	operand: ?{
		reg: uint,
		literal: *_Literal,
		structLiteral: *_Interop_Vector<_Operand>,
		function: *_Function 
	}
}

state _Return
{
	operand: _Operand
}

state _Jump
{
	label: *_Label
}

state _Branch
{
	test: _Operand,
	true_: *_Label,
	false_: *_Label
}

state _Call
{
	function: *_Function,
	params: *_Interop_Vector<_Operand>,
	result: uint
}

state _CallPtr
{
	funcPtr: _Operand,
	params: *_Interop_Vector<_Operand>,
	result: uint
}

state _Load
{
	dst: _Operand,
	src: _Operand,
	offset: _Operand,
	indexType: *_Type,
}

state _LoadGlobal
{
	dst: _Operand,
	src: uint
}

state _Store
{
	src: _Operand,
	dst: _Operand
}

state _Free
{
	operand: _Operand
}

state _Cast
{
	from: _Operand,
	to: _Operand
}

state _Switch
{
	test: _Operand 
	cases: *_Interop_Map<int, *_Label>
	defaultCase: *_Label 
}

enum _BinaryOpKind: int32
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
}

state _BinaryOp
{
	kind: _BinaryOpKind,
	left: _Operand,
	right: _Operand, 
	result: uint
}

enum _UnaryOpKind: int32
{
	Subtract,
	Not,
	XOr,
}

state _UnaryOp
{
	kind: _UnaryOpKind,
	operand: _Operand,
	result: uint,
}

state _Log
{
	operands: *_Interop_Vector<_Operand>
}

state _Assert
{
	test: _Operand,
	message: _Operand
}

state _Position
{
	file: *_Interop_String,
	fileOffset: uint,
	line: uint,
	columnOffset: uint
}

state _InstructionMetadata
{
	statementPosition: _Position,
	expressionPosition: _Position
}

enum _InstructionKind: int
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
	StoreFunc,
	Move,
	Reference,
	Dereference,
	Cast,
	Switch,
	BinOp,
	UnOp,
	Log,
	Assert
}

state _Instruction
{
	kind: _InstructionKind,
	metadata: *_InstructionMetadata,

	inst: ?{
		return_: _Return,
		jump: _Jump,
		branch: _Branch,
		call: _Call,
		callPtr: _CallPtr,
		load: _Load,
		loadGlobal: _LoadGlobal,
		store: _Store,
		free: _Free,
		cast: _Cast,
		switch_: _Switch,
		binOp: _BinaryOp,
		unOp: _UnaryOp,
		log_: _Log,
		assert_: _Assert,
	}
}

enum _TypeKind: int32
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
}

enum _PrimitiveKind: int32
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
}

state _Type
{
	size: uint,
	alignment: uint,
	kind: _TypeKind,
	isValueType: bool,
	type: ?{
		primitive: {isSigned: bool, primitiveKind: _PrimitiveKind},
		stateType: *_State,
		structureType: *_Interop_Vector<_Member>,
		pointer: *_Type,
		reference: *_Type,
		dynamicArray: *_Type,
		fixedArray: {count: uint, type: *_Type},
		function: {returnType: *_Type, params: *_Interop_Vector<*_Type>}
	}
}

state _Value
{
	type: *_Type, 
	name: _Interop_String
}

state _GlobalVariable
{
	parent: *_Package,
	index: uint,
	type: *_Type,
	name: _Interop_String
}

state _Argument
{
	parent: *_Function,
	value: *_Value
}

enum FunctionFlags: int
{
	Inline = 1 << 1,
}

state _PlatformLib
{
	platform: _Interop_String,
	lib: _Interop_String
}

state _ExternFunction
{
	libs: *_Interop_Vector<_PlatformLib>,
	externName: _Interop_String,
	callName: _Interop_String
}

state _Function
{
	parent: *_Package,
	metadata: {externFunc: *_ExternFunction, flags: uint},

	name: _Interop_String,
	returnType: *_Type,
	arguments: _Interop_Vector<*_Argument>,
	block: *_Block
}

state _Member
{
	value: *_Value,
	offset: uint
}

enum StateFlags: int32
{
	Size = 1 << 1,
	SOA = 1 << 2,
	Serialized = 1 << 3,
	NoAlign = 1 << 4,
}

state _State
{
	parent: *_Package,
	size: uint,
	alignment: uint,

	flags: uint,

	members: _Interop_Vector<_Member>,
	methods: _Interop_Vector<*_Function>,
	operators: _Interop_Map<_Interop_String, _Interop_Vector<*_Function>>,
	constructors: _Interop_Vector<*_Function>,
	defaultConstructor: *_Function,
	destructor: *_Function,
	name: _Interop_String,
}

state _Package
{
	parent: *_IR,
	file: _Interop_String,
	name: _Interop_String,
	imports: _Interop_Vector<*_Package>,
	globalVariables: _Interop_Vector<*_GlobalVariable>,
	globalVariableLookup: _Interop_Map<_Interop_String, uint>,
	states: _Interop_Map<_Interop_String, *_State>,
	functions: _Interop_Map<_Interop_String, *_Function>,
	initializer: *_Function,
}

state _IR
{
	packages: _Interop_Vector<*_Package>,
	runtime: *_Package,
	entry: *_Function
}

