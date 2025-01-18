package Main

import Vec

state MyState
{
	f: float,
	i: int,
}

Main()
{
	// Primitive types
	intType: int = 0;
	int16Type: int16 = 0;
	int32Type: int32 = 0;
	int64Type: int64 = 0;
	unsignedIntType: uint = 0;
	unsignedInt16Type: uint16 = 0;
	unsignedInt32Type: uint32 = 0;
	unsignedInt64Type: uint64 = 0;
	byteType: byte = 0;
	unsignedByteType: ubyte = 0;
	boolType: bool = false;
	
	floatType: float = 0.0;
	float32Type: float32 = 0.0;

	inferredPrimitiveType := int16(0);

	// String types
	stringType: string = "String";

	// Array types
	fixedArr: [12]int = [12]int; // Fixed Array
	dynamicArr: []int = []int; // Dynamic Array

	// Structure types
	structType: { x: float, y: float, z: float } = { 0.0, 0.0, 0.0 };

	// Named types
	stateType: MyState = MyState();

	// Pointer types
	pointerType: *MyState = stateType@;

	// Value types (Copies value)
	valueType: ~MyState = stateType;

	// Function types
	functionType: ::int(int) = ::int (val: int) {
		return val * 2;
	}

	// Union types (Can be assigned from any type represented in the union)
	unionType: ?{ i: int, f: float, s: string } = 0;

	// Templated types
	templatedType: Map<int, float> = Map<int, float>();

	// Explicitly imported named types
	importedType: Vec.Vec3 =  Vec.Vec3();
}