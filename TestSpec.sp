using Standard;

namespace Vec 
{
	state Vec3 
	{
		[size],
		x: float,
		y: float,
		z: float,
	}

	Vec3::(x: float, y: float, z: float) 
	{
		Vec3({x,y,z});
	}

	Vec3::(vec: {x: float, y: float, z: float})
	{
		this.x = vec.x;
		this.y = vec.y;
		this.z = vec.z;
	}

	float Vec3::Length() => Math.Sqrt(SqrLength());

	float Vec3::SqrLength() => x * x + y * y + z * z;

	Vec3 Vec3::operator::+(vec: Vec3) => Vec3(x + vec.r, y + vec.g, z + vec.b);

	bool Vec3::operator::!() => x == 0.0 && y == 0.0 && z == 0.0;
}

// No default generics, eg. T2 = float is invalid
// Where is a compile time function that validates type constraints
state ComplexState<T, T2 : where(t: T, t2: T2) { t + t2; t.size == t2.size; }>
{
	[size],
	vecPtr: *Vec3,
	mat: float[][],
	callback: void(int, int, int),
	callbackArr: void(int, int)[],
	genMap: Map<T, T2>,
	valWithDefault: float = 1.0f,
	valWithDefault2 := 2.0f,
	obj: {int, float, string},
	unownedRef: ~*int,
	[null],
}

ComplexState::delete 
{
	delete unownedRef;
}

//Implicit Generics in methods
void ComplexState::InsertInMap(key: T, val: T2)
{
	genMap[key] = val;
}

// Generic function
T Add<T>(left: T, right: T) Where(t: T) => t + t;
{
	return left + right;
}

state Map<T, T2> 
{
	
}

// Scoped to filename or filename.namespace
localVal: int = 3;
localVal2 := "Hello";

void HumanTest() {
	PrintLine("What's 4 + 6?");
	input := ReadLine();
	// Assert(assertion: bool, errMsg: string, passMsg: string = '');
	Assert(input == '10', "You are not a human, will not compile", "You got it right, will compile");
}

int Return13() {
	return 13;
}

// Always runs during compilation
#compile HumanTest();
// #compile functions can have a return type, the return value will be processed as a constant value during the final compilation
compileTimeVal := #compile Return13();
// The final generated code for this line would look something like this
compileTimeVal := = 13;

// Only runs when the compiler is in run in debug mode 
// Debug compile time statements cannot have a return type as they're not always run
// All debug compile time statements will be run before any #compile statements
#debug {
	Print("Compiling debug binary");
}

void Main() 
{
	v := Vec3(1.0f, 2.0f, 3.0f);
	v2: Vec3 = Vec3({3.0f, 4.0f, 5.0f});
	vPtr: *Vec3 = new Vec3({v2});
	vPtr2 := new Vec3({v});

	//Invalid, can't expand a pointer
	v3 := Vec3({vPtr});
	//Valid, dereference pointer
	v3 := Vec3({vPtr*});

	f: float = v2.Length();
	r: float = v2.r;
	g := v2.g;

	r := v2.r;

	// 4(32 bit) or 8(64 bit) byte int
	i: int;
	// 2 byte int
	i2: int16;
	// 8 byte int
	i3: int64;
	// 4 byte int, all implicit int assignments with a constant create a 4 byte int
	i4 := 16;

	// Valid, implicit type conversions
	i5: int32 = 12;
	i6 := i5;
	i7: int64 = i5;

	// Valid, explicit casting
	i8: int64 = i5 as int64;

	// 1 byte
	b: byte;
	// 6 bytes
	bArr: []byte = byte[6];

	// All states are defaultable, all primitives have default values
	com: ComplexState<int, float> = ComplexState<int, float>();

	// arr is a dynamic array, arr2 is an 8 byte representation of the size of the array and an 8 byte pointer to the start of the array(x64)
	// dynamic arrays can be resized
	arr := int[4];
	arr: []int = int[4];
	arr := int[]{1, 2, 3, 4};
	l := arr.count;
	l: int64 = arr.count;
	// Invalid code, arr is not a standard pointer
	// arr: *int = int[];
	// Example of how a dynamic array would look in high level code
	// public state Array<T> {
	//	 count: uint64,
	//	 start: *T,
	// }

	// arr2 is a pointer to a static array, no size representation is stored
	arr2 := fixed int[3];
	arr2: *int = fixed int[3];
	// Can be a raw pointer
	arr2: ~*int = fixed int[3];
	//Invalid code, treat static arrays as just pointers, this should also make C interop easier
	arr2: []int = fixed int[3];

	callback := void() { Print("Here"); }
	callback: void() = void() { Print("Here"); };
	callback: int(int, int) = int(l: int, r: int) { return l + r; };

	str := "Hello world";
	str2: String = "Hello world";

	// for loops are explicitly for the complete iteration of a set, any non-standard iteration will be scoped to while loops and managed by programmers
	for(b: byte in bArr) 
	{

	}

	for(b in bArr)
	{
	
	}

	// 0 to 10
	for(index: int .. 10)
	{
	
	}

	for(index .. bArr.count - 1) 
	{
		b := bArr[index];
	}

	for(b: byte in str) 
	{
		if(b == 'd') break;
	}

	// Non-standard iteration
	i: uint = 0;
	while(i < 10) { i += 2; }

	if (i == 10) 
	{
	}
	else if (i > 11)
	{
	}
	else
	{
	}

	toSwitchOn := 4;
	switch(toSwitchOn) 
	{
		case(1)
		{
			break;
		}
		case(2)
		{
			continue;
		}
		case(3)
		{
			break;
		}
		default
		{
			break;
		}
	}

	// By default pointers are owned by the scope they're initialized in and automatically deallocated during stack unwinding
	createPtr := void() {
		ptr: *int = new int(4);
	};// ptr is deallocated here

	// To create a raw pointer a slightly different syntax is used
	createPtr := void() {
		ptr: ~*int = new int(2);
	};// ptr is not deallocated, leak

	// Raw pointers must be deallocated manually
	ptr: ~*int = new int();
	delete ptr;
	arr2: ~*int = fixed int[3];
	defer delete arr2[];

	genericTypePointer: *Map<string, int> = new Map<string, int>

	implicitTypePointer: *{typeId, val, ratio} = new {typeId: int = 2, val := "new value", ratio: float = 0.5}

	explicitTypePointer: *{typeId: int, val: string, ratio: float} = new {typeId: int = 2, val := "new value", ratio: float = 0.5}

	// defer defers execution to the end of a scope
	createPtr := void() {
		ptr: ~*int = new int();
		defer delete ptr;
		RunOtherCode(ptr);
	};// ptr is deallocated here

	// Object notation is similar to tuples
	{ fileHandle, err } := IO.Open("filePath");
	// If one assignment is not implicit, all assignments must not be implicit
	// Invalid
	{ fileHandle: FileHandle, err } := IO.Open("filePath");
	// Invalid
	{ fileHandle: FileHandle, err } = IO.Open("filePath");
	// Valid
	{ fileHandle: FileHandle, err: Error } = IO.Open("filePath"); 
	if(err) return;

	// valid
	{ fileHandle: { fileName: string, path: string, bytes: []byte }, err: Error } = IO.Open("filePath");
	defer if(!err)
	{
		fileHandle.Flush();
		fileHandle.Close();
	}

	// valid
	{ one: int, two: float } = { 2, 3.0 };
	{ one, two } := { 2, 3.0 };
	three := one + two;

	// valid
	nums: { one: int, two: float } = { 2, 3.0 };
	nums := { 2, 3.0 }
	three := nums.one + nums.two;

	// valid
	nums: *{ one: int, two: float } = new { 2, 3.0 };
	nums := new { 2, 3.0 }
	three := nums.one + nums.two;

	// invalid, would immediately leak, creates no reference to the pointer
	{ one: int, two: float } = new { 2, 3.0 };
	{ one, two } := { 2, 3.0 };

	// Pointer placement
	arena: ~*byte = new fixed byte[1024];
	curr: ~*byte = arena + 112;
	
	Vec3(1.0f, 2.0f, 3.0f) at curr;
	Vec3(1.0f, 2.0f, 3.0f) at arena + 112;
	vecPtr: *Vec3 = Vec3(1.0f, 2.0f, 3.0f) at curr;
	vecPtr := Vec3(1.0f, 2.0f, 3.0f) at curr;

	// Equivalent to
	arena: ~*byte = new fixed byte[1024];
	curr: ~*byte = (arena + 112);
	vecPtr := curr as ~*Vec3;
	vecPtr* = Vec3(1.0f, 2.0f, 3.0f);
}

*T EmplaceAt<T, ...Args : where(args: Args) => T({args});>(ptr: *void, args: Args...)
{
	return T({args}) at ptr;
}

