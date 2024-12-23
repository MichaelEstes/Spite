package _

state _Interop_Vector<T>
{
	begin: *void,
	end: *void,
	allocator: [2]*void
}

*T _Interop_Vector::operator::[](index: uint)
{
	return this.begin[index];
}

string _Interop_Vector::log() => "Interop Vector";

state _Interop_Map_Node<Value>
{
	value: Value,
	next: *_Interop_Map_Node<Value>
}

state _Interop_Map<Key, Value>
{
	padding: int,
	//bucketArr: **_Interop_Map_Node<Value>,
	bucketArr: *void,
	bucketCount: uint,
	elementCount: uint,
	rehash: [16]byte,
	allocator: *void
}

string _Interop_Map::log() => "Interop Map";

state _Interop_String
{
	str: [24]byte
}

//string _Interop_String::log() => return {this.size, this.str} as string;
