package _

state _Interop_Vector<T>
{
	begin: *T,
	end: *T,
	allocator: [2]*void
}

*T _Interop_Vector::operator::[](index: uint)
{
	return this.begin[index];
}

state _Interop_Map_Node<Value>
{
	value: Value,
	next: *_Interop_Map_Node<Value>
}

state _Interop_Map<Key, Value>
{
	padding: int,
	bucketArr: **_Interop_Map_Node<Value>,
	bucketCount: uint,
	elementCount: uint,
	rehash: [16]byte,
	allocator: *void
}