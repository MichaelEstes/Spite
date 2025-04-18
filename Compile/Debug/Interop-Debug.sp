package _

state _Interop_Vector<T>
{
	begin: *T,
	end: *T,
	allocator: *void,
	debug: *void
}

*T _Interop_Vector::operator::[](index: uint)
{
	return this.begin[index];
}

Iterator _Interop_Vector::operator::in()
{
	return {null, -1};
}

bool _Interop_Vector::next(it: Iterator)
{
	it.index += 1;
	count := (this.end - this.begin) as int / #sizeof T;
	return it.index < count;
}

*T _Interop_Vector::current(it: Iterator)
{
	return this[it.index];
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
	rehash: *void,
	allocator: *void,
	debug: *void
}

string _Interop_Map::log() => "Interop Map";

state _Interop_String
{
	str: *byte,
	count: uint,
	allocator: *void,
	debug: *void
}

string _Interop_String::ToString()
{
	count := (#sizeof int) * 3;
	buf := this.str as [count]byte;
	flag := buf[count - 1];
	heapAllocated: bool = !!(flag & 0x80);

	if (heapAllocated)
	{
		return {this.count, this.str} as string;
	}
	else
	{
		size := 0;
		while(buf[size] && size < count) size += 1;
		return {size, fixed buf} as string;
	}
}

string _Interop_String::log()
{
	return this.ToString();
}
