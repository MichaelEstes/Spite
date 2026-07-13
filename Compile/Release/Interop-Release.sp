package _

state _Interop_Vector<T>
{
	begin: *T,
	end: *T,
	allocated: *T
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
	return it.index < this.Count();
}

*T _Interop_Vector::current(it: Iterator)
{
	return this[it.index];
}

uint _Interop_Vector::Count()
{
	return (this.end - this.begin) as int / #sizeof T;
}

uint _Interop_Vector::Capacity()
{
	return (this.allocated - this.begin) as int / #sizeof T;
}

[]T _Interop_Vector::AsArray()
{
	arr := []T;
	arr.count = this.Count();
	arr.capacity = this.Capacity();
	arr.memory = this.begin as Allocator<byte>;
	arr.itemBytes = #sizeof T;

	return arr;
}

_Interop_Vector::FromArray(arr: []T)
{
	this.begin = arr.memory[0];
	this.end = this.begin + (arr.count * #sizeof T);
	this.allocated = this.begin + (arr.capacity * #sizeof T);
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
}

string _Interop_Map::log() => "Interop Map";

state _Interop_String
{
	str: *byte,
	count: uint,
	allocator: *void
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
