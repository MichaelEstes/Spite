package Allocator

state Allocator<Type> 
{
	ptr: *Type
}

*Type Allocator::operator::[](index: uint) => this.ptr[index];

*Type Allocator::Alloc(count: uint)
{
	this.ptr = alloc(count * #sizeof Type) as *Type;
	return this.ptr;
}

*Type Allocator::Resize(count: uint, prevCount: uint)
{
	this.ptr = realloc(this.ptr, count * #sizeof Type) as *Type;
	return this.ptr;
}

Allocator::Dealloc()
{
	dealloc(this.ptr);
}

state InitAllocator<Type>
{
	ptr: *Type,
	count: uint
}

*Type InitAllocator::operator::[](index: uint) => this.ptr[index];

*Type InitAllocator::Alloc(count: uint)
{
	this.ptr = alloc(count * #sizeof Type) as *Type;
	if (this.ptr) 
	{
		for (i .. count) this.ptr[i]~ = Type();
		this.count = count;
	}
	else this.count = 0;

	return this.ptr;
}

*Type InitAllocator::Resize(count: uint, prevCount: uint)
{
	this.ptr = realloc(this.ptr, count * #sizeof Type) as *Type;
	if (this.ptr)
	{
		curr := prevCount;
		while (curr < count)
		{
			this.ptr[curr]~ = Type();
			curr += 1;
		}
		this.count = count;
	} else this.count = 0;

	return this.ptr;
}

InitAllocator::Dealloc()
{
	for (i .. this.count) delete this.ptr[i]~;
	dealloc(this.ptr);
}

state ZeroedAllocator<Type>
{
	ptr: *Type,
}

*Type ZeroedAllocator::operator::[](index: uint) => this.ptr[index];

*Type ZeroedAllocator::Alloc(count: uint)
{
	this.ptr = alloc_zeroed(count, #sizeof Type) as *Type;
	return this.ptr;
}

*Type ZeroedAllocator::Resize(count: uint, prevCount: uint)
{
	this.ptr = realloc(this.ptr, count * #sizeof Type) as *Type;
	if (this.ptr)
	{
		zero_out_bytes(this.ptr[prevCount], (count - prevCount) * #sizeof Type);
	}

	return this.ptr;
}

ZeroedAllocator::Dealloc()
{
	dealloc(this.ptr);
}