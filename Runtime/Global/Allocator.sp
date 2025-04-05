package _

state Allocator<Type> 
{
	[value]
	ptr: *Type
}

*Type Allocator::operator::[](index: uint) => this.ptr[index];

Allocator<Type> Allocator::Alloc(count: uint)
{
	this.ptr = alloc(count * #sizeof Type) as *Type;
	return this;
}

Allocator<Type> Allocator::Resize(count: uint, prevCount: uint)
{
	this.ptr = realloc(this.ptr, count * #sizeof Type) as *Type;
	return this;
}

Allocator::Dealloc(count: uint)
{
	dealloc(this.ptr);
}

state InitAllocator<Type>
{
	[value]
	ptr: *Type,
}

*Type InitAllocator::operator::[](index: uint) => this.ptr[index];

InitAllocator<Type> InitAllocator::Alloc(count: uint)
{
	this.ptr = alloc(count * #sizeof Type) as *Type;
	if (this.ptr) 
	{
		for (i .. count) this.ptr[i]~ = Type();
	}

	return this;
}

InitAllocator<Type> InitAllocator::Resize(count: uint, prevCount: uint)
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
	}

	return this;
}

InitAllocator::Dealloc(count: uint)
{
	for (i .. count) delete this.ptr[i]~;
	dealloc(this.ptr);
}

state ZeroedAllocator<Type>
{
	[value]
	ptr: *Type,
}

*Type ZeroedAllocator::operator::[](index: uint) => this.ptr[index];

ZeroedAllocator<Type> ZeroedAllocator::Alloc(count: uint)
{
	this.ptr = alloc_zeroed(count, #sizeof Type) as *Type;
	return this;
}

ZeroedAllocator<Type> ZeroedAllocator::Resize(count: uint, prevCount: uint)
{
	this.ptr = realloc(this.ptr, count * #sizeof Type) as *Type;
	if (this.ptr)
	{
		zero_out_bytes(this.ptr[prevCount], (count - prevCount) * #sizeof Type);
	}

	return this;
}

ZeroedAllocator::Dealloc(count: uint)
{
	dealloc(this.ptr);
}