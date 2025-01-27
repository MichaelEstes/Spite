package Allocator

state Allocator<Type> 
{
	ptr: *Type
}

*Type Allocator::operator::[](index: int) => this.ptr[index];

*Type Allocator::Alloc(count: int)
{
	this.ptr = alloc(count * #sizeof Type) as *Type;
	return this.ptr;
}

*Type Allocator::Resize(count: int)
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
	count: int
}

*Type InitAllocator::operator::[](index: int) => this.ptr[index];

*Type InitAllocator::Alloc(count: int)
{
	this.count = count;
	this.ptr = alloc(count * #sizeof Type) as *Type;
	if (this.ptr)
	{
		for (i .. count) this.ptr[i]~ = Type();
	}

	return this.ptr;
}

*Type InitAllocator::Resize(count: int)
{
	this.ptr = realloc(this.ptr, count * #sizeof Type) as *Type;
	if (this.ptr)
	{
		curr := this.count;
		while (curr < this.count)
		{
			this.ptr[curr]~ = Type();
			curr += 1;
		}
	}

	this.count = count;
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
	count: int
}

*Type ZeroedAllocator::operator::[](index: int) => this.ptr[index];

*Type ZeroedAllocator::Alloc(count: int)
{
	this.count = count;
	this.ptr = alloc_zeroed(count, #sizeof Type) as *Type;
	return this.ptr;
}

*Type ZeroedAllocator::Resize(count: int)
{
	resized := alloc_zeroed(count, #sizeof Type) as *Type;
	if (resized)
	{
		for (i .. count) resized[i]~ = this.ptr[i]~;
	}

	this.ptr = resized;
	this.count = count;
	return this.ptr;
}

ZeroedAllocator::Dealloc()
{
	dealloc(this.ptr);
}