package Allocator

state Allocator<Context>
{
	alloc: ::*byte(int, Context),
	dealloc: ::(*byte, Context),
	context: Context
}

Allocator::(alloc: ::*byte(int, Context), dealloc: ::(*byte, Context), 
			context: Context)
{
	this.alloc = alloc;
	this.dealloc = dealloc;
	this.context = context;
}

*byte Allocator::Alloc(size: int)
{
	return this.alloc(size, this.context);
}

Allocator::Dealloc(ptr: *byte)
{
	this.dealloc(ptr, this.context);
}

defaultAllocator := Allocator<*void>(
	::*byte(size: int, context: *void) => {
		return alloc(size);
	},
	::(ptr: *byte, context: *void) => {
		dealloc(ptr);
	},
	null
)




