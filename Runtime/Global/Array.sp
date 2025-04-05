package _

array make_array(itemBytes: uint)
{
	arr: array = array();
	arr.itemBytes = itemBytes;
	return arr;
}

size_array(arr: array, count: uint)
{
	arr.SizeTo(count);
}

array make_array_from(itemBytes: uint, count: uint, start: *byte)
{
	arr: array = array();
	alloc := Allocator<byte>();
	alloc.Alloc(itemBytes * count);
	copy_bytes(alloc[0], start, count * itemBytes);

	arr.memory = alloc;
	arr.itemBytes = itemBytes;
	arr.capacity = count;
	arr.count = count;
	return arr;
}

state Iterator
{
	current: *void,
	index: int,
}

Iterator::(begin: *void, index: int)
{
	this.current = begin;
	this.index = index;
}

state array
{
	count: uint,
	capacity: uint,
	memory: Allocator<byte>,
	itemBytes: uint,
}

array::delete 
{
	this.memory.Dealloc(this.count);
}

any array::operator::[](index: uint)
{
	return this.memory[index * this.itemBytes];	
}

Iterator array::operator::in()
{
	return {null, -1};
}

bool array::next(it: Iterator)
{
	it.index += 1;
	return it.index < this.count;
}

any array::current(it: Iterator)
{
	return this.memory[it.index * this.itemBytes];	
}

array::Clear()
{
	this.count = 0;
}

bool array::Remove(index: int)
{
	if (index >= this.count) return false;

	copy_bytes(this[index], this[index + 1], (this.count - index) * this.itemBytes);
	this.count -= 1;
	return true;
}

// Returns the index of the item added
int array::Add(item: any)
{
	if(this.count >= this.capacity) this.Expand();	

	index := this.count;
	copy_bytes(this[this.count], item, this.itemBytes);
	this.count = this.count + 1;
	return index;
}

array::AddAll(items: []any)
{
	newCount := this.count + items.count;
	if(newCount >= this.capacity) this.ExpandAtLeastTo(newCount);
	
	copy_bytes(this[this.count], items[0], items.count * this.itemBytes);
	this.count = newCount;
}

array::Expand()
{
	this.SizeTo((this.capacity + 1) * 2);
}

array::ExpandAtLeastTo(size: uint)
{
	capacity := this.capacity;
	while (capacity < size) capacity = (capacity + 1) * 2;
	this.SizeTo(capacity);
}

array::SizeTo(capacity: uint)
{
	this.memory.Resize(capacity * this.itemBytes, this.capacity)
	this.capacity = capacity;
}

[]T array::Map<T>(func: ::T(any))
{
	mapped := [this.count]T;
	for(i .. this.count)
	{
		mapped.Add(func(this[i]));
	}
	return mapped;
}

[]*T array::Filter<T>(pred: ::bool(*T))
{
	filtered := make_array(#sizeof *void);
	for(i .. this.count)
	{
		item := this[i];
		if (pred(item)) filtered.Add(item);
	}

	return filtered;
}

// Fills an array with a value from the start to the current count of the array
array::Fill(with: any)
{
	fill_memory(this.memory.ptr, with, this.itemBytes, this.count * this.itemBytes);
}

// Fills an array with a value from the start to the current capacity of the array
array::FillAll(with: any)
{
	fill_memory(this.memory.ptr, with, this.itemBytes, this.capacity * this.itemBytes);
}