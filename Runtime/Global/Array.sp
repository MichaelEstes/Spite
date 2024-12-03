package _

*byte alloc_array(count: uint, itemBytes: uint)
{
	return alloc(count * itemBytes);
}

array make_array(itemBytes: uint)
{
	arr: array = array();
	arr.itemBytes = itemBytes;
	return arr;
}

size_array(arr: array, count: uint)
{
	arr.start = alloc_array(count, arr.itemBytes);
	arr.capacity = count;
}

array make_array_from(itemBytes: uint, count: uint, start: *byte)
{
	arr: array = array();
	arr.itemBytes = itemBytes;
	arr.start = start;
	arr.count = count;
	arr.capacity = count;
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
	start: *byte,
	capacity: uint,
	itemBytes: uint,
}

array::delete 
{
	delete this.start;
}

any array::operator::[](index: uint)
{
	return this.start[index * this.itemBytes];	
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
	return this[it.index];
}

array::Clear()
{
	this.count = 0;
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
	
	copy_bytes(this[this.count], items.start, items.count * this.itemBytes);
	this.count = newCount;
}

array::Expand()
{
	this.capacity = (this.capacity + 1) * 2;
	newStart := alloc_array(this.capacity, this.itemBytes);
	copy_bytes(newStart, this.start, this.count * this.itemBytes);
	//delete this.start;
	this.start = newStart;
}

array::ExpandAtLeastTo(size: uint)
{
	while (this.capacity < size) this.capacity = (this.capacity + 1) * 2;
	newStart := alloc_array(this.capacity, this.itemBytes);
	copy_bytes(newStart, this.start, this.count * this.itemBytes);
	//delete this.start;
	this.start = newStart;
}

[]T array::Map<T>(func: ::T(any))
{
	mapped := [this.count]T;
	for(i .. this.count)
	{
		mapped.Add(func(this.start[i]));
	}
	return mapped;
}

[]*T array::Filter<T>(pred: ::bool(*T))
{
	filtered := make_array(#sizeof *void);
	for(i .. this.count)
	{
		item := this.start[i];
		if (pred(item)) filtered.Add(item);
	}

	return filtered;
}

array::SizeTo(count: uint)
{
	newStart := alloc_array(count, this.itemBytes);
	// delete this.start;

	this.start = newStart;
	this.count = count;
	this.capacity = count;
}

// Fills an array with a value from the start to the current count of the array
array::Fill(with: any)
{
	fill_memory(this.start, with, this.itemBytes, this.count * this.itemBytes);
}

// Fills an array with a value from the start to the current capacity of the array
array::FillAll(with: any)
{
	fill_memory(this.start, with, this.itemBytes, this.capacity * this.itemBytes);
}