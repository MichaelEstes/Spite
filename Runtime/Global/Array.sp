package _

*byte alloc_array(count: int, itemBytes: int)
{
	return alloc(count * itemBytes);
}

array make_array(itemBytes: int)
{
	arr: array = array();
	arr.itemBytes = itemBytes;
	return arr;
}

array make_array_from(itemBytes: int, count: int, start: *byte)
{
	arr: array = array();
	arr.itemBytes = itemBytes;
	arr.start = start;
	arr.count = count;
	arr.capacity = count;
	return arr;
}

state array
{
	count: int,
	start: *byte,
	capacity: int,
	itemBytes: int,
}

any array::operator::[](index: int)
{
	return this.start[index * this.itemBytes];	
}

array::delete 
{
	delete this.start;
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
	this.start = newStart;
}

array::ExpandAtLeastTo(size: int)
{
	while (this.capacity < size) this.capacity = (this.capacity + 1) * 2;
	newStart := alloc_array(this.capacity, this.itemBytes);
	copy_bytes(newStart, this.start, this.count * this.itemBytes);
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