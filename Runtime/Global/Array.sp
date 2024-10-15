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
	delete[] this.start;
}

array::Add(item: any)
{
	if(this.count >= this.capacity) this.Expand();
	
	copy_bytes(this[this.count], item, this.itemBytes);
	this.count = this.count + 1;
}

array::AddAll(items: []any)
{
	newCount := this.count + items.count;
	if(newCount >= this.capacity) this.Expand();
	
	for(i .. items.count)
	{
		this.start[this.count] = items[i];
	}
	this.count = newCount;
}

array::Expand()
{
	this.capacity = this.capacity * 2;
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