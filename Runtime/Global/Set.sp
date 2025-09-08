package _

state Set<Key, Hash = DefaultHash<Key>, Equals = DefaultEqual<Key>,
		  KeyAllocator = Allocator, InitialCapacity = 16
		  : where(key: Key) { Hash<Key>(key); Equals<Key>(key, key) == true; }>
{
	keys: KeyAllocator<Key>,
	status: ZeroedAllocator<byte>,
	count: uint,
	capacity: uint
}

Set::()
{
	this.keys.Alloc(InitialCapacity);
	this.status.Alloc(InitialCapacity);
	this.capacity = InitialCapacity;
	this.count = 0;
}

Set::delete
{
	this.keys.Dealloc(this.capacity);
	this.status.Dealloc(this.capacity);
}

[]Key Set::log()
{
	values := []Key;
	for(i .. this.capacity)
	{
		if (this.status[i]~ == _Status.Full)
		{
			values.Add(this.keys[i]~);
		}
	}

	return values;
}

*Key Set::operator::[](key: Key)
{
	return this.Find(key);
}

Iterator Set::operator::in()
{
	return {null, -1};
}

bool Set::next(it: Iterator)
{
	it.index += 1;
	while(it.index < this.capacity && this.status[it.index]~ != _Status.Full)
	{
		it.index += 1;
	}
	return it.index < this.capacity;
}

*Key Set::current(it: Iterator)
{
	index := it.index;
	return this.keys[index];
}

*Key Set::Find(key: Key)
{
	index := this.FindIndex(key);

	if(index == InvalidIndex) return null;
	return this.keys[index];
}

uint Set::FindIndex(key: Key)
{
	if (!this.capacity) return InvalidIndex;

	hash: int = Hash(key);
	index := hash % this.capacity;
	start := index;

	while (this.status[index]~ != _Status.Empty)
	{
		if (this.status[index]~ == _Status.Full && Equals(this.keys[index]~, key))
			return index;

		index = (index + 1) % this.capacity;
		if (index == start) break;
	}

	return InvalidIndex;
}

bool Set::Has(key: Key)
{
	return this.FindIndex(key) != InvalidIndex;
}

bool Set::Insert(key: Key)
{
	if (this.count * 3 >= this.capacity * 2)
	{
		this.ResizeTo((this.capacity + 1) * 2);
	}

	this.count += 1;
	return this.InsertInternal(
			key, 
			this.keys, 
			this.status,
			this.capacity
	);
}

bool Set::Remove(key: Key)
{
	index := this.FindIndex(key);
	if(index == InvalidIndex) return false;
	this.status[index]~ = _Status.Deleted;
	this.count -= 1;
	return true;
}

Set::ResizeTo(capacity: int)
{	
	newKeys := KeyAllocator<Key>();
	newStatus := ZeroedAllocator<byte>();
	
	newKeys.Alloc(capacity);
	newStatus.Alloc(capacity);

	this.InsertAllInternal(newKeys, newStatus, capacity);

	this.keys.Dealloc(this.capacity);
	this.status.Dealloc(this.capacity);

	this.keys = newKeys;
	this.status = newStatus;
	this.capacity = capacity;
}

Set::Clear()
{
	this.count = 0;
	zero_out_bytes(this.status[0], this.capacity);
}

bool Set::InsertInternal(key: Key, keys: KeyAllocator<Key>, 
						 status: ZeroedAllocator<byte>, capacity: uint)
{
	hash: uint = Hash(key);
	index := hash % capacity;
	start := index;
	deletedIndex := InvalidIndex;

	currStatus := status[index]~;
	while (currStatus != _Status.Empty)
	{
		if (currStatus == _Status.Full && Equals(keys[index]~, key))
		{
			return false;
		}

		if (currStatus == _Status.Deleted)
		{
			deletedIndex = index;
		}
		
		index = (index + 1) % capacity;
		if (index == start) break;

		currStatus = status[index]~;
	}

	if (deletedIndex != InvalidIndex)
	{
		index = deletedIndex;
	}

	keys[index]~ = key;
	status[index]~ = _Status.Full;
	return true;
}

Set::InsertAllInternal(keys: KeyAllocator<Key>, status: ZeroedAllocator<byte>, capacity: uint)
{
	for (i .. this.capacity)
	{
		if(this.status[i]~ == _Status.Full)
		{
			key := this.keys[i]~;
			this.InsertInternal(
				key, 
				keys, 
				status,
				capacity
			);
		}
	}
}
