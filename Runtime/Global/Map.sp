package _

enum _Status: byte
{
	Empty,
	Full,
	Deleted
}

InvalidIndex := -1 as uint

state KeyValue<Key, Value>
{
	key: *Key,
	value: *Value
}

state Map<Key, Value, Hash = DefaultHash<Key>, Equals = DefaultEqual<Key>,
	KeyAllocator = Allocator, ValueAllocator = Allocator, InitialCapacity = 16
: where(key: Key) { Hash<Key>(key); Equals<Key>(key, key) == true; }>
{
	keys: KeyAllocator<Key>,
	values: ValueAllocator<Value>,
	status: ZeroedAllocator<byte>,
	count: uint,
	capacity: uint
}

Map::()
{
	this.keys.Alloc(InitialCapacity);
	this.values.Alloc(InitialCapacity);
	this.status.Alloc(InitialCapacity);
	this.capacity = InitialCapacity;
	this.count = 0;
}

Map::delete
{
	this.keys.Dealloc(this.capacity);
	this.values.Dealloc(this.capacity);
	this.status.Dealloc(this.capacity);
}

[]KeyValue<Key, Value> Map::log()
{
	values := []KeyValue<Key, Value>;
	for(i .. this.capacity)
	{
		if (this.status[i]~ == _Status.Full)
		{
			values.Add({this.keys[i], this.values[i]} as KeyValue<Key, Value>);
		}
	}

	return values;
}

*Value Map::operator::[](key: Key)
{
	return this.Find(key);
}

Iterator Map::operator::in()
{
	return {null, -1};
}

bool Map::next(it: Iterator)
{
	it.index += 1;
	while(it.index < this.capacity && this.status[it.index]~ != _Status.Full)
	{
		it.index += 1;
	}
	return it.index < this.capacity;
}

KeyValue<Key, Value> Map::current(it: Iterator)
{
	index := it.index;
	return {this.keys[index], this.values[index]} as KeyValue<Key, Value>;
}

MapValueIterator<Key, KeyAllocator> Map::Keys()
{
	return {
		this.keys,
		this.status,
		this.capacity
	} as MapValueIterator<Key, KeyAllocator>;
}

MapValueIterator<Value, ValueAllocator> Map::Values()
{
	return {
		this.values,
		this.status,
		this.capacity
	} as MapValueIterator<Value, ValueAllocator>;
}

*Value Map::Find(key: Key)
{
	index := this.FindIndex(key);

	if(index == InvalidIndex) return null;
	return this.values[index];
}

uint Map::FindIndex(key: Key)
{
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

bool Map::Has(key: Key)
{
	return this.FindIndex(key) != InvalidIndex;
}

bool Map::Insert(key: Key, value: Value)
{
	if (this.count * 3 >= this.capacity * 2)
	{
		this.ResizeTo((this.capacity + 1) * 2);
	}

	this.count += 1;
	return this.InsertInternal(
			key, 
			value,
			this.keys, 
			this.values, 
			this.status,
			this.capacity
	);
}

*Value Map::Emplace(key: Key)
{
	if (!this.Insert(key, Value())) return null;
	return this.Find(key);
}

bool Map::Remove(key: Key)
{
	index := this.FindIndex(key);
	if(index == InvalidIndex) return false;
	this.status[index]~ = _Status.Deleted;
	this.count -= 1;
	return true;
}

Map::ResizeTo(capacity: int)
{	
	newKeys := KeyAllocator<Key>();
	newValues := ValueAllocator<Value>();
	newStatus := ZeroedAllocator<byte>();
	
	newKeys.Alloc(capacity);
	newValues.Alloc(capacity);
	newStatus.Alloc(capacity);

	this.InsertAllInternal(newKeys, newValues, newStatus, capacity);

	this.keys.Dealloc(this.capacity);
	this.values.Dealloc(this.capacity);
	this.status.Dealloc(this.capacity);

	this.keys = newKeys;
	this.values = newValues;
	this.status = newStatus;
	this.capacity = capacity;
}

Map::Clear()
{
	this.count = 0;
	zero_out_bytes(this.status[0], this.capacity);
}

bool Map::InsertInternal(key: Key, value: Value, keys: KeyAllocator<Key>, 
						 values: ValueAllocator<Value>, status: ZeroedAllocator<byte>,
						 capacity: uint)
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
			values[index]~ = value;
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
	values[index]~ = value;
	status[index]~ = _Status.Full;
	return true;
}

Map::InsertAllInternal(keys: KeyAllocator<Key>, values: ValueAllocator<Value>, 
						status: ZeroedAllocator<byte>, capacity: uint)
{
	for (i .. this.capacity)
	{
		if(this.status[i]~ == _Status.Full)
		{
			key := this.keys[i]~;
			value := this.values[i]~;
			this.InsertInternal(
				key, 
				value,
				keys, 
				values, 
				status,
				capacity
			);
		}
	}
}

state MapValueIterator<Value, ValueAllocator>
{
	values: ValueAllocator<Value>,
	status: ZeroedAllocator<byte>,
	capacity: uint
}

Iterator MapValueIterator::operator::in()
{
	return {null, -1};
}

bool MapValueIterator::next(it: Iterator)
{
	it.index += 1;
	while(it.index < this.capacity && this.status[it.index]~ != _Status.Full)
	{
		it.index += 1;
	}
	return it.index < this.capacity;
}

ref Value MapValueIterator::current(it: Iterator)
{
	index := it.index;
	return this.values[index]~;
}
