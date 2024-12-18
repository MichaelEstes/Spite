package _

Empty := byte(0);
Full := byte(1);
Deleted := byte(2);

InvalidIndex := -1 as uint

int DefaultHash<Key>(key: Key)
{
	return key as int;
}

bool DefaultEqual<Key>(left: Key, right: Key)
{
	return left == right;
}

state KeyValue<Key, Value>
{
	key: *Key,
	value: *Value
}

state Map<Key, Value, Hash = DefaultHash<Key>, Equals = DefaultEqual<Key> : where(key: Key) { Hash<Key>(key); Equals<Key>(key, key) == true; }>
{
	keys: []Key,
	values: []Value,
	status: []byte,
	count: uint
}

*Value Map::operator::[](key: Key)
{
	return this.Find(key);
}

Iterator Map::operator::in()
{
	return {null, 0};
}

bool Map::next(it: Iterator)
{
	it.index += 1;
	while(it.index < this.status.count && this.status[it.index] != Full)
	{
		it.index += 1;
	}
	return it.index < this.status.count;
}

KeyValue<Key, Value> Map::current(it: Iterator)
{
	index := it.index;
	return {this.keys[index]@, this.values[index]@} as KeyValue<Key, Value>;
}

*Value Map::Find(key: Key)
{
	index := this.FindIndex(key);

	if(index == InvalidIndex) return null;
	return this.values[index]@;
}

uint Map::FindIndex(key: Key)
{
	hash: int = Hash(key);
	index := hash % this.status.capacity;
	start := index;

	while (this.status[index] != Empty)
	{
		if (this.status[index] == Full && Equals(this.keys[index], key))
			return index;

		index = (index + 1) % this.status.capacity;
		if (index == start) break;
	}

	return InvalidIndex;
}

bool Map::Has(key: Key)
{
	this.FindIndex(key) != InvalidIndex;
}

bool Map::Insert(key: Key, value: Value)
{
	if (this.count * 3 >= this.status.capacity * 2)
	{
		this.ResizeTo((this.status.capacity + 1) * 2);
	}

	this.count += 1;
	return MapInsertInternal<Key, Value, Hash, Equals>(this.keys, this.values, this.status, key, value);
}

bool Map::Remove(key: Key)
{
	index := this.FindIndex(key);
	if(index == InvalidIndex) return false;
	this.status[index] = Deleted;
}

Map::ResizeTo(count: int)
{	
	newKeys := [count]Key;
	newKeys.count = newKeys.capacity;
	newValues := [count]Value;
	newValues.count = newValues.capacity;
	newStatus := [count]byte;
	newStatus.count = newStatus.capacity;

	for (i .. newStatus.count) newStatus[i] = Empty;

	MapInsertAllInternal<Key, Value, Hash, Equals>(newKeys, newValues, newStatus, 
													this.keys, this.values, this.status);

	delete this.keys;
	delete this.values;
	delete this.status;
	this.keys = newKeys;
	this.values = newValues;
	this.status = newStatus;
}

bool MapInsertInternal<Key, Value, Hash, Equals>(keys: []Key, values: []Value, status: []byte
													key: Key, value: Value)
{
	//Implment assert
	//assert keys.count == values.count && values.count == status.count;
	hash: uint = Hash(key);
	index := hash % status.capacity;
	start := index;
	deletedIndex := InvalidIndex;

	while (status[index] != Empty)
	{
		if (status[index] == Full && Equals(keys[index], key))
		{
			values[index] = value;
			return true;
		}

		if (status[index] == Deleted)
		{
			deletedIndex = index;
		}
		
		index = (index + 1) % status.capacity;

		if (index == start) break;
	}

	if (deletedIndex != InvalidIndex)
	{
		index = deletedIndex;
	}

	keys[index] = key;
	values[index] = value;
	status[index] = Full;
	return true;
}

MapInsertAllInternal<Key, Value, Hash, Equals>(keys: []Key, values: []Value, status: []byte
										insertKeys: []Key, insertValues: []Value, insertStatus: []byte)
{
	//Implment assert
	assert keys.count == values.count && values.count == status.count;
	assert insertKeys.count == insertValues.count;

	for (i .. insertStatus.count)
	{
		if(insertStatus[i] == Full)
		{
			key := insertKeys[i]
			value := insertValues[i]
			MapInsertInternal<Key, Value, Hash, Equals>(keys, values, status, key, value);
		}
	}
}
