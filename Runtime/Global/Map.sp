package _

Empty := byte(0);
Full := byte(1);
Deleted := byte(2);

InvalidIndex := -1 as uint

DefaultMapSize := 12;

int DefaultHash<Key>(key: Key)
{
	return key as int;
}

bool DefaultEqual<Key>(left: Key, right: Key)
{
	return left == right;
}

state Map<Key, Value, Hash, Equals : where(key: Key) { Hash<Key>(key); Equals<Key>(key, key) == true; }>
{
	keys: []Key,
	values: []Value,
	status: []byte,
	count: int
}

*Value Map::operator::[](key: Key)
{
	return this.Find(key);
}

*Value Map::Find(key: Key)
{
	hash: int = Hash<Key>(key);
	index := hash % this.status.capacity;
	start := index;

	while (this.status[index] != Empty)
	{
		if (this.status[index] == Full && Equals<Key>(this.keys[index], key))
			return this.values[index]@;

		index = (index + 1) % this.status.capacity;
		if (index == start) break;
	}

	return null;
}

bool Map::Insert(key: Key, value: Value)
{
	if (this.count * 3 >= this.status.capacity * 2)
	{
		this.ResizeTo((this.status.capacity + 1) * 2);
	}

	return MapInsertInternal<Key, Value, Hash, Equals>(this.keys, this.values, this.status, key, value);
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
													this.keys, this.values);

	delete this.keys;
	delete this.values;
	delete this.status;
	this.keys = newKeys;
	this.values = newValues;
	this.status = newStatus;
	this.count = count;
}

bool MapInsertInternal<Key, Value, Hash, Equals>(keys: []Key, values: []Value, status: []byte
													key: Key, value: Value)
{
	//Implment assert
	//assert keys.count == values.count && values.count == status.count;
	hash: uint = Hash<Key>(key);
	index := hash % status.capacity;
	start := index;
	deletedIndex := InvalidIndex;

	while (status[index] != Empty)
	{
		if (status[index] == Full && Equals<Key>(keys[index], key))
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
												insertKeys: []Key, insertValues: []Value)
{
	//Implment assert
	//Implement logic short circuiting
	//assert keys.count == values.count && values.count == status.count;
	//assert insertKeys.count == insertValues.count;

	for (i .. insertKeys.count)
	{
		key := insertKeys[i]
		value := insertValues[i]
		MapInsertInternal<Key, Value, Hash, Equals>(keys, values, status, key, value);
	}
}
