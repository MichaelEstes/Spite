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

state Map<Key, Value, Hash, Equals : where(key: Key) { Hash<Key>(key); Equals(key, key) == true; }>
{
	keys: []Key,
	values: []Value,
	status: []byte,
	count: int
}

*Value Map::operator::[](key: Key)
{
	hash: int = Hash<Key>(key);

}

bool Map::Insert(key: Key, value: Value)
{
	if (this.count * 3 > this.status.count * 2)
	{
		this.ResizeTo(this.status.count * 2);
	}

	return MapInsertInternal<Key, Value, Hash, Equals>(this.keys, this.values, this.status, key, value);
}

Map::ResizeTo(count: int)
{
	newKeys := [count]Key;
	newValues := [count]Value;
	newStatus := [count]byte;

	for(i .. newStatus.count) newStatus[i] = Empty;

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
	index := hash % status.count;
	start := index;
	deletedIndex := InvalidIndex;

	while(status[index] != Empty)
	{
		//Implement logic short circuiting
		if(status[index] == Full && Equals(keys[index], key))
		{
			values[index] = value;
			return true;
		}

		if(status[index] == Deleted)
		{
			deletedIndex = index;
		}
		
		index = (index + 1) % status.count;

		if(index == start) break;
	}

	if(deletedIndex != InvalidIndex)
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

	for(i .. insertKeys.count)
	{
		key := insertKeys[i]
		value := insertValues[i]
		MapInsertInternal<Key, Value, Hash, Equals>(keys, values, status, key, value);
	}
}
