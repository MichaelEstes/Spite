package _

Empty := byte(0);
Full := byte(1);
Deleted := byte(2);

InvalidIndex := -1 as uint

int DefaultStringHash(str: string)
{
	result: int = 0;
	for (i .. str.count) result += (i * 0xDEAD) ^ str[i]~;
	return result;
}

int DefaultPrimitiveHash<Prim>(value: Prim)
{
	return value as int;
}

int DefaultPointerHash(value: *void)
{
	return value as int;
}

int DefaultStructuredHash<Type>(value: Type)
{
	byteCount := #sizeof Type;
	byteArr := value@ as *byte;
	result: int = 0;
	for (i .. byteCount) result += (i * 0xDEAD) ^ byteArr[i]~;

	return result;
}

::int(Type) GetDefaultHashFunctionFor<Type>(type: *_Type)
{
	typeKind := type.kind;
	typeUnion := type.type;

	switch (typeKind)
	{
		case (_TypeKind.PrimitiveType)
		{
			if(typeUnion.primitive.primitiveKind == _PrimitiveKind.String)
			{
				return DefaultStringHash as ::int(Type);
			}
			else
			{
				return DefaultPrimitiveHash<Type>;
			}
		}
		case (_TypeKind.StateType) continue;
		case (_TypeKind.UnionType) continue;
		case (_TypeKind.StructureType)
		{
			return DefaultStructuredHash<Type>;
		}
		case (_TypeKind.FunctionType) continue;
		case (_TypeKind.PointerType)
		{
			return DefaultPointerHash as ::int(Type);
		}
		case (_TypeKind.DynamicArrayType) break;
		case (_TypeKind.FixedArrayType) break;
		case (_TypeKind.ReferenceType) break;
	}

	log "Warning: Unable to create default hash for type";
	return ::int(value: Type) => return value as int;
}

int DefaultHash<Key>(key: Key)
{
	hashFunc := #compile ::int(Key) {
		typeOfKey := #typeof Key;
		return GetDefaultHashFunctionFor<Key>(typeOfKey);
	}

	return hashFunc(key);
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

[]KeyValue<Key, Value> Map::log()
{
	values := []KeyValue<Key, Value>;
	for(i .. this.capacity)
	{
		if (this.status[i]~ == Full)
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
	while(it.index < this.capacity && this.status[it.index]~ != Full)
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

	while (this.status[index]~ != Empty)
	{
		if (this.status[index]~ == Full && Equals(this.keys[index]~, key))
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

bool Map::Remove(key: Key)
{
	index := this.FindIndex(key);
	if(index == InvalidIndex) return false;
	this.status[index]~ = Deleted;
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

bool Map::InsertInternal(key: Key, value: Value, keys: KeyAllocator<Key>, 
						 values: ValueAllocator<Value>, status: ZeroedAllocator<byte>,
						 capacity: uint)
{
	hash: uint = Hash(key);
	index := hash % capacity;
	start := index;
	deletedIndex := InvalidIndex;

	currStatus := status[index]~;
	while (currStatus != Empty)
	{
		if (currStatus == Full && Equals(keys[index]~, key))
		{
			values[index]~ = value;
			return false;
		}

		if (currStatus == Deleted)
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
	status[index]~ = Full;
	return true;
}

Map::InsertAllInternal(keys: KeyAllocator<Key>, values: ValueAllocator<Value>, 
						status: ZeroedAllocator<byte>, capacity: uint)
{
	for (i .. this.capacity)
	{
		if(this.status[i]~ == Full)
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
