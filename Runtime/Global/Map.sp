package _

state Map<Key, Value, Hash, Equal 
: where(key: Key) { Hash(key); Equal(key, key) == true; }>
{
	keys: []Key,
	values: []Value
}

Value Map::operator::[](key: Key)
{
	hash: int = Hash(key);
}
