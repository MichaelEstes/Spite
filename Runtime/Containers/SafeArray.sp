package SafeArray

state SafeArray<Size, Type>
{
	arr: [Size]Type
}

*Type SafeArray::operator::[](index: uint)
{
	if(index >= Size) return null;
	return this.arr[index]@;
}