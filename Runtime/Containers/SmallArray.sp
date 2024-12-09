package SmallArray

state SmallArray<Size, Type>
{
	arr: [Size]Type
}

*Type SmallArray::operator::[](index: uint)
{
	return this.arr[index]@;
}

SmallArray::Fill(value: Type)
{
	for (i .. Size)
	{
		this[i]~ = value;
	}
}