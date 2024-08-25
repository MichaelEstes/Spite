package _

state Array<T>
{
	count: int
	array: *T,
}

T Array::operator::[](index: int)
{
	return this.array[index];	
}

Array::delete 
{
	delete[] this.array;
}