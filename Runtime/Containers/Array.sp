package Std

state Array<T>
{
	start: *T,
	count: int
}

Array::(count: int)
{
	this.count = count;
	this.start = new T[count];
}

Array::delete 
{
	delete[] this.start;
}