package Std

state Array<T>
{
	count: int
	start: *T,
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