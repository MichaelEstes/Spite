package Std

state<T> Array
{
	start: *T,
	count: int
}

Array::(count: int)
{
	this.count = count;
	this.start = new fixed T[count];
}

Array::delete 
{
	delete[] this.start;
}