extern c
{
	*byte malloc(size: int);
}


*byte allocate(size: int)
{
	return malloc(size);
}