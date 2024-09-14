extern c
{
	*byte malloc(size: int);
}


*byte alloc(size: int)
{
	return malloc(size);
}

void free(ptr: *void)
{
	return malloc(size);
}
