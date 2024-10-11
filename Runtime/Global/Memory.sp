package _

extern
{
	#link linux "libm.so";
	#link windows "msvcrt.dll";

	*byte malloc(size: int);
	void free(ptr: *void);
}

*byte alloc(size: int)
{
	return malloc(size);
}

void dealloc(ptr: *void)
{
	free(ptr);
}
