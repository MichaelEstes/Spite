package _

null := 0 as *void;

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

void copy_bytes(dst: *byte, src: *byte, toCopy: int)
{
	for(i .. toCopy)
	{
		(dst + i)~ = (src + i)~;
	}
}
