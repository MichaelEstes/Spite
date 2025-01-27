package _

null := 0 as *void;

extern
{
	#link linux "libc";
	#link windows "msvcrt";

	*byte malloc(size: uint);
	*byte realloc(ptr: *void, size: uint);
	*byte calloc(count: uint, size: uint);
	void free(ptr: *void);
}

*byte alloc(size: uint)
{
	return malloc(size);
}

*byte alloc_zeroed(count: uint, size: uint)
{
	return calloc(count, size);
}

void dealloc(ptr: *void)
{
	free(ptr);
}

void copy_bytes(dst: *byte, src: *byte, toCopy: uint)
{
	for (i .. toCopy)
	{
		dst[i]~ = src[i]~;
	}
}

void fill_memory(dst: *byte, item: *byte, itemSize: uint, dstSize: uint)
{
	assert dstSize % itemSize == 0;

	for (i .. dstSize)
	{
		dst[i]~ = item[i % itemSize]~;
	}
}


