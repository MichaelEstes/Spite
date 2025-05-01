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

	*void memcpy(dest: *void, src: *void, count: uint);
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

void zero_out_bytes(dst: *void, byteCount: uint)
{
	dstBytes := dst as *byte;
	for (i .. byteCount)
	{
		dstBytes[i]~ = byte(0);
	}
}

*void copy_bytes(dst: *void, src: *void, count: uint) => memcpy(dst, src, count);

void fill_memory(dst: *byte, item: *byte, itemSize: uint, dstSize: uint)
{
	assert dstSize % itemSize == 0;

	for (i .. dstSize)
	{
		dst[i]~ = item[i % itemSize]~;
	}
}

uint align_up(value: uint, alignment: uint) => 
		(value + alignment - 1) & ^(alignment - 1);


