package OS

extern
{
	#link linux "libc";
	#link windows "msvcrt";

	*CFile fopen(filename: *byte, mode: *byte);
	int32 fseek(stream: *CFile, offset: int32, origin: int32);
	int32 ftell(stream: *CFile);
	uint fread(buf: *byte, size: uint, count: uint, stream: *CFile);
	int32 fclose(stream: *CFile);
}

enum CSeek: int32
{
	Set = 0,
	Cur = 1,
	End = 2
}

state CFile
{
	opaque: *void
}

state File
{
	name: string,
	dir: string,

}

enum FileMode: byte
{
	Read = 0,
	Write,
	Append,
	ReadWrite,

	ReadBinary,
	WriteBinary,
	AppendBinary,
	ReadWriteBinary,

	ReadText,
	WriteText,
	AppendText,
	ReadWriteText,
}

modesArr := [ 
	"r",  "w",  "a",  "r+", 
	"rb", "wb", "ab", "rb+",
	"rt", "wt", "at", "rt+"
];

*byte GetFileMode(mode: FileMode)
{
	return modesArr[mode][0];
}

string ReadFile(path: string)
{
	file := fopen(path[0], GetFileMode(FileMode.ReadBinary))
	if (!file) return "";
	defer fclose(file);

	fseek(file, 0, CSeek.End);
	size := ftell(file);
	fseek(file, 0, CSeek.Set);

	buf := alloc(size + 1);
	fread(buf, size, 1, file);
	buf[size]~ = byte(0);

	return string(size, buf);
}

