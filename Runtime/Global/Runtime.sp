package _

extern
{
	#link windows "Kernel32.dll";

	void GetModuleFileNameW(hModule: *void, lpFilename: *int16, nSize: int32);
}

enum OS_Kind: int
{
	Windows = 0,
	Linux = 1,
	OSX = 2,
	Android = 3,
	iOS = 4
}

enum Arch_Kind: int
{
	X64 = 0,
	X86 = 1,
	Arm32 = 2,
	Arm64 = 3
}

// These values are set during compilation
os := OS_Kind.Windows;
targetOs := OS_Kind.Windows;
arch := Arch_Kind.X64;
targetArch := Arch_Kind.X64;
interpreted := true;
exec_dir := "";

string GetExecDirWindows()
{
	path := [260]int16;
	GetModuleFileNameW(null, fixed path, 260);

	for (i .. 260)
		if(!path[i]) break;

	byteCount := i * #sizeof int16;
	buf := alloc(byteCount);
	copy_bytes(buf, fixed path, byteCount);
	pathStr := string(byteCount, buf);

	return pathStr.PrecedingLast('\\');
}

string GetExecDirLinux()
{
	log "Not implemented";
	return "";
}

string GetExecDir()
{
	osGetExecDir := #compile ::string() {
		if(targetOs == OS_Kind.Windows)
		{
			return GetExecDirWindows;
		}
		else
		{
			return GetExecDirLinux;
		}
	}

	return osGetExecDir();
}

void Print(str: string)
{

}

void PrintLine(str: string)
{

}

string IntToString(i: int)
{
	if (i == 0) return "0";
	
	maxCount := 22;
	buf := [maxCount]byte;
	for (index .. maxCount) buf[index] = byte(0);

	count := maxCount;
	negative := i < 0;
	if (negative) i *= -1;

	while (i > 0)
	{
		count -= 1;
		digit: byte = '0' + (i % 10);
		buf[count] = digit;
		i /= 10;
	}

	if (negative)
	{
		count -= 1;
		buf[count] = '-';
	}

	count = maxCount - count;
	start := (fixed buf)[maxCount - count];
	heapBuf := alloc(count);
	copy_bytes(heapBuf, start, count);
	return {count, heapBuf} as string;
}

floatFormatStr := ['%', 'i', '\0'];

string FloatToString(f: float, precision := 4)
{
	if (f == 0.0) return "0.0";

    integerPart: int = f as int;
	intStr := IntToString(integerPart);

    decimals: float = f - integerPart;
	decimalsInt := 0;

	buf := alloc(precision);
	for (i .. precision) {
        decimals *= 10;
		digit := decimals as byte;
		buf[i]~ = digit + '0';
        decimals -= digit;
    }
	decimalsStr := {precision, buf} as string;

    result := intStr + "." + decimalsStr;
	delete intStr;
	delete decimalsStr;

    return result;
}

string _SerializeType(value: *void, type: *_Type)
{
	typeKind := type.kind;
	typeData := type.type;

	switch (typeKind)
	{
		case (_TypeKind.PrimitiveType)
		{
			switch (typeData.primitive.primitiveKind)
			{
				case (_PrimitiveKind.Void) return "void";
				case (_PrimitiveKind.Bool)
				{
					bPtr := value as *bool;
					if(bPtr~) return "true";
					else return "false";
				}
				case (_PrimitiveKind.Byte) return IntToString((value as *byte)~);
				case (_PrimitiveKind.I16) return IntToString((value as *int16)~);
				case (_PrimitiveKind.I32) return IntToString((value as *int32)~);
				case (_PrimitiveKind.I64) return IntToString((value as *int64)~);
				case (_PrimitiveKind.Int) return IntToString((value as *int)~);
				case (_PrimitiveKind.F32) return FloatToString((value as *float32)~);
				case (_PrimitiveKind.Float) return FloatToString((value as *float64)~);
				case (_PrimitiveKind.String) return (value as *string)~;
			}
		}
		case (_TypeKind.StateType) break;
		case (_TypeKind.UnionType) break;
		case (_TypeKind.StructureType) break;
		case (_TypeKind.FunctionType) break;
		case (_TypeKind.PointerType) break;
		case (_TypeKind.DynamicArrayType) break;
		case (_TypeKind.FixedArrayType) break;
		case (_TypeKind.ReferenceType) break;
	}

	return "";
}

_log(value: any, type: *_Type)
{
	str := _SerializeType(value, type);
	log str;
}