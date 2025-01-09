package _

extern
{
	#link windows "kernel32";

	void GetModuleFileNameW(hModule: *void, lpFilename: *int16, nSize: int32);
}

extern
{
	#link linux "libc";
	#link windows "msvcrt";

	int32 putchar(c: int32);
	int32 puts(buffer: *byte);
}

extern
{
	#link windows "msvcrt";
	int32 _snprintf(buffer: *byte, count: uint, format: *byte, arg: float);
}

extern
{
	#link linux "libc";
	int32 snprintf(buffer: *byte, count: uint, format: *byte, arg: float);
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
	osGetExecDir := #compile ::string() 
	{
		if(targetOs == OS_Kind.Windows) return GetExecDirWindows;
		else return GetExecDirLinux;
	}

	return osGetExecDir();
}

void Print(str: string)
{
	for (i .. str.count) putchar(str[i]~ as int32);
}

void PrintLine(str: string)
{
	Print(str);
	putchar('\n');
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

PrintFloat := #compile ::int32(*byte, uint, *byte, float) 
{
	if(targetOs == OS_Kind.Windows) return _snprintf;
	else return snprintf;
}

floatFormatStr := "%f";

string FloatToString(f: float, precision := 4)
{
	format := floatFormatStr.str;
	len := PrintFloat(null, 0, format, f);
	buffer := alloc(len + 1);
	PrintFloat(buffer, len + 1, format, f);

	return {len as int, buffer} as string;

    //integerPart: int = f as int;
	//intStr := IntToString(integerPart);
	//defer delete intStr;
	//
	//if (f < 0.0) 
	//{
	//	f *= -1;
	//	integerPart *= -1;
	//}
    //decimals: float = f - integerPart;
	//decimalsInt := 0;
	//
	//buf := alloc(precision);
	//for (i .. precision) 
	//{
    //    decimals *= 10;
	//	digit := decimals as byte;
	//	buf[i]~ = '0' + digit;
    //    decimals -= digit;
    //}
	//decimalsStr := {precision, buf} as string;
	//defer delete decimalsStr;
	//
    //result := intStr + "." + decimalsStr;
    //return result;
}

string _SerializeType(value: *byte, type: *_Type)
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
				case (_PrimitiveKind.Byte) return IntToString(value~);
				case (_PrimitiveKind.I16) return IntToString((value as *int16)~);
				case (_PrimitiveKind.I32) return IntToString((value as *int32)~);
				case (_PrimitiveKind.I64) return IntToString((value as *int64)~);
				case (_PrimitiveKind.Int) return IntToString((value as *int)~);
				case (_PrimitiveKind.F32) return FloatToString((value as *float32)~);
				case (_PrimitiveKind.Float) return FloatToString((value as *float64)~);
				case (_PrimitiveKind.String) return (value as *string)~;
			}
		}
		case (_TypeKind.StateType)
		{
			_state := typeData.stateType;
			out := _state.name.ToString() + " : {\n";

			for (member: **_Member in _state.members)
			{
				out = out + " " + member.value.name.ToString() + ": ";
				memberValue := value + member.offset;
				out = out + _SerializeType(memberValue, member.value.type);
				out = out + ",";
			}

			out.Last()~ = ' ';
			out = out + "\n}";
			return out;
		}
		case (_TypeKind.StructureType)
		{
			out := "struct {\n";
			for (member: **_Member in typeData.structureType)
			{
				memberValue := value + member.offset;
				out = out + " " + _SerializeType(memberValue, member.value.type) + ",";
			}

			out.Last()~ = ' ';
			out = out + "\n}";
			return out;
		}
		case (_TypeKind.UnionType)
		{
			out := "union { " + IntToString(type.size) + " bytes }";
			return out;
		}
		case (_TypeKind.FunctionType)
		{
			ptrInt := value as int;
			return "func (@" + IntToString(ptrInt) + ")";
		}
		case (_TypeKind.PointerType)
		{
			value = (value as **byte)~
			ptrInt := value as int;
			out := "Ptr @" + IntToString(ptrInt) + " ";

			if (ptrInt) out = out + _SerializeType(value, typeData.pointer);
			else out = out + "null";

			return out;
		}
		case (_TypeKind.ReferenceType)
		{
			ptrInt := value as int;
			out := "Ref @" + IntToString(ptrInt) + " ";

			if (ptrInt) out = out + _SerializeType(value, typeData.reference);
			else out = out + "nullref (error)";

			return out;
		}
		case (_TypeKind.DynamicArrayType)
		{
			itemType := typeData.dynamicArray;
			itemSize := itemType.size;
			
			arr := value as *array;
			count := arr.count;
			start := arr.start;

			if (!count) return "[]";

			out := "[";
			for (i .. count)
			{
				offset := i * itemSize;
				itemStart := start + offset;
				out = out + " " + _SerializeType(itemStart, itemType) + ",";
			}

			out.Last()~ = ' ';
			out = out + "]";
			return out;
		}
		case (_TypeKind.FixedArrayType)
		{
			itemType := typeData.fixedArray.type;
			itemSize := itemType.size;
			count := typeData.fixedArray.count;

			out := "fixed [";
			for (i .. count)
			{
				offset := i * itemSize;
				itemStart := value + offset;
				out = out + " " + _SerializeType(itemStart, itemType) + ",";
			}
			out.Last()~ = ' ';
			out = out + "]";
			return out;
		}
	}

	return "Error: Invalid type for _SerializeType";
}

_log(value: any, type: *_Type)
{
	str := _SerializeType(value, type);
	PrintLine(str);
}