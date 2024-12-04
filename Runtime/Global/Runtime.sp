package _

extern
{
	#link linux "libm.so";
	#link windows "Kernel32.dll";

	void GetModuleFileNameW(hModule: *void, lpFilename: *int16, nSize: int32);
}

Windows := 0;
Linux := 1;
OSX := 2;
Android := 3;
iOS := 4;

os := Windows;
targetOs := Windows;

X64 := 0;
X86 := 1;
Arm32 := 2;
Arm64 := 3;

arch := X64;
targetArch := X64;

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

	return pathStr.PrecedingFirst(byte(92));
}

string GetExecDirLinux()
{
	log "Not implemented";
	return "";
}

string GetExecDir()
{
	osGetExecDir := #compile ::string() {
		if(targetOs == Windows)
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