package OS

extern
{
	#link windows "kernel32";

	void GetModuleFileNameA(hModule: *void, lpFilename: *byte, nSize: int32);
}

string GetExecDirWindows()
{
	path := [260]byte;
	GetModuleFileNameA(null, fixed path, 260);

	for (i .. 260)
		if(!path[i]) break;

	buf := alloc(i);
	copy_bytes(buf, fixed path, i);
	pathStr := string(i, buf);

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

string GetWorkingDir()
{
	if (interpreted) return workingDir;
	
	return GetExecDir();
}