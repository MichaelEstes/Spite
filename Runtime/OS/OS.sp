package OS

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

string GetWorkingDir()
{
	if (interpreted) return workingDir;
	
	return GetExecDir();
}