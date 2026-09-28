package OS

extern
{
    #link windows "kernel32";

    *void FindFirstFileA(lpFileName: *byte, lpFindFileData: *Win32FindData);
    bool FindNextFileA(hFindFile: *void, lpFindFileData: *Win32FindData);
    bool FindClose(hFindFile: *void);
    uint32 GetFileAttributesA(lpFileName: *byte);
}

extern
{
    #link linux "libc";

    *void opendir(name: *byte);
    *LinuxDirent readdir(dirp: *void);
    int32 closedir(dirp: *void);
    int32 access(pathname: *byte, mode: int32);
}

state Win32FileTime
{
    lowDateTime: uint32,
    highDateTime: uint32,
}

state Win32FindData
{
    fileAttributes: uint32,
    creationTime: Win32FileTime,
    lastAccessTime: Win32FileTime,
    lastWriteTime: Win32FileTime,
    fileSizeHigh: uint32,
    fileSizeLow: uint32,
    reserved0: uint32,
    reserved1: uint32,
    fileName: [260]byte,
    alternateFileName: [14]byte,
    fileType: uint32,
    creatorType: uint32,
    finderFlags: uint16,
}

state LinuxDirent
{
    inode: uint64,
    offset: int64,
    recordLength: uint16,
    fileType: byte,
    name: [256]byte,
}

pathSeparator := #compile byte {
    if (targetOs == OS_Kind.Windows) return '\\';
    return '/';
}

win32FileAttributeDirectory := uint32(0x10);
invalidFileAttributes := uint32(0xFFFFFFFF);
linuxDirentUnknown := byte(0);
linuxDirentDirectory := byte(4);
linuxDirentRegularFile := byte(8);
linuxExistsOk := int32(0);

bool IsCurrentOrParentDirectory(name: string)
{
    if (name == ".") return true;
    if (name == "..") return true;
    return false;
}

string NormalizePath(path: string)
{
    result := path.Copy();
    if (targetOs == OS_Kind.Windows)
    {
        for (i .. result.count)
        {
            if (result[i]~ == '/') result[i]~ = '\\';
        }
    }
    else 
    {
        for (i .. result.count)
        {
            if (result[i]~ == '\\') result[i]~ = '/';
        }
    }

    return result;
}

string GetAbsolutePath(path: string)
{
    if (IsAbsolute(path)) return NormalizePath(path);
    
    cwd := GetWorkingDir();

    joined := JoinPaths([cwd, path]);
    defer delete joined;
    normalized := NormalizePath(joined);
    return normalized;
}

bool IsAbsolute(path: string) 
{
    if (path.count == 0) return false;
    
    if (targetOs == OS_Kind.Windows)
    {
        // Check for drive letter (e.g. C:\) or UNC path (\\server)
        if (path.count < 2) return false;
        if (path[1]~ == ':') return true;
        if (path[0]~ == '\\' && path[1]~ == '\\') return true;
        return false;
    }
    
    return path[0]~ == '/';
}

string JoinPaths(paths: []string)
{
    if (paths.count == 0) return "";
    
    result := paths[0].Copy();
    for (i := 1 .. paths.count)
    {
        if (result.count > 0 && result[result.count - 1] != pathSeparator)
        {
            result.AppendIn(string(1, pathSeparator@));
        }
        
        path := paths[i];
        start := 0;
        if (path.count > 0 && (path[0]~ == '/' || path[0]~ == '\\'))
        {
            start = 1;
        }
        
        append := string(path.count - start, path[start]);
        result.AppendIn(append);
    }
    
    return result;
}

string GetDirectoryName(path: string)
{
    if (path.count == 0) return "";
    
    normalized := NormalizePath(path);
    lastSep := -1;
    
    for (i .. normalized.count)
    {
        if (normalized[i]~ == pathSeparator) lastSep = i;
    }
    
    if (lastSep == -1) return "";
    
    return string(lastSep, normalized[0]);
}

string GetFileName(path: string)
{
    if (path.count == 0) return "";
    
    normalized := NormalizePath(path);
    lastSep := -1;
    
    for (i .. normalized.count)
    {
        if (normalized[i]~ == pathSeparator) lastSep = i;
    }
    
    if (lastSep == -1) return normalized;
    
    startPtr := normalized[lastSep + 1];
    count := normalized.count - (lastSep + 1);
    return string(count, startPtr);
}

[]string GetFilesInDirectoryWindows(path: string)
{
    files := []string;

    absPath := GetAbsolutePath(path);
    defer delete absPath;

    searchPath := JoinPaths([absPath, "*"]);
    defer delete searchPath;

    findData := Win32FindData();
    handle := FindFirstFileA(searchPath[0], findData@);
    if ((handle as int) == -1) return files;
    defer FindClose(handle);

    hasNext := true;
    while (hasNext)
    {
        name := string(fixed findData.fileName);
        if (!IsCurrentOrParentDirectory(name))
        {
            if (!(findData.fileAttributes & win32FileAttributeDirectory))
            {
                files.Add(name.Copy());
            }
        }

        hasNext = FindNextFileA(handle, findData@);
    }

    return files;
}

[]string GetFilesInDirectoryLinux(path: string)
{
    files := []string;

    absPath := GetAbsolutePath(path);
    defer delete absPath;

    dir := opendir(absPath[0]);
    if (!dir) return files;
    defer closedir(dir);

    entry := readdir(dir);
    while (entry)
    {
        name := string(fixed entry.name);
        if (!IsCurrentOrParentDirectory(name))
        {
            if (entry.fileType == linuxDirentRegularFile)
            {
                files.Add(name.Copy());
            }
        }

        entry = readdir(dir);
    }

    return files;
}

[]string GetFilesInDirectory(path: string)
{
    getFiles := #compile ::[]string(string)
    {
        if (targetOs == OS_Kind.Windows) return GetFilesInDirectoryWindows;
        else return GetFilesInDirectoryLinux;
    }

    return getFiles(path);
}

bool PathExistsWindows(path: string)
{
    absPath := GetAbsolutePath(path);
    defer delete absPath;

    return GetFileAttributesA(absPath[0]) != invalidFileAttributes;
}

bool PathExistsLinux(path: string)
{
    absPath := GetAbsolutePath(path);
    defer delete absPath;

    return access(absPath[0], linuxExistsOk) == 0;
}

bool PathExists(path: string)
{
    pathExists := #compile ::bool(string)
    {
        if (targetOs == OS_Kind.Windows) return PathExistsWindows;
        else return PathExistsLinux;
    }

    return pathExists(path);
}

bool IsDirectoryWindows(path: string)
{
    absPath := GetAbsolutePath(path);
    defer delete absPath;

    attributes := GetFileAttributesA(absPath[0]);
    if (attributes == invalidFileAttributes) return false;

    return (attributes & win32FileAttributeDirectory) == win32FileAttributeDirectory;
}

bool IsDirectoryLinux(path: string)
{
    absPath := GetAbsolutePath(path);
    defer delete absPath;

    dir := opendir(absPath[0]);
    if (!dir) return false;
    closedir(dir);

    return true;
}

bool IsDirectory(path: string)
{
    isDirectory := #compile ::bool(string)
    {
        if (targetOs == OS_Kind.Windows) return IsDirectoryWindows;
        else return IsDirectoryLinux;
    }

    return isDirectory(path);
}

IterateDirectoryWindows(path: string, callback: ::(string, bool, *any), arg: *any)
{
    absPath := GetAbsolutePath(path);
    defer delete absPath;

    searchPath := JoinPaths([absPath, "*"]);
    defer delete searchPath;

    findData := Win32FindData();
    handle := FindFirstFileA(searchPath[0], findData@);
    if ((handle as int) == -1) return;
    defer FindClose(handle);

    hasNext := true;
    while (hasNext)
    {
        name := string(fixed findData.fileName);
        if (!IsCurrentOrParentDirectory(name))
        {
            entryPath := JoinPaths([absPath, name]);
            isDirectory := (findData.fileAttributes & win32FileAttributeDirectory) == win32FileAttributeDirectory;

            callback(entryPath, isDirectory, arg);
            if (isDirectory) IterateDirectoryWindows(entryPath, callback, arg);

            delete entryPath;
        }

        hasNext = FindNextFileA(handle, findData@);
    }
}

IterateDirectoryLinux(path: string, callback: ::(string, bool, *any), arg: *any)
{
    absPath := GetAbsolutePath(path);
    defer delete absPath;

    dir := opendir(absPath[0]);
    if (!dir) return;
    defer closedir(dir);

    entry := readdir(dir);
    while (entry)
    {
        name := string(fixed entry.name);
        if (!IsCurrentOrParentDirectory(name))
        {
            entryPath := JoinPaths([absPath, name]);

            isDirectory := entry.fileType == linuxDirentDirectory;
            if (entry.fileType == linuxDirentUnknown) isDirectory = IsDirectoryLinux(entryPath);

            callback(entryPath, isDirectory, arg);
            if (isDirectory) IterateDirectoryLinux(entryPath, callback, arg);

            delete entryPath;
        }

        entry = readdir(dir);
    }
}

IterateDirectory(path: string, callback: ::(string, bool, *any), arg: *any = null)
{
    iterate := #compile ::(string, ::(string, bool, *any), *any)
    {
        if (targetOs == OS_Kind.Windows) return IterateDirectoryWindows;
        else return IterateDirectoryLinux;
    }

    iterate(path, callback, arg);
}