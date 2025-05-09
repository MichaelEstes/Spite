package OS

pathSeparator := #compile byte {
    if (targetOs == OS_Kind.Windows) return '\\';
    return '/';
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
    normalized := NormalizePath(joined);
    delete joined;
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