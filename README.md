
![Logo](https://raw.githubusercontent.com/MichaelEstes/Spite/refs/heads/main/spite.ico)


# Spite Programming Language


```c
package Main

Main()
{
	#compile void => log "Hello ";
	log "World";
}
```


A compiled (LLVM) and interpreted C like programming language

It is still early on in it's development lifecycles, expect bugs and unhelpful error messages
## To compile

Requires LLVM 17 installed

Make a 'Build' folder in the project's root directory and cd into it

Run
```bash
  cmake ..
  make
```

You'll have an executable file named 'spite' in your 'Build' folder

Optionally you can add the 'spite' executable to your path

## Command line arguments

`-file`  
Description: File path of the file to compile  

`-dir`  
Description: Directory to compile all .sp files in  

`-entry`  
Description: Name of the function in the file to run first, defaults to Main  
Default: Main

`-name`  
Description: Name of the final executable file  
Default: a

`-output`  
Description: Sets the compiler output format, defaults to llvm  
Default: llvm  
Options: llvm, c, ir, run  

`-arch`  
Description: Sets the target architecture to build the binary for  
Default: x64  
Options: x64, x86, arm32, arm64  

`-os`  
Description: Sets the target os to build the binary for, defaults to windows  
Options: windows, linux, mac, android, ios  


`"-file Test.sp -dir Test -entry Init -os linux -arch x64"`

See the LinkerExamples.txt file in the root directory of the repo for linking commands  
Running with `-output run` will run the program through the interpreter without compiling

See the 'Examples' folder for code examples