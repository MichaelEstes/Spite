
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

-file  
description File path of the file to compile  
required  
type string

-dir  
description Directory to compile all .sp files in  
type string  

-entry  
description Name of the function in the file to run first, defaults to Main  
default Main

-name  
description Name of the final executable file  
default a

-output  
description Sets the compiler output format, defaults to llvm  
default llvm  
options llvm, c, ir, run  
type enum

-arch  
description Sets the target architecture to build the binary for  
default x64  
options x64, x86, arm32, arm64  
type enum

-os  
description Sets the target os to build the binary for, defaults to windows  
options windows, linux, mac, android, ios  
type enum


View the LinkerExamples.txt file in the root directory of the repo for linking commands  
Running with `-output run` will run the program through the interpreter without compiling
