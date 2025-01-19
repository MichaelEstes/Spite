package Main

Main()
{
	// The compile command will run a block of code at compile time and
	// store the returned output in the final program
	compileFunc := #compile ::() {
		if(targetOs == OS_Kind.Linux)
		{
			return ::() {
				log "Building for Linux";
			}
		}
		else
		{
			return ::() {
				log "Not building for Linux";
			}
		}

	}

	compileFunc();
}
