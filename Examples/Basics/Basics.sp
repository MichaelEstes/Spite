package Main

Main()
{
	//Variable declaration	
	myVar: int = 10;
	myVarImplicit := 10;

	// If else statements
	if (myVar == 10)
	{
		log "myVar is 10";
	}
	else if (myVar == 20)
	{
		log "myVar is 20";
	}
	else
	{
		log "myVar is " + IntToString(myVar);
	}

	// For loops
	for (i .. myVar)
	{
		log i;
	}

	// For in loops
	arr := [4, 3, 6, 9]
	for (item in arr)
	{
		log item;
	}

	//While loops
	ix := 0;
	while (ix < myVar)
	{
		log ix;
		ix += 1;
	}

	//Switch case statements
	switch (myVar)
	{
		case (9) log "case 9";
		case (10)
		{
			log "case 10";
			continue;
		}
		case (11) log "Also case 11";
		default log "Optional Default case";
	}

	// Defer statements
	defer { if (true) log "Forth"; }
	if (true)
	{
		defer log "Second";
		log "First";
	}
	defer if (true) log "Fifth";
	log "Third";
}