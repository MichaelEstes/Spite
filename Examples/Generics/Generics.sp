package Main

// Generics are the singular token 

// T is a generic expression, being used here as a type
state GenericState<T>
{
	val: T
}

// Generics can be represented with any non-binary expression
GenericFunction<ToLog>()
{
	log ToLog;
}

// Generics can have default templates
state SizedArray<Size, Type = int> 
{
	arr: [Size]Type
}

SizedArray::()
{
	// arr is allocated, but not initialized, set each item to default
	for (i .. Size) this.arr[i] = Type();
}

Main()
{
	// GenericState is instatiated with the template int
	genericState := GenericState<int>();
	log genericState;
	// outputs { val: 0 }

	GenericFunction<19.2>();
	// outputs 19.2

	sizedArray := SizedArray<4>();
	log sizedArray;
	// outputs { arr: fixed [ 0, 0, 0, 0 ] } 
}