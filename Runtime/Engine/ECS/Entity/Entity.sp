package Entity

// Get MAX_ENTITY_COUNT from properties somewhere
//MAX_ENTITY_COUNT := #compile int => 100000;

state Entity 
{
	index: uint32,
}

Entity::(index: uint32)
{
	this.index = index;
}

Entity GetNextEntity()
{

}

Entity Create()
{

}