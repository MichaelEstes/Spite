package Main

state Player
{
	id: int,
	name: string,
	position: { x: float, y: float}
}

Main()
{
	val := Add(12, 43);
	log val; // outputs 55

	player := Player();
	// State types default to being passed by reference
	UpdatePlayerID(12, player);
	log player;
	// outputs { id: 12, name: "", position: struct { 0.000000, 0.000000 } }

	// Player is passed by value
	id := IncrementPlayerID(player);
	log id; // outputs 13
	log player;
	// outputs { id: 12, name: "", position: struct { 0.000000, 0.000000 } }

	anonFunc := ::() => log "Anonymous function call";
	anonFunc();
}

int Add(left: int, right: int)
{
	return left + right;
}

UpdatePlayerID(id: int, player: Player)
{
	player.id = id;
}

// Value types will copy the argument
int IncrementPlayerID(player: ~Player)
{
	player.id += 1;
	return player.id;
}