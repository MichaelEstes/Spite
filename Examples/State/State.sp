package Main

state Player
{
	id: int,
	name: string,
	position: { x: float, y: float}
}

Player::ChangeName(name: string)
{
	this.name = name;
}

Player::Reset()
{
	this.id = 0;
	this.name = "";
	this.position = { 0.0, 0.0 };
}

Main()
{
	player := Player();
	log player;
	// outputs { id: 0, name: "", position: struct { 0.000000, 0.000000 } }

	player.id = 12;
	player.name = "Spitey";
	player.position = { 31.45, 76.29 };
	log player;
	// outputs { id: 12, name: Spitey, position: struct { 31.450001, 76.290001 } }

	player2: Player = { 7, "Spiter", { 99.32, 104.2 } };
	log player2;
	// outputs { id: 7, name: Spiter, position: struct { 99.320000, 104.199997 } }

	player2.ChangeName("Spitee");
	log player2;
	// outputs { id: 7, name: Spitee, position: struct { 99.320000, 104.199997 } }

	player.Reset();
	log player;
	// outputs { id: 0, name: "", position: struct { 0.000000, 0.000000 } }

	player3 := player2;
	player3.id = 9;
	log player2;
	// outputs { id: 9, name: Spitee, position: struct { 99.320000, 104.199997 } }
	log player3;
	// outputs { id: 9, name: Spitee, position: struct { 99.320000, 104.199997 } }

}