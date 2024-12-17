package ECS

import System;
import Entity;

ECSList := []ECS;

state ECS
{
	systems: []System,
	entities: []Entity
}

ECS::Tick()
{
	for (system: System in this.systems)
	{
		
	}
}

int32 CreateECS()
{
	return ECSList.Add(ECS());
}

*ECS GetECS(index: int32)
{
	return ECSList[index]@;
}