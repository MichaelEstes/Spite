#pragma once
#include "../Intermediate/GlobalTable.h"
#include "PackageChecker.h"

struct Checker 
{
	GlobalTable* globalTable;

	Checker(GlobalTable* globalTable)
	{
		this->globalTable = globalTable;
	}

	void Check()
	{
		for (auto& [key, value] : globalTable->symbolTableMap)
		{
			PackageChecker packageChecker = PackageChecker(globalTable, value);
			packageChecker.Check();
		}
	}
};