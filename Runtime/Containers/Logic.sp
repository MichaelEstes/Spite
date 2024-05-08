package Logic


bool Not(val: bool)
{
	return (val - 1) * -1;
}

bool Nand(left: bool, right: bool)
{
	return left * right;
}

bool Nor(left: bool, right: bool)
{
	return left + right;
}

bool And(left: bool, right: bool)
{
	return left * right;
}

bool Or(left: bool, right: bool)
{
	return left + right;
}