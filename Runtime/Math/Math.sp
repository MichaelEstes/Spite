package Math

extern
{
	#link linux "libm";
	#link windows "msvcrt";

	float64 sqrt(val: float64);
}

E: float = 2.718281828459045;

float Sqrt(val: float) => sqrt(val);

int Min(l: int, r: int) => 
{
	if (l < r)
	{
		return l;
	}

	return r;
}

int Max(l: int, r: int) => 
{
	if (l > r)
	{
		return l;
	}

	return r;
}