package Math

extern
{
	#link linux "libm";
	#link windows "msvcrt";

	float64 sqrt(val: float64);
	float64 sin(val: float64);
	float64 cos(val: float64);
	float64 tan(val: float64);
}

E: float = 2.718281828459045;

pi: float = 3.14159265359;

float Sqrt(val: float) => sqrt(val);

float Sin(val: float) => sin(val);

float Cos(val: float) => cos(val);

float Tan(val: float) => tan(val);

float Deg2Rad(deg: float) => deg * pi / 180.0;

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

uint UMin(l: uint, r: uint) => 
{
	if (l < r)
	{
		return l;
	}

	return r;
}

uint UMax(l: uint, r: uint) => 
{
	if (l > r)
	{
		return l;
	}

	return r;
}

int Clamp(value: int, min: int, max: int) => Min(Max(value, min), max);

uint NextPowerOfTwo(value: uint)
{
	if (!value) return 1;

	value -= 1;

    value |= value >> 1;
    value |= value >> 2;
    value |= value >> 4;
    value |= value >> 8;
    value |= value >> 16;
    value |= value >> 32;

    value += 1;

    return value;
}