package Math

extern
{
	#link linux "libm";
	#link windows "msvcrt";

	float64 sqrt(val: float64);
}

E: float = 2.718281828459045;

float Sqrt(val: float) => sqrt(val);