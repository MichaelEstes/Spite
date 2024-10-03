package Math

extern
{
	#link linux "libm.so";
	#link windows "msvcrt.dll";

	float64 sqrt(val: float64);
}

//extern llvm
//{
//	float64 "llvm.sqrt.f64"(val: float64) as sqrt;
//}

// No js link required since Math is a global object
//extern js
//{
//	float "Math.sqrt"(val: float) as sqrt;
//}

E: float = 2.718281828459045;

float Sqrt(val: float) => sqrt(val);