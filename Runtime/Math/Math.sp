package Math

//#link linux "libc";
//#link windows "msvcrt";

//extern llvm
//{
//	float64 "llvm.sqrt.f64"(val: float64) as sqrt;
//}

//extern c
//{
//	float64 sqrt(val: float64);
//}

// No js link required since Math is a global object
//extern js
//{
//	float "Math.sqrt"(val: float) as sqrt;
//}

E: float = 2.718281828459045;

float Sqrt(val: float) => val * 0.5;