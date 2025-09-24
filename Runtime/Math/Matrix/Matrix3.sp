package Matrix

import Vec
import Quaternion

state Matrix3
{
	m: [3][3]float32 = [
		float32:[1.0, 0.0, 0.0],
		float32:[0.0, 1.0, 0.0],
		float32:[0.0, 0.0, 1.0],
	]
}

Matrix3::(val: float32)
{
	this.m[0] = float32:[val, val, val];
	this.m[1] = float32:[val, val, val];
	this.m[2] = float32:[val, val, val];
}

Matrix3::(m: [3][3]float32)
{
	this.m = m;
}

Matrix3::(mat4: Matrix4)
{
	m := mat4.m;
	m0 := m[0];
	m1 := m[1];
	m2 := m[2];

	this.m[0] = float32:[m0[0], m0[1], m0[2]];
	this.m[1] = float32:[m1[0], m1[1], m1[2]];
	this.m[2] = float32:[m2[0], m2[1], m2[2]];
}

Matrix3::SetIdentity() =>
{
	this.m = [
		float32:[1.0, 0.0, 0.0],
		float32:[0.0, 1.0, 0.0],
		float32:[0.0, 0.0, 1.0],
	];
}

float32 Matrix3::Determinant()
{
	return (
		this.m[0][0] * (this.m[1][1] * this.m[2][2] - this.m[1][2] * this.m[2][1]) -
		this.m[0][1] * (this.m[1][0] * this.m[2][2] - this.m[1][2] * this.m[2][0]) +
		this.m[0][2] * (this.m[1][0] * this.m[2][1] - this.m[1][1] * this.m[2][0])
	);
}

ref [3]float32 Matrix3::operator::[](index: uint32) => this.m[index];

Matrix3 Matrix3::operator::*(r: Matrix3)
{
	result := Matrix3();
	x := this.m[0][0];
	y := this.m[0][1];
	z := this.m[0][2];

	result.m[0][0] = x * r.m[0][0] + y * r.m[1][0] + z * r.m[2][0];
	result.m[0][1] = x * r.m[0][1] + y * r.m[1][1] + z * r.m[2][1];
	result.m[0][2] = x * r.m[0][2] + y * r.m[1][2] + z * r.m[2][2];
	
	x = this.m[1][0];
	y = this.m[1][1];
	z = this.m[1][2];

	result.m[1][0] = x * r.m[0][0] + y * r.m[1][0] + z * r.m[2][0];
	result.m[1][1] = x * r.m[0][1] + y * r.m[1][1] + z * r.m[2][1];
	result.m[1][2] = x * r.m[0][2] + y * r.m[1][2] + z * r.m[2][2];

	x = this.m[2][0];
	y = this.m[2][1];
	z = this.m[2][2];

	result.m[2][0] = x * r.m[0][0] + y * r.m[1][0] + z * r.m[2][0];
	result.m[2][1] = x * r.m[0][1] + y * r.m[1][1] + z * r.m[2][1];
	result.m[2][2] = x * r.m[0][2] + y * r.m[1][2] + z * r.m[2][2];

	return result;
}

Vec3 Matrix3::operator::*(v: Vec3)
{
	result := Vec3();
	
	x := this.m[0];
	y := this.m[1];
	z := this.m[2];

	result.x = v.x * x[0] + v.y * y[0] + v.z * z[0];
	result.y = v.x * x[1] + v.y * y[1] + v.z * z[1];
	result.z = v.x * x[2] + v.y * y[2] + v.z * z[2];

	return result;
}