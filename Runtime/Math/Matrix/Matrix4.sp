package Matrix

import Vec

state Matrix4
{
	values: [4][4]float32
}

Matrix4::(val: float32)
{
	this.values[0] = float32:[val, val, val, val];
	this.values[1] = float32:[val, val, val, val];
	this.values[2] = float32:[val, val, val, val];
	this.values[3] = float32:[val, val, val, val];
}

Matrix4::SetIdentity() =>
{
	this.values[0] = float32:[1.0, 0.0, 0.0, 0.0];
	this.values[1] = float32:[0.0, 1.0, 0.0, 0.0];
	this.values[2] = float32:[0.0, 0.0, 1.0, 0.0];
	this.values[3] = float32:[0.0, 0.0, 0.0, 1.0];
}

Vec3 Matrix4::Position()
{
	return Vec3(this.values[3] as [3]float32);
}

ref Matrix4 Matrix4::Rotate(deg: float, axis: Vec3)
{

	return this;
}

