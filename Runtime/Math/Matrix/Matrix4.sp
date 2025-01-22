package Matrix

import Vec

state Matrix4
{
	values: [4][4]float
}

Matrix4::SetIdentity() =>
{
	this.values[0] = [1.0, 0.0, 0.0, 0.0];
	this.values[1] = [0.0, 1.0, 0.0, 0.0];
	this.values[2] = [0.0, 0.0, 1.0, 0.0];
	this.values[3] = [0.0, 0.0, 0.0, 1.0];
}

Vec3 Matrix4::Position()
{
	return Vec3(this.values[3] as [3]float);
}
