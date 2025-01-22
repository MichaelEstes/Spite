package Vec;

import Math

state Vec3
{
	x: float,
	y: float,
	z: float
}

Vec3::(x: float, y: float, z: float) 
{
	this.x = x;
	this.y = y;
	this.z = z;
}

Vec3::(vec: {x: float, y: float, z: float})
{
	this.x = vec.x;
	this.y = vec.y;
	this.z = vec.z;
}

Vec3::(vec: [3]float)
{
	this.x = vec[0];
	this.y = vec[1];
	this.z = vec[2];
}

float Vec3::SqrLength() => this.x * this.x + this.y * this.y + this.z * this.z;

float Vec3::Length() => Math.Sqrt(this.SqrLength());

float Vec3::Dot(right: Vec3) => this.x * right.x + this.y * right.y + this.z * right.z;

Vec3 Vec3::Cross(right: Vec3) =>
	Vec3(this.y * right.z - right.y * this.z,
	    -(this.x * right.z - right.x * this.z),
	    this.x * right.y - right.x * this.y);

