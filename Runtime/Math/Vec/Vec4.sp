package Vec;

import Math

state Vec4
{
	x: float32,
	y: float32,
	z: float32,
	w: float32
}

Vec4::(x: float32, y: float32, z: float32, w: float32) 
{
	this.x = x;
	this.y = y;
	this.z = z;
	this.w = w;
}

Vec4::(vec: {x: float32, y: float32, z: float32, w: float32})
{
	this.x = vec.x;
	this.y = vec.y;
	this.z = vec.z;
	this.w = vec.w;
}

Vec4::(vec: [4]float32)
{
	this.x = vec[0];
	this.y = vec[1];
	this.z = vec[2];
	this.z = vec[3];
}

float32 Vec4::SqrLength() => this.x * this.x + this.y * this.y + this.z * this.z + this.w * this.w;

float32 Vec4::Length() => Math.Sqrt(this.SqrLength());

float32 Vec4::Dot(right: Vec4) => this.x * right.x + this.y * right.y + this.z * right.z + this.w * right.w;

ref Norm<Vec4> Vec4::Normalize()
{
	len := this.Length();

	this.x /= len;
	this.y /= len;
	this.z /= len;
	this.w /= len

	return this as Norm<Vec4>;
}

Vec4 Vec4::operator::-(r: Vec4) => Vec4(this.x - r.x, this.y - r.y, this.z - r.z, this.w - r.w);
Vec4 Vec4::operator::-(r: float32) => Vec4(this.x - r, this.y - r, this.z - r, this.w - r);

Vec4 Vec4::operator::+(r: Vec4) => Vec4(this.x + r.x, this.y + r.y, this.z + r.z, this.w + r.w);
Vec4 Vec4::operator::+(r: float32) => Vec4(this.x + r, this.y + r, this.z + r, this.w + r);

Vec4 Vec4::operator::*(r: Vec4) => Vec4(this.x * r.x, this.y * r.y, this.z * r.z, this.w * r.w);
Vec4 Vec4::operator::*(r: float32) => Vec4(this.x * r, this.y * r, this.z * r, this.w * r);

Vec4 Vec4::operator::/(r: Vec4) => Vec4(this.x / r.x, this.y / r.y, this.z / r.z, this.w / r.w);
Vec4 Vec4::operator::/(r: float32) => Vec4(this.x / r, this.y / r, this.z / r, this.w / r);
