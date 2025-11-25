package Vec;

import Math

state Vec2
{
	x: float32,
	y: float32
}

Vec2::(x: float32, y: float32) 
{
	this.x = x;
	this.y = y;
}

Vec2::(vec: {x: float32, y: float32})
{
	this.x = vec.x;
	this.y = vec.y;
}

Vec2::(vec: [2]float32)
{
	this.x = vec[0];
	this.y = vec[1];
}

float32 Vec2::SqrLength() => this.x * this.x + this.y * this.y;

float32 Vec2::Length() => Math.Sqrt(this.SqrLength());

float32 Vec2::Dot(right: Vec2) => this.x * right.x + this.y * right.y;

ref Norm<Vec2> Vec2::Normalize()
{
	len := this.Length();

	this.x /= len;
	this.y /= len;

	return this as Norm<Vec2>;
}

Vec2 Vec2::operator::-(r: Vec2) => Vec2(this.x - r.x, this.y - r.y);
Vec2 Vec2::operator::-(r: float32) => Vec2(this.x - r, this.y - r);

Vec2 Vec2::operator::+(r: Vec2) => Vec2(this.x + r.x, this.y + r.y);
Vec2 Vec2::operator::+(r: float32) => Vec2(this.x + r, this.y + r);

Vec2 Vec2::operator::*(r: Vec2) => Vec2(this.x * r.x, this.y * r.y);
Vec2 Vec2::operator::*(r: float32) => Vec2(this.x * r, this.y * r);

Vec2 Vec2::operator::/(r: Vec2) => Vec2(this.x / r.x, this.y / r.y);
Vec2 Vec2::operator::/(r: float32) => Vec2(this.x / r, this.y / r);

