package Quaternion

import Math
import Vec
import Matrix

state Quaternion
{
	x: float32,
	y: float32,
	z: float32,
	w: float32 = 1.0
}

Quaternion::(x: float32, y: float32, z: float32, w: float32) 
{
	this.x = x;
	this.y = y;
	this.z = z;
	this.w = w;
}

Quaternion::(vec: {x: float32, y: float32, z: float32, w: float32})
{
	this.x = vec.x;
	this.y = vec.y;
	this.z = vec.z;
	this.w = vec.w;
}

Quaternion::(vec: [4]float32)
{
	this.x = vec[0];
	this.y = vec[1];
	this.z = vec[2];
	this.w = vec[3];
}

Quaternion::(unitVec: Norm<Vec3>, angle: float32)
{
	vectorPart := unitVec.vec * Math.Sin(angle * 0.5);
	this.x = vectorPart.x;
	this.y = vectorPart.y;
	this.z = vectorPart.z;
	this.w = Math.Cos(angle * 0.5);
}

Quaternion::SetIdentity() =>
{
	this.x = 0.0;
	this.y = 0.0;
	this.z = 0.0;
	this.w = 1.0;
}

ref Norm<Quaternion> Quaternion::Normalize()
{
	len := Math.Sqrt(this.x * this.x + this.y * this.y + this.z * this.z + this.w * this.w);
	if (len > 0.0)
	{
		inv := 1.0 / len;
		this.x *= inv;
		this.y *= inv;
		this.z *= inv;
		this.w *= inv;
	}

	return this as Norm<Quaternion>;
}

ref Quaternion Quaternion::FromAxes(right: Vec3, up: Vec3, forward: Vec3)
{
	tr := right.x + up.y + forward.z;
	if (tr > 0.0)
	{
		s := Math.Sqrt(tr + 1.0) * 2.0;
		this.w = 0.25 * s;
		this.x = (up.z - forward.y) / s;
		this.y = (forward.x - right.z) / s;
		this.z = (right.y - up.x) / s;
	}
	else if (right.x > up.y && right.x > forward.z)
	{
		s := Math.Sqrt(1.0 + right.x - up.y - forward.z) * 2.0;
		this.w = (up.z - forward.y) / s;
		this.x = 0.25 * s;
		this.y = (up.x + right.y) / s;
		this.z = (forward.x + right.z) / s;
	}
	else if (up.y > forward.z)
	{
		s := Math.Sqrt(1.0 + up.y - right.x - forward.z) * 2.0;
		this.w = (forward.x - right.z) / s;
		this.x = (up.x + right.y) / s;
		this.y = 0.25 * s;
		this.z = (up.z + forward.y) / s;
	}
	else
	{
		s := Math.Sqrt(1.0 + forward.z - right.x - up.y) * 2.0;
		this.w = (right.y - up.x) / s;
		this.x = (forward.x - right.z) / s;
		this.y = (up.z + forward.y) / s;
		this.z = 0.25 * s;
	}

	this.Normalize();
	return this;
}

ref Quaternion Quaternion::FromRotationMatrix(mat: Matrix3)
{
	right   :=	Vec3(mat[0][0], mat[1][0], mat[2][0]);
	up      :=	Vec3(mat[0][1], mat[1][1], mat[2][1]);
	forward :=  Vec3(mat[0][2], mat[1][2], mat[2][2]);

	return this.FromAxes(right, up, forward);
}

Quaternion Quaternion::Inverse()
{
	return Quaternion(-this.x, -this.y, -this.z, this.w)
}

Matrix3 Quaternion::ToRotationMatrix()
{
	mat := Matrix3();

	xx := this.x * this.x;
	yy := this.y * this.y;
	zz := this.z * this.z;

	xy := this.x * this.y;
	wz := this.w * this.z;
	xz := this.x * this.z;
	wy := this.w * this.y;
	yz := this.y * this.z;
	wx := this.w * this.x;

	mat.m[0] = float32:[1.0 - 2.0 * (yy + zz), 2.0 * (xy - wz), 2.0 * (xz + wy)];
	mat.m[1] = float32:[2.0 * (xy + wz), 1.0 - 2.0 * (xx + zz), 2.0 * (yz - wx)];
	mat.m[2] = float32:[2.0 * (xz - wy), 2.0 * (yz + wx), 1.0 - 2.0 * (xx + yy)];

	return mat;
}

Vec3 Quaternion::Forward()
{
	f := Vec3();
	f.x = 2.0 * (this.x * this.z + this.w * this.y);
	f.y = 2.0 * (this.y * this.z - this.w * this.x);
	f.z = 1.0 - 2.0 * (this.x * this.x + this.y * this.y);
	return f;
}

Vec3 Quaternion::Up()
{
	u := Vec3();
	u.x = 2.0 * (this.x * this.y - this.w * this.z);
	u.y = 1.0 - 2.0 * (this.x * this.x + this.z * this.z);
	u.z = 2.0 * (this.y * this.z + this.w * this.x);
	return u;
}

Vec3 Quaternion::Left()
{
	l := Vec3();
	l.x = 1.0 - 2.0 * (this.y * this.y + this.z * this.z);
	l.y = 2.0 * (this.x * this.y + this.w * this.z);
	l.z = 2.0 * (this.x * this.z - this.w * this.y);
	return l;
}

Quaternion Quaternion::PowerOf(n: float32)
{
	halfCosAngle := Math.FClamp(this.w, -1.0, 1.0);
	angle := Math.Acos(halfCosAngle) * 2;
	if (Math.FAbs(angle) < 0.000001) return Quaternion(0, 0, 0, 1);

	scaledAngle := angle * n;

	axis := Vec3(this.x, this.y, this.z).Normalize();
	return Quaternion(axis, scaledAngle);
}

Quaternion Quaternion::Slerp(q: Quaternion, t: float32)
{
	return (q * this.Inverse()).PowerOf(t) * this;
}

Quaternion Quaternion::operator::*(q: Quaternion)
{
	result := Quaternion();

	vec1 := Vec3(this.x, this.y, this.z);
	vec2 := Vec3(q.x, q.y, q.z);
	scalar1 := this.w;
	scalar2 := q.w;

	result.w = scalar1 * scalar2 - vec1.Dot(vec2);

	scaledVec1 := vec2 * scalar1;
	scaledVec2 := vec1 * scalar2;
	cross := vec1.Cross(vec2);
	vectorPart := scaledVec1 + scaledVec2 + cross;

	result.x = vectorPart.x;
	result.y = vectorPart.y;
	result.z = vectorPart.z;

	return result;
}

Vec3 Quaternion::operator::*(v: Vec3)
{
	quat := Quaternion(v.x, v.y, v.z, 0.0);

	temp := this * quat;
	res := temp * this.Inverse();

	return Vec3(res.x, res.y, res.z);
}
