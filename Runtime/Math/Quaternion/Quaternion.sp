package Quaternion

import Math
import Vec
import Matrix

state Quaternion
{
	x: float32 = 1.0,
	y: float32,
	z: float32,
	w: float32
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

ref Norm<Quaternion> Quaternion::Normalize()
{
	len := Math.Sqrt(this.x * this.x + this.y * this.y + this.z * this.z + this.w * this.w);
	this.x /= len;
	this.y /= len;
	this.z /= len;
	this.w /= len;

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

	return this;
}

ref Quaternion Quaternion::FromRotationMatrix(mat: Matrix3)
{
	return this.FromAxes(mat[0] as Vec3, mat[1] as Vec3, mat[2] as Vec3);
}
