package Matrix

import Vec

state Matrix4
{
	m: [4][4]float32 = [
		float32:[1.0, 0.0, 0.0, 0.0],
		float32:[0.0, 1.0, 0.0, 0.0],
		float32:[0.0, 0.0, 1.0, 0.0],
		float32:[0.0, 0.0, 0.0, 1.0],
	]
}

Matrix4::(val: float32)
{
	this.m[0] = float32:[val, val, val, val];
	this.m[1] = float32:[val, val, val, val];
	this.m[2] = float32:[val, val, val, val];
	this.m[3] = float32:[val, val, val, val];
}

Matrix4::SetIdentity() =>
{
	this.m = [
		float32:[1.0, 0.0, 0.0, 0.0],
		float32:[0.0, 1.0, 0.0, 0.0],
		float32:[0.0, 0.0, 1.0, 0.0],
		float32:[0.0, 0.0, 0.0, 1.0],
	];
}

Vec3 Matrix4::Position()
{
	return Vec3(this.m[3] as [3]float32);
}

[4]float32 Matrix4::operator::[](index: uint32) => this.m[index];

Matrix4 Matrix4::operator::*(r: Matrix4)
{
	result := Matrix4();
	x := this.m[0][0];
	y := this.m[0][1];
	z := this.m[0][2];
	w := this.m[0][3];

	result.m[0][0] = x * r.m[0][0] + y * r.m[1][0] + z * r.m[2][0] + w * r.m[3][0];
	result.m[0][1] = x * r.m[0][1] + y * r.m[1][1] + z * r.m[2][1] + w * r.m[3][1];
	result.m[0][2] = x * r.m[0][2] + y * r.m[1][2] + z * r.m[2][2] + w * r.m[3][2];
	result.m[0][3] = x * r.m[0][3] + y * r.m[1][3] + z * r.m[2][3] + w * r.m[3][3];
	
	x = this.m[1][0];
	y = this.m[1][1];
	z = this.m[1][2];
	w = this.m[1][3];

	result.m[1][0] = x * r.m[0][0] + y * r.m[1][0] + z * r.m[2][0] + w * r.m[3][0];
	result.m[1][1] = x * r.m[0][1] + y * r.m[1][1] + z * r.m[2][1] + w * r.m[3][1];
	result.m[1][2] = x * r.m[0][2] + y * r.m[1][2] + z * r.m[2][2] + w * r.m[3][2];
	result.m[1][3] = x * r.m[0][3] + y * r.m[1][3] + z * r.m[2][3] + w * r.m[3][3];

	x = this.m[2][0];
	y = this.m[2][1];
	z = this.m[2][2];
	w = this.m[2][3];

	result.m[2][0] = x * r.m[0][0] + y * r.m[1][0] + z * r.m[2][0] + w * r.m[3][0];
	result.m[2][1] = x * r.m[0][1] + y * r.m[1][1] + z * r.m[2][1] + w * r.m[3][1];
	result.m[2][2] = x * r.m[0][2] + y * r.m[1][2] + z * r.m[2][2] + w * r.m[3][2];
	result.m[2][3] = x * r.m[0][3] + y * r.m[1][3] + z * r.m[2][3] + w * r.m[3][3];

	x = this.m[3][0];
	y = this.m[3][1];
	z = this.m[3][2];
	w = this.m[3][3];

	result.m[3][0] = x * r.m[0][0] + y * r.m[1][0] + z * r.m[2][0] + w * r.m[3][0];
	result.m[3][1] = x * r.m[0][1] + y * r.m[1][1] + z * r.m[2][1] + w * r.m[3][1];
	result.m[3][2] = x * r.m[0][2] + y * r.m[1][2] + z * r.m[2][2] + w * r.m[3][2];
	result.m[3][3] = x * r.m[0][3] + y * r.m[1][3] + z * r.m[2][3] + w * r.m[3][3];

	return result;
}

ref Matrix4 Matrix4::MakeRotation(angle: float, axis: Norm<Vec3>)
{
	vec := axis.vec;

	c := Math.Cos(angle);
	s := Math.Sin(angle);
	t := 1.0 - c;

	x := vec.x;
	y := vec.y;
	z := vec.z;

	tx := t * x;
	ty := t * y;
	tz := t * z;

	sx := s * x;
	sy := s * y;
	sz := s * z;
	
	this.m[0] = float32:[tx * x + c,  tx * y + sz, tx * z - sy, 0];
	this.m[1] = float32:[tx * y - sz, ty * y + c,  ty * z + sx, 0];
	this.m[2] = float32:[tx * z + sy, ty * z - sx, tz * z + c,  0];
	this.m[3] = float32:[0,           0,           0,           1];

	return this;
}

ref Matrix4 Matrix4::Rotate(angle: float, axis: Norm<Vec3>)
{
	this = this * Matrix4().MakeRotation(angle, axis);
	return this;
}

ref Matrix4 Matrix4::LookAt(eye: Vec3, target: Vec3, up: Vec3)
{
	forward := (eye - target).Normalize().vec;
	right := up.Cross(forward).Normalize().vec;
	newUp := forward.Cross(right);

	this.m[0] = float32:[right.x,   right.y,   right.z,   0];
	this.m[1] = float32:[newUp.x,   newUp.y,   newUp.z,   0];
	this.m[2] = float32:[forward.x, forward.y, forward.z, 0];
	this.m[3] = float32:[
		-(right.Dot(eye)),
		-(newUp.Dot(eye)),
		-(forward.Dot(eye)),
		1
	];

	return this;
}

ref Matrix4 Matrix4::Perspective(fovY: float, aspect: float, near: float, far: float)
{
	f: float32 = 1.0 / Math.Tan(fovY * 0.5);
	nf: float32 = 1.0 / (near - far);

	this.m[0] = float32:[f / aspect, 0, 0,                          0];
	this.m[1] = float32:[0,          f, 0,                          0];
	this.m[2] = float32:[0,          0, (far + near) * nf,         -1];
	this.m[3] = float32:[0,          0, (2 * far * near) * nf,      0];

	return this;
}