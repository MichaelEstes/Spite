package Matrix

import Vec
import Quaternion

state Matrix4
{
	m: [4][4]float32 = [
		float32:[1.0, 0.0, 0.0, 0.0],
		float32:[0.0, 1.0, 0.0, 0.0],
		float32:[0.0, 0.0, 1.0, 0.0],
		float32:[0.0, 0.0, 0.0, 1.0],
	]
}

Matrix4::(m: [4][4]float32)
{
	this.m = m;
}

Matrix4::(val: float32)
{
	this.m[0] = float32:[val, val, val, val];
	this.m[1] = float32:[val, val, val, val];
	this.m[2] = float32:[val, val, val, val];
	this.m[3] = float32:[val, val, val, val];
}

Matrix4::(mat3: Matrix3)
{
	m := mat3.m;
	m0 := m[0];
	m1 := m[1];
	m2 := m[2];

	this.m[0] = float32:[m0[0], m0[1], m0[2], 0.0];
	this.m[1] = float32:[m1[0], m1[1], m1[2], 0.0];
	this.m[2] = float32:[m2[0], m2[1], m2[2], 0.0];
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

float32 Matrix4::Determinant()
{
	det := 0.0;

	subMat1 := Matrix3([
		float32:[this.m[1][1], this.m[1][2], this.m[1][3]],
		float32:[this.m[2][1], this.m[2][2], this.m[2][3]],
		float32:[this.m[3][1], this.m[3][2], this.m[3][3]],
	]);
	det += this.m[0][0] * subMat1.Determinant();

	subMat2 := Matrix3([
		float32:[this.m[1][0], this.m[1][2], this.m[1][3]],
		float32:[this.m[2][0], this.m[2][2], this.m[2][3]],
		float32:[this.m[3][0], this.m[3][2], this.m[3][3]],
	]);
	det -= this.m[0][1] * subMat2.Determinant();

	subMat3 := Matrix3([
		float32:[this.m[1][0], this.m[1][1], this.m[1][3]],
		float32:[this.m[2][0], this.m[2][1], this.m[2][3]],
		float32:[this.m[3][0], this.m[3][1], this.m[3][3]],
	]);
	det += this.m[0][2] * subMat3.Determinant();

	subMat4 := Matrix3([
		float32:[this.m[1][0], this.m[1][1], this.m[1][2]],
		float32:[this.m[2][0], this.m[2][1], this.m[2][2]],
		float32:[this.m[3][0], this.m[3][1], this.m[3][2]],
	]);
	det -= this.m[0][3] * subMat4.Determinant();

	return det;
}

Matrix4 Matrix4::Inverse()
{
	a00 := this.m[0][0];
	a01 := this.m[0][1];
	a02 := this.m[0][2];
	a03 := this.m[0][3];
	a10 := this.m[1][0];
	a11 := this.m[1][1];
	a12 := this.m[1][2];
	a13 := this.m[1][3];
	a20 := this.m[2][0];
	a21 := this.m[2][1];
	a22 := this.m[2][2];
	a23 := this.m[2][3];
	a30 := this.m[3][0];
	a31 := this.m[3][1];
	a32 := this.m[3][2];
	a33 := this.m[3][3];

	b00 := a00 * a11 - a01 * a10;
	b01 := a00 * a12 - a02 * a10;
	b02 := a00 * a13 - a03 * a10;
	b03 := a01 * a12 - a02 * a11;
	b04 := a01 * a13 - a03 * a11;
	b05 := a02 * a13 - a03 * a12;
	b06 := a20 * a31 - a21 * a30;
	b07 := a20 * a32 - a22 * a30;
	b08 := a20 * a33 - a23 * a30;
	b09 := a21 * a32 - a22 * a31;
	b10 := a21 * a33 - a23 * a31;
	b11 := a22 * a33 - a23 * a32;

	inverseDeterminant := 1.0 / (
		b00 * b11 - b01 * b10 + b02 * b09 +
		b03 * b08 - b04 * b07 + b05 * b06
	);

	result := Matrix4();
	result.m[0] = float32:[
		(a11 * b11 - a12 * b10 + a13 * b09) * inverseDeterminant,
		(a02 * b10 - a01 * b11 - a03 * b09) * inverseDeterminant,
		(a31 * b05 - a32 * b04 + a33 * b03) * inverseDeterminant,
		(a22 * b04 - a21 * b05 - a23 * b03) * inverseDeterminant,
	];
	result.m[1] = float32:[
		(a12 * b08 - a10 * b11 - a13 * b07) * inverseDeterminant,
		(a00 * b11 - a02 * b08 + a03 * b07) * inverseDeterminant,
		(a32 * b02 - a30 * b05 - a33 * b01) * inverseDeterminant,
		(a20 * b05 - a22 * b02 + a23 * b01) * inverseDeterminant,
	];
	result.m[2] = float32:[
		(a10 * b10 - a11 * b08 + a13 * b06) * inverseDeterminant,
		(a01 * b08 - a00 * b10 - a03 * b06) * inverseDeterminant,
		(a30 * b04 - a31 * b02 + a33 * b00) * inverseDeterminant,
		(a21 * b02 - a20 * b04 - a23 * b00) * inverseDeterminant,
	];
	result.m[3] = float32:[
		(a11 * b07 - a10 * b09 - a12 * b06) * inverseDeterminant,
		(a00 * b09 - a01 * b07 + a02 * b06) * inverseDeterminant,
		(a31 * b01 - a30 * b03 - a32 * b00) * inverseDeterminant,
		(a20 * b03 - a21 * b01 + a22 * b00) * inverseDeterminant,
	];

	return result;
}

ref Matrix4 Matrix4::Compose(pos: Vec3, rot: Quaternion, scale: Vec3)
{
	x := rot.x;
	y := rot.y;
	z := rot.z;
	w := rot.w;

	x2 := x + x;  y2 := y + y;  z2 := z + z;
	xx := x * x2; xy := x * y2; xz := x * z2;
	yy := y * y2; yz := y * z2; zz := z * z2;
	wx := w * x2; wy := w * y2; wz := w * z2;

	sx := scale.x; sy := scale.y; sz := scale.z;

	this.m[0] = float32:[(1.0 - yy - zz) * sx, (xy + wz) * sx, (xz - wy) * sx, 0.0];
	this.m[1] = float32:[(xy - wz) * sy, (1.0 - xx - zz) * sy, (yz + wx) * sy, 0.0];
	this.m[2] = float32:[(xz + wy) * sz, (yz - wx) * sz, (1.0 - xx - yy) * sz, 0.0];
	this.m[3] = float32:[pos.x, pos.y, pos.z, 1.0];

	return this;
}

Matrix4::Decompose(outPos: *Vec3, outRot: *Quaternion, outScale: *Vec3)
{
	// Scale
	sx := Vec3(this.m[0][0], this.m[0][1], this.m[0][2]).Length();
	sy := Vec3(this.m[1][0], this.m[1][1], this.m[1][2]).Length();
	sz := Vec3(this.m[2][0], this.m[2][1], this.m[2][2]).Length();

	det := this.Determinant();
	if (det < 0.0) sx = -sx;

	outScale~ = Vec3(sx, sy, sz);

	// Rotation
	invSx := 1.0 / sx;
	invSy := 1.0 / sy;
	invSz := 1.0 / sz;

	mat3 := Matrix3(
		[
			float32:[this.m[0][0] * invSx, this.m[0][1] * invSx, this.m[0][2] * invSx],
			float32:[this.m[1][0] * invSy, this.m[1][1] * invSy, this.m[1][2] * invSy],
			float32:[this.m[2][0] * invSz, this.m[2][1] * invSz, this.m[2][2] * invSz],
		]
	);

	outRot.FromRotationMatrix(mat3);

	// Position
	outPos~ = Vec3(this.m[3][0], this.m[3][1], this.m[3][2]);
}

ref Matrix4 Matrix4::MakeRotation(angle: float32, axis: Norm<Vec3>)
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

ref Matrix4 Matrix4::Rotate(angle: float32, axis: Norm<Vec3>)
{
	this = this * Matrix4().MakeRotation(angle, axis);
	return this;
}

ref Matrix4 Matrix4::MakeTranslation(vec: Vec3)
{
	this.m[3] = float32:[vec.x, vec.y, vec.z, 1.0];
	
	return this;
}

ref Matrix4 Matrix4::LookAt(camera: Vec3, center: Vec3, up: Vec3)
{
	forward := (center - camera).Normalize().vec;
	right := forward.Cross(up).Normalize().vec;
	trueUp := right.Cross(forward);

	this.m[0] = float32:[right.x, trueUp.x, -forward.x, 0];
	this.m[1] = float32:[right.y, trueUp.y, -forward.y, 0];
	this.m[2] = float32:[right.z, trueUp.z, -forward.z, 0];
	this.m[3] = float32:[
		-(right.Dot(camera)),
		-(trueUp.Dot(camera)),
		forward.Dot(camera),
		1
	];

	return this;
}

ref Matrix4 Matrix4::Frustrum(left: float32, right: float32, bottom: float32, top: float32, 
							  near: float32, far: float32)
{
	this.m[0] = float32:[(2.0 * near) / (right - left), 0.0, 0.0, 0.0];
	this.m[1] = float32:[0.0, (2.0 * near) / (top - bottom), 0.0, 0.0];
	this.m[2] = float32:[(right + left) / (right - left), (top + bottom) / (top - bottom), 
						 -(far + near) / (far - near), -1.0];
	this.m[3] = float32:[0.0, 0.0, -(2.0 * far * near) / (far - near), 0.0];

	return this;
}

ref Matrix4 Matrix4::Perspective(fov: float32, aspect: float32, near: float32, far: float32)
{
	f: float32 = Math.Tan(fov * 0.5);
	
	this.m[0] = float32:[1.0 / (aspect * f), 0.0, 0.0, 0.0];
	this.m[1] = float32:[0.0, 1.0 / f, 0.0, 0.0];
	this.m[2] = float32:[0.0, 0.0, far / (near - far), -1.0];
	this.m[3] = float32:[0.0, 0.0, -(far * near) / (far - near), 0.0];

	return this;
}

ref [4]float32 Matrix4::operator::[](index: uint32) => this.m[index];

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

Vec4 Matrix3::operator::*(v: Vec4)
{
	result := Vec4();
	
	x := this.m[0];
	y := this.m[1];
	z := this.m[2];
	w := this.m[3];

	result.x = v.x * x[0] + v.y * y[0] + v.z * z[0] + v.w * w[0];
	result.y = v.x * x[1] + v.y * y[1] + v.z * z[1] + v.w * w[1];
	result.z = v.x * x[2] + v.y * y[2] + v.z * z[2] + v.w * w[2];
	result.w = v.x * x[3] + v.y * y[3] + v.z * z[3] + v.w * w[3];

	return result;
}