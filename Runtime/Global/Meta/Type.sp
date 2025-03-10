package _

bool _Type::IsPrimitive() => this.kind == _TypeKind.PrimitiveType;

bool _Type::IsString() => this.IsPrimitive() && 
							this.type.primitive.primitiveKind == _PrimitiveKind.String;

bool _Type::IsInteger() => this.IsPrimitive() && (
	this.type.primitive.primitiveKind == _PrimitiveKind.Bool ||
	this.type.primitive.primitiveKind == _PrimitiveKind.Byte ||
	this.type.primitive.primitiveKind == _PrimitiveKind.I16 ||
	this.type.primitive.primitiveKind == _PrimitiveKind.I32 ||
	this.type.primitive.primitiveKind == _PrimitiveKind.I64 ||
	this.type.primitive.primitiveKind == _PrimitiveKind.Int 
);

bool _Type::IsState() => this.kind == _TypeKind.StateType;

bool _Type::IsStructure() => this.kind == _TypeKind.StructureType;

bool _Type::IsPointer() => this.kind == _TypeKind.PointerType;
