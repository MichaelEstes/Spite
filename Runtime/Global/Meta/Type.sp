package _

bool _Type::IsPrimitive() => this.kind == _TypeKind.PrimitiveType;

bool _Type::IsString() => this.IsPrimitive() && 
							this.type.primitive.primitiveKind == _PrimitiveKind.String;

bool _Type::IsState() => this.kind == _TypeKind.StateType;

bool _Type::IsStructure() => this.kind == _TypeKind.StructureType;
