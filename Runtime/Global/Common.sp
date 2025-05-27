package _

int DefaultStringHash(str: string)
{
	result: int = 0;
	for (i .. str.count) result += (i * 0xDEAD) ^ str[i]~;
	return result;
}

int DefaultPrimitiveHash<Prim>(value: Prim)
{
	return value as int;
}

int DefaultPointerHash(value: *void)
{
	return value as int;
}

int DefaultStructuredHash<Type>(value: Type)
{
	byteCount := #sizeof Type;
	byteArr := value@ as *byte;
	result: int = 0;
	for (i .. byteCount) result += (i * 0xDEAD) ^ byteArr[i]~;

	return result;
}

::int(Type) GetDefaultHashFunctionFor<Type>(type: *_Type)
{
	typeKind := type.kind;
	typeUnion := type.type;

	switch (typeKind)
	{
		case (_TypeKind.PrimitiveType)
		{
			if(typeUnion.primitive.primitiveKind == _PrimitiveKind.String)
			{
				return DefaultStringHash as ::int(Type);
			}
			else
			{
				return DefaultPrimitiveHash<Type>;
			}
		}
		case (_TypeKind.StateType) continue;
		case (_TypeKind.UnionType) continue;
		case (_TypeKind.StructureType)
		{
			return DefaultStructuredHash<Type>;
		}
		case (_TypeKind.FunctionType) continue;
		case (_TypeKind.PointerType)
		{
			return DefaultPointerHash as ::int(Type);
		}
		case (_TypeKind.DynamicArrayType) break;
		case (_TypeKind.FixedArrayType) break;
		case (_TypeKind.ReferenceType) break;
	}

	log "Warning: Unable to create default hash for type";
	return ::int(value: Type) => return value as int;
}

int DefaultHash<Key>(key: Key)
{
	hashFunc := #compile ::int(Key) {
		typeOfKey := #typeof Key;
		return GetDefaultHashFunctionFor<Key>(typeOfKey);
	}

	return hashFunc(key);
}

bool _MembersEqual(left: *byte, right: *byte, members: *_Interop_Vector<*_Member>)
{
	for (member: **_Member in members)
	{
		memberType := member.value.type;
		memberOffset := member.offset;
		leftOffset := left + memberOffset;
		rightOffset := right + memberOffset;
		if (!_EqualType(leftOffset, rightOffset, memberType)) return false;
	}

	return true;
}

bool _EqualType(left: *byte, right: *byte, type: *_Type)
{
	typeData := type.type;

	switch (type.kind)
	{
		case (_TypeKind.PrimitiveType)
		{
			switch (typeData.primitive.primitiveKind)
			{
				case (_PrimitiveKind.Void) return false;
				case (_PrimitiveKind.Bool) return (left as *bool)~ == (right as *bool)~;
				case (_PrimitiveKind.Byte) return left~ == right~;
				case (_PrimitiveKind.I16) return (left as *int16)~ == (right as *int16)~;
				case (_PrimitiveKind.I32) return (left as *int32)~ == (right as *int32)~;
				case (_PrimitiveKind.I64) return (left as *int64)~ == (right as *int64)~;
				case (_PrimitiveKind.Int) return (left as *int)~ == (right as *int)~;
				case (_PrimitiveKind.F32) return (left as *float32)~ == (right as *float32)~;
				case (_PrimitiveKind.Float) return (left as *float64)~ == (right as *float64)~;
				case (_PrimitiveKind.String) return (left as *string)~ == (right as *string)~;
			}
		}
		case (_TypeKind.StateType)
		{
			_state := typeData.stateType;
			return _MembersEqual(left, right, _state.members@)
		}
		case (_TypeKind.StructureType)
		{
			return _MembersEqual(left, right, typeData.structureType)
		}
		case (_TypeKind.UnionType)
		{
			return memcmp(left, right, type.size) == 0;
		}
		case (_TypeKind.FunctionType) continue;
		case (_TypeKind.PointerType)
		{
			leftPtr := (left as **byte)~;
			rightPtr := (right as **byte)~;
			return leftPtr == rightPtr;
		}
		case (_TypeKind.ReferenceType)
		{
			return _EqualType(left, right, typeData.reference);
		}
		case (_TypeKind.DynamicArrayType)
		{
			itemType := typeData.dynamicArray;
			itemSize := itemType.size;
			
			leftArr := left as *array;
			rightArr := right as *array;
			if (leftArr.count != rightArr.count) return false;
			
			count := leftArr.count;
			leftStart := leftArr.memory.ptr;
			rightStart := leftArr.memory.ptr;

			for (i .. count)
			{
				offset := i * itemSize;
				leftOffset := leftStart + offset;
				rightOffset := rightStart + offset;
				if (!_EqualType(leftOffset, rightOffset, itemType)) return false;
			}

			return true;
		}
		case (_TypeKind.FixedArrayType)
		{
			itemType := typeData.fixedArray.type;
			itemSize := itemType.size;
			count := typeData.fixedArray.count;

			for (i .. count)
			{
				offset := i * itemSize;
				leftOffset := left + offset;
				rightOffset := right + offset;
				if (!_EqualType(leftOffset, rightOffset, itemType)) return false;
			}
			
			return true;
		}
	}

	return false;
}

bool DefaultEqual<Type>(left: Type, right: Type)
{
	type := #typeof Type;
	return _EqualType(left@ as *byte, right@ as *byte, type);
}