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

bool DefaultEqual<Key>(left: Key, right: Key)
{
	return left == right;
}