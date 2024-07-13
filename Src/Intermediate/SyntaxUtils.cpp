#include "SyntaxUtils.h"

bool operator==(const Type& left, const Type& right)
{
	if (left.typeID == TypeID::ValueType) return *left.valueType.type == right;
	if (right.typeID == TypeID::ValueType) return *right.valueType.type == left;
	if (left.typeID != right.typeID) return false;

	switch (left.typeID)
	{
	case PrimitiveType:
	{
		auto& l = left.primitiveType;
		auto& r = right.primitiveType;
		return l.isSigned == r.isSigned && l.size == r.size && l.type == r.type;
	}
	case NamedType:
		return left.namedType.typeName->val == right.namedType.typeName->val;
	case ExplicitType:
	{
		if (left.explicitType.declarations->size() != right.explicitType.declarations->size()) return false;
		for (int i = 0; i < left.explicitType.declarations->size(); i++)
		{
			if (*left.explicitType.declarations->at(i)->definition.type !=
				*right.explicitType.declarations->at(i)->definition.type) return false;
		}
		return true;
	}
	case ImplicitType:
		// Implicit Types cannot be compared
		return false;
	case PointerType:
	{
		auto& l = left.pointerType;
		auto& r = right.pointerType;
		return *l.type == *r.type;
	}
	case ValueType:
		// Should never get here, value types are just a signal to copy a type's memory into a new location
		// Essentially the opposite of a reference type, since everything defaults to a reference
		return false;
	case ArrayType:
		return *left.arrayType.type == *right.arrayType.type;
	case TemplatedType:
	{
		auto& l = left.templatedType;
		auto& r = right.templatedType;
		eastl::vector<Expr*>* lTempl = l.templates->templateExpr.templateArgs;
		eastl::vector<Expr*>* rTempl = r.templates->templateExpr.templateArgs;
		if (lTempl->size() != rTempl->size()) return false;
		for (int i = 0; i < lTempl->size(); i++)
		{
			if (*lTempl->at(i) != *rTempl->at(i)) return false;
		}

		return *l.type == *r.type;
	}
	case FunctionType:
	{
		auto& l = left.functionType;
		auto& r = right.functionType;
		if (*l.returnType != *r.returnType) return false;
		if (l.paramTypes->size() != r.paramTypes->size()) return false;
		for (int i = 0; i < l.paramTypes->size(); i++)
		{
			if (*l.paramTypes->at(i) != *r.paramTypes->at(i)) return false;
		}
		return true;
	}
	case ImportedType:
		return left.importedType.packageName->val == right.importedType.packageName->val &&
			left.importedType.typeName->val == right.importedType.typeName->val;
	case FixedArrayType:
		return *left.fixedArrayType.type == *right.fixedArrayType.type &&
			left.fixedArrayType.size == right.fixedArrayType.size;
	default:
		break;
	}

	return false;
}

bool operator!=(const Type& left, const Type& right)
{
	return !(left == right);
}

bool operator==(const Expr& left, const Expr& right)
{
	if (left.typeID != right.typeID) return false;

	switch (left.typeID)
	{
	case InvalidExpr:
		//Maybe this should return false?
		return true;
	case LiteralExpr:
		return left.literalExpr.val->val == right.literalExpr.val->val &&
			left.literalExpr.type == right.literalExpr.type;
	case IdentifierExpr:
		return left.identifierExpr.identifier->val == right.identifierExpr.identifier->val;
	case PrimitiveExpr:
		return left.primitiveExpr.primitive == right.primitiveExpr.primitive;
	case SelectorExpr:
		return *left.selectorExpr.on == *right.selectorExpr.on &&
			*left.selectorExpr.select == *right.selectorExpr.select;
	case IndexExpr:
		return *left.indexExpr.of == *right.indexExpr.of &&
			*left.indexExpr.index == *right.indexExpr.index;
	case FunctionCallExpr:
	{
		if (left.functionCallExpr.params->size() != right.functionCallExpr.params->size()) return false;

		for (size_t i = 0; i < left.functionCallExpr.params->size(); i++)
		{
			if (*left.functionCallExpr.params->at(i) != *right.functionCallExpr.params->at(i)) return false;
		}

		return *left.functionCallExpr.function == *right.functionCallExpr.function;
	}
	case NewExpr:
		return *left.newExpr.primaryExpr == *right.newExpr.primaryExpr;
	case FixedExpr:
		return *left.fixedExpr.atExpr == *right.fixedExpr.atExpr;
	//Not sure what good comparing anon or explicit types is
	case AnonTypeExpr:
	{
		if (left.anonTypeExpr.values->size() != right.anonTypeExpr.values->size()) return false;
		for (size_t i = 0; i < left.anonTypeExpr.values->size(); i++)
		{
			if (!(*left.anonTypeExpr.values->at(i) == *right.anonTypeExpr.values->at(i))) return false;
		}
		return true;
	}
	case ExplicitTypeExpr:
		return left.explicitTypeExpr.values->size() == right.explicitTypeExpr.values->size();
	case AsExpr:
		return *left.asExpr.to == *right.asExpr.to &&
			*left.asExpr.of == *right.asExpr.of;
	case DereferenceExpr:
		return *left.dereferenceExpr.of == *right.dereferenceExpr.of;
	case ReferenceExpr:
		return *left.referenceExpr.of == *right.referenceExpr.of;
	case BinaryExpr:
		return *left.binaryExpr.left == *right.binaryExpr.left &&
			*left.binaryExpr.right == *right.binaryExpr.right &&
			left.binaryExpr.opType == right.binaryExpr.opType;
		break;
	case UnaryExpr:
		return *left.unaryExpr.expr == *right.unaryExpr.expr &&
			left.unaryExpr.opType == right.unaryExpr.opType;
	case GroupedExpr:
		return *left.groupedExpr.expr == *right.groupedExpr.expr;
	case TemplateExpr:
	{
		auto& lGenerics = left.templateExpr;
		auto& rGenerics = right.templateExpr;
		if (*lGenerics.expr != *rGenerics.expr) return false;
		if (lGenerics.templateArgs->size() != rGenerics.templateArgs->size()) return false;

		for (size_t i = 0; i < lGenerics.templateArgs->size(); i++)
		{
			if (*lGenerics.templateArgs->at(i) != *rGenerics.templateArgs->at(i)) return false;
		}
		return true;
	}
	case TypeExpr:
		return *left.typeExpr.type == *right.typeExpr.type;
	case FunctionTypeDeclExpr:
		return false;
	case CompileExpr:
		return false;
	default:
		break;
	}

	return false;
}

bool operator!=(const Expr& left, const Expr& right)
{
	return !(left == right);
}

StringViewHash inplaceStrHasher;
inline size_t HashType(const Type* type)
{
	switch (type->typeID)
	{
	case PrimitiveType:
		return type->primitiveType.type;
	case NamedType:
	{
		size_t hash = 0;
		auto& namedType = type->namedType;
		hash += inplaceStrHasher(namedType.typeName->val);
		return hash;
	}
	case ExplicitType:
	{
		size_t hash = 0;
		for (Stmnt* node : *type->explicitType.declarations)
		{
			Type* defType = node->definition.type;
			hash += HashType(defType);
		}
		return hash;
	}
	case PointerType:
	{
		size_t hash = '*';
		return hash + HashType(type->pointerType.type);
	}
	case ValueType:
	{
		size_t hash = '~';
		return hash + HashType(type->valueType.type);
	}
	case ArrayType:
	{
		size_t hash = '[' + ']';
		return hash + HashType(type->arrayType.type);
	}
	case TemplatedType:
	{
		size_t hash = 0;
		auto& genericType = type->templatedType;
		for (Expr* templ : *genericType.templates->templateExpr.templateArgs)
		{
			hash += HashExpr(templ);
		}
		return hash + HashType(genericType.type);
	}
	case FunctionType:
	{
		size_t hash = 0;
		auto& functionType = type->functionType;
		hash += HashType(functionType.returnType);
		for (Type* param : *functionType.paramTypes)
		{
			hash += HashType(param);
		}
		return hash;
	}
	case ImportedType:
	{
		size_t hash = 0;
		auto& importedType = type->importedType;
		hash += inplaceStrHasher(importedType.packageName->val);
		hash += inplaceStrHasher(importedType.typeName->val);
		return hash;
	}
	default:
		break;
	}

	Logger::FatalError("SymbolTable:TypeHash Unable to create hash for Type");
	return 0;
}

inline size_t HashExpr(const Expr* expr)
{
	switch (expr->typeID)
	{
	case InvalidExpr:
		return 0;
	case LiteralExpr:
		return inplaceStrHasher(expr->literalExpr.val->val);
	case IdentifierExpr:
		return inplaceStrHasher(expr->identifierExpr.identifier->val);
	case PrimitiveExpr:
		return inplaceStrHasher(expr->primitiveExpr.primitive->val);
	case SelectorExpr:
		return HashExpr(expr->selectorExpr.on) + HashExpr(expr->selectorExpr.select);
	case IndexExpr:
		return HashExpr(expr->indexExpr.of) + HashExpr(expr->indexExpr.index);
	case FunctionCallExpr:
	{
		size_t hash = 0;
		hash += HashExpr(expr->functionCallExpr.function);
		for (Expr* param : *expr->functionCallExpr.params)
		{
			hash += HashExpr(param);
		}
		return hash;
	}
	case NewExpr:
	{
		size_t hash = HashExpr(expr->newExpr.primaryExpr) + UniqueType::New;
		if (expr->newExpr.atExpr) hash += HashExpr(expr->newExpr.atExpr);
		return hash;
	}
	case FixedExpr:
		return HashExpr(expr->fixedExpr.atExpr) + UniqueType::Fixed;
	case AnonTypeExpr:
	{
		size_t hash = 0;
		for (Expr* param : *expr->anonTypeExpr.values)
		{
			hash += HashExpr(param);
		}
		return hash;
	}
	case ExplicitTypeExpr:
	{
		size_t hash = 0;
		for (Stmnt* param : *expr->explicitTypeExpr.values)
		{
			hash += HashType(param->definition.type);
			hash += inplaceStrHasher(param->definition.name->val);
			if (param->definition.assignment) hash += HashExpr(param->definition.assignment);
		}
		return hash;
	}
	case AsExpr:
		return HashExpr(expr->asExpr.of) + HashType(expr->asExpr.to) + UniqueType::As;
	case DereferenceExpr:
		return HashExpr(expr->dereferenceExpr.of) + UniqueType::Tilde;
	case ReferenceExpr:
		return HashExpr(expr->referenceExpr.of) + UniqueType::AtOp;
	case BinaryExpr:
		return HashExpr(expr->binaryExpr.left) + HashExpr(expr->binaryExpr.right) + expr->binaryExpr.opType;
	case UnaryExpr:
		return HashExpr(expr->unaryExpr.expr) + expr->unaryExpr.opType;
	case GroupedExpr:
		return HashExpr(expr->groupedExpr.expr) + UniqueType::Lparen + UniqueType::Rparen;
	case TemplateExpr:
	{
		size_t hash = 0;
		hash += HashExpr(expr->templateExpr.expr);
		for (Expr* templ : *expr->templateExpr.templateArgs) hash += HashExpr(templ);
		return hash;
	}
	case TypeExpr:
		return HashType(expr->typeExpr.type);
	case FunctionTypeDeclExpr:
		return 0;
	case CompileExpr:
		return 0;
	default:
		break;
	}

	Logger::FatalError("SymbolTable:TypeHash Unable to create hash for Type");
	return 0;
}

size_t TypeArrHash::operator()(const eastl::vector<Type*>* types) const
{
	size_t hash = 0;
	for (size_t i = 0; i < types->size(); i++)
	{
		Type* type = types->at(i);
		hash += HashType(type) + i;
	}
	return hash;

}

size_t ExprArrHash::operator()(const eastl::vector<Expr*>* exprs) const
{
	size_t hash = 0;
	for (size_t i = 0; i < exprs->size(); i++)
	{
		Expr* expr = exprs->at(i);
		hash += HashExpr(expr) + i;
	}
	return hash;
}

bool ExprArrEqual::operator()(const eastl::vector<Expr*>* l, const eastl::vector<Expr*>* r) const
{
	if (l->size() != r->size()) return false;
	for (size_t i = 0; i < l->size(); i++)
	{
		Expr* lexpr = l->at(i);
		Expr* rexpr = r->at(i);
		if (*lexpr != *rexpr) return false;
	}

	return true;
}


/*
	ToString Functions
*/
eastl::string ToString(Stmnt* node)
{
	switch (node->nodeID)
	{
	case InvalidStmnt:
		return "INVALID";
	case CommentStmnt:
		return node->start->ToString();
	case ExpressionStmnt:
		return ToString(node->expressionStmnt.expression);
	case ImportStmnt:
		return node->start->ToString() + " " +
			node->importStmnt.packageName->ToString() +
			(node->importStmnt.alias ? " as " + node->importStmnt.alias->ToString() : "");
	case Definition:
		return node->definition.name->ToString() + " : " +
			ToString(node->definition.type) +
			(node->definition.assignment != nullptr
				? " " + node->definition.op->ToString() + " " + ToString(node->definition.assignment) : "");
	case InlineDefinition:
		return ToString(node->inlineDefinition.type) +
			" " + node->inlineDefinition.op->ToString() + " " +
			ToString(node->inlineDefinition.assignment);
	case FunctionStmnt:
		return ToString(node->function.returnType) + " " +
			node->function.name->ToString() +
			(node->function.generics != nullptr ? ToString(node->function.generics) : "") +
			ToString(node->function.decl);
	case AnonFunction:
		return ToString(node->anonFunction.returnType) +
			ToString(node->anonFunction.decl);
	case Method:
		return ToString(node->method.returnType) + " " +
			node->method.stateName->ToString() + "::" +
			node->method.name->ToString() +
			(node->method.generics != nullptr ? ToString(node->method.generics) : "") +
			ToString(node->method.decl);
	case StateOperator:
		return ToString(node->stateOperator.returnType) + " " +
			node->stateOperator.stateName->ToString() + "::operator::" +
			node->stateOperator.op->ToString() +
			ToString(node->stateOperator.decl);
	case Destructor:
		return node->destructor.stateName->ToString() + "::" +
			node->destructor.del->ToString() +
			ToString(node->destructor.body);
	case Constructor:
		return node->destructor.stateName->ToString() + "::" +
			ToString(node->constructor.decl);
	case FunctionDecl:
	{
		eastl::string params = "(";
		if (node->functionDecl.parameters != nullptr)
		{
			for (Stmnt* param : *node->functionDecl.parameters)
			{
				params += ToString(param) + ", ";
			}
		}
		params += ")";
		return params + ToString(node->functionDecl.body);
	}
	case Conditional:
		return "(" + ToString(node->conditional.condition) + ")" +
			ToString(node->conditional.body);
	case AssignmentStmnt:
		return ToString(node->assignmentStmnt.assignTo) +
			" " + node->assignmentStmnt.op->ToString() + " " +
			ToString(node->assignmentStmnt.assignment);
	case IfStmnt:
	{
		eastl::string elseifs = "";
		for (Stmnt* elseif : *node->ifStmnt.elifs)
		{
			elseifs += "else if " + ToString(elseif);
		}

		return node->start->ToString() + " " +
			ToString(node->ifStmnt.condition) +
			elseifs +
			(node->ifStmnt.elseCondition ? "else " + ToString(node->ifStmnt.elseCondition) : "");
	}
	case ForStmnt:
		return node->start->ToString() + " (" +
			(node->forStmnt.isDeclaration
				? ToString(node->forStmnt.iterated.declaration)
				: node->forStmnt.iterated.identifier->ToString()) +
			" " + node->forStmnt.iterator->ToString() + " " +
			ToString(node->forStmnt.toIterate) + ")" +
			ToString(node->forStmnt.body);
	case WhileStmnt:
		return node->start->ToString() + " "
			+ ToString(node->whileStmnt.conditional);
	case SwitchStmnt:
	{
		eastl::string cases = "";
		for (Stmnt* node : *node->switchStmnt.cases)
		{
			cases += "case " + ToString(node);
		}

		return node->start->ToString() + " (" +
			ToString(node->switchStmnt.switchOn) + " )\n" +
			cases +
			"default" + ToString(node->switchStmnt.defaultCase) + "\n";
	}
	case DeleteStmnt:
		return node->start->ToString() +
			(node->deleteStmnt.arrDelete ? "[]" : "") + " " +
			ToString(node->deleteStmnt.primaryExpr);
	case DeferStmnt:
		return node->start->ToString() + " " +
			(node->deferStmnt.deferIf
				? ToString(node->deferStmnt.conditional)
				: ToString(node->deferStmnt.body));
	case ContinueStmnt:
		return node->continueStmnt.token->ToString();
	case BreakStmnt:
		return node->breakStmnt.token->ToString();
	case ReturnStmnt:
		return node->start->ToString() + " " + ToString(node->returnStmnt.expr);
	case WhereStmnt:
	{
		return node->start->ToString() + ToString(node->whereStmnt.decl);
	}
	case StateStmnt:
	{
		eastl::string insets = "";
		Flags<>* flags = node->state.insetFlags;
		if ((*flags)[SizeInset]) insets += "[size]\n";
		if ((*flags)[SOAInset]) insets += "[soa]\n";
		if ((*flags)[SerializedInset]) insets += "[serialized]\n";
		if ((*flags)[NoAlignInset]) insets += "[noalign]\n";

		eastl::string members = "";
		for (Stmnt* member : *node->state.members)
		{
			members += ToString(member) + ",\n";
		}

		return node->start->ToString() + " " +
			node->state.name->ToString() +
			(node->state.generics ? ToString(node->state.generics) : "") +
			"\n{\n" + insets + members + "}\n";
	}
	case GenericsDecl:
	{
		eastl::string names = "";
		for (Token* name : *node->generics.names)
		{
			names += name->ToString() + ", ";
		}

		return "<" + names +
			(node->generics.whereStmnt ? " : " + ToString(node->generics.whereStmnt) : "") + ">";
	}
	case CompileStmnt:
		return node->start->ToString() + " " +
			ToString(node->compileStmnt.returnType) + " " +
			ToString(node->compileStmnt.body);
	case CompileDebugStmnt:
		return node->start->ToString() + " " + ToString(node->compileDebugStmnt.body);
	case Block:
	{
		eastl::string stmnts = "";
		for (Stmnt* stmnt : *node->block.inner)
		{
			stmnts += ToString(stmnt) + "\n";
		}
		return "\n{\n" + stmnts + "}\n";
	}
	default:
		return "";
	}
}

eastl::string ToString(Body& body)
{
	if (!body) return "";
	if (body.statement) return " " + ToString(body.body) + "\n";
	else return ToString(body.body);
}

eastl::string ToString(Expr* expr)
{
	switch (expr->typeID)
	{
	case LiteralExpr:
		return expr->literalExpr.val->ToString();
	case IdentifierExpr:
		return expr->identifierExpr.identifier->ToString();
	case PrimitiveExpr:
		return expr->primitiveExpr.primitive->ToString();
	case SelectorExpr:
		return ToString(expr->selectorExpr.on) + "." + ToString(expr->selectorExpr.select);
	case IndexExpr:
		return ToString(expr->indexExpr.of) +
			expr->indexExpr.lBrack->ToString() +
			ToString(expr->indexExpr.index) +
			expr->indexExpr.rBrack->ToString();
		break;
	case FunctionCallExpr:
	{
		eastl::string params = "";
		if (expr->functionCallExpr.params != nullptr)
		{
			for (Expr* expr : *expr->functionCallExpr.params)
			{
				params += ToString(expr) + ", ";
			}
		}

		return ToString(expr->functionCallExpr.function) +
			expr->functionCallExpr.lParen->ToString() +
			params +
			expr->functionCallExpr.rParen->ToString();
	}
	break;
	case NewExpr:
		return expr->newExpr.newIndex->ToString() + " " +
			ToString(expr->newExpr.primaryExpr) +
			(expr->newExpr.atExpr != nullptr ? " at " + ToString(expr->newExpr.atExpr) : "");
	case FixedExpr:
		return expr->fixedExpr.fixed->ToString() + " " +
			ToString(expr->fixedExpr.atExpr);
	case AnonTypeExpr:
	{
		eastl::string values = "";
		for (Expr* expr : *expr->anonTypeExpr.values)
		{
			values += ToString(expr) + ", ";
		}

		return "{" + values + "}";
	}
	case ExplicitTypeExpr:
	{
		eastl::string values = "";
		for (Stmnt* stmnt : *expr->explicitTypeExpr.values)
		{
			values += ToString(stmnt) + ", ";
		}

		return "{" + values + "}";
	}
	case AsExpr:
		return ToString(expr->asExpr.of) + " as " +
			ToString(expr->asExpr.to);
	case DereferenceExpr:
		return ToString(expr->dereferenceExpr.of) +
			expr->dereferenceExpr.op->ToString();
	case ReferenceExpr:
		return ToString(expr->referenceExpr.of) +
			expr->referenceExpr.op->ToString();
	case BinaryExpr:
		return ToString(expr->binaryExpr.left) +
			expr->binaryExpr.op->ToString() +
			ToString(expr->binaryExpr.right);
	case UnaryExpr:
		return expr->unaryExpr.op->ToString() +
			ToString(expr->unaryExpr.expr);
	case GroupedExpr:
		return expr->groupedExpr.lParen->ToString() +
			ToString(expr->groupedExpr.expr) +
			expr->groupedExpr.rParen->ToString();
	case TemplateExpr:
	{
		eastl::string templateArgs = "";

		for (Expr* templ : *expr->templateExpr.templateArgs)
		{
			templateArgs += ToString(templ) + ", ";
		}

		return (expr->templateExpr.expr != nullptr ? ToString(expr->templateExpr.expr) : "") +
			expr->templateExpr.open->ToString() +
			templateArgs +
			expr->templateExpr.close->ToString();
	}
	case TypeExpr:
		return ToString(expr->typeExpr.type);
	case FunctionTypeDeclExpr:
		return ToString(expr->functionTypeDeclExpr.anonFunction);
	case CompileExpr:
		return ToString(expr->compileExpr.compile);
	default:
		return "";
	}
}

eastl::string PrimitiveToString(UniqueType type)
{
	switch (type)
	{
	case UniqueType::Void:
		return "void";
	case UniqueType::Bool:
		return "bool";
	case UniqueType::Byte:
		return "byte";
	case UniqueType::Ubyte:
		return "ubyte";
	case UniqueType::Int:
		return "int";
	case UniqueType::Int16:
		return "int16";
	case UniqueType::Int32:
		return "int32";
	case UniqueType::Int64:
		return "int64";
	case UniqueType::Int128:
		return "int128";
	case UniqueType::Uint:
		return "uint";
	case UniqueType::Uint16:
		return "uint16";
	case UniqueType::Uint32:
		return "uint32";
	case UniqueType::Uint64:
		return "uint64";
	case UniqueType::Uint128:
		return "uint128";
	case UniqueType::Float:
		return "float";
	case UniqueType::Float32:
		return "float32";
	case UniqueType::Float64:
		return "float64";
	case UniqueType::String:
		return "string";
	default:
		break;
	}

	return "INVALID";
}

eastl::string ToString(Type* type)
{
	switch (type->typeID)
	{
	case UnknownType:
		return "implicit";
	case PrimitiveType:
		return PrimitiveToString(type->primitiveType.type);
	case NamedType:
		return type->namedType.typeName->ToString();
	case ExplicitType:
	{
		eastl::string types = "";

		for (Stmnt* node : *type->explicitType.declarations)
		{
			types += ToString(node) + ", ";
		}

		return "{ " + types + "}";
	}
	case ImplicitType:
	{
		eastl::string types = "";

		for (Token* index : *type->implicitType.identifiers)
		{
			types += index->ToString() + ", ";
		}

		return "{ " + types + "}";
	}
	case PointerType:
		return "*" + ToString(type->pointerType.type);
	case ValueType:
		return "~" + ToString(type->valueType.type);
	case ArrayType:
		return "[]" + ToString(type->arrayType.type);
	case FixedArrayType:
		return "[" + eastl::to_string(type->fixedArrayType.size) + "]" + ToString(type->fixedArrayType.type);
	case TemplatedType:
		return ToString(type->templatedType.type) + ToString(type->templatedType.templates);
	case FunctionType:
	{
		eastl::string params = "";

		for (Type* type : *type->functionType.paramTypes)
		{
			params += ToString(type) + ", ";
		}

		return "::" + ToString(type->functionType.returnType) + "(" + params + ")";
	}
	case ImportedType:
		return type->importedType.packageName->ToString() +
			"." +
			type->importedType.typeName->ToString();
	default:
		return "";
	}
}