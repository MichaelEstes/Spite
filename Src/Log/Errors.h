#pragma once

#include "EASTL/string.h"

struct Errors {
	const eastl::string missingSemicolon = "Expected a semicolon";

	const eastl::string missingPackage = "File must start with a 'package' statement";
	const eastl::string missingPackageName = "Expected an identifier after package token";
	const eastl::string multiplePackages = "File cannot have multiple 'package' statements";

	const eastl::string usingOutsideOfGlobal = "Cannot import packages outside of the global scope";
	const eastl::string missingUsingName = "Expected an identifier after 'using' token";
	const eastl::string expectedUsingAlias = "Expected an alias identifier after 'as' in using statement";

	const eastl::string identifierExpected = "Expected an identifier, possible compiler bug";
	const eastl::string invalidTokenAfterIdentifier = "Unexpected token after identifier";
	const eastl::string expectedDefinition = "Expected a variable definition";

	const eastl::string expectedColon = "Expected a colon (':') after identifier in non-implicit assignment";
	const eastl::string expectedAssignment = "Variable declerations must have assignments";
	const eastl::string expectedType = "Expected type decleration";

	const eastl::string unclosedGroupExpression = "Expected ')' after group expression";
	const eastl::string unclosedFunctionCall = "Missing ')' at end of function call";
	const eastl::string missingOperand = "No operand found for expression";
	const eastl::string expectedBinaryOperator = "Expected binary operator, possible compiler bug";

	const eastl::string invalidSelector = "Expect an identifier after '.'";
	const eastl::string emptyIndex = "Unexpected empty index ('[]') in expression";
	const eastl::string unclosedIndex = "Expected ']' after index expression";

	const eastl::string emptyAnonymousType = "Found empty anonymous type";

	const eastl::string expectedImportedType = "Expected identifier after selector ('.') in declaration type";
	const eastl::string expectedFunctionName = "Expected function name identifier";
	const eastl::string expectedFunction = "Expected function starting with ('(')";
	const eastl::string expectedFunctionClosure = "Expected function closure (')')";
	const eastl::string expectedBlockStart = "Expected function block opening ('{') or ('=>')";
	const eastl::string expectedBlockEnd = "Expected function block closure ('}')";

	const eastl::string emptyGenerics = "Expected generic types not '<>'";
	const eastl::string expectedGenericsClosure = "Expected generic declaration closure ('>')";

};
