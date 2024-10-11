# CMakeList.txt : CMake project for Spite_Lang, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)
project ("Spite_Lang")

set(EASTL_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../EASTL)
include_directories (${EASTL_ROOT_DIR}/include)
include_directories (${EASTL_ROOT_DIR}/test/packages/EAAssert/include)
include_directories (${EASTL_ROOT_DIR}/test/packages/EABase/include/Common)
include_directories (${EASTL_ROOT_DIR}/test/packages/EAMain/include)
include_directories (${EASTL_ROOT_DIR}/test/packages/EAStdC/include)
include_directories (${EASTL_ROOT_DIR}/test/packages/EATest/include)
include_directories (${EASTL_ROOT_DIR}/test/packages/EAThread/include)

set(DYNCALL_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../dyncall)
set(DYNCALL_LIB_DIR ${DYNCALL_ROOT_DIR}/build/dyncall)
set(DYNCALLBACK_LIB_DIR ${DYNCALL_ROOT_DIR}/build/dyncallback)
set(DYNLOAD_LIB_DIR ${DYNCALL_ROOT_DIR}/build/dynload)
include_directories (${DYNCALL_ROOT_DIR})

find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Including LLVM Dirs: ${LLVM_INCLUDE_DIRS}")

# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

set(RUNTIME_PATH "${PROJECT_SOURCE_DIR}/Runtime/")
file(REMOVE_RECURSE "${CMAKE_BINARY_DIR}/Runtime")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/Runtime")
file(COPY ${RUNTIME_PATH} DESTINATION "${CMAKE_BINARY_DIR}/Runtime")

# Add source to this project's executable.
add_executable (Spite_Lang 
"Src/Spite_Lang.cpp" "Src/Spite_Lang.h" "Src/Parsing/Parser.h" "Src/Parsing/Parser.cpp"
"Src/Log/Logger.h" "Src/Log/Logger.cpp" "Src/Parsing/Scanner.h" "Src/Parsing/Position.h" 
"Src/Syntax/Syntax.h" "Src/Syntax/SymbolTable.h" "Src/Utils/Profiler.h" "Src/Tokens/TokenTree.h" 
"Src/Containers/Flags.h" "Src/Tokens/TokenTree.h" "Src/Utils/Utils.h" "Src/Containers/StringView.h" 
"Src/Containers/Arena.h" "Src/Syntax/Type.h" "Src/Syntax/Expr.h" "Src/Syntax/Stmnt.h" 
"Src/Config/BuildConfig.h"  "Src/Config/Config.h" "Src/Containers/Table.h" "Src/Output/LLVM/LLVMBuilder.h" 
"Src/Checking/PackageChecker.h" "Src/Containers/TupleLookup.h" "Src/Tokens/Token.h" "Src/Checking/TypeChecker.h" 
"Src/Checking/ExprChecker.h" "Src/Checking/CheckerUtils.h"  "Src/Syntax/GlobalTable.h" 
"Src/Checking/Checker.h" "Src/Syntax/SyntaxUtils.h" "Src/Syntax/SyntaxUtils.cpp" "Src/IR/IR.h" 
"Src/Lower/Lower.h" "Src/Lower/LowerDeclarations.h" "Src/Lower/LowerUtils.h" "Src/Checking/DeferredChecker.h" 
"Src/Checking/CheckerContext.h" "Src/Lower/LowerDefinitions.h" "Src/Containers/Array.h" 
"Src/Lower/LowerContext.h" "Src/IR/Interpreter/Interpreter.h" "Src/IR/Interpreter/Decompiler.h" 
"Src/Syntax/TypeInference.h" "Src/Syntax/ScopeUtils.h" "Src/IR/Interpreter/ExternCall.h")

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET Spite_Lang PROPERTY CXX_STANDARD 20)
endif()

if(MSVC)
	set(EASTL_LIBRARY debug ${EASTL_ROOT_DIR}/debug/lib/EASTL.lib optimized ${EASTL_ROOT_DIR}/lib/EASTL.lib)
	add_custom_target(NatVis SOURCES ${EASTL_ROOT_DIR}/doc/EASTL.natvis)
	
	set(DYNCALL_LIBRARY debug ${DYNCALL_LIB_DIR}/Debug/dyncall_s.lib optimized ${DYNCALL_LIB_DIR}/Release/dyncall_s.lib)
	set(DYNCALLBACK_LIBRARY debug ${DYNCALLBACK_LIB_DIR}/Debug/dyncallback_s.lib optimized ${DYNCALLBACK_LIB_DIR}/Release/dyncallback_s.lib)
	set(DYNLOAD_LIBRARY debug ${DYNLOAD_LIB_DIR}/Debug/dynload_s.lib optimized ${DYNLOAD_LIB_DIR}/Release/dynload_s.lib)
else()
	set(CMAKE_BUILD_TYPE "Release")
	#set(CMAKE_CXX_FLAGS "-Wall -Wextra")
	set(CMAKE_CXX_FLAGS_DEBUG "-g")
	set(CMAKE_CXX_FLAGS_RELEASE "-O2")

	set(EASTL_LIBRARY debug ${EASTL_ROOT_DIR}/lib/libEASTL.a optimized ${EASTL_ROOT_DIR}/lib/libEASTL.a)
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/out/build)
endif()

llvm_map_components_to_libnames(llvm_libs support core irreader)

target_link_libraries(Spite_Lang ${EASTL_LIBRARY} ${llvm_libs} 
${DYNCALL_LIBRARY} ${DYNCALLBACK_LIBRARY} ${DYNLOAD_LIBRARY})