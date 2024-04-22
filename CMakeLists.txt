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

# Add source to this project's executable.
add_executable (Spite_Lang 
"Spite_Lang.cpp" "Spite_Lang.h" "Src/Parsing/Parser.h" "Src/Parsing/Parser.cpp"
"Src/Log/Logger.h" "Src/Log/Logger.cpp" "Src/Parsing/Scanner.h" "Src/Parsing/Position.h" "Src/Intermediate/Syntax.h"
"Src/Intermediate/SymbolTable.h" "Src/Utils/Profiler.h" "Src/Tokens/TokenTree.h" "Src/Containers/Flags.h"
"Src/Tokens/TokenTree.h" "Src/Utils/Utils.h" "Src/Containers/InplaceString.h" 
"Src/Containers/Arena.h" "Src/Intermediate/Type.h" "Src/Intermediate/Expr.h" "Src/Intermediate/Node.h" 
"Src/Config/BuildConfig.h"  "Src/Config/Config.h" "Src/Containers/Table.h" "Src/Output/LLVM/LLVMBuilder.h" 
"Src/Checking/Checker.h" "Src/Containers/TupleLookup.h" "Src/Tokens/Token.h" "Src/Checking/TypeChecker.h" "Src/Checking/ExprChecker.h")

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET Spite_Lang PROPERTY CXX_STANDARD 20)
endif()

if(MSVC)
	set(EASTL_LIBRARY debug ${EASTL_ROOT_DIR}/debug/lib/EASTL.lib optimized ${EASTL_ROOT_DIR}/lib/EASTL.lib)
	add_custom_target(NatVis SOURCES ${EASTL_ROOT_DIR}/doc/EASTL.natvis)
else()
	set(CMAKE_BUILD_TYPE "Release")
	#set(CMAKE_CXX_FLAGS "-Wall -Wextra")
	set(CMAKE_CXX_FLAGS_DEBUG "-g")
	set(CMAKE_CXX_FLAGS_RELEASE "-O2")

	set(EASTL_LIBRARY debug ${EASTL_ROOT_DIR}/lib/libEASTL.a optimized ${EASTL_ROOT_DIR}/lib/libEASTL.a)
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/out/build)
endif()

llvm_map_components_to_libnames(llvm_libs support core irreader)

target_link_libraries(Spite_Lang ${EASTL_LIBRARY} ${llvm_libs})
# TODO: Add tests and install targets if needed.
