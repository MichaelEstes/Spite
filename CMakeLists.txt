# CMakeList.txt : CMake project for Spite_Lang, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)
include(ExternalProject)

project ("Spite_Lang")

set(EASTL_ROOT_DIR ${CMAKE_BINARY_DIR}/EASTL)
set(EASTL_BUILD_DIR ${EASTL_ROOT_DIR}/build)
set(EASTL_LIB_DIR ${EASTL_BUILD_DIR}/lib)
set(EASTL_INCLUDE_DIR ${EASTL_BUILD_DIR}/include)
ExternalProject_Add(EASTL
    PREFIX ${CMAKE_BINARY_DIR}/EASTL
    GIT_REPOSITORY https://github.com/electronicarts/EASTL.git
    GIT_TAG master
	CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/EASTL/build -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
	BUILD_BYPRODUCTS ${EASTL_LIB_DIR}/libEASTL.a ${EASTL_LIB_DIR}/EASTL.lib ${EASTL_BUILD_DIR}/doc/EASTL.natvis
)
include_directories(${EASTL_INCLUDE_DIR})
link_directories(${EASTL_LIB_DIR})

set(DYNCALL_ROOT_DIR ${CMAKE_BINARY_DIR}/dyncall)
set(DYNCALL_BUILD_DIR ${DYNCALL_ROOT_DIR}/build)
set(DYNCALL_LIB_DIR ${DYNCALL_BUILD_DIR}/lib)
set(DYNCALL_INCLUDE_DIR ${DYNCALL_BUILD_DIR}/include)
ExternalProject_Add(dyncall
    PREFIX ${CMAKE_BINARY_DIR}/dyncall
    URL https://dyncall.org/r1.4/dyncall-1.4.zip
	CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/dyncall/build -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
)
include_directories(${DYNCALL_INCLUDE_DIR})
link_directories(${DYNCALL_LIB_DIR})

find_package(LLVM 17 REQUIRED CONFIG)
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

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	file(COPY "${PROJECT_SOURCE_DIR}/Compile/Debug" DESTINATION "${CMAKE_BINARY_DIR}/Runtime/Compile")
else()
	file(COPY "${PROJECT_SOURCE_DIR}/Compile/Release" DESTINATION "${CMAKE_BINARY_DIR}/Runtime/Compile")
endif()

# Add source to this project's executable.
add_executable (Spite_Lang 
"Src/Spite_Lang.cpp" "Src/Spite_Lang.h" "Src/Parsing/Parser.h" "Src/Parsing/Parser.cpp"
"Src/Log/Logger.h" "Src/Log/Logger.cpp" "Src/Parsing/Scanner.h" "Src/Parsing/Position.h" 
"Src/Syntax/Syntax.h" "Src/Syntax/SymbolTable.h" "Src/Utils/Profiler.h" "Src/Tokens/TokenTree.h" 
"Src/Containers/Flags.h" "Src/Tokens/TokenTree.h" "Src/Utils/Utils.h" "Src/Containers/StringView.h" 
"Src/Containers/Arena.h" "Src/Syntax/Type.h" "Src/Syntax/Expr.h" "Src/Syntax/Stmnt.h" 
"Src/Config/BuildConfig.h" "Src/Config/Config.h" "Src/Output/LLVM/LLVMBuilder.h" 
"Src/Checking/PackageChecker.h"  "Src/Tokens/Token.h" "Src/Checking/TypeChecker.h" 
"Src/Checking/ExprChecker.h" "Src/Checking/CheckerUtils.h" "Src/Syntax/GlobalTable.h" 
"Src/Checking/Checker.h" "Src/Syntax/SyntaxUtils.h" "Src/Syntax/SyntaxUtils.cpp" "Src/IR/IR.h" 
"Src/Lower/Lower.h" "Src/Lower/LowerDeclarations.h" "Src/Lower/LowerUtils.h" "Src/Checking/DeferredChecker.h" 
"Src/Checking/CheckerContext.h" "Src/Lower/LowerDefinitions.h"  
"Src/Lower/LowerContext.h" "Src/IR/Interpreter/Interpreter.h" "Src/IR/Interpreter/Decompiler.h" 
"Src/Syntax/TypeInference.h" "Src/Syntax/ScopeUtils.h" "Src/IR/Interpreter/ExternCall.h" 
"Src/Output/LLVM/LLVMTypes.h" "Src/Output/LLVM/LLVMEntry.h" "Src/Output/LLVM/LLVMContext.h" 
"Src/Output/LLVM/LLVMCompile.h" "Src/Output/LLVM/LLVMOptimize.h")

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET Spite_Lang PROPERTY CXX_STANDARD 20)
endif()

if(MSVC)
	set(EASTL_LIBRARY ${EASTL_LIB_DIR}/EASTL.lib)
	add_custom_target(NatVis SOURCES ${EASTL_BUILD_DIR}/doc/EASTL.natvis)
else()
	#set(CMAKE_CXX_FLAGS "-Wall -Wextra")
	set(CMAKE_CXX_FLAGS_DEBUG "-g")
	set(CMAKE_CXX_FLAGS_RELEASE "-O2")

	set(EASTL_LIBRARY ${EASTL_LIB_DIR}/libEASTL.a)
endif()

llvm_map_components_to_libnames(llvm_libs ${LLVM_TARGETS_TO_BUILD} target support core irreader codegen mc mcparser)

target_link_libraries(Spite_Lang ${EASTL_LIBRARY} ${llvm_libs} dyncall_s dynload_s dyncallback_s)
add_dependencies(Spite_Lang dyncall)
add_dependencies(Spite_Lang EASTL)

