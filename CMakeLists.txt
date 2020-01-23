# adapted a bit from http://llvm.org/docs/CMake.html#embedding-llvm-in-your-project

cmake_minimum_required(VERSION 3.7)
project(MCInfo)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(mc-info mc-info.cpp CodeRegionGenerator.cpp CodeRegion.cpp PipelinePrinter.cpp SummaryView.cpp View.cpp InstructionInfoView.cpp DispatchStatistics.cpp BottleneckAnalysis.cpp)

set_property(TARGET mc-info PROPERTY CXX_STANDARD 17)

llvm_map_components_to_libnames(llvm_libs analysis asmparser asmprinter bitreader bitwriter codegen core coroutines debuginfodwarf executionengine instrumentation interpreter ipo irreader linker mc mcjit objcarcopts option profiledata scalaropts support target X86AsmParser X86CodeGen MCA)

target_link_libraries(mc-info ${llvm_libs})
