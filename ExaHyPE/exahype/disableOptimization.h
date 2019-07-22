/**
 * Compiler Workaround: Disable optimization in the current processing unit.
 * Do so by just including this file.
 *
 * 2017-05-04, Sven.
 **/

#ifdef __GNUC__ /* GCC */
#pragma GCC optimize ("O0")
#endif

#ifdef __INTEL_COMPILER
#pragma optimize("", off) 
#endif

// for other compilers (such as Clang/LLVM), we dont have pragmas. Sorry.

/*
 * If you want to turn off optimizations for certain functions, use in GCC
 *   __attribute__((optimize("O0")))
 * and in Intel compiler, but the pragma directly in front of the function.
 *
 * If you want to use the GCC pragma locally, enclose the scope with
 * #pragma GCC push_options
 * and #pragma GCC pop_options
 *
 */



