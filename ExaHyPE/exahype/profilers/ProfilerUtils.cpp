#include "ProfilerUtils.h"

namespace exahype {
namespace profilers {
namespace utils {

void escape(void* p) { asm volatile("" : : "g"(p) : "memory"); }

void clobber() { asm volatile("" : : : "memory"); }

void noop() {}

}  // namespace utils
}  // namespace profilers
}  // namespace exahype
