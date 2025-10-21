

# 20 Oct 2025 (in preparation for 0.4.0):
- changed order of args for oflenQ: from `oflenQ(xs,n)` to `oflenQ(n,xs)`
- removed `with_execution_time`, replaced with simplier `timing`
- removed `curry`, `rcurry` and `autocurry` (because `partial` is enough for fptk needs)
- added `resultM` module
- renamed `write_file` to `write_to_file`
- fptk source is now splitted into several files (tests/scripts upd accrodingly)
- completely removed auto full import of all modules (like re, os, etc.)

# 11 Oct 2025
- refactored to submodules

# 30 Jul 2025
- added many iterator funcs and also tests for them

# 28 Jul 2025
- lmulticut_by: full functionality and test
- upd: enlengthen
- removed funcs on strings (ends_with, subsitute or smth)
  that can be replaced with regex funcs with similar functionality
