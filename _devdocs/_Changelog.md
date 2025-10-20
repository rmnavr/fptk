↑ newer

In preparation for 0.4.0:
- changed order of args for oflenQ: from `oflenQ(xs,n)` to `oflenQ(n,xs)`
- removed `with_execution_time`, replaced with simplier `timing`
- removed `curry`, `rcurry` and `autocurry` (because `partial` is enough for fptk needs)
- added `resultM` module
- renamed `write_file` to `write_to_file`
- fptk source is now splitted into several files (tests/scripts upd accrodingly)
- completely removed auto full import of all modules (like re, os, etc.)

↓ older
