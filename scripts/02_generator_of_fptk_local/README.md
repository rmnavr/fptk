
# Packaging helper

This script is supposed to be executed before every commit (or at least large commit, or push to github).

What this utility script does:
1. Runs tests and prints their results
2. Runs `doc_generator/doc_generator.hy` (that generates doc-table for fptk funcs/imports/modules)
3. Creates `_fptk_local.hy` by uniting `fptk/fptk_funcs.hy` and `fptk/fptk_macros.hy`. It also attaches version info.
   > This is needed for using `_fptk_local.hy` in other projects to have stable fptk inside them.
   > This is until fptk will reach stable release.
4. Prompts fptk version as given in setup.py
