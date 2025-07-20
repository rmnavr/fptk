
# Packaging helper

What this utility script does:
1. Runs `doc_generator/doc_generator.hy` (that generates table of fptk funcs/imports/modules)
2. Creates `_fptk_local.hy` by uniting `fptk/fpext.hy` and `fptk/macros.hy`.
   It also attaches version/date info.
   > This is needed for using `_fptk_local.hy` in other projects to have stable fptk inside them.
   > This is until fptk will reach stable release.
3. Prompts latest local git tag and fptk version as given in setup.py
