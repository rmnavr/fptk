
# Generator of fptk local

This script creates `_fptk_local.hy` by uniting files from `/src` into one single file.
Generated files are placed to `/_etc/local_versions/`

This is needed for using `_fptk_local.hy` in other projects to have stable fptk inside them.
This is until fptk will reach stable release.

Be aware, that src files content undergoes some sanitation in order to produce working merged file:
1. all `(export ...)` statements are removed
   > dev note: 
   > 
   > do not use `)` symbols inside export statement (like having `; smth (smth)`
   > inside multiline export statement) — it will mess with regex that finds export statements
2. all `(require fptk._macros ...)` statements are removed
3. `hy.R.fptk.fm` (and alike) calls are replaced with `fm` calls.
   It means that you should:
   - always import `fm` if you plan to use `f>`, `(l)mapm` or `(l)filterm`
   - always import `lns` if you plan to use `&+`, `&+>`, `l>` or `l>=`

