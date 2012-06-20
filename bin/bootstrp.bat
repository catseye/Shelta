@echo off
REM BOOTSTRP.BAT v2002.1208 (c)1999 Chris Pressey, Cat's-Eye Technologies.
REM Builds the bootstrapped versions (S & S2) of the Shelta compiler.
@echo on
call bin\shelta 86 prj\sheltas
copy prj\sheltas.com bin\sheltas.com
call bin\shelta s prj\sheltas
copy prj\sheltas.com bin\sheltas2.com
call bin\shelta s2 prj\sheltas
diff prj\sheltas.com bin\sheltas2.com
del prj\sheltas.com