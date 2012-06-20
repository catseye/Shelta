@echo off
REM BOOTSTRP.BAT
REM v1.1.2009.0307 (c)1999-2009 Chris Pressey, Cat's-Eye Technologies.
REM Builds the bootstrapped versions (S & S2) of the Shelta compiler.
@echo on
call bin\shelta n eg\sheltas
copy eg\sheltas.com bin\sheltas.com
call bin\shelta s eg\sheltas
copy eg\sheltas.com bin\sheltas2.com
call bin\shelta s2 eg\sheltas
del eg\sheltas.com
