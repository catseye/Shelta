@echo off
REM SHELTA.BAT v2002.1208 (c)2002 Cat's-Eye Technologies.
REM A 'make'-like utility for Shelta compilers, as an MS-DOS batch.

REM -- Change the following lines to tailor what libraries are
REM -- included by default.  See readme.txt
type lib\8086\8086.she >s
type lib\8086\gupi.she >>s
type lib\8086\dos.she >>s
type lib\8086\string.she >>s
type lib\gupi\linklist.she >>s

REM -- This section builds the source file, always called 'S'.
if not exist %2.she echo Can't find project file %2.she!
if exist %3.she type %3.she >>s
if exist %4.she type %4.she >>s
if exist %5.she type %5.she >>s
if exist %6.she type %6.she >>s
if exist %7.she type %7.she >>s
if exist %8.she type %8.she >>s
if exist %9.she type %9.she >>s
if exist %2.she type %2.she >>s
type null.txt >>s

bin\shelta%1.com <s > %2.com

if errorlevel 32 echo Source file could not be opened.
if errorlevel 16 echo Error - Unknown identifier in source file.
del s
