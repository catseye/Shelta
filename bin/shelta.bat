@echo off
REM SHELTA.BAT
REM v1.2-2013.1130 (c)1999-2013 Chris Pressey, Cat's-Eye Technologies.
REM A 'make'-like utility for Shelta compilers, as an MS-DOS batchfile.

REM -- Change the following lines to tailor what libraries are
REM -- included by default.  See readme.txt
type lib\8086\8086.she >s.she
type lib\8086\gupi.she >>s.she
type lib\8086\dos.she >>s.she
type lib\8086\string.she >>s.she
type lib\gupi\linklist.she >>s.she

REM -- This section builds the source file, always called 's.she'.
if not exist %2.she echo Can't find project file %2.she!
if exist %3.she type %3.she >>s.she
if exist %4.she type %4.she >>s.she
if exist %5.she type %5.she >>s.she
if exist %6.she type %6.she >>s.she
if exist %7.she type %7.she >>s.she
if exist %8.she type %8.she >>s.she
if exist %9.she type %9.she >>s.she
if exist %2.she type %2.she >>s.she
type bin\vtab.txt >>s.she

rem bin\shelta%1.com <s.she
bin\shelta%1.com <s.she >%2.com

if errorlevel 32 echo Source file could not be opened.
if errorlevel 16 echo Error - Unknown identifier in source file.
del s.she
