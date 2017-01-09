@echo off
setlocal
for %%s in (true false) do call :workers %%s
goto :EOF

:workers
for %%w in (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) do call :rbsize %1 %%w
goto :EOF

:rbsize
for %%r in (2 10 50 100 1000) do call :trials %1 %%w %%r
goto :EOF

:trials
for %%i in (1 2 3 4 5) do call :trial %1 %2 %3
goto :EOF

:trial
@echo on
cmd /c timeit part2_parallel.exe jlmsuwbz 16 2016 %2 %3 %1
@echo off
goto :EOF
