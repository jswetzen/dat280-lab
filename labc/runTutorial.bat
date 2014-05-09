
REM use -s for stats or -l for an eventlog
ghc -O2 -threaded -rtsopts -eventlog tutorial.hs
tutorial.exe -o test.html +RTS -N4 %1 > output

