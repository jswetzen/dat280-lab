REM use -s for stats or -l for an eventlog
ghc -O2 -threaded -rtsopts -eventlog LabA.hs
LabA.exe -o test.html +RTS -N4 -A50m %1 > output

