@echo off
if not exist obj mkdir obj
ghc --make Main.hs -odir obj -hidir obj && main.exe
