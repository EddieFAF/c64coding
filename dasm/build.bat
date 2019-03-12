
dasm tst_unp.s -v3 -otest_unp.prg -ltst_unp.lst
pause
del image.d64
makedisk image.d64 diskfile
pause

