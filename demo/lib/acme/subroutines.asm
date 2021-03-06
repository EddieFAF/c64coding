; ======================================================
; = S U B R O U T I N E S 
; ======================================================

; Variable Delay
delay:            ;delay 84-accu cycles, 0<=accu<=65
  lsr             ;2 cycles akku=akku/2 carry=1 if accu was odd, 0 otherwise
  bcc waste1cycle ;2/3 cycles, depending on lowest bit, same operation for both
waste1cycle:
  sta smod+1      ;4 cycles selfmodifies the argument of branch
  clc             ;2 cycles 
;now we have burned 10/11 cycles.. and jumping into a nopfield 
smod:
  bcc *+10        ;3 cycles
  !fill 32, $ea   ;just type 32x nop if your assembler doesnt support this command
  rts             ;6 cycles
