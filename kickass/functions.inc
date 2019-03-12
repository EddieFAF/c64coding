.function _16bit_nextArgument(arg) { 
  .if (arg.getType()==AT_IMMEDIATE) .return CmdArgument(arg.getType(),>arg.getValue()) 
  .return CmdArgument(arg.getType(),arg.getValue()+1)
}

.pseudocommand mov16 src:tar { 
  lda src
  sta tar 
  lda _16bit_nextArgument(src) 
  sta _16bit_nextArgument(tar)
} 

.pseudocommand irqEnd d12:irq {
      lda d12
      sta $d012
      :mov16 irq:$0314
}
