; zeropage
loader_zp_first = $e0
loadaddrlo      = $e0
loadaddrhi      = $e1
decdestlo       = $e2
decdesthi       = $e3
endaddrlo       = $e4
endaddrhi       = $e5
loader_zp_last  = $f0

; resident
loadraw         = $0b00
loadcompd       = $0b0b
loadedtab       = $0d3d
decrunch        = $0d5d

; install
install         = $2fd8
