;==============================================================================
;	ACME - Basicstart-Template ;-)  by St0fF/Neoplasia
;==============================================================================
 	* = $0801
	!byte <.basend,>.basend,<year,>year,$9e
	!byte (.run/1000)+48,((.run/100)%10)+48,((.run/10)%10)+48,.run%10+48
	!byte ":",$8f	;REM
	!fill 11,20
	+der_text
	!byte 0
.basend !byte 0,0
.run
 
;YEAR SYS.run:REM~~~~~~~~~~~der_text
