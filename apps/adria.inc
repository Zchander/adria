;---------------
; Support file for Adria
;
; Will contain Adria specific ROM entry points, definitions,
; routines, etc...
;---------------
; ROM_ROUTINES:
;---------------
smon816		= $D800			; assume (always!) cold start

rom_char_out	= $FD0C			; fixed routine in ROM
rom_char_scan	= $FD14			; fxied routine is ROM

;
; I/O routines (overrides)
char_out:
	jmp	rom_char_out		; This should be easy (enough)
	rts				; Shouldn't get here...

char_scan:
	jsr	rom_char_scan		; This is just the other way around
					; This will return with carry clear and .A
					; when a datum had been received....
	bcc	@overrideCarry

	clc
	rts

@overrideCarry:
	sec
	rts

;
; To have (at least) a placeholder, we just return right now
a_LOAD:
a_SAVE:
	rts	
