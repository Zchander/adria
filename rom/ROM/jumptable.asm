;
; Adria ROM Jump table
;
; Version history:
;	23 feb 2021	- Initial version
;
; This file contains the a static jump table. It should grow "down", so new
; entries should be added to the top of the file.
; It main purpose is to have a "fixed"/known entrypoint for general purpose 
; routines.
;
; The jump table will be (at start) max 127 bytes! and will only support
; "Local bank" jumps?
;=======================================================================
	.res	JUMP_END - *, $00
j_mon:
		.word	mon
j_bintoasc:
	.word	binasc
j_asctobin:
	.word	ascbin
j_nibtobin:
	.word	nybtobin
j_mul_8_8:
	.word	mul8_8_8
j_mul_8_16:
	.word	mul8_8_16
j_div_16_8:
	.word	div16_8_8
j_div_8_8:
	.word	div8_8_8
j_newline:
	.word	newline
j_dpyhex:
	.word	dpyhex
j_dpyhexw:
	.word	dpyhexw
j_sprint:
	.word	sprint
j_loadintel:
	.word	strtintel
;---------------------------------------
; Used to calculate memory footprint of the module
;---------------------------------------
JUMP_END	= $FCFF			; Must be declared before any
					; calculations
JUMP_SIZE	= JUMP_END - JUMP
	.out .concat("Jump table                        $", .sprintf("%04x", JUMP), "      $", .sprintf("%04x", JUMP_END), "    $", .sprintf("%04x", JUMP_SIZE), "  (", .sprintf("%05d", JUMP_SIZE), ")")
