;
; Adria reset routines
;
; Version history:
;	05 Jan 2021	- Initial version
;	18 feb 2021	- Added seperate IRQ handlers for emulation and
;			  native operation
;=======================================================================
; Local equates, macros, definitions
;---------------------------------------
	.org	RESET_ROUTINES		; Base address of this module
	
;---------------------------------------
; Fixed/known jump table(s)
;---------------------------------------
; Native/24-bit address support
;###################
L_char_out:
	jsr	char_out
	rtl
	
L_char_in:
	jsr	char_in
	rtl
	
L_char_scan:
	jsr	char_scan
	rtl
	
;###################
; Emulation/16-bit address support
;###################
char_out:
	jmp	(VECTOR_CHAR_OUT)
	rts
	
char_in:
	jmp	(VECTOR_CHAR_IN)
	rts
	
char_scan:
	jmp	(VECTOR_CHAR_SCAN)
	rts
	
;---------------------------------------
; Reset handler
;---------------------------------------
reset:					; We always start in 8-bit mode
	sei				; Disable interrupts
	cld				; Clear flags: - decimal
	clc				;              - carry
	xce				; but we want to go to native...
					; ... as soon as possible
					
	longr				; Set all registers to 16-bit
	
	lda	#stack			; Load base stack address ...
	tcs				; ... and copy .C as stack base
	
;###################
; Each vector has a 2-byte checksum. We are going to check this and
; (re)calculate it here
;###################
@set_vector_reset:
	lda	VECTOR_RES		; On reset, perform code at
	eor	#$DEAD			; label. If checksum is ok,
	cmp	CHKSUM_RES		; jump indirect to address.
	beq	@set_vector_nmi		;
	lda	#JMON			; Else jump into monitor
	sta	VECTOR_RES
	eor	#$DEAD
	sta	CHKSUM_RES
	
@set_vector_nmi:
	lda	VECTOR_NMI
	eor	#$DEAD
	cmp	CHKSUM_NMI
	beq	@set_vector_int
	lda	#return_int
	sta	VECTOR_NMI
	eor	#$DEAD
	sta	CHKSUM_NMI
	
@set_vector_int:
	lda	VECTOR_INT
	eor	#$DEAD
	cmp	CHKSUM_INT
	beq	@set_vector_brk
	lda	#iirq
	sta	VECTOR_INT
	eor	#$DEAD
	sta	CHKSUM_INT
	
@set_vector_brk:
	lda	VECTOR_BRK
	eor	#$DEAD
	cmp	CHKSUM_BRK
	beq	@set_vector_inout
	lda	#return_int
	sta	VECTOR_BRK
	eor	#$DEAD
	sta	CHKSUM_BRK
	
; Set Character I/O vectors
@set_vector_inout:
	lda	#uart_output
	sta	VECTOR_CHAR_OUT
	
	lda	#uart_scan
	sta	VECTOR_CHAR_SCAN
	
	lda	#uart_input
	sta	VECTOR_CHAR_IN
	

@set_vector_done:
	shortr				; Return to 8-bit registers
	
	lda	#$00			; Set DBR to $00
	pha				; So we start in bank $00 ...
	plb				; ... for sure

;###################
; In the future:
;	Test here for which input device (UART or KBD/video) and set
; 	corretc vectors ;)
;###################
	jsr 	uart_init		; Initialize uart
	
	longr				; And back to 16-bit registers
	lda	#$0000			; Clear registers
	tay
	tax
	
	shortr				; And back again... 8-bit
	
	clc				; Make sure carry is cleared
	cld				; As should decimal be
	cli				; And enable interrupts
	
;###################
; Show BIOS header
;###################
	pea	bios_init
	jsr	sprint
	
	jmp	JMON

KNOWN_RTS:
	rts
;---------------------------------------
; End of reset handler
;---------------------------------------

;---------------------------------------
; Interrupt handler(s)
;---------------------------------------
return_int:

	rti				; Just a simple (fixed) RTI
	
handler_nmi:
	jmp	(VECTOR_NMI)		; Let the user handle NMI

; Handle IRQ in native 65C816 mode
handler_nirq:
	phb				; Save current DB
	phd				; Save current DP
	longr				; 16-bit registers
	pha				; Save .A, .X and .Y
	phx
	phy
;-----------------------
;	Contents of stack
;
;irq_yreg	= 1			; 16 bit .Y
;irq_xreg	= irq_yreg + s_word	; 16 bit .X
;irq_areg	= irq_xreg + s_word	; 16 bit .A
;irq_dpreg	= irq_areg + s_word	; DP
;irq_dbreg	= irq_dpreg + s_mpudpx	; DBR
; pushed by hardware
;irq_srreg	= irq_dbreg + s_mpudbx	; SR
;irq_pcreg	= irq_srreg + s_mpusrx	; PC
;irq_pbreg	= irq_pcreg + s_mpupcx	; PBR
;-----------------------
	jmp	(VECTOR_INT)		; Jump to (indirect) vector
	
; Handle IRQ/BRK in 65C02 Emulation mode
handler_eirq:
	shortr
	phx				; Save .A and .X
	pha
	tsx				; Get stack pointer
	lda	$0103, x		; Load .P from stack
	and	#%00010000		; Mask BRK flag
;-----------------------
;	Contents of stack
; pushed by ISR
;irq_xreg	= 1			; 16 bit .X
;irq_areg	= irq_xreg + s_byte	; 16 bit .A
; pushed by hardware
;irq_srreg	= irq_dbreg + s_mpudbx	; SR
;irq_pcreg	= irq_srreg + s_mpusrx	; PC
;irq_pbreg	= irq_pcreg + s_mpupcx	; PBR
;-----------------------
	bne	handler_brk		; Did we have BRK?
	pla				; Nope, restore .A and .X
	plx
;-----------------------
;	Contents of stack
; pushed by ISR
; pushed by hardware
;irq_srreg	= 1		; SR
;irq_pcreg	= irq_srreg + s_mpusrx	; PC
;irq_pbreg	= irq_pcreg + s_mpupcx	; PBR
;-----------------------
	jmp	(VECTOR_INT)		; Let user handle interrupt
	
handler_brk:
	shortr				; 8-bit registers

	phb				; Save .DBR
	phd				; Save .DP
	
	longr				; 16-bit registers
	pha				; Save registers (16-bit!)
	phx
	phy
	
	jmp	(VECTOR_BRK)		; handle BRK interrupt

lowlevel_brq:
	ldy	#$00
@next:
	lda	bios_ll_brk, Y
	beq	@done
	jsr	char_out
	iny
	bra 	@next
@done:
	bra	@done			; Loop!
	
;###################
; Main BIOS strings
;###################
bios_ll_brk:
	.byte	" *** LOW LEVEL BIOS BRK *** ", LF, CR, $00
bios_init:
	.byte	"      A   DDDDD    RRRRR    IIIII   A      ", LF, CR
	.byte	"     AA   D    D   R    R     I     AA     ", LF, CR
	.byte	"    A A   D    D   R    R     I     A A    ", LF, CR
	.byte	"   A  A   D    D   RRRRR      I     A  A   ", LF, CR
	.byte	"  AAAAA   D    D   R R        I     AAAAA  ", LF, CR
	.byte	" A    A   D    D   R  R       I     A    A ", LF, CR
	.byte	"A     A   DDDDD    R    R   IIIII   A     A", LF, CR
	.byte	LF, CR
	.byte	LF, CR
	.byte	"Version: "
	bios_version
	.byte	" - Xander Maas, 2021", LF, CR
	.byte	"<development version!", LF, CR, LF, CR, $00
;---------------------------------------
; Fill to 65816 reset vectors ($FFE0)
;---------------------------------------
	.res	$FFD0 - *, $00
	
;---------------------------------------
; Required reset vectors
;---------------------------------------
	.org	$FFE0
	.segment	"VECTORS"
reset16:
	.word	reset			; $FFE0 - Reserved
	.word	reset			; $FFE2 - Reserved
	.word	reset			; $FFE4 - COP
	.word	handler_brk		; $FFE6 - BRK
	.word	reset			; $FFE8 - ABRT
	.word	handler_nmi		; $FFEA - NMI
	.word	reset			; $FFEC - Reset
	.word	handler_nirq		; $FFEE - IRQ
	
reset8:
	.word	reset			; $FFF0 - Reserved
	.word	reset			; $FFF2 - Reserved
	.word	reset			; $FFF4 - COP
	.word	reset			; $FFF6 - Reserved
	.word	reset			; $FFF8 - ABRT
	.word	handler_nmi		; $FFFA - NMI
	.word	reset			; $FFFC - Reset
	.word	handler_eirq		; $FFFE - IRQ/BRK
	
;=======================================================================
; End of routines
; Calculation of usage
_reset_end	=	* -1
_reset_size 	=	_reset_end - RESET_ROUTINES
;	.out	.concat("---- Size of Reset routines:          $", .sprintf("%04x", _reset_size), "(", .sprintf("%05d", _reset_size), ")")
	.out .concat("Reset                             $", .sprintf("%04x", RESET_ROUTINES), "      $", .sprintf("%04x", _reset_end), "    $", .sprintf("%04x", _reset_size), "  (", .sprintf("%05d", _reset_size), ")")
;=======================================================================
