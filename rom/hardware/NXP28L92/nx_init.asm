;===============================================================================
;   A D R I A   - Philips/NXP 28L9x Initialization routine(s)
;
; 	This source file holds the initialization routine(s) for the 
;	Philips/NXP 28L9x IC.
;
;	Version history:
;		26 jan 2021 	- Inital version
;		18 feb 2021 	- Userland driver seems to work, migrating to
;				  ROM based driver
;===============================================================================
; nx_init - Initialize 28L9x environment
;
; Will read the configuration parameters from the table (bottom-up) and write
; them to the 29L9x.
;
; Currently the table is made for the 28L92 (2 ports), but this can be extended
; to the 8-port variant.
;
; Returns with carry cleared.
;-------------------------------------------------------------------------------
uart_init:
nx_init:
	shortr
	sei				; Disable interrupts until done
;---------------------------------------
; We use register $0C (dr_misc) of the 28L9x as temporary storage
; and to indicate the driver initialization has succeeded. Initially we zero
; this register
	stz	NX_IOBASE + dr_misc
;
; Clear used/reserved zeropage (DP) locations
;---------------------------------------
	ldx	#$00
@nextZP:
	stz	nx_zeropage, X
	inx
	cpx	#$20			; Reserve $20 (32) bytes and clear these
	bne	@nextZP
;
; Clear RxQ and TxQ buffer space
	longi				; We use 16-bit index registers
	ldx	#$0000
@nextBuf:
	stz	nx_q_base, X		; Clear buffer space with $00
	inx
	cpx	#$0200			; 4 * $80
	bne	@nextBuf
	shorti				; And don't forget to return to 8-bit
;
; Prepare jiffy counter
;---------------------------------------	
	lda	#hz
	sta	nx_jiffycnt
;
; Set up the 28L9x IC using the setup/lookup table
;---------------------------------------
	ldy	#size_l92_suTable - 2	; Get size of setup/lookup table
@setup:
	ldx	l92_suTable, Y		; Read register offset
	lda	l92_suTable + s_byte, Y	; Read register parameter
;
	sta	NX_IOBASE, X		; Write to register
;
	dey				; As we use two bytes per register, ...
	dey				; ... we have to decrement .Y by 2
	bpl	@setup			; Continue until we have reached top

; Only used when debugging/userland
.if NX_USERDRIVER = 1
; before we return, just point to our IRQ handler (when in userland!)
	longr
	lda	VECTOR_INT		; Get original /IRQ vector
	sta	rom_irq			; Save it for later
;
	lda	#nx_irq			; Install our new /IRQ vector
	tax
	eor	#$DEAD			; Calculate checksum (force invalid for
					; now)
	sta	CHKSUM_INT		; Store the checksum
	stx	VECTOR_INT		; Save the new vector
;
	shortr				; 8 bit registers
.endif
;
; Before we return we have to start the counter by reading IOBASE + dr_cnt_start
; As BDD mentioned, it might be safer to use a BIT instruction
;	lda	IOBASE + dr_cnt_start
	bit	NX_IOBASE + dr_cnt_start
;
; Now we should get a 100Hz ticker and by enabling the interrupts (below), we
; should now have a 0.01 second clock timer ;)
;
; Before returning, set the nx_driver_init flag
	lda	#nx_driver_init
	tsb	NX_IOBASE + dr_misc	; Store this is the dr_misc register
	tsb	nx_tx_status
;
	cli				; Enable interrupts
	clc				; C is cleared, no error
	rts
;
