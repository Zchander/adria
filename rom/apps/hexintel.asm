;
;  Adria Intel Hex Uploader
;
;  Version history:
;	2020-09-09	- Initial version, for use with Supermon816 monitor
;
; This Intel Hex Downloader is based on/copied from/written by Ross Archer
;==============================================================================
; (Additional assembler/linker directives
	.listbytes	255
	.p816

	.org	HEXINTEL

; Equates
DPL		= $60			; $60 - Data pointer (two bytes)
DPH		= DPL + s_byte		; $61 - High byte of data pointer
RECLEN		= DPH + s_byte		; $62 - Record length in bytes
START_LO	= RECLEN + s_byte	; $63 - Low byte start address record
START_HI	= START_LO + s_byte	; $64 - High byte start address record
RECTYPE		= START_HI + s_byte	; $65 - Record type
RECCHKSUM	= RECTYPE + s_byte	; $66 - Record checksum accumulator
DLFAIL		= RECCHKSUM + s_byte	; $67 - Flag for download failure
TEMP		= DLFAIL + s_byte	; $68 - Save temporary hex value
PROG_START	= TEMP + s_byte		; $69 - Program start address

strtintel:
	stz	DLFAIL			; Start by assuming no failure (yet)
	stz	PROG_START
	stz	PROG_START + 1
	
	pea	mm_strt			; Print prompt
	jsr	sprint
	
	
hdwrecs:
	jsr	char_in			; Get data from ACIA
	cmp	#$03			; Did we get a <Ctrl-C>
	bne	@a
	cmp	#ESC			; Did we get an <Esc>
	bne	@a
	
	pea	mm_cancel		; Yep, show cancelled message
	jsr	sprint
	rts				
	
@a:					; Nope, continue
	cmp	#':'			; Wait for start of record mark ":"
	bne	hdwrecs			; not yet?
	; Start of record marker has been found
	jsr	GETHEX			; get record length
	sta	RECLEN			; save it
	sta	RECCHKSUM		; And save first byte of chksum
	jsr	GETHEX			; Get high byte of start address
	sta	START_HI
	clc
	adc	RECCHKSUM		; Add in the chksum
	sta	RECCHKSUM
	jsr	GETHEX			; Get low byte of address
	sta	START_LO
	clc
	adc	RECCHKSUM
	sta	RECCHKSUM
	jsr	GETHEX			; Get record type
	sta	RECTYPE
	clc
	adc	RECCHKSUM
	sta	RECCHKSUM
	lda	RECTYPE
	bne	HDER1			; End-Of-Record
	ldx	RECLEN			; Number of bytes to write to memory
	ldy	#$00			; Start offset at 0
HDLP1:
	jsr	GETHEX			; Get data byte
	sta	(START_LO), y		; Save it to RAM
	clc
	adc	RECCHKSUM
	sta	RECCHKSUM
	iny				; Increment data pointer
	dex				; Decrement count
	bne	HDLP1			; Continue until counter = 0
	jsr	GETHEX			; get the checksum
	clc
	adc	RECCHKSUM
	bne	HDDLF1			; If failed, report it
; Another successful record processed
	lda	#'#'			; Record == OK ==> "#"
;	sta	ACIA_DATA		; Write to ACIA, but don't wait
	jsr	uart_output
	jmp	hdwrecs			; get next record
HDDLF1:
	lda	#'F'			; Record == Fail ==> "F"
	sta	DLFAIL
;	sta	ACIA_DATA
	jsr	uart_output
	jmp	hdwrecs

HDER1:
	cmp	#1			; Check for End-of-record type
	beq	HDER2
	pea	mm_unkn
	jsr	sprint
	lda	RECTYPE
	sta	DLFAIL
;	jsr	PUTHEX
	jsr	dpyhex
	jsr	newline			; Known entry in ROM ;)
	jmp	hdwrecs
	
HDER2:
	jsr	GETHEX			; Get checksum
	clc
	adc	RECCHKSUM		; Add previous .A value
	beq	HDER3			; CHKSUM = 0 means OK
	pea	mm_bad			; Warn user of bad checksum
	jsr	sprint
	rts
	
HDER3:
	lda	DLFAIL
	beq	HDEROK			; 0 = OK, non-zero = error
	
	pea	mm_fail
	jsr	sprint
	
	sec				; Indicate an error
	jmp	monerr
	
HDEROK:
	pea 	mm_succ
	jsr	sprint
	lda	START_HI
;	jsr	PUTHEX
	jsr	dpyhex
	lda	START_LO
;	jsr	PUTHEX
	jsr	dpyhex
	jsr	newline
	jmp	monce
	
;
; GETHEX - get hex value from ACIA
;	We might reuse the Supermon816 routines later on (just to shrink the
;	size of the code)
;
GETHEX:			
	jsr	char_in			; We use a blocking ACIA get char
	jsr	MKNIBL			; Convert to 0..F numeric
	asl
	asl
	asl
	asl				; Now we have the upper nibble
	and	#$f0
	sta	TEMP
	jsr	char_in			; Get second nibble
	jsr	MKNIBL
	ora	TEMP
	rts				; Return with complete byte in TEMP
	
;
; MKNIBL - Convert the ASCII nibble to numeric value 0..F
;	We might reuse the Supermon816 routines later on (just to shrink the
;	size of the code)
;	
MKNIBL:
	cmp	#'9' + 1		; See if it is 0-9 or 'A'..'F'
	bcc	MKNNH			; If we borrowed, we lost the carry, so 0..9
	sbc	#7 + 1			; Subtract off extra 7 (sbc subtracts off one less)
	; If we fall through, carry is set unlike direct entry at MKNNH
MKNNH:
	sbc	#'0' - 1		; Subtract off '0' (if carry clear coming in)
	and	#$0f			; No upper nibble, no matter what
	rts				; And return with nibble
	
; PUTHEX - Put byte in .A as hexidecimal
PUTHEX:
;	pha
;	lsr
;	lsr
;	lsr
;	lsr
;	jsr	PRNIBL
;	pla
;PRNIBL:
;	and	#$0f			; Strip off the low nibble
;	cmp	#$0a
;	bcc	NOTHEX
;	adc	#$06			; Add 7 (6+carry), result will be carry clear
;NOTHEX:
;	adc	#'0'			; If carry=0, 0..9
;	jmp	putcha			; Write .A to terminal
	jmp	dpyhex

;
; Strings to print		
mm_strt:
	.byte	LF, CR, LF, CR
	.byte	"Send binary code in Intel Hex format"
	.byte	LF, CR
	.byte 	"at 19200, n, 8, 1 -->"
	.byte	LF, CR
	.byte	$00

mm_strt_lcd:
	.asciiz	"Intel Loader"
	
mm_unkn:
	.byte	LF, CR, LF, CR
	.byte	"Unknown record type $"
	.byte	$00
	
mm_bad:
	.byte	LF, CR, LF, CR
	.byte	"Bad record checksum!"
	.byte	LF, CR, LF, CR
	.byte 	$00
	
mm_fail:
	.byte	LF, CR, LF, CR
	.byte	"Download failed"
	.byte	LF, CR
	.byte	"Aborting ...."
	.byte	LF, CR, LF, CR
	.byte 	$00
	
mm_succ:
	.byte	LF, CR, LF, CR
	.byte	"Download successful"
	.byte	LF, CR
	.byte 	"Program located at $00/"
	.byte	$00
	
mm_cancel:
	.byte	LF, CR, LF, CR
	.byte	"Received <Control-C> or <Escape>", LF, CR
	.byte	"Transfer aborted...", LF, CR
	.byte	LF, CR
	.byte	$00
	
;---------------------------------------
; Used to calculate memory footprint of the module
;---------------------------------------
INTEL_END	= *			; Must be declared before any
					; calculations
INTEL_SIZE	= INTEL_END - HEXINTEL
	.out .concat("Intel hex uploader                $", .sprintf("%04x", HEXINTEL), "      $", .sprintf("%04x", INTEL_END), "    $", .sprintf("%04x", INTEL_SIZE), "  (", .sprintf("%05d", INTEL_SIZE), ")")
;	.out .concat("---- Size of INTEL Hex uploader:                 $", .sprintf("%04x", INTEL_SIZE), " (", .sprintf("%5d", INTEL_SIZE), ")")
