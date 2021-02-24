;
; Adria 6551 based UART routines
;
; Version history:
;	05 Jan 2021	- Initial version
;=======================================================================
; Local equates, macros, definitions
;---------------------------------------
	.org	UART_DRIVER
	
ACIA_DATA	=	UART_IOBASE
ACIA_STATUS	=	UART_IOBASE + 1	
ACIA_CMD	=	UART_IOBASE + 2
ACIA_CTL	=	UART_IOBASE + 3

;---------------------------------------
; Initialize the 6551 (ACIA)
;---------------------------------------
uart_init:
	php				; Save .P
	
	shortr				; Make sure all registers are
					; 8-bit wide
	
;###################
; SEE DATASHEET FOR MORE INFO
;###################
					; Load .A with 6551 mode setting
	lda	#%00011111		; 19k2 / 8 / 1
;	lda	#%00011110		;  9k6 / 8 / 1

					; Load .X with ACIA CTL
	ldx	#%00001011		; No parity / no echo / rx int
	
	sta	ACIA_CTL		; Store .A in Control Register
;*** WHY OH WHY WOULD WE DO THIS?
;	txa				; Move .X to .A
;	sta	ACIA_CMD		; Store .X in Command Register
;*** INSTEAD OF THIS?
	stx	ACIA_CMD		; Store .X in Command Register

;###################
; OPTIONAL: Print a message
;###################
	ldy	#$00
@next_char:
	lda	uart_msg, y
	beq	@done			; Did we receive a $00
	jsr	uart_output		; Nope, print the character
	iny
	bra	@next_char
@done:
	plp				; Restore .P

	rts				; Done, initialized UART				
	
uart_output:
	pha				; Save .A
@wait:
	lda	ACIA_STATUS		; Get status of the port
	and	#$10			; Is Tx buffer empty?
	beq	@wait			; Not yet, we'll wait..
	pla				; Get the byte to send ...
					; ... from stack
	sta	ACIA_DATA		; Put the byte in the Tx buffer

;					; in case we use the 65C51 from WDC,
;	jsr	_delay_65c51		; We have to also use this routine

	rts

uart_input:
@wait:
	lda	ACIA_STATUS		; Get 6551 serial port status
	and	#$08			; Mask off receiver full bit
	beq	@wait			; Nothing received? Wait...
	lda	ACIA_DATA		; Get the data from the buffer
	rts
	
;###################
; uart_scan
;
; carry = 0 => .A holds byte received 
; carry = 1 => Nothing received (yet)
;###################
uart_scan:
	sec
	lda	ACIA_STATUS		; Get status of port
	and	#$08			; Mask off buffer full bit
	beq	@done			; Nothing yet, return with ...
					; ... carry = 1
	lda	ACIA_DATA		; Get byte from Rx buffer
	clc				; And set carry = 0
	
@done:
	rts

;###################
; uart_port_set
;
; Configures the 6551 (ACIA)
;
; 
;###################
uart_port_set:
	pha				; Save .A, .X and .P registers
	phx
	php
	
	sta	ACIA_CTL		; Store .A in Control Register
;*** WHY OH WHY WOULD WE DO THIS?
;	txa				; Move .X to .A
;	sta	ACIA_CMD		; Store .X in Command Register
;*** INSTEAD OF THIS?
	stx	ACIA_CMD		; Store .X in Command Register
	
	plp				; Restore .A, .X and .P
	plx
	pla
	
	rts

;###################
; delay_65c51
;
; (source Forum 6502.org: http://forum.6502.org/viewtopic.php?f=4&t=2543&start=30#p29795)
; Latest WDC 65C51 has a bug - Xmit bit in status register is stuck on
; IRQ driven transmit is not possible as a result - interrupts are endlessly triggered
; Polled I/O mode also doesn't work as the Xmit bit is polled - delay routine is the only option
; The following delay routine kills time to allow W65C51 to complete a character transmit
; 0.523 milliseconds required loop time for 19,200 baud rate
; MINIDLY routine takes 524 clock cycles to complete - X Reg is used for the count loop
; Y Reg is loaded with the CPU clock rate in MHz (whole increments only) and used as a multiplier
;
;###################
_delay_65c51:
	phy				; Save .Y
	phx				; Save .X

@delay_loop:
	ldy	#SYSCLK			; System clock speed (oscillator / 4)
@delay2:
	ldx	#$80			; Seed .X
@delay3:
	dex				; Decrement .X (low index)
	bne	@delay3			; Loop back until done
	
	dey				; Decrement .Y (high index)
	bne	@delay2			; Loop until done
	
	plx				; Restore .X
	ply				; Restore .Y
@delay_done:
	rts

;=======================================================================
; Message/string storage
;=======================================================================
uart_msg:
	.byte	"Adria UART initialized ...", LF, CR, LF, CR, $00
;	.byte	"Welcome to Adria", LF, CR, LF, CR, LF, CR, $00
	
;=======================================================================
; End of routines
; Calculation of usage
_uart_end	=	*
_uart_size 	=	_uart_end - UART_DRIVER
	.out .concat("UART driver                       $", .sprintf("%04x", UART_DRIVER), "      $", .sprintf("%04x", _uart_end), "    $", .sprintf("%04x", _uart_size), "  (", .sprintf("%05d", _uart_size), ")")
;	.out	.concat("---- Size of UART (6551) routines:    $", .sprintf("%04x", _uart_size), "(", .sprintf("%05d", _uart_size), ")")
;=======================================================================

