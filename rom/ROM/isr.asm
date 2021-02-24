;===============================================================================
;   A D R I A   - Interrupt handler(s)
;
; 	This source file holds the interrupt handler(s)
;
;	Version history:
;		26 jan 2021 	- Inital version
;		18 feb 2021 	- Userland driver seems to work, migrating to
;				  ROM based driver
;				- For now, channel B ISR is INCOMPLETE
;		19 feb 2021	- Moved ISR to "generic" ROM space
;===============================================================================
; irq - Hardware Interrupt Request Service routine
;-------------------------------------------------------------------------------
iirq:
	shortr				; 8 bit registers
;
; Determine source of IRQ(s). Here we also determine the priority of the IRQ

	lda	NX_IOBASE + dr_isr	; Load IRQ register, did UART interrupt?
;
;
; Seems when adding another channel, the branch is taken too long ;) had to 
; solve it somehow
;	beq	nx_irq_end		; Nope
	bne	@save_ISR		; Very short branch
	jmp	iirq_end		; And we use a jump
;
@save_ISR:
	shorta
	pha				; As per tip from BDD 
					; (see forum.6502.org) I now push .A 
					; to the stack before!
;
;-----------------------
;	Contents of stack, which we
;	(might) use in the routine
;-----------------------
irq_isrx	= 1			; UART IRQ status
irq_yreg	= irq_isrx + s_byte			; 16 bit .Y
irq_xreg	= irq_yreg  + s_word	; 16 bit .X
irq_areg	= irq_xreg  + s_word	; 16 bit .A
irq_dpreg	= irq_areg  + s_word	; DP
irq_dbreg	= irq_dpreg + s_mpudpx	; DBR
; pushed by hardware
irq_srreg	= irq_dbreg + s_mpudbrx	; SR
irq_pcreg	= irq_srreg + s_mpusrx	; PC
irq_pbreg	= irq_pcreg + s_mpupcx	; PBR
;	
;-----------------------	
; Test for C/T IRQ
;-----------------------
test_ct:
;					
	bit	#msk_irq_ct		; Did C/T interrupt?
	beq	test_rhr_a		; Nope, skip this section
;
;-----------------------	
; Process C/T IRQ
;-----------------------
@handle_ct:
	ldx	NX_IOBASE + dr_cnt_stop	; Reset C/T IRQ
	ldx	nx_jiffycnt		; Get jiffy counter
	dex				; Decrement by one
	bne	@upd_jiffy		; Not time, yet, to update uptime
;
	longa				; 16 bit .A
	inc	nx_uptimecnt		; increment uptime LSW
	bne	@reset_jiffy		; Done with uptime
;
	inc	nx_uptimecnt + s_word	; Increment uptime MSW
;
@reset_jiffy:
	shortr				; 8 bit .A
	ldx	#hz			; Reset jiffy count
;
@upd_jiffy:
	stx	nx_jiffycnt		; Set new jiffy count value
;
;-----------------------	
; Test for channel A RHR IRQ
;-----------------------
test_rhr_a:
	shortr
	lda	irq_isrx, S		; Get original ISR from stack
	bit	#msk_irq_rxa		; Did receiver A request?
	beq	test_rhr_b		; Nope, check channel B
;-----------------------	
; Process channel A RHR IRQ
;
; Enter loop to service RHR. Loop will not break until RHR is empty
;-----------------------
@handle_rhr_a:
	ldy	#msk_sr_rx_rdy		; RHR status mask
@process_datum:
	tya
	bit	NX_IOBASE + dr_sra	; Do we have anything in our RHR for
	beq	test_rhr_b		; channel A? Nope: go to B

	lda	NX_IOBASE + dr_rxfifoa	; YES, read datum from RHR
	ldx	nx_rx_put_a		; RxD "put" index
	inx				; Increment
	cpx	nx_rx_get_a		; Compare with "get" index
	beq	@process_datum		; RxQ is full
;-----------------------
; If RxQ is full, the datum cannot be processed and is lost, corrupting the
; data stream. This should not happen is the frontend gets from RxQ in time
;-----------------------
	dex				; Realign RxD "put" index
	sta	nx_rx_qa, X		; Store datum into RxQ
;
	txa
	inc	a			; Increment RxD "put" index
	and	#buf_idx_mask		; Deal with index wrap
	sta	nx_rx_put_a		; Save new "put" index
;
	bra	@process_datum		; Loop for next
test_rhr_b:
	lda	irq_isrx, S
@handle_rhr_b:
;
;-----------------------	
; Test for channel A THR IRQ
;-----------------------
test_thr_a:
	lda	irq_isrx, S		; Get ISR from stack
;
	bit	#msk_irq_txa		; Did Xmitter A request interrupt?
	beq	test_thr_b		; Nope, try Xmitter B
;-----------------------	
; Process channel A THR IRQ
;-----------------------	
@handle_thr_a:
	ldx	nx_tx_get_a		; Get current TxQ "get" index
	ldy	#msk_sr_tx_rdy		; THR status mask
;-----------------------
; Enter the loop to service the THR interrupt. This loop will continue until
; the THR is full or TxQ is empty
;-----------------------
@next_datum:
	cpx	nx_tx_put_a		; Any (new) datums in TxQ?
	beq	@tx_off			; Nope, shutdown Xmitter and go to B
;
	tya				; YES ...
	bit	NX_IOBASE + dr_sra	; Any space left in THR
	beq	@done			; no, done for now...
;-----------------------
; If the THR is full, the transmitter IRQ processing is done for now...
; Later on, when the THR's level has been reduced, another IRQ will occur and
; more datums can be processed
;-----------------------
	lda	nx_tx_qa, X		; Get datum from queue and ...
	sta	NX_IOBASE + dr_txfifoa	; ... write to THR (FIFO)
	txa				; Move .X to .A
	inc	a			; Increment "get" index
	and	#buf_idx_mask		; Deal with wrap?
	tax				; move .A back to .X
	stx	nx_tx_get_a		; Intermediate save "get" index
	bra	@next_datum		; And process next
;
@tx_off:
	lda	#nx_cr_tx_dis		; Shutdown Xmitter A to suppress ...
	sta	NX_IOBASE + dr_cra	; ... IRQs
	lda	#nx_txa_dis		; Set Xmitter A disabled flag
	tsb	NX_IOBASE + dr_misc	; Do in MISC register!
	tsb	nx_tx_status		; And in ZP (for debug)
;
@done:
	stx	nx_tx_get_a		; Save final TxQ "get" index
;
test_thr_b:
	lda	irq_isrx, S
@handle_thr_b:

;
;-----------------------
; We have reached the end of our IRQ handler
;
; Clean stack from UART IRQ status
;-----------------------
	shortr
	pla				; Clear saved UART IRQ Status & discard
;
iirq_end:
;
;-----------------------
; Clear stack and restore saved registers
;-----------------------
	longr				; 16 bit registers to recover
	ply				; .Y
	plx				; .X
	pla				; .A
;
	pld				; Restore DP
	plb				; Restore DBR
;
;-----------------------
;	Contents of stack
; pushed by hardware
;irq_srreg	= 1			; SR
;irq_pcreg	= irq_srreg + s_mpusrx	; PC
;irq_pbreg	= irq_pcreg + s_mpupcx	; PBR
	rti				; Return from IRQ

;---------------------------------------
; Used to calculate memory footprint of the module
;---------------------------------------
ISR_END		= *			; Must be declared before any
					; calculations
ISR_SIZE	= ISR_END - ISR
	.out .concat("Interrupt Service Routine(s)      $", .sprintf("%04x", ISR), "      $", .sprintf("%04x", ISR_END), "    $", .sprintf("%04x", ISR_SIZE), "  (", .sprintf("%05d", ISR_SIZE), ")")
;	.out .concat("---- Size of ISR uploader:                 $", .sprintf("%04x", ISR_SIZE), " (", .sprintf("%5d", ISR_SIZE), ")")
