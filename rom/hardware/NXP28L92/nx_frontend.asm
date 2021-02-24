;===============================================================================
;   A D R I A   - Philips/NXP 28L9x Front-end Test routine(s)
;
; 	This source file holds the front-end routine(s) for the 
;	Philips/NXP 28L9x IC.
;
;	Version history:
;		26 jan 2021 	- Inital version
;		18 feb 2021 	- Userland driver seems to work, migrating to
;				  ROM based driver
;===============================================================================
; Process Receive Request
;
; Call the subroutine and the routine will try to "get" a datum from the
; corresponding RxQ. If this succeeds, the routine will return:
;	.A	- valid datum
;	carry	- clear
;
; If no datum is present in the RxQ, the routine will return:
;	.A	- unchanged
;	carry	- set
;
; NOTE: Carry will also be returned set, if the driver hasn't been initialized,
;	yet...
;-------------------------------------------------------------------------------
; nx_sioget_a - Process datum from channel A RxQ
;-----------------------
uart_scan:
nx_sioget_a:
	shortr				; Make sure we are in 8-bit registers
	sec				; Default: assume error
	bit	NX_IOBASE + dr_misc	; Read bit 7 of dr_misc, if set, driver
					; has been initialized
	bpl	@done			; No, error
;
	phx				; Save .X
	ldx	nx_rx_get_a		; RxD "get" index
	cpx	nx_rx_put_a		; Compare with RxD "put" index
	beq	@no_datum		; No datum available
;
	lda	nx_rx_qa, X		; Get datum from queue
	xba				; Move .A to .B
	lda	nx_rx_get_a		; Get "get" index
	inc	a			; bump "get" index
	and	#buf_idx_mask		; Deal with wrap
	sta	nx_rx_get_a		; Store new "get" index
	xba				; Restore datum to .A (from .B)
;	inc	nx_rx_get_a		; Increment "get" index
	clc				; Clear error (carry)
;
@no_datum:
	plx				; Restore .X
;	
@done:
	rts
;===============================================================================
; Process Receive Request (blocking)
;
; Call the subroutine and the routine will try to "get" a datum from the
; corresponding RxQ. If this succeeds, the routine will return:
;	.A	- valid datum
;	
; If no datum is present in the RxQ (yet), the routine loop until there is
;-------------------------------------------------------------------------------
; nx_sioget_a_block - Read datum Channel A RxQ, blocks when no data is available
;-----------------------	
uart_input:
nx_sioget_a_block:
	shortr
	bit	NX_IOBASE + dr_misc	; Is the driver initialized?
	bpl	@done			; Nope, just return!
;
	phx				; Save .X
@try:
	ldx	nx_rx_get_a		; get "get" index
	cpx	nx_rx_put_a		; Same as "put" index"
	beq	@try			; If no new datum, retry ...
;	bne	@datum			; No, process datum
;					; Otherwise, waste some time waiting
;	bra	@try
;	
@datum:
	lda	nx_rx_qa, X
	xba				; Save datum to .B
	lda	nx_rx_get_a		; Get current "get" index
	inc	a			; Bump index
	and	#buf_idx_mask		; Deal with wrap
	sta	nx_rx_get_a		; Save new "get" index
	xba				; Restore datum from .B
;	inc	nx_rx_get_a

	plx				; Restore .X
@done:
	rts
	
;
;-----------------------
; nx_sioget_b - Process datum from channel B RxQ
;-----------------------	
nx_sioget_b:
	shortr				; Make sure we are in 8-bit registers
	sec				; Default: assume error
	bit	NX_IOBASE + dr_misc	; Read bit 7 of dr_misc, if set, driver
					; has been initialized
	bpl	@done			; No, error
;
	phx				; Save .X
	ldx	nx_rx_get_b		; RxD "get" index
	cpx	nx_rx_put_b		; Compare with RxD "put" index
	beq	@no_datum		; No datum available
;
	lda	nx_rx_qb, X		; Get datum from queue
	inc	nx_rx_get_b		; Increment "get" index
	clc				; Clear error (carry)
;
@no_datum:
	plx				; Restore .X
;	
@done:
	rts
;
;-------------------------------------------------------------------------------
; Process Transmit Request
;
; Call the subroutine and the routine will try to "put" a datum into the
; corresponding TxQ. If this succeeds, the routine will return:
;	carry	- clear
;
; NOTE: Carry will be set, if the driver hasn't been initialized,
;	yet...
;-------------------------------------------------------------------------------
; nx_sioput_a - Process datum for channel A TxQ
;-----------------------
uart_output:
nx_sioput_a:
	php
	shortr				; 8 bit registers
	sec				; Default, assume error
	bit	NX_IOBASE + dr_misc	; Read bit 7 of dr_misc, if set, driver
					; has been initialized
	bpl	@done			; No, error!
;
	pha				; Preserve .A
	phx				; Preserve .X
;
	xba				; Save datum to .B
;	
	lda	nx_tx_put_a		; Get TxD "put" index
	ina				; Increment "put" index
	and	#buf_idx_mask		; Deal with wrap
;
@full:					; Loop here until there is space in TxQ
	cmp	nx_tx_get_a		; TxQ "get" index
	beq	@full			; TxQ is full, block...
;
	ldx	nx_tx_put_a
	xba				; Restore datum in .A, save new "put"
	sta	nx_tx_qa, X		; Put datum into TxQ
	xba				; Retrieve new "put" index from .B
	sta	nx_tx_put_a
;	
;	ldx	nx_tx_put_a		; TxQ "put" index
;	inx
;
;@full:
;	cpx	nx_tx_get_a		; TxQ "get" index
;	beq	@full			; TxQ is full, block...
;
;	dex				; Realign "put" index and ...
;	sta	nx_tx_qa, X		; ... put datum into TxQ
;	inc	nx_tx_put_a		; Bump "put" index
;
;---------------
;	Manage transmitter
;---------------
	lda	#nx_txa_dis		; Transmitter disabled flag
	trb	NX_IOBASE + dr_misc	; is transmitter disabled?
	beq	@cleanup		; No

	lda	#nx_cr_tx_ena		; Yes, enable transmitter
	sta	NX_IOBASE + dr_cra
;
@cleanup:
	plx
	pla
	clc
;
@done:
	plp				; Restore .SR
	rts
;
;-----------------------
; nx_sioput_b - Process datum from channel B TxQ
;-----------------------
nx_sioput_b:
	php
	shortr				; 8 bit registers
	sec				; Default, assume error
	bit	NX_IOBASE + dr_misc	; Read bit 7 of dr_misc, if set, driver
					; has been initialized
	bpl	@done			; No, error!
;
	pha				; Preserve .A
	phx				; Preserve .X
;
	xba				; Save datum to .B
;	
	lda	nx_tx_put_b		; Get TxD "put" index
	inc	a			; Increment "put" index
	and	#buf_idx_mask		; Deal with wrap
;
@full:					; Loop here until there is space in TxQ
	cmp	nx_tx_get_b		; TxQ "get" index
	beq	@full			; TxQ is full, block...
;
	ldx	nx_tx_put_b
	xba				; Restore datum in .A, save new "put"
	sta	nx_tx_qb, X		; Put datum into TxQ
	xba				; Retrieve new "put" index from .B
	sta	nx_tx_put_b
;
;---------------
;	Manage transmitter
;---------------
	lda	#nx_txb_dis		; Transmitter disabled flag
	trb	NX_IOBASE + dr_misc	; is transmitter disabled?
	beq	@cleanup		; No

	lda	#nx_cr_tx_ena		; Yes, enable transmitter
	sta	NX_IOBASE + dr_crb
;
@cleanup:
	plx
	pla
	clc
;
@done:
	plp				; Restore .SR
	rts
;
;-------------------------------------------------------------------------------
; UART Channel B Control
; 
; Enable/disable channel based on carry value. Routine is based (copied ;)) from
; BDD's SBC POCv1 ROM
;
; carry = 1	=> Enable channel B
; carry = 0	=> Disable channel B
;-------------------------------------------------------------------------------
nx_chan_b_ctl:
	shorta
	bcc	nx_dis_chanb		; If carry is cleared, disable channel B
;
nx_ena_chanb:
	lda	#(nx_cr_rxtx_ena | nx_cr_rts_ass)
	bra	nx_chanctl		; Write to DUART command register
;
nx_dis_chanb:
	lda	#(nx_cr_rxtx_dis | nx_cr_rts_dea)
;
nx_chanctl:
	sta	NX_IOBASE + dr_crb	; Save to Command Register channel B
	wai				; We have to wait two /IRQs?
	wai			
	rts
