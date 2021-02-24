;===============================================================================
;   A D R I A   - Philips/NXP 28L9x Fixed data/reference/lookup tables
;
;	Version history:
;		26 jan 2021 - Inital version
;		08 feb 2020 - Changed Counter mode to Timer mode
;===============================================================================
; l92_suTable - 28L9x initialization sequence for two (2) ports
;---------------------------------------
l92_suTable:				; Philip/NXP 28L9x Initialization Table
;
; vvv  DEBUG  vvv
; Disabled IRQ for now, we have to start them manually)
	.byte	dr_imr, nx_irq_msk	; IMR (Enable IRQs)
;	.byte	dr_imr, msk_irq_ct	; For testing, only enable CT IRQs
; ^^^  DEBUG  ^^^
;
	.byte	dr_ctu, nx_cnt_hi	; CTU
	.byte	dr_ctl, nx_cnt_lo	; CTL

	.byte	dr_crb, nx_cr_rxtx_dis	; CRB - Disable RxD and TxD
;---------------------------------------
; Port B will be disabled, for now!
;---------------------------------------
;
	.byte	dr_csrb, nx_csr		; CSR - Set Clock Register
	.byte	dr_mrb, nx_mr2		; MR2 - Set Mode Register 2
	.byte	dr_mrb, nx_mr1		; MR1 - Set Mode Register 1
	.byte	dr_crb, nx_cr_mr1	; CRA - Select Mode Register 1
	.byte	dr_mrb, nx_mr0		; MR0 - Set Memory Register 0
	.byte	dr_crb, nx_cr_mr0	; CRA - Select Mode Register 0
;---------------------------------------
; Port B
;---------------------------------------
;
	.byte	dr_cra, nx_cr_rts_ass	; CRA - Assert RTS
	.byte	dr_cra, nx_cr_rxtx_ena	; CRA - Enable RxD and TxD

;	.byte	dr_cra, nx_cr_rxtx_dis	; CRA - For now, disable RxD and TxD
;
;---------------------------------------
; Enable port A
;---------------------------------------
;
	.byte	dr_csra, nx_csr		; CSR - Set Clock Register
	.byte	dr_mra, nx_mr2		; MR2 - Set Mode Register 2
	.byte	dr_mra, nx_mr1		; MR1 - Set Mode Register 1
	.byte	dr_cra, nx_cr_mr1	; CRA - Select Mode Register 1
	.byte	dr_mra, nx_mr0		; MR0 - Set Memory Register 0
	.byte	dr_cra, nx_cr_mr0	; CRA - Select Mode Register 0
;---------------------------------------
; Port A
;---------------------------------------
;
	.byte	dr_acr, nx_acr_def	; ACR - Auxiliary Control Register
;---------------------------------------
; ACR
;---------------------------------------
;
	.byte	dr_opcr, nx_opcr_def	; OPCR - Output Port Configuration Reg
;---------------------------------------
; OPCR
;---------------------------------------
;
	.byte	dr_crb, nx_cr_cnt_mod 	; CRB - Select C/T Counter mode
	.byte	dr_crb, nx_cr_err_res	; CRB - Reset error status
	.byte	dr_crb, nx_cr_bir_res	; CRB - Reset Received Break Change IRQ
	.byte	dr_crb, nx_cr_tx_res	; CRB - Reset transmitter
	.byte	dr_crb, nx_cr_rx_res	; CRB - Reset receiver
	.byte	dr_crb, nx_cr_rts_dea 	; CRB - Deassert RTS
;---------------------------------------
; Init port B
;---------------------------------------

	.byte	dr_cra, nx_cr_cnt_mod 	; CRA - Select C/T Counter mode
	.byte	dr_cra, nx_cr_err_res	; CRA - Reset error status
	.byte	dr_cra, nx_cr_bir_res	; CRA - Reset Received Break Change IRQ
	.byte	dr_cra, nx_cr_tx_res	; CRA - Reset transmitter
	.byte	dr_cra, nx_cr_rx_res	; CRA - Reset receiver
	.byte	dr_cra, nx_cr_rts_dea 	; CRA - Deassert RTS
;---------------------------------------
; Init port A
;---------------------------------------
;	
	.byte	dr_imr, $00		; IMR - Disable IRQs
	.byte	dr_cra, nx_cr_pwr_up	; CRA - Power up
;---------------------------------------
; Size of table
;---------------------------------------
size_l92_suTable	= * - l92_suTable
;
;***********************
; DEBUG, remove when done!
;***********************
	.word	size_l92_suTable
	.word	nx_ct_scale/hz
	.byte	<(nx_ct_scale/hz)
	.byte	>(nx_ct_scale/hz)
;
	.byte	nx_jiffycnt
	.byte	nx_uptimecnt
	.byte	nx_rx_get_a
	.byte	nx_rx_put_a
	.byte	nx_tx_get_a
	.byte	nx_tx_put_a
	.byte	nx_tx_status
	.byte	rom_irq
