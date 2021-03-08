;===============================================================================
;   A D R I A   - Philips/NXP 28L9x Global definitions file
;
; 	This source file holds the global definitions for the drivre for the 
;	Philips/NXP 28L9x IC.
;
;	Version history:
;		26 jan 2021 	- Initial version
;		18 feb 2021 	- Userland driver seems to work, migrating to
;				  ROM based driver
;		25 feb 2021	- Consolidate ZP usage (try to make it one 
;				  block)
;===============================================================================
; GENERIC
;=======================================
; Size data types
;---------------------------------------
.if NX_USERDRIVER
s_bi_bit	= 1			;  1 bit
s_bi_nibble	= 4			;  4 bit, nibble (or nybble)
s_bi_nybble	= s_bi_nibble
s_bi_byte	= 8			;  8 bit, 1 byte
;
s_byte		= 1			;  8 bit, 1 byte
s_word		= 2			; 16 bit, 2 bytes
s_xword		= 3			; 24 bit, 3 bytes
s_dword		= 4			; 32 bit, 4 bytes
;
s_ptr		= s_word		; Pointer	, 16 bit, 2 bytes
s_dptr		= s_word * 2		; Double pointer, 32 bit, 4 bytes
s_rampage	= $0100			; 65xx size of RAM page (256 bytes)
s_bank		= $ffff			; 65816 size of RAM bank (65536 bytes)
;
s_mpudbrx	= s_byte		; Size of Data Bank register
s_mpudpx	= s_word		; Size of Direct Page register
s_mpupbx	= s_byte		; Size of Program Bank register
s_mpupcx	= s_word		; Size of Program Counter register
s_mpuspx	= s_word		; Size of Stack Pointer register
s_mpusrx	= s_byte		; Size of Processor Status register
;
;---------------------------------------
; Flags
;---------------------------------------
; 	Status Register
;		   +----------> 1 = result = negative
;		   |+---------> 1 = sign overflow
;		   ||           1 = 8 bit .A & memory
;		   ||+--------> 0 = 16 bit .A & memory
;		   |||          1 = 8 bit index
;		   |||+-------> 0 = 16 bit index
;		   ||||         1 = decimal arithmetic mode
;		   ||||+------> 0 = binary arithmetic mode
;		   |||||+-----> 1 = IRQ disabled
;		   ||||||+----> 1 = result = zero
;		   |||||||+---> 1 = carry set/generated
;		   ||||||||
;		   NVmxDIZC
;---------------
sr_carry	= %0000001		; C => carry
sr_zero		= sr_carry 	<< 1	; Z => zero
sr_irq		= sr_zero 	<< 1	; I => IRQ
sr_bdm		= sr_irq 	<< 1	; D => decimal mode
sr_ixw		= sr_bdm	<< 1	; x => index register width
sr_amw		= sr_ixw	<< 1	; m => memory and .A width
sr_ovl		= sr_amw	<< 1	; V => overflow
sr_neg		= sr_ovl	<< 1	; N => negative
;---------------
; 	Status Register - Inverted
;---------------
sr_carry_i	= sr_carry ^ %11111111	; C
sr_zero_i	= sr_zero  ^ %11111111	; Z
sr_irq_i	= sr_irq   ^ %11111111	; I
sr_bdm_i	= sr_bdm   ^ %11111111	; D
sr_ixw_i	= sr_ixw   ^ %11111111	; x
sr_amw_i	= sr_amw   ^ %11111111	; m
sr_ovl_i	= sr_ovl   ^ %11111111	; V
sr_neg_i	= sr_neg   ^ %11111111	; N
;
;---------------------------------------
; Constants
;---------------------------------------
BELL		= $07			; <Bell>
BS		= $08			; <Backspace>
HTAB		= $09			; <Tab>
LF		= $0A			; <LF>
CR		= $0D			; <CR>
SPACE		= $20			; <Space>
DEL		= $7F			; <Del>
;
;---------------------------------------
; Pointers/addresses
;---------------------------------------
;	SUPERMON816
;---------------
DPYHEX		= $DFAD			; Display contents of .A as byte
NEWLINE		= $DFC4			; Go to new line
;---------------
;	ROM
;---------------
VECTOR_INT	= $02D8			; /IRQ jump vector
CHKSUM_INT	= $02DA			; /IRQ jump vector checksum
CHAR_OUT	= $FD0C			; Current Character Out jump routine
;
.endif
;
;=======================================
; 	DRIVER
;=======================================
; Size data types
;---------------------------------------
s_uptime	= s_dword		; Uptime ticker	, 32 bit, 4 bytes
;
s_e32_buf	= s_rampage / 2		; Size of a UART I/O buffer
s_e32_space	= s_e32_buf * n_e32_buf	; Total space reserved for UART I/O
					; buffers
;
;---------------------------------------
; Flags
;---------------------------------------
;	Channel TxD status
; 	Only A and B are used, rest is
;	already defined for future use
e32_tx_sta	= %00000001		; Channel A TxD status
e32_tx_stb	= e32_tx_sta	<< 1	; Channel B TxD status
e32_tx_stc	= e32_tx_stb	<< 1	; Channel C TxD status
e32_tx_std	= e32_tx_stc	<< 1	; Channel D TxD status
e32_tx_ste	= e32_tx_std	<< 1	; Channel E TxD status
e32_tx_stf	= e32_tx_ste	<< 1	; Channel F TxD status
e32_tx_stg	= e32_tx_stf	<< 1	; Channel G TxD status
e32_tx_sth	= e32_tx_stg	<< 1	; Channel H TxD status
;
;buf_idx_mask	= s_e32_buf ^ %01111111	; Buffer index wrap mask 
buf_idx_mask	= s_e32_buf - 1		; Buffer index wrap mask (as per BDD)
;
; 28L9x driver status flags
nx_driver_init	= %10000000		; Driver initialization status
nx_txa_dis	= %01000000		; TxD channel A flag
nx_txb_dis	= %00100000		; TxD channel B flag
;
;	SR masks
msk_sr_rx_rdy	= %00000001		; RHR not empty mask
msk_sr_rx_full	= %00000010		; RHR full empty
msk_sr_tx_rdy	= %00000100		; THR not full mask
msk_sr_tx_empty	= %00001000		; THR empty mask
;
;	IRQ masks
;---------------
msk_irq_txa	= %00000001		; Channel A TxRDY IRQ mask
msk_irq_rxa	= %00000010		; Channel A RxRDY IRQ mask
msk_irq_ct	= %00001000		; Counter Ready Mask
msk_irq_txb	= %00010000		; Channel B TxRDY IRQ mask
msk_irq_rxb	= %00100000		; Channel B RxRDY IRQ mask
;
;---------------------------------------
; Constants
;---------------------------------------
n_chan_9x	= 2			; Number of channels
n_reg_9x	= 8			; Number of registers per channel
n_tot_reg_9x	= n_chan_9x * n_reg_9x	; Total number of registers
;
nx_q_size	= s_rampage / 2		; Size of a ring buffer (127)
nx_q_base	= $2000			; For now we define our queues at $2000
nx_rx_qa	= nx_q_base		; RxD queue channel A
nx_tx_qa	= nx_rx_qa + nx_q_size	; TxD queue channel A
nx_rx_qb	= nx_tx_qa + nx_q_size	; RxD queue channel B
nx_tx_qb	= nx_rx_qb + nx_q_size	; TxD queue channel B
;
n_e32_buf	= 4			; # of UART I/O buffers
;
x1_freq		= 3686400		; Clock freq of X1 in Hz
nx_ct_scale	= x1_freq / 2		; C/T scaled clock
hz		= 100			; Frequency of C/T IRQs (in Hz)
;
;---------------------------------------
; Pointers/addresses
;---------------------------------------
; 	Zeropage / Direct Page
;---------------
nx_zeropage	= $60			; We use ZP from $60
nx_jiffycnt	= nx_zeropage		; Jiffy counter
;---------------
;	Uptime is little endian!
;---------------
nx_uptimecnt	= nx_jiffycnt  + s_byte	; 32 bit uptime counter
;
;	RxD and TxD GET and PUT indexes channel A
nx_rx_get_a	= nx_uptimecnt + s_dword; 
nx_tx_get_a	= nx_rx_get_a  + s_byte	;
nx_rx_put_a	= nx_tx_get_a  + s_byte	;
nx_tx_put_a	= nx_rx_put_a  + s_byte	;
;
;	RxD and TxD GET and PUT indexes channel B
nx_rx_get_b	= nx_tx_put_a  + s_byte	;
nx_tx_get_b	= nx_rx_get_b  + s_byte	;
nx_rx_put_b	= nx_tx_get_b  + s_byte	;
nx_tx_put_b	= nx_rx_put_b  + s_byte	;
;
nx_tx_status	= nx_tx_put_b + s_byte	; Driver TxD status register
;
rom_irq		= nx_tx_status + s_byte	; Store original IRQ jump vector!
;---------------
; 	I/O
;---------------
.if	NX_USERDRIVER
NX_IOBASE	= $C400			; NXP 28L92 is based in slot #4 
					; (userland) driver
.else
;
NX_IOBASE	= UART_IOBASE
.endif
;
;---------------------------------------
; Data Registers
;---------------------------------------
;	Offsets to IO_BASE
;---------------
dr_mra		= %0000			; Mode Register A
dr_sra		= %0001			; Status Register a
dr_rxfifoa	= %0011			; Rx Holding Register A
dr_ipcr		= %0100			; Input Port Change Register
dr_isr		= %0101			; Interrupt Status Register
dr_ctu		= %0110			; Counter/Timer (MSB)
dr_ctl		= %0111			; Counter/Timer (LSB)
dr_mrb		= %1000			; Mode Register B
dr_srb		= %1001			; Status Register B
dr_rxfifob	= %1011			; Rx Holding Register B
dr_misc		= %1100			; Miscellaneous Register (Intel mode)
dr_ipr		= %1101			; Input Port Register
dr_cnt_start	= %1110			; Start counter command
dr_cnt_stop	= %1111			; Stop counter command
; During write
;dr_mra	= %0000				; Mode Register A (Same when Reading)
dr_csra		= %0001			; Clock Select Register A
dr_cra		= %0010			; Command Register A
dr_txfifoa	= %0011			; Tx Holding Register A
dr_acr		= %0100			; Auxiliary Control Register
dr_imr		= %0101			; Interrupt Mask Register
dr_ctpu		= %0110			; Counter/Timer Upper Preset Reg (MSB)
dr_ctpl		= %0111			; Counter/Timer Lower Preset Reg (LSB)
;dr_mrb		= %1000			; Mode Register B (Same when Reading)
dr_csrb		= %1001			; Clock Select Register B
dr_crb		= %1010			; Command Register B
dr_txfifob	= %1011			; Tx Holding Register B
;dr_misc	= %1100			; Miscellaneous Register (Intel mode)
dr_opcr		= %1101			; Output Port Configuration Register
dr_sopr		= %1110			; Set Output Port Bits Command
dr_ropr		= %1111			; Reset Output Port Bits Command
;---------------------------------------
; 29L9x Registers (explained)
;---------------------------------------
;	MISC - Miscellaneous Register (Intel mode)
;
; We only explain the possible flags
;
nx_misc_def	= %00000000
;		   xxxxxxxx
;		   ||||||||
;		   |||+++++---> z: Unused
;		   |||
;		   ||+--------> 1: Disable transmitter channel B
;		   |+---------> 1: Disable transmitter channel A
;		   +----------> 1: Driver initialized
;---------------
; 	ACR - Auxiliary Control Register
;---------------
;nx_acr_def	= %01100000
nx_acr_def	= %11100000
;		   xxxxxxxx
;		   ||||||||
;		   |||||||+---> 1: enable IP0 IRQ
;		   ||||||+----> 1: enable IP1 IRQ
;		   |||||+-----> 1: enable IP2 IRQ
;		   ||||+------> 1: enable IP3 IRQ
;		   |+++-------> C/T setup:
;		   |
;		   |            654  Mode     Source
;		   |            -------------------------------
;		   |            000  counter  IP2
;		   |            001  counter  TxD Ch 1 1X clock
;		   |            010  counter  TxD Ch 2 1X clock
;		   |            011  counter  Xtal/16
;		   |            100  timer    IP2
;		   |            101  timer    IP2/16
;		   |            110  timer    Xtal
;		   |            111  timer    Xtal/16
;		   |            -------------------------------
;		   |
;		   +----------> 0: select BRG set #1 (38.4k max)
;		                1: select BRG set #2 (19.2k max)
;
;---------------
; 	CR - Command Register
;---------------
nx_cr_rx_ena	= %00000001		; Enable receiver
nx_cr_rx_dis	= %00000010		; Disable receiver
;
nx_cr_tx_ena	= %00000100		; Enable transmitter
nx_cr_tx_dis	= %00001000		; Disable transmitter
;
nx_cr_mr1	= %00010000		; Select MR1
;
nx_cr_rx_res	= %00100000		; Reset receiver
nx_cr_tx_res	= %00110000		; Reset transmitter
nx_cr_err_res	= %01000000		; Reset error status
nx_cr_bir_res	= %01010000		; Reset Received Break Change IRQ
;
nx_cr_brk_sta	= %01100000		; Start break
nx_cr_brk_stp	= %01110000		; Stop break
;
nx_cr_rts_ass	= %10000000		; Assert RTS
nx_cr_rts_dea	= %10010000		; Deassert RTS
;
nx_cr_tmr_mod	= %10100000		; Select C/T timer mode
;
nx_cr_mr0	= %10110000		; Select MR0
;
nx_cr_cnt_mod	= %11000000		; Select C/T counter mode
;
nx_cr_pwr_dwn	= %11100000		; Power down mode	(Port A only)
nx_cr_pwr_up	= %11110000		; Normal power mode	(Port A only)
;
;	Combined CR commands
nx_cr_rxtx_ena	= nx_cr_rx_ena | nx_cr_tx_ena
nx_cr_rxtx_dis	= nx_cr_rx_dis | nx_cr_tx_dis
;
;---------------
; 	CSR - Clock Select Register
;---------------
nx_csr		= %11001100		; RxD and TxD at 38k4 baud
					; MR[0] = 0 & ACR[7] = 0
;---------------
; 	CT - Counter / Timer
;---------------
nx_cnt_lo	= <(nx_ct_scale/hz)	; Underflows/sec LSB
nx_cnt_hi	= >(nx_ct_scale/hz)	; Underflows/sec MSB
;
;---------------
; 	IMR - Interrupt Mask Register
;---------------
					; Enable channel A RxD and TxD IRQs
nx_irq_a	= msk_irq_txa | msk_irq_rxa
					; Enable channel B RxD and TxD IRQs
nx_irq_b	= msk_irq_txb | msk_irq_rxb
					; Set IRQ Sources (Port A, B and C/T)
nx_irq_msk	= nx_irq_a | nx_irq_b | msk_irq_ct
;
;---------------
; 	MR0 - Mode 0 Register
;---------------
;nx_mr0		= %11001000
nx_mr0		= %11001001
;		   ||||||||
;		   |||||+++----> Baud rate: (See datasheet table 24 and 35)
;		   |||||	 		000 - Normal mode
;		   |||||			001 - Extended mode I
;		   |||||			100 - Extended mode II
;		   ||||+-------> 16-deep FIFO
;		   ||++--------> TxD interrupts only when FIFO is empty
;		   |+----------> RxD interrupts only when FIFO is full 
;		   |             (also see MR1[6])
;		   +-----------> Enable RxD watchdog timer
;
;---------------
; 	MR1 - Mode 1 Register
;---------------
nx_mr1		= %11010011
;		   ||||||||
;		   ||||||++----> 8 bit character size
;		   |||||+------> Parity type (ignored)
;		   |||++-------> No parity generated or checked
;		   ||+---------> Character error mode
;		   |+----------> RxD interrupts only when FIFO is full
;		   |		 (See also MR0[6]
;		   +-----------> RxD controls RTS
;
;---------------
; 	MR2 - Mode 2 Register
;---------------
nx_mr2		= %00010111		; Normal mode, auto RTS
;		   ||||||||
;		   ||||++++---> stop bit length
;		   ||||
;		   |||+-------> TxD CTS mode:  	 0: off
;		   |||                         	 1: on
;		   ||+--------> TxD RTS mode:  	 0: off
;		   ||                          	 1: on
;		   ++---------> channel mode: 	00: normal
;	           		                01: auto echo 
;	                           		10: local loop
;	                           		11: remote loop
;
;---------------
; 	OPCR - Output Port Configuration Register
;---------------
nx_opcr_def	= %11110000		; OP4-7 as IRQ outputs


