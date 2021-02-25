;
; Adria global equations, macros, etc
;
; Version history:
;	05 Jan 2021	- Initial version
;	08 feb 2021	- Some minor changes to interrupt handlers
;	25 feb 2021	- Consolidate ZP usage (try to make it one block)
;=======================================================================
; Assembly related variables (for conditional assembly)
;---------------------------------------
OSCCLK		=	4		; Clock speed of main oscillator
SYSCLK		=	OSCCLK / 4	; Clock speed of PHI2

;---------------------------------------
; Macros
;---------------------------------------
	.macro	bios_version
	.byte	'0'			; Major
	.byte	'.'
	.byte	'1'			; Minor
	.byte	'.'
	.byte	'3'			; Revision
	.endmacro
	
	.macro	longi			; Set .X and .Y to 16-bit
	rep	#%00010000
	.i16
	.endmacro
	
	.macro	shorti			; Set .X and .Y to 8-bit
	sep	#%00010000
	.i8
	.endmacro
	
	.macro	longa			; Set .A to 16-bit
	rep	#%00100000		
	.a16
	.endmacro
	
	.macro	shorta			; Set .A to 8-bit
	sep	#%00100000
	.a8
	.endmacro
	
	.macro	longr			; Set all registers to 16-bit
	rep	#%00110000		; Set 16-bit .A, .X and .Y
	.a16
	.i16
	.endmacro
	
	.macro	shortr			; Set all registers to 8-bit
	sep	#%00110000		; Set 8-bit .A, .X and .Y
	.a8
	.i8
	.endmacro

;---------------------------------------
; Constants
;---------------------------------------
_constants:
BELL		= $07			; <Bell>	/ <Ctrl-G>
BS		= $08			; <Backspace> 	/ <Ctrl-H>
HT		= $09			; <Tab>		/ <Ctrl-I>
LF		= $0A			; <LF>
CR		= $0D			; <CR>
ESC		= $1B			; <Esc>
SPACE		= $20			; <Space>
DEL		= $7f			; <Del>

a_asclcl	= 'a'			; Start of lowercase ASCII
a_asclch	= 'z'			; End of lowercase ASCII
a_lctouc	= $5F			; Lower to Upper conversion mask

PROMPT		= '#'			; Our monitor prompt

;---------------------------------------
; Data type sizes
;---------------------------------------
_data_sizes:
s_bi_nibble	= 4			; Nibble		(4 bits)
s_bi_byte	= 8			; Byte			(8 bits)
;
;s_byte		= 1			; Byte 			(8 bits)
;s_word		= 2			; Word 			(16 bits)
;s_xword	= 3			; Extended word 	(24 bits)
;s_dword	= 4			; Double word		(32 bits
;s_rampage	= $0100			; 65xx RAM page size	(256 bytes)
               
; Registers
;s_dbr		= s_byte		; Data Bank Register size
;s_dp		= s_word		; Direct Page size
;s_pbr		= s_byte		; Program Bank Register size
;s_pc		= s_word		; Program Counter size
;s_sp		= s_word		; Stack Pointer size
;s_sr		= s_byte		; Status/Processor Register size

; Misc
;s_address	= s_xword		; 24-bit address
               
;---------------------------------------
; Status register flags
;---------------------------------------
;sr_carry	= %00000001		; Carry
;sr_zero		= sr_carry 	<< 1	; Zero
;sr_irq		= sr_zero	<< 1	; IRQ
;sr_dbm		= sr_irq   	<< 1	; Decimal mode
;sr_ixw		= sr_dbm	<< 1	; Index width (native)
      					;	0 - 16 bit .X and .Y
       					;	1 -  8 bit .X and .Y
       					; Break (emulation)
;sr_amw		= sr_ixw   	<< 1	; Memory, .A width (native)
       					;	0 - 16 bit .A
       					;	1 -  8 bit .A
;sr_ovl		= sr_amw   	<< 1	; Overflow
;sr_neg		= sr_ovl   	<< 1	; Negative
               
;---------------------------------------
; Systemwide equations
;---------------------------------------
_equations:
zero_page	= $80			; We use ZP/DP $80-$FF for monitor
kbdbuffer	= $0200			; We want to create a 127 byte buffer?
stack		= $0100			; Default stack in native mode
               
;---------------------------------------
; Zero Page/Direct Page usage
;---------------------------------------
;_zp_usage:
;registers
;reg_pbr		= zero_page		; $80 - .PBR     - Program Bank Register
;reg_pc		= reg_pbr  + s_pbr	; $81 - .PC      - Program Counter
;reg_sr		= reg_pc   + s_pc	; $83 - .SR      - Status/Processor
;       					;		   Register
;reg_a		= reg_sr   + s_sr	; $84 - .C       - 16 bit Accumulator
;       					;	- .B MSB
;       					;	- .A LSB - 8 bit Accumulator
;reg_x		= reg_a    + s_word	; $86 - .X       - 16 bit X register
;reg_y		= reg_x    + s_word	; $88 - .Y       - 16 bit Y register
;reg_sp		= reg_y    + s_word	; $8A - .SP      - Stack Pointer
;reg_dp		= reg_sp   + s_sp	; $8C - .DP      - Direct Page
;reg_dbr		= reg_dp   + s_dp	; $8e - .DBR     - Data Bank Register
;               
;General Workspace
;addr_a		= reg_dbr  + s_dbr	; $8f - Address #1 placeholder
;addr_b		= addr_a   + s_address	; $92 - Address #2 placeholder
;kbdbuffer_ptr	= addr_b   + s_address	; $95 - Keyboard buffer index pointer
;sysbrqvector	= kbdbuffer_ptr + s_word; $98 - Original (system) BRQ vector

;---------------------------------------
; Vector location(s)
;---------------------------------------
_vectors:

;---------------------------------------
; Reset vector location(s)
;---------------------------------------
_reset_vectors:				; All vectors should have a
VECTOR_RES	=	$02D0		;  - 2-byte address
CHKSUM_RES	=	$02D2		;  - 2-byte checksum

VECTOR_NMI	=	$02D4
CHKSUM_NMI	=	$02D6

VECTOR_INT	=	$02D8
CHKSUM_INT	=	$02DA

VECTOR_BRK	=	$02DC
CHKSUM_BRK	=	$02DE
;---------------------------------------
; Input/output vector location(s)
;---------------------------------------
_io_vectors:				; We expect the I/O vectors to
VECTOR_CHAR_OUT	=	$02E0		; be 2 bytes wide, without
VECTOR_CHAR_IN	=	$02E2		; checksum(s)
VECTOR_CHAR_SCAN=	$02E4

;---------------------------------------
; Slot/device assignments
;---------------------------------------
_io_assignments:
_ACIA:					; ACIA (UART) will be mapped to
UART_IOBASE	=	$C020
; OLD: TO BE REMOVED
;ACIA_DATA	=	$C020		; slot #1 (/IOSEL0) at address
;ACIA_STATUS	=	$C021		; $00/C020..$00/C02F with the help
;ACIA_CMD	=	$C022		; from the GALs
;ACIA_CTL	=	$C023

;---------------------------------------
; Device driver entry point(s)
; ROM routines
;---------------------------------------
_intel_upload:
HEXINTEL	=	$D000

_sys_monitor:
SYSMONITOR	=	$D800

_device_drivers:
UART_DRIVER	=	$CD00

_isr:
ISR		=	$FC00

_jumptables:
JUMP		= 	$FCA0

_ROM_routines:
RESET_ROUTINES	=	$FD00
