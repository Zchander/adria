;
; Adria main glue file
;
; Version history:
;	05 Jan 2021	- Initial version
;	16 jan 2021	- Added comment about use of WDC65C51 with known bug for
;			  Rx status bit stuck
;	18 feb 2021	- In SPRINT we now save the 16 bit .Y to the
;			  stack, before actually printing char. When 
;			  returned, we reset the register widths and
;			  retrieve the .Y
;
;
; BEFORE USAGE, SET THE CORRECT OSCILLATOR CLOCK SPEED IN "global.asm"
;=======================================================================
; Assembler target selection
	.p816				; Build for 65816 CPU
	
	.listbytes	unlimited	; Show all bytes (on Linux/macOS)
;	.listbytes	255		; Show max 255 bytes on Windows
	.smart		+		; Let the assembler act smart
	
	.a8				; But for now, start in 8-bit
	.i8
	.debuginfo			; Generate label information
	
;=======================================================================
; Assembler/linker directives
;=======================================================================
; Data segment - Not here (yet)
;---------------------------------------
	.data
	
;---------------------------------------
; Code segment
;---------------------------------------
	.code
	.org	$C000			; The ROM starts at $00/C000
	
;---------------------------------------
; Include global equations, macros, etc
;---------------------------------------
	.include	"config/global.asm"

;###################
; Fill "empty" space(s)/unused addresses with $00
;###################
; Size table header row
;###################
	.out .concat("                                  Start      End      Size   (Decimal)")
	.out .concat("                                  -----      ----     -----  ---------")

; UART driver(s)
;=======================
; In order to support multiple UART options (e.g. 6551, NXP SC28L92), replace
; the included filename for the desired solution.
;=======================
_incl_uart:	
	.res		UART_DRIVER - *, $00
;	.include	"hardware/6551/6551.asm"
	.include	"hardware/NXP28L92/NXP28L92.asm"
	
; Intel Hex Uploader	
_incl_intel:
	.res		HEXINTEL - *, $00
	.include	"apps/hexintel.asm"
	
; Supermon 816
_incl_monitor:
	.res		SYSMONITOR - *, $00
	.include	"supermon816/monitor.asm"

; Interrupt Service Routines
_incl_isr:
	.res		ISR - *, $00
	.include	"ROM/isr.asm"
	
; Fixed jump tables (not functional (yet?)
;_incl_jump:
;	.res		JUMP - *, $00
;	.include	"ROM/jumptable.asm"
	
; Reset handler(s)
_incl_reset:	
	.res		RESET_ROUTINES - *, $00
	.include	"ROM/reset.asm"
	
