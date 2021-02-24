;===============================================================================
;   A D R I A   - Philips/NXP 28L9x Driver skeleton
;
; 	This source file is the base skeleton for the driver for the 
;	Philips/NXP 28L9x IC.
;
;	This driver is (largely) based on the driver by 
;	BDD (sbc.bcstechnology.net) and his documents "28l91 Driving" and 
;	"28L92 Interfacing".
;	(Most of this driver is copied from/based on his work!)
;
;	Written for use with the CC65 assembler.
;
;	Version history:
;		26 jan 2021 	- Inital version
;		18 feb 2021 	- Userland driver seems to work, migrating to
;				  ROM based driver
;				- For now, channel B ISR is INCOMPLETE
;				- Added blocking input routine for channel A
;-------------------------------------------------------------------------------
; For the userland driver we use the unique functions names when accessing the
; frontend of the driver (e.g. routines available to users).
; for the ROM based driver, we the more "generic" names. This is to avoid major 
; rewrites.
; Also the userland driver was/is loaded at $00/4000. The ROM based driver will 
; be loaded at $00/FB00.
;
; ----------------------
; Routine name "translation" table
; ----------------------
; Userland		ROM
; ______________	________________
; nx_init		uart_init	- Initialize driver
; nx_sioget_a		uart_scan	- Non blocking input (scan)
; nx_sioget_a_block	uart_input	- Blocking input (non-scan!)
; nx_sioput_a		uart_output	- Output
; nx_sioget_b		
; nx_sioput_b
;
; Userland or ROM driver
;-----------------------
; If we want to assemble the userland driver, set NX_USERDRIVER to 1, otherwise
; set to =
NX_USERDRIVER		= 0
;===============================================================================
;	Linker directives
;---------------------------------------
	.p816
	.listbytes	unlimited
	.smart 		+
;	
.if	NX_USERDRIVER 	= 1
	UART_DRIVER	= $4000		; Load address for userland
.endif
;
	.org		UART_DRIVER
;
;---------------------------------------
;	Includes to sub-modules
;---------------------------------------
	.include	"hardware/NXP28L92/nx_macro.asm"
	.include	"hardware/NXP28L92/nx_global.asm"
	.include	"hardware/NXP28L92/nx_frontend.asm"
;
; Interrupt Service handler has been moved to the "generic" ROM environment
;	.include	"interrupt.asm"
	.include	"hardware/NXP28L92/nx_init.asm"
	.include	"hardware/NXP28L92/nx_tables.asm"
;
;---------------------------------------
; Used to calculate memory footprint of the module
;---------------------------------------
; End of routines
; Calculation of usage
_uart_end	=	*
_uart_size 	=	_uart_end - UART_DRIVER
	.out .concat("UART driver                       $", .sprintf("%04x", UART_DRIVER), "      $", .sprintf("%04x", _uart_end), "    $", .sprintf("%04x", _uart_size), "  (", .sprintf("%05d", _uart_size), ")")
;	.out	.concat("---- Size of UART (28L92) routines:   $", .sprintf("%04x", _uart_size), "(", .sprintf("%05d", _uart_size), ")")
;=======================================================================
