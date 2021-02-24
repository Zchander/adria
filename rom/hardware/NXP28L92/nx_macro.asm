;===============================================================================
;   A D R I A   - Philips/NXP 28L9x Macro file
;
; 	This source file holds the global macro definitions to ease the
;	development of the drivre for the Philips/NXP 28L9x IC.
;
;	Version history:
;		26 jan 2021 	- Inital version
;		18 feb 2021 	- Userland driver seems to work, migrating to
;				  ROM based driver
;===============================================================================
; 	Version information	
;---------------------------------------
	.macro	nx_9x_version
	.byte	'0'			; Major
	.byte	'.'
	.byte	'2'			; Minor
	.byte	'.'
	.byte	'0'			; Revision
	.endmacro
;
.if	NX_USERDRIVER	= 1
;
;---------------------------------------
;	Register width selection
;---------------------------------------
	.macro	longi			; Set .X and .Y to 16-bit
	rep	#%00010000
	.i16
	.endmacro
;	
	.macro	shorti			; Set .X and .Y to 8-bit
	sep	#%00010000
	.i8
	.endmacro
;	
	.macro	longa			; Set .A to 16-bit
	rep	#%00100000		
	.a16
	.endmacro
;	
	.macro	shorta			; Set .A to 8-bit
	sep	#%00100000
	.a8
	.endmacro
;	
	.macro	longr			; Set all registers to 16-bit
	rep	#%00110000		; Set 16-bit .A, .X and .Y
	.a16
	.i16
	.endmacro
;	
	.macro	shortr			; Set all registers to 8-bit
	sep	#%00110000		; Set 8-bit .A, .X and .Y
	.a8
	.i8
	.endmacro
.endif
