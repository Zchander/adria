;==============================================================================
;  A D R I A 
;  A modular computer
;
;  SUPERMON 816 implemtation
;
;==============================================================================
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                                                                                 *
;*      SUPERMON 816 MACHINE LANGUAGE MONITOR FOR THE W65C816S MICROPROCESSOR      *
;* ------------------------------------------------------------------------------- *
;*      Copyright ©1991-2014 by BCS Technology Limited.  All rights reserved.      *
;*                                                                                 *
;* Permission is hereby granted to use, copy, modify and distribute this software, *
;* provided this copyright notice remains in the source code and  proper  attribu- *
;* tion is given.  Redistribution, regardless of form, must be at no charge to the *
;* end  user.  This code or any part thereof, including any derivation, MAY NOT be *
;* incorporated into any package intended for sale,  unless written permission has * 
;* been given by the copyright holder.                                             *
;*                                                                                 *
;* THERE IS NO WARRANTY OF ANY KIND WITH THIS SOFTWARE.  The user assumes all risk *
;* in connection with the incorporation of this software into any system.          *
;* ------------------------------------------------------------------------------- *
;* Supermon 816 is a salute to Jim Butterfield, who passed away on June 29, 2007.  *
;*                                                                                 *
;* Jim, who was the unofficial  spokesman for  Commodore  International during the *  
;* heyday of the company's 8 bit supremacy, scratch-developed the Supermon machine *
;* language monitor for the PET & CBM computers.   When the best-selling Commodore *
;* 64 was introduced, Jim adapted his software to the new machine & gave the adap- *
;* tation the name Supermon 64.   Commodore  subsequently  integrated a customized *
;* version of Supermon 64 into the C-128 to act as the resident M/L monitor.       *
;*                                                                                 *
;* Although Supermon 816 is not an adaptation of Supermon 64,  it was  decided  to *
;* keep the Supermon name alive, since Supermon 816's general operation & user in- *
;* terface is similar to that of Supermon 64.   Supermon 816 is 100 percent native *
;* mode 65C816 code & was developed from a blank canvas.                           *
;* ------------------------------------------------------------------------------- *
;* Supermon 816 is a full featured monitor and supports the following operations:  *
;*                                                                                 *
;*     A - Assemble code                                                           *
;*     C - Compare memory regions                                                  *
;*     D - Disassemble code                                                        *
;*     F - Fill memory region (cannot span banks)                                  *
;*     G - Execute code (stops at BRK)                                             *
;*     H - Search (hunt) memory region                                             *
;*     J - Execute code as a subroutine (stops at BRK or RTS)                      *
;*     M - Dump & display memory range                                             *
;*     R - Dump & display 65C816 registers                                         *
;*     T - Copy (transfer) memory region                                           *
;*     X - Exit Supermon 816 & return to operating environment                     *
;*     > - Modify up to 32 bytes of memory                                         *
;*     ; - Modify 65C816 registers                                                 *
;*                                                                                 *
;* Supermon 816 accepts binary (%), octal (%), decimal (+) and hexadecimal ($) as  *
;* input for numeric parameters.  Additionally, the H and > operations accept an   *
;* ASCII string in place of numeric values by preceding the string with ', e.g.:   *
;*                                                                                 *
;*     h 042000 042FFF 'BCS Technology Limited                                     *
;*                                                                                 *
;* If no radix symbol is entered hex is assumed.                                   *
;*                                                                                 *
;* Numeric conversion is also available.  For example, typing:                     *
;*                                                                                 *
;*     +1234567 <CR>                                                               *
;*                                                                                 *
;* will display:                                                                   *
;*                                                                                 *
;*         $12D687                                                                 *
;*         +1234567                                                                *
;*         %04553207                                                               *
;*         %100101101011010000111                                                  *
;*                                                                                 *
;* In the above example, <CR> means the console keyboard's return or enter key.    *
;*                                                                                 *
;* All numeric values are internally processed as 32 bit unsigned integers.  Addr- *
;* esses may be entered as 8, 16 or 24 bit values.  During instruction assembly,   *
;* immediate mode operands may be forced to 16 bits by preceding the operand with  *
;* an exclamation point if the instruction can accept a 16 bit operand, e.g.:      *
;*                                                                                 *
;*     a 1f2000 lda !#4                                                            *
;*                                                                                 *
;* The above will assemble as:                                                     *
;*                                                                                 *
;*     A 1F2000  A9 04 00     LDA #$0004                                           *
;*                                                                                 *
;* Entering:                                                                       *
;*                                                                                 *
;*     a 1f2000 ldx !#+157                                                         *
;*                                                                                 *
;* will assemble as:                                                               *
;*                                                                                 *
;*     A 1F2000  A2 9D 00     LDX #$009D                                           *
;*                                                                                 *
;* Absent the ! in the operand field, the above would have been assembled as:      *
;*                                                                                 *
;*     A 1F2000  A2 9D        LDX #$9D                                             *
;*                                                                                 *
;* If an immediate mode operand is greater than $FF assembly of a 16 bit operand   *
;* is implied.                                                                     *
;* ------------------------------------------------------------------------------- *
;* A Note on the PEA & PEI Instructions                                            *
;* ------------------------------------                                            *
;*                                                                                 *
;* The Eyes and Lichty programming manual uses the following syntax for the PEA    *
;* and PEI instructions:                                                           *
;*                                                                                 *
;*     PEA <operand>                                                               *
;*     PEI (<operand>)                                                             *
;*                                                                                 *
;* The WDC data sheet that was published at the time of the 65C816's release in    *
;* 1984 does not indicate a recommended or preferred syntax for any of the above   *
;* instructions.  PEA pushes its operand to the stack and hence operates like any  *
;* other immediate mode instruction, in that the operand is the data (however, PEA *
;* doesn't affect the status register).  Similarly, PEI pushes the 16 bit value    *
;* stored at <operand> and <operand>+1, and hence operates like any other direct   *
;* (zero) page instruction, again without affecting the status register.           *
;*                                                                                 *
;* BCS Technology Limited is of the opinion that the developer of the ORCA/M as-   *
;* sembler, which is the assembler referred to in the Eyes and Lichty manual, mis- *
;* understood how PEA and PEI behave during runtime, and hence chose an incorrect  *
;* syntax for these two instructions.  This error was subsequently carried forward *
;* by Eyes and Lichty.                                                             *
;*                                                                                 *
;* Supermon 816's assembler uses the following syntax for PEA and PEI:             *
;*                                                                                 *
;*     PEA #<operand>                                                              *
;*     PEI <operand>                                                               *
;*                                                                                 *
;* The operand for PEA is treated as a 16 bit value, even if entered as an 8 bit   *
;* value.  The operand for PEI must be 8 bits.                                     *
;*                                                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;==============================================================================
; (Additional assembler/linker directives)
	.listbytes	255
	.p816
	
	.macro softvers                 ; software version — change with each 
					; revision...
	.byte "1"             		; major
	.byte "."
	.byte "0"             		; minor
	.byte "."
	.byte "4"             		; revision
	.endmacro
         
;------------------------------------------------------------------------------
; Revision Table
;------------------------------------------------------------------------------
; Version	Revision Date	Description
;------------------------------------------------------------------------------
;  1.0  	09 jun 2020  	A) Original derived from the Supermon 816 by
;				   BCS Technology Limited
;				B) Used the firmware for 65C816 ECB SBC by 
;				   RetroBrew Computers as additional reference
; 				C) Adaption for CA65 (assembler of CC65)
;  1.0.1	10 nov 2020	A) Added 8-bit multiplication (8-bit or 16-bit 
;				   product)
;			 	   Added 8-bit division (from 8-bit or 16-bit 
;				   divisor, with 8-bit quotient, dividend and 
;				   remainder)
;  1.0.2	10 jan 2021	A) Implementation for Adria
;  1.0.3	18 feb 2021	A) In SPRINT we now save the 16 bit .Y to the
;				   stack, before actually printing char. When 
;				   returned, we reset the register widths and
;				   retrieve the .Y
;  1.0.4	08 mar 2021	A) line 680 - Corrected value of mne_waix. Was
;				   $21, but should be #32
;				B) line 4244 - Forgot to branch when stopkey was
;				   found
;  1.0.4	18 mar 2021	A) Moved hwstack reference to global.asm (ROM)
;------------------------------------------------------------------------------
;
;
;	        COMMENT ABBREVIATIONS
;	----------------------------------------------------
;	  BCD   binary-coded decimal
;	   DP   direct page or page zero
;	  EOF   end-of-field
;	  EOI   end-of-input
;	  LSB   least significant byte/bit
;	  LSD   least significant digit
;	  LSN   least significant nybble
;	  LSW   least significant word
;	  MPU   microprocessor
;	  MSB   most significant byte/bit
;	  MSD   most significant digit
;	  MSN   most significant nybble
;	  MSW   most-significant word
;	  RAM   random access memory
;	   WS   whitespace, i.e., blanks & horizontal tabs
;	----------------------------------------------------
;	A word is defined as 16 bits.
;
;	   MPU REGISTER SYMBOLS
;	--------------------------
;	   .A   accumulator LSB
;	   .B   accumulator MSB
;	   .C   16 bit accumulator
;	   .X   X-index
;	   .Y   Y-index
;	   DB   data bank
;	   DP   direct page
;	   PB   program bank
;	   PC   program counter
;	   SP   stack pointer
;	   SR   MPU status
;	----------------------------
;
;	  MPU STATUS REGISTER SYMBOLS
;	-------------------------------
;	    C   carry
;	    D   decimal mode
;	    I   maskable interrupts
;	    m   accumulator/memory size
;	    N   result negative
;	    V   sign overflow
;	    x   index registers size
;	    Z   result zero
;	-------------------------------
;
;==============================================================================
;
;SYSTEM INTERFACE DEFINITIONS
;
;	------------------------------------------------------------------
;	This section defines the interface between Supermon 816 & the host
;	system.   Change these definitions to suit your system, but do not
;	change any label names.  All definitions must have valid values in
;	order to assemble Supermon 816.
;	------------------------------------------------------------------
;
;	--------------------------------------------------------
ORIGIN:
	.org	SYSMONITOR
;   
;	Set _ORIGIN_ to Supermon 816's desired assembly address.
;	--------------------------------------------------------
;
;	------------------------------------------------------------------------
;vecexit  .EQU $002000              ;exit to environment address...
;
;	Set VECEXIT to where Supermon 816 should go when it exits.  Supermon 816
;	will do a JML (long jump) to this address, which means VECEXIT must be a
;	24 bit address.
;	------------------------------------------------------------------------
;
;	------------------------------------------------------------------------
;
;getcha                  ;get keystroke from console...
;
;	GETCHA refers to an operating system API call that returns a keystroke
;	in the 8 bit accumulator.  Supermon 816  assumes that GETCHA is a non-
;	blocking subroutine & returns with carry clear to indicate that a key-
;	stroke is in .A, or with carry set to indicate that no keystroke was
;	available.  GETCHA will be called with a JSR instruction.
;
;	Supermon 816 expects .X & .Y to be preserved upon return from GETCHA.
;	You may have to modify Supermon 816 at all calls to GETCHA if your "get
;	keystroke" routine works differently than described.
;	------------------------------------------------------------------------
;getcha		=  CHAR_IN	
getcha		=  char_scan

;------------------------------------------------------------------------
;putcha   print character on console...
;
;	PUTCHA refers to an operating system API call that prints a character to
;	the console screen.  The character to be printed will be in .A, which
;	will be set to 8-bit width.  Supermon 816 assumes that PUTCHA will block
;	until the character can be processed.  PUTCHA will be called with a JSR
;	instructions.
;
;	Supermon 816 expects .X & .Y to be preserved upon return from PUTCHA.
;	You may have to modify Supermon 816 at all calls to PUTCHA if your "put
;	character" routine works differently than described.
;
putcha 		= char_out
;
;	------------------------------------------------------------------------
;
;	------------------------------------------------------------------------
vecbrki  	= VECTOR_BRK           ;BRK handler indirect vector...
;
;	Supermon 816 will modify this vector so that execution of a BRK instruc-
;	tion is intercepted & the registers  are  captured.   Your BRK front end
;	should jump through this vector after pushing the registers as follows:
;
;	         phb                   ;save DB
;	         phd                   ;save DP
;	         rep #%00110000        ;16 bit registers
;	         pha
;	         phx
;	         phy
;	         jmp (vecbrki)         ;indirect vector
;
;	When a G or J command is issued, the above sequence will be reversed be-
;	fore a jump is made to the code to be executed.  Upon exit from Supermon
;	816, the original address at VECBRKI will be restored.
;
;	If your BRK front end doesn't conform to the above you will have to mod-
;	ify Supermon 816 to accommodate the differences.  The most likely needed
;	changes will be in the order in which registers are pushed to the stack.
;	------------------------------------------------------------------------
;
;	------------------------------------------------------------------------
;hwstack  	= $7fff                ;top of hardware stack...
;
;	Supermon 816 initializes the stack pointer to this address when the cold
;	start at MONCOLD is called to enter the monitor.  The stack pointer will
;	be undisturbed when entry into Supermon 816 is through JMONBRK (see jump
;	table definitions).
;	------------------------------------------------------------------------
;
;	------------------------------------------------------------------------
zeropage 	= $80                  ;Supermon 816's direct page...
;
;	Supermon 816 uses direct page starting at this address.  Be sure that no
;	conflict occurs with other software.
;	------------------------------------------------------------------------
;
;	------------------------------------------------------------------------
stopkey  	= $03                  ;display abort key...
;
;	Supermon 816 will poll for a "stop key" during display operations, such
;	as code disassembly & memory dumps, so as to abort further processing &
;	return to the command prompt.  STOPKEY must be defined with the ASCII
;	value that the "stop key" will emit when typed.  The polling is via a
;	call to GETCHA (described above).  The default STOPKEY definition of $03
;	is for ASCII <ETX> or [Ctrl-C].
;	------------------------------------------------------------------------
;
ibuffer  	= $000200               ;input buffer &...
auxbuf   	= ibuffer+s_ibuf+s_byte ;auxiliary buffer...
;
;	------------------------------------------------------------------------
;	Supermon 816 will use the above definitions for input buffers.  These
;	buffers may be located anywhere in RAM that is convenient.  The buffers
;	are stateless, which means that unless Supermon 816 has control of your
;	system, they may be overwritten without consequence.
;	------------------------------------------------------------------------
;
;===============================================================================
; Originally the Supermon 816 source holds macros for the (additional) 65816
; opcodes, but CA65 already supports them
;===============================================================================
; Also all macros have moved to global.asm
;===============================================================================
; Data type sizes
;------------------------------------------------------------------------------
s_byte		= 1			; Byte
s_word		= 2			; Word (16 bits)
s_xword		= 3			; Extended word (24 bits)
s_dword		= 4			; Double word (32 bits)
s_rampage	= $0100			; 65xx RAM page size
;------------------------------------------------------------------------------
; Data type sizes in bits
;------------------------------------------------------------------------------
s_bibyte	= 8			; Byte
s_binibble	= 4			; Nibble/Nybble
;------------------------------------------------------------------------------
; 65C816 Native mode status register definitions
;------------------------------------------------------------------------------
s_mpudbrx	= s_byte		; Databank size
s_mpudpx	= s_word		; Direct Page size
s_mpupbrx	= s_byte		; Program Bank size
s_mpupcx	= s_word		; Program Counter size
s_mpuspx	= s_word		; Stack Pointer size
s_mpusrx	= s_byte		; Status Register size
;------------------------------------------------------------------------------
; Status register flags
;------------------------------------------------------------------------------
sr_Carry	= %00000001		; C
sr_Zero		= sr_Carry <<1		; Z
sr_IRQ		= sr_Zero  <<1		; I
sr_dbm		= sr_IRQ   <<1		; D
sr_ixw		= sr_dbm   <<1		; x
sr_amw		= sr_ixw   <<1		; m
sr_ovl		= sr_amw   <<1		; V
sr_neg		= sr_ovl   <<1		; N
;	NVmxDIZC
;	xxxxxxxx
;	||||||||
;	|||||||+---> 1 = carry set/generated
;	||||||+----> 1 = result = zero
;	|||||+-----> 1 = IRQs ignored
;	||||+------> 0 = binary arithmetic mode
;	||||         1 = decimal arithmetic mode
;	|||+-------> 0 = 16 bit index
;	|||          1 = 8 bit index
;	||+--------> 0 = 16 bit .A & memory
;	||           1 = 8 bit .A & memory
;	|+---------> 1 = sign overflow
;	+----------> 1 = result = negative
;
;------------------------------------------------------------------------------
; "Size-of" constants
;------------------------------------------------------------------------------
s_address	= s_xword		; 24-bit address
s_auxBuffer	= 32			; Auxiliary buffer (32 bytes)
s_ibuf		= 69			; Input buffer (69 bytes)
s_mnemonic	= 3			; MPU ASCII Menumonic size
s_mnepck	= 2			; MPU Encoded mnemonic size
s_mvInstruc	= 3			; MVN/MVP instruction
s_opcode	= s_byte		; MPU opcode
s_oper		= s_xword		; Operand
s_pfac		= s_dword		; Primary math accumulator
s_sfac		= s_dword+s_word	; Secondary math accumulators
;------------------------------------------------------------------------------
; "Number-of" constants
;------------------------------------------------------------------------------
n_dBytes	= 21			; Default disassembly bytes (21)
n_dump		= 16			; Bytes per memory dump line (16)
n_mBytes	= s_rampage-1		; Default memory dump bytes
n_hccols	= 10			; Compare/hunt display columns (10)
n_opcols	= 3 * s_oper		; Disassembly operand columns
n_opsLSR	= 4			; LSRs to extract instruction size
n_shfEncode	= 5			; Shifts to encode/decode mnemonic
;------------------------------------------------------------------------------
; Numeric conversion constants
;------------------------------------------------------------------------------
a_hexdec	= 'A'-'9'-2		; Hex to decimal differences
c_bin		= '%'			; Binary prefix
c_dec		= '+'			; Decimal prefix
c_hex		= '$'			; Hexadecimal prefix
c_oct		= '@'			; Octal prefix
k_hex		= 'f'			; Hex ASCII conversion
m_bits		= s_pfac * s_bibyte	; Operand bit size
m_cbits		= s_sfac * s_bibyte	; Workspace bit size
bcdumask	= %00001111		; Isolate BCD units mask
btoamask	= %00110000		; Binary to ASCII mask	
;------------------------------------------------------------------------------
; Assembler/Disassembler constants
;------------------------------------------------------------------------------
a_mnecvt	= '?'			; Encoded mnemonic conversion base
aimmaska	= %00011111		; .A immediate opcode test #1
aimmaskb	= %00001001		; .A immediate opcode test #2
asmprfx		= 'A'			; Assemble code prefix
ascprmct	= 9			; Assembler prompt "size-of"
disprfx		= '.'			; Disassemble code prefix
flimmask	= %11000000		; Force long immediate flag
opc_cpxi	= $e0			; CPX # opcode
opc_cpyi	= $c0			; CPY # opcode
opc_ldxi	= $a2			; LDX # opcode
opc_ldyi	= $a0			; LDY # opcode
opc_mvn		= $54			; MVN opcode
opc_mvp		= $44			; MVP opcode
opc_rep		= $c2			; REP opcode
opc_sep		= $e2			; SEP opcode
pfxmxmask	= sr_amw | sr_ixw	; MPU m & x flag bits mask
;------------------------------------------------------------------------------
; Assembler prompt buffer offsets
;------------------------------------------------------------------------------
apadrbkh	= s_word		; Instruction address bank MSN
apadrbkl	= apadrbkh + s_byte	; Instruction address bank LSN
apadrmbh	= apadrbkl + s_byte	; Instruction address MSB MSN
apadrmbl	= apadrmbh + s_byte	; instruction address MSB LSN
apadrlbh	= apadrmbl + s_byte	; instruction address LSB MSN
apadrlbl	= apadrlbh + s_byte	; Instruction address LSB MSN
;------------------------------------------------------------------------------
; Addressing mode preamble symbols
;------------------------------------------------------------------------------
amp_flim	= '!'			; Force long immediate
amp_imm		= '#'			; Immediate
amp_ind		= '('			; Indirect
amp_indl	= '['			; Indirect Long
;------------------------------------------------------------------------------
; Addressing mode symbolic translation indices
;------------------------------------------------------------------------------
am_nam		= %0000			; No symbol
am_imm		= %0001			; #
am_adrx		= %0010			; <addr>, X
am_adry		= %0011			; <addr>, Y
am_ind		= %0100			; (<addr>)
am_indl		= %0101			; [<dp>]
am_indly	= %0110			; [<dp>], Y
am_indx		= %0111			; (<addr>, X)
am_indy		= %1000			; (<dp>), Y
am_stk		= %1001			; <offset>, S
am_stky		= %1010			; (<offset>, S), Y
am_move		= %1011			; <srcbank>, <destbank>
;------------------------------------------------------------------------------
; Operand size translation indices
;------------------------------------------------------------------------------
ops0		= %0000 << 4		; No operand
ops1		= %0001 << 4		; 8 Bit operand
ops2		= %0010 << 4		; 16 Bit operand
ops3		= %0011 << 4		; 24 Bit operand
bop1		= %0101 << 4		; 8 Bit relative branch
bop2		= %0110 << 4		; 16 Bit relative branch
vops		= %1001 << 4		; 8 or 16 Bit operand
;------------------------------------------------------------------------------
; operand size and addressing mode extraction masks
;------------------------------------------------------------------------------
amodmask	= %00001111		; Addressing mode index
opsmask		= %00110000		; Operand size
vopsmask	= %11000000		; BOPS & VOPS flag bits
;------------------------------------------------------------------------------
; Instruction mnemonic encoding
;------------------------------------------------------------------------------
mne_adc		= $2144			; ADC
mne_and		= $2bc4			; AND
mne_asl		= $6d04			; ASL
mne_bcc		= $2106			; BCC
mne_bcs		= $a106			; BCS
mne_beq		= $9186			; BCC
mne_bit		= $aa86			; BIT
mne_bmi		= $5386			; BMI
mne_bne		= $33c6			; BNE
mne_bpl		= $6c46			; BPL
mne_bra		= $14c6			; BRA
mne_brk		= $64c6			; BRK
mne_brl		= $6cc6			; BRL
mne_bvc		= $25c6			; BVC
mne_bvs		= $a5c6			; BVS
mne_clc		= $2348			; CLC
mne_cld		= $2b48			; CLD
mne_cli		= $5348			; CLI
mne_clv		= $bb48			; CLV
mne_cmp		= $8b88			; CMP
mne_cop		= $8c08			; COP
mne_cpx		= $cc48			; CPX
mne_cpy		= $d448			; CPY
mne_dec		= $218a			; DEC
mne_dex		= $c98a			; DEX
mne_dey		= $d18a			; DEY
mne_eor		= $9c0c			; EOR
mne_inc		= $23d4			; INC
mne_inx		= $cbd4			; INX
mne_iny		= $d3d4			; INY
mne_jml		= $6b96			; JML
mne_jmp		= $8b96			; JMP
mne_jsl		= $6d16			; JSL
mne_jsr		= $9d16			; JSR
mne_lda		= $115a			; LDA
mne_ldx		= $c95a			; LDX
mne_ldy		= $d15a			; LDY
mne_lsr		= $9d1a			; LSR
mne_mvn		= $7ddc			; MVN
mne_mvp		= $8ddc			; MVP
mne_nop		= $8c1e			; NOP
mne_ora		= $14e0			; ORA
mne_pea		= $11a2			; PEA
mne_pei		= $51a2			; PEI
mne_per		= $99a2			; PER
mne_pha		= $1262			; PHA
mne_phb		= $1a62			; PHB
mne_phd		= $2a62			; PHD
mne_phk		= $6262			; PHK
mne_php		= $8a62			; PHP
mne_phx		= $ca62			; PHX
mne_phy		= $d262			; PHY
mne_pla		= $1362			; PLA
mne_plb		= $1b62			; PLB
mne_pld		= $2b62			; PLD
mne_plp		= $8b62			; PLP
mne_plx		= $cb62			; PLX
mne_ply		= $d362			; PLY
mne_rep		= $89a6			; REP
mne_rol		= $6c26			; ROL
mne_ror		= $9c26			; ROR
mne_rti		= $5566			; RTI
mne_rtl		= $6d66			; RTL
mne_rts		= $a566			; RTS
mne_sbc		= $20e8			; SBC
mne_sec		= $21a8			; SEC
mne_sed		= $29a8			; SED
mne_sei		= $51a8			; SEI
mne_sep		= $89a8			; SEP
mne_sta		= $1568			; STA
mne_stp		= $8d68			; STP
mne_stx		= $cd68			; STX
mne_sty		= $d568			; STY
mne_stz		= $dd68			; STZ
mne_tax		= $c8aa			; TAX
mne_tay		= $d0aa			; TAY
mne_tcd		= $292a			; TCD
mne_tcs		= $a12a			; TCS
mne_tdc		= $216a			; TDC
mne_trb		= $1cea			; TRB
mne_tsb		= $1d2a			; TSB
mne_tsc		= $252a			; TSC
mne_tsx		= $cd2a			; TSX
mne_txa		= $166a			; TXA
mne_txs		= $a66a			; TXS
mne_txy		= $d66a			; TXY
mne_tya		= $166a			; TYA
mne_tyx		= $ceaa			; TYX
mne_wai		= $50b0			; WAI
mne_wdm		= $7170			; WDM
mne_xba		= $10f2			; XBA
mne_xce		= $3132			; XCE
;------------------------------------------------------------------------------
; Encoded instruction mnemonic indices
;------------------------------------------------------------------------------
mne_adcx	= 16			; ADC
mne_andx	= 29			; AND
mne_aslx	= 44			; ASL
mne_bccx	= 15			; BCC
mne_bcsx	= 65			; BCS
mne_beqx	= 59			; BCC
mne_bitx	= 70			; BIT
mne_bmix	= 36			; BMI
mne_bnex	= 31			; BNE
mne_bplx	= 42			; BPL
mne_brax	= 5			; BRA
mne_brkx	= 39			; BRK
mne_brlx	= 43			; BRL
mne_bvcx	= 23			; BVC
mne_bvsx	= 68			; BVS
mne_clcx	= 20			; CLC
mne_cldx	= 27			; CLD
mne_clix	= 35			; CLI
mne_clvx	= 71			; CLV
mne_cmpx	= 53			; CMP
mne_copx	= 55			; COP
mne_cpxx	= 78			; CPX
mne_cpyx	= 88			; CPY
mne_decx	= 18			; DEC
mne_dexx	= 74			; DEX
mne_deyx	= 84			; DEY
mne_eorx	= 61			; EOR
mne_incx	= 21			; INC
mne_inxx	= 77			; INX
mne_inyx	= 87			; INY
mne_jmlx	= 40			; JML
mne_jmpx	= 54			; JMP
mne_jslx	= 45			; JSL
mne_jsrx	= 63			; JSR
mne_ldax	= 1			; LDA
mne_ldxx	= 73			; LDX
mne_ldyx	= 83			; LDY
mne_lsrx	= 64			; LSR
mne_mvnx	= 48			; MVN
mne_mvpx	= 58			; MVP
mne_nopx	= 56			; NOP
mne_orax	= 6			; ORA
mne_peax	= 2			; PEA
mne_peix	= 33			; PEI
mne_perx	= 60			; PER
mne_phax	= 3			; PHA
mne_phbx	= 10			; PHB
mne_phdx	= 26			; PHD
mne_phkx	= 38			; PHK
mne_phpx	= 51			; PHP
mne_phxx	= 75			; PHX
mne_phyx	= 85			; PHY
mne_plax	= 4			; PLA
mne_plbx	= 11			; PLB
mne_pldx	= 28			; PLD
mne_plpx	= 52			; PLP
mne_plxx	= 76			; PLX
mne_plyx	= 86			; PLY
mne_repx	= 49			; REP
mne_rolx	= 41			; ROL
mne_rorx	= 62			; ROR
mne_rtix	= 37			; RTI
mne_rtlx	= 46			; RTL
mne_rtsx	= 67			; RTS
mne_sbcx	= 14			; SBC
mne_secx	= 19			; SEC
mne_sedx	= 25			; SED
mne_seix	= 34			; SEI
mne_sepx	= 50			; SEP
mne_stax	= 7			; STA
mne_stpx	= 57			; STP
mne_stxx	= 80			; STX
mne_styx	= 89			; STY
mne_stzx	= 91			; STZ
mne_taxx	= 72			; TAX
mne_tayx	= 82			; TAY
mne_tcdx	= 24			; TCD
mne_tcsx	= 66			; TCS
mne_tdcx	= 17			; TDC
mne_trbx	= 12			; TRB
mne_tsbx	= 13			; TSB
mne_tscx	= 22			; TSC
mne_tsxx	= 79			; TSX
mne_txax	= 8			; TXA
mne_txsx	= 69			; TXS
mne_txyx	= 90			; XXY
mne_tyax	= 9			; TYA
mne_tyxx	= 81			; TYX
mne_waix	= 32			; WAI
mne_wdmx	= 47			; WDM
mne_xbax	= 0			; XBA
mne_xcex	= 30			; XCE
;------------------------------------------------------------------------------
; Miscellaneous constants
;------------------------------------------------------------------------------
halfTab		= 4			; 1/2 tabulation spacing
memPrefix	= '>'			; Memory dump prefix
memSepChar	= ':'			; Memory dump separator
memSubChar	= '.'			; Memory dump non-print character
srInit		= %00110000		; SR initialization value
;------------------------------------------------------------------------------
; Direct page usage/storage
;------------------------------------------------------------------------------
reg_pbx		= zeropage		; $80 - PB (Program Bank register)
reg_pcx		= reg_pbx + s_mpupbrx	; $81 - PC (Program Counter)
reg_srx		= reg_pcx + s_mpupcx	; $83 - SR (MPU status)
reg_ax		= reg_srx + s_mpusrx	; $84 - .C (.B (MSB) .A (LSB) register(s))
reg_xx		= reg_ax  + s_word	; $86 - .X (.X register)
reg_yx		= reg_xx  + s_word	; $88 - .Y (.Y register)
reg_spx		= reg_yx  + s_word	; $8a - SP (Stack pointer)
reg_dpx		= reg_spx + s_mpuspx	; $8c - DP (Direct Page)
reg_dbx		= reg_dpx + s_mpudpx	; $8e - DB (Data Bank register)
;------------------------------------------------------------------------------
; General workspace
;------------------------------------------------------------------------------
addra		= reg_dbx + s_mpudbrx	; $8f - Address #1
addrb		= addra   + s_address	; $92 - Address #2
faca		= addrb   + s_address	; $95 - Primary accumulator
facax		= faca    + s_pfac	; $99 - Extended primary accumulator
facb		= facax   + s_pfac	; $9d - Seconday accumulator
facc		= facb    + s_sfac	; $a1 - Tertiary accumulator
operand		= facc    + s_sfac	; $a5 - Instruction operand
auxbufindex	= operand + s_oper	; $a8 - Auxiliary buffer index
ibufidx		= auxbufindex + s_byte	; $a9 - Input buffer index
bitsdig		= ibufidx + s_byte	; $aa - Bits per numeral
numeral		= bitsdig + s_byte	; $ab - Numeral buffer
radix		= numeral + s_byte	; $ac - Radix index
admodidx	= radix   + s_byte	; $ad - Addressing mode index
charcnt		= admodidx  + s_byte	; $ae - Character count
instsize	= charcnt + s_word	; $b0 - Instruction size
mnepck		= instsize  + s_word	; $b2 - Encoded mnemonic
opcode		= mnepck  + s_mnepck	; $b4 - Current opcode
status		= opcode  + s_byte	; $b5 - I/O status flag
xrtemp		= status  + s_byte	; $b6 - Temporary .X storage (16-bit)
eopsize		= xrtemp  + s_byte	; $b8 - Entered operand size
flimflag	= eopsize + s_byte	; $b9 - Forced long immediate
vecbrkia	= flimflag  + s_byte	; $ba - System indirect BRK vector
;------------------------------------------------------------------------------
; During assembly, FLIMFLAG indicates the operand size used with an immediate
; mode instruction, thus causing the following disassembly to display the
; assembled operand size. During disassembly, FLIMFLAG will mirror the effect
; of the most recent REP or SEP instruction.
;------------------------------------------------------------------------------
iopsize		= vecbrkia + s_word	; $bc - Operand size
range		= iopsize + s_byte	; $bd - Allowable radix range
vopsflag	= range   + s_byte	; $be - VOPS & ROPS mode bits
;------------------------------------------------------------------------------
; Copy/fill workspace (overlap with some of above)
;------------------------------------------------------------------------------
mcftwork	= faca			; $95 - Start of copy/fill code
mcftopc		= mcftwork  + s_byte	; $96 - Instruction opcode
mcftbnk		= mcftopc   + s_byte	; $97 - Banks
;------------------------------------------------------------------------------
; Math temporary storage
;------------------------------------------------------------------------------
add1L		= vopsflag  + s_byte
add1H		= add1L     + s_byte
add2L		= add1L     + s_word
add2H		= add2L     + s_byte
sum1L		= add2L     + s_word
sum1H		= sum1L     + s_byte
;
;------------------------------------------------------------------------------
;   S U P E R M O N   8 1 6   J U M P   T A B L E
;------------------------------------------------------------------------------
	.org	SYSMONITOR
JMON:
	bra 	mon			; Cold start entry
JMONBRK:
	bra 	monbrk			; Software interrupt intercept
	
;------------------------------------------------------------------------------
; Supermon 816 Cold start
;------------------------------------------------------------------------------
mon:
	longa				; Set 16-bit .A
	
	lda	vecbrki			; Get current BRK vector
	cmp	#monbrk			; Pointing at monitor?
;	bne	@continue
;	jmp	monreg			; Yes, ignore cold start
	beq	monreg			; yes, ignore cold start
	
@continue:
	sta	vecbrkia		; Save vector for exit
	
	lda	#monbrk			; Supermon should intercept BRK
	sta	vecbrki			; handler

	shortr				; Set 8-bit registers
	
	ldx	#vopsflag - reg_pbx
@a:
	stz	reg_pbx, X		; Clear DP storage
	dex
	bpl	@a
;------------------------------------------------------------------------------
; Initialize register shadows
;------------------------------------------------------------------------------
	lda	#srInit
	sta	reg_srx			; SR (Status Register)
	
	longa				; 16-bit .A
	
	lda	#hwstack		; Top of hardware stack
	tcs				; Set up SP (Stack Pointer)
	tdc				; Get DP (Direct Page) pointer
	sta	reg_dpx			; ... and save
	lda	#$0000
	
	shorta				; 8-Bit .A

	phk				; Push Program Bank to stack
	pla				; ... and save it
	sta	reg_pbx
	phb				; Push Data Bank to stack
	pla				; ... and save it
	sta	reg_dbx
;------------------------------------------------------------------------------
; Print startup banner
;------------------------------------------------------------------------------	
	pea	mm_entry		; "...ready..."
	bra	moncom
		
;------------------------------------------------------------------------------
; monbrk - Software Interrupt Intercept
;
;  This is the entry point taken when a BRK instruction is executed.  It is
;  assumed that the BRK  handler has pushed the registers to the stack that
;  are not automatically pushed by the MPU in response to BRK.	
;------------------------------------------------------------------------------
monbrk:
;	cld				; Make sure decimal mode is offset
;	clc				; Go to native mode, just to make sure
;	xce
;	phb				; Save DB
;	phd				; Save DP
;	
;	longr				; 16-Bit registers

	cli				; Re-enable interrupts
	ply
	plx
	pla
	
	longr
	
	sta	reg_ax			; Save .C (.B and .A)
	stx	reg_xx			; Save .X
	sty	reg_yx			; Save .Y
	
	shorti				; 8-Bit .X and .Y

	pla				; Save DP (16-bits)
	sta	reg_dpx
	plx				; Save DB (8-bits)
	stx	reg_dbx	
	plx				; Save SR (Processor Status)
	stx	reg_srx
	pla				; Save PC (Program Counter)
	sta	reg_pcx
;	plx				; Save PB
;	stx	reg_pbx
	
;	lda	#hwstack		; Set top of hardware stack
;	tcs				; Set SP (Stack Pointer)
;	lda	#$0000			; Set DP
;	tcd
;	ldx	#$00			; Set DB (Data Bank)
;	phx
;	plb
	shorta
	pla
	sta	reg_pbx			; Save PB
	pea	mm_brk			; *BRK
	
;------------------------------------------------------------------------------
; moncom: Common Entry Pointer
;
; === DO NOT CALL THIS ENTRY POINT ===
;------------------------------------------------------------------------------
moncom:
	jsr	sprint			; Print heading
	
	longa				; Make sure we have a 16-bit .A

	tsc				; get current SP (Stack Pointer)
	sta	reg_spx
	rep	#%11111111		; Clear SR (Processor Status)
	nop				; Added two NOPs after REP
	nop				
	sep	#srInit			; Set default Processor flags
	nop				; Added two NOPs after SEP
	nop
	sec				; Needed for next branch

;------------------------------------------------------------------------------
; monreg: Display MPU Registers
;
; Syntax: R
;------------------------------------------------------------------------------
monreg:
	bcs	@a			; Okay to proceed
	jmp	monerr			; Error if called with a parameter

@a:
	pea	mm_regs
	jsr	sprint			; Display heading
;------------------------------------------------------------------------------
; Display Program bank and Program Counter
;------------------------------------------------------------------------------
	lda	reg_pbx			; First process PB
	jsr	dpyhex			; Display as hex ASCII
	jsr	printspc		; Print interfield-space
	
	longa				; Make sure we have a 16-bit .A

	lda	reg_pcx			; Next show the PC (Program Counter)
	
	shorta				; Return to 8-bit .A

	jsr	dpyhexw			; Go print as 2 byte hex ASCII
	ldx	#$02			; Print 2 spaces ...
	jsr	multspc			; ...for inter-field spacing
;------------------------------------------------------------------------------
; Display SR (Processor Status) bitwise
;------------------------------------------------------------------------------
	ldx	reg_srx
	ldy	#s_bibyte		; Number of bits in a byte (duh!)
@b:
	txa				; Remaining SR bits
	asl				; Get one of them
	tax				; Save remainder
	lda	#'0'			; A clear bit, but ...
	adc	#0			; If carry is set, we get a '1'
	jsr	putcha			; Print the byte
	dey				; Current bit done...
	bne	@b			; Do we have any more?
;------------------------------------------------------------------------------
; Display .C, .X, .Y, SP and DP
;------------------------------------------------------------------------------
@c:
	jsr	printspc
	
	longa				; 16-Bit .A

	lda	reg_ax, y		; Get register value
	
	shorta				; 8-bit .A

	jsr	dpyhexw			; Convert and print
	iny
	iny
	cpy	#reg_dbx-reg_ax		; Are we done processing?
	bcc	@c			; Nope, get next
;------------------------------------------------------------------------------
; Show DB
;------------------------------------------------------------------------------
	jsr	printspc		; One more space
	lda	reg_dbx			; Get DB
	jsr	dpyhex			; Display as hex ASCII
	
;------------------------------------------------------------------------------
; monce: Command Executive
;------------------------------------------------------------------------------
monce:
	shorta				; 8-Bit .A
	
	lda	#$00			; Default buffer index
;
moncea:					; Alternate entry point
	pha

; Migrating to gLCD
;	jsr	lcd_clear
;	pea	mm_monce
;	jsr	lcd_sprint

	pla
	
	shortr				; 8-bit .A, .X and .Y
	
	sta	ibufidx			; (Re)set buffer index
	pea	mm_prmpt
	jsr	sprint			; Display input prompt
	jsr	input			; Wait for some input
	
@a:
	jsr	getcharc		; Read from buffer
	beq	monce			; Reached a terminator, just loop
	
	cmp	#SPACE			; Strip off any leading spaces
	beq	@a
	
	ldx	#n_mpctab - 1		; Number of primary commands
	
@b:
	cmp	mpctab, X		; Search primary command list
	bne	@c
	
	txa				; Get index number
	asl				; Double for offset
	tax
	
	longa				; 16-bit .A

	lda	mpcextab, X		; Command address - 1
	
	pha				; Throw on stack
	
	shorta				; 8-bit .A
	
	jmp	getparm			; Evaluate parameters and ...
					; ... execute command

@c:
	dex
	bpl	@b			; Continue searching primary commands
	ldx	#n_radix - 1		; Number of radices
	
@d:
	cmp	radxtab, X		; Search conversion command list
	bne	@e
	
	jmp	monenv			; Convert and display parameter
@e:
	dex
	bpl	@d
	
;------------------------------------------------------------------------------
; monerr: Common Error handler
;------------------------------------------------------------------------------
monerr:
; Migrating to gLCD
;	jsr	lcd_clear
;	pea	mm_monerr
;	jsr	lcd_sprint

	shorti				; 8-Bit indexes

monerraa:
	jsr	dpyerr			; Indicate an error and ...
	bra	monce			; ... return to main input loop
	
;------------------------------------------------------------------------------
; monasc: Assemble Code
;
;	syntax: A <addr> <mnemonic> [<argument>]
;
;	After a line of code has been successfully assembled it will be disass-
;	embled & displayed,  & the monitor will prompt with the next address to
;	which code may be assembled.
;------------------------------------------------------------------------------
monasc:
	bcc	@b			; Assemble address entered
@a:
	jmp	monerr			; Terminate with error
;------------------------------------------------------------------------------
; Evaluate assembly address
;------------------------------------------------------------------------------
@b:
; Migrating to gLCD
;	jsr	lcd_clear
;	pea	mm_monasc
;	jsr	lcd_sprint
	
	jsr	facasize		; Check address
	cmp	#s_dword		; Range?
	bcs	@a			; Out of range -> error
	
	jsr	facaddra		; Store assembly address
	
;------------------------------------------------------------------------------
; Initialize workspace
;------------------------------------------------------------------------------
	ldx	#s_auxBuffer - s_byte
@c:
	stz	auxbuf, X		; Clear addressing mode buffer
	dex
	bne	@c
	
	lda	#SPACE
	sta	auxbuf			; Preamble placeholder
	jsr	clroper			; Clear operand
	stz	auxbufindex		; Reset addressing mode index
	stz	flimflag		; Clear forced log immediate
	stz	mnepck			; Clear encoded ...
	stz	mnepck + s_byte		; ... mnemonic workspace
	stz	vopsflag		; Clear 8/16 or relative flag
;------------------------------------------------------------------------------
; Encode mnemonic
;------------------------------------------------------------------------------
	ldy	#s_mnemonic		; Expected mnemonic size
@d:
	jsr	getcharw		; Get from buffer w/o whitespace
	bne	@f			; Did we get any?
	
	cpy	#s_mnemonic		; Did we get anything at all?
	bcc	@e			; Yes
	
	jmp	monce			; No, abort further assembly
	
@e:
	jmp	monasc10		; incomplete mnemonic -> error
	
@f:
	sec
	sbc	#a_mnecvt		; ASCII to binary factor
	ldx	#n_shfEncode		; Shifts required to encode
	
@g:
	lsr				; Shift out a bit ...
	ror	mnepck + s_byte		; ... into ...
	ror	mnepck			; ... an encoded mnemonic
	dex
	bne	@g			; Next bit
	
	dey
	bne	@d			; Get next character
;------------------------------------------------------------------------------
; Test for copy instruction
;
;	The MVN & MVP instructions accept two operands & hence have an irregular
;	syntax.  Therefore, special handling is necessary to assemble either of
;	these instructions.
;
;	The official WDC syntax has the programmer entering a pair of 24 bit ad-
;	dresses as operands, with the assembler isolating bits 16-23 to	use as
;	operands.  This formality has been dispensed with in this monitor & the
;	operands are expected to be 8 bit bank values.
;------------------------------------------------------------------------------
	longa				; 16-Bit .A

	lda	mnepck			; Retrieve packed mnemonic
	ldx	#opc_mvn		; MVN opcode
	cmp	#mne_mvn		; Is it MVN?
	beq	monasc01		; Yes
	
	ldx	#opc_mvp		; MVP opcode
	cmp	#mne_mvp		; Is it a MVP, then?
	bne	monasc02		; No?

;------------------------------------------------------------------------------
; Assembly copy instruction
;------------------------------------------------------------------------------
monasc01:
	stx	opcode			; Store relevant opcode
	
	shorta				; 8-Bit .A

	jsr	instdata		; Get instruction data
	stx	eopsize			; Effective operand size
	inx
	stx	instsize		; Instruction size
	ldx	#s_oper - s_word	; operand index
	stx	xrtemp			; Set it
	
@a:
	jsr	ascbin			; Evaluate bank number
	bcs	monasc04		; Conversion error
	beq	monasc04		; Returned nothing -> error
	
	jsr	facasize		; Bank must be ...
	cmp	#s_word			; ... 8 Bits
	bcs	monasc04		; If it isn't -> error
	
	lda	faca			; Retrieve bank
	ldx	xrtemp			; Get operand index
	sta	operand, X		; Store it
	dec	xrtemp			; Index = index - 1
	bpl	@a			; Get destination bank
	
	jsr	getcharr		; There should be no more input
	bne	monasc04		; If there is -> error
	
	jmp	monasc08		; Finish MVN/MVP assembly
	
;------------------------------------------------------------------------------
; Continue normal assembly
;------------------------------------------------------------------------------
monasc02:
	shorta				; Back to 8-bit .A

monasc03:
	jsr	getcharw		; Get next character
	beq	monasc06		; EOI, no argument
	
	cmp	#amp_flim		; Test for forced log immediate
	bne	@a			; No forced long immediate
	
	lda	flimflag		; FLIM already set?
	bne	monasc04		; Yes -> error
	
	lda	#flimmask
	sta	flimflag		; Set flag and ...
	bra	monasc03		; Get next character
	
@a:
	cmp	#amp_imm		; Immidiate mode?
	beq	@b			; Yes
	
	cmp	#amp_ind		; Indirect mode?
	beq	@b			; Yes
	
	cmp	#amp_indl		; Indirect long mode?
	bne	@c			; No
	
@b:
	sta	auxbuf			; Set addressing mode preamble
	inc 	auxbufindex		; Bump aux buffer index and ...
	bra	@d			; ... evaluate operand
	
@c:
	dec	ibufidx			; Position back to character
	
@d:
	jsr	ascbin			; Evaluate operand
	bne	monasc05		; Evaluated...
	
	bcs	monasc04		; Conversion failed -> error
	
	lda	auxbufindex		; No operand... Any preamble?
	beq	monasc06		; No, syntax is okay so far ...
	
monasc04:
	jmp	monasc10		; Abort w/ error

monasc05:
	jsr	facasize		; Size operand
	cmp	#s_dword		; maximum is 8-bits
	bcs	monasc04		; Too big -> error
	
	sta	eopsize			; Save operand size
	jsr	facaoper		; Store operand
	
monasc06:
	dec	ibufidx			; Back to last character
	ldx	auxbufindex		; mode buffer index
	bne	@a			; Preamble in buffer
	
	inx				; Step past preamble position
	
@a:
	jsr	getcharc		; Get a char w/ forced UC
	beq	@c			; EOI (End-Of-Input)
	
	cpx	#s_auxBuffer		; mode buffer full?
	bcs	monasc04		; Yes, too much input -> error
	
@b:
	sta	auxbuf, X		; Store for comparison
	inx
	bne	@a
	
;------------------------------------------------------------------------------
; Evaluate mnemonic
;------------------------------------------------------------------------------
@c:
	ldx	#n_mnemon - 1		; Starting mnemonic index
	
monasc07:
	txa				; Convert index ...
	asl				; ... to offset
	tay				; Now mnemonic table index
	
	longa				; 16-bit .A for 16-bit compare

	lda	mnetab, Y		; Get mnemonic from table
	cmp	mnepck			; Compare to entered mnemonic
	
	shorta				; Back to 8-bit .A

	beq	@b			; Match
	
@a:
	dex				; Try next mnemonic
	bmi	monasc04		; Unknown mnemonic -> error
	
	bra	monasc07		; Kep on going ...
	
@b:
	stx	mnepck			; Save mnemonic index
	txa
	ldx	#$00			; Trial opcode
	
@c:
	cmp	mnetabidx, X		; Search index table ...
	beq	@e			; ... for a match
	
@d:
	inx				; Keep on going until ...
	bne	@c			; ... we searched the entire table
	
	bra	monasc04		; We shouldn't get here :(

;------------------------------------------------------------------------------
;	If the mnemonic index table search fails then there is a coding error
;	somewhere, as every entry in the mnemonic table is supposed to have a
;	matching cardinal index.
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
; Evaluate addressing mode
;------------------------------------------------------------------------------
@e:
	stx	opcode			; Save trial opcode
	jsr	instdata		; Get related instruction data
	sta	vopsflag		; Save 8/16 or relative flag
	stx	iopsize			; Save operand size
	inx
	stx	instsize		; Save instruction size
	ldx	opcode			; Recover trial opcode
	tya				; Addressing mode
	asl				; Create table index
	tay
	
	longa				; 16-Bit .A

	lda	ms_lutab, Y		; Mode lookup table
	sta	addrb			; Set pointer
	
	shorta				; 8-Bit .A

	ldy	#$00
	
@f:
	lda	(addrb), Y		; Table addressing mode
	cmp	auxbuf, Y		; Compare with entered addressing mode
	beq	@h			; Looks okay, so far
	
@g:
	lda	mnepck			; Reload mnemonic index
	bra	@d			; Wrong opcode for addressing mode
	
@h:
	ora	#$00			; Is the last character the terminator?
	beq	@i			; Yes, evaluate operand
	
	iny
	bra	@f			; Keep testing
	
;------------------------------------------------------------------------------
; Evaluate operand
;------------------------------------------------------------------------------
@i:
	lda	eopsize			; Retrieve entered operand size
	bne	@j			; Non-zero
	
	ora	iopsize			; Instruction operand size
	bne	@g			; Wrong opcode -> keep trying
	
	bra	monasc08		; Assembly instruction
	
@j:
	bit	vopsflag		; Is it a branch?
	bvs	@p			; Yes -> evaluate
	
	lda	iopsize			; Instruction operand size
	bit	vopsflag		; Variable size operand allowed?
	bmi	@m			; Yes ...
	
	bit	flimflag		; Was forced immediate set?
	bpl	@k			; No
	
	jmp	monasc10		; Yes -> error
	
@k:
	cmp	eopsize			; Compare to entered operand size
	bcc	@g			; Operand too big
	
	sta	eopsize			; New operand size
	bra	monasc08		; Assemble, otherwise ...
	
@l:
	cmp	eopsize			; Exact size match required
	bne	@g			; Mismatch -> wrong opcode
	
	bra	monasc08		; Assemble
	
;------------------------------------------------------------------------------
; Process variable size immediate mode operand
;------------------------------------------------------------------------------
@m:
	ldx	eopsize			; Entered operand size
	cpx	#s_word			; Check size -> 16 bits?
	bcs	monasc10		; Too big -> error
	
	bit	flimflag		; Forced long immediate?
	bpl	@n			; No
	
	ldx	#s_word			; Promote operand size to ...
	stx	eopsize			; ... 16 bits
	bra	@o
	
@n:
	cpx	#s_word			; 16-Bits?
	bne	@o			; Nope....
	
	ldy	#flimmask		; Yes, so force long
	sty	flimflag		; Immediate disassembly
	
@o:
	ina				; New instruction operand size
	cmp	eopsize			; Compare against eoprand size
	bcc	@g			; Mismatch, cannot assemble
	
	bra	monasc08		; Okay, assemble

;------------------------------------------------------------------------------
; Process relative branch
;------------------------------------------------------------------------------	
@p:
	jsr	targoff			; Compute branch offset
	bcs	monasc10		; Branche out of range -> error
	
	sta	eopsize			; Effectice operand size
	
;------------------------------------------------------------------------------
; Assemble instruction
;------------------------------------------------------------------------------
monasc08:
	lda	opcode			; Get opcode
	sta	[addra]			; Store at assembly address
	ldx	eopsize			; Any operand to process?
	beq	@a			; No
	
	txy				; Also store offset
	
@a:
	dex
	lda	operand, X		; Get operand byte and ...
	sta	[addra], Y		; ... store in memory
	dey
	bne	@a			; Next ...
	
@b:
	lda	#CR
	jsr	putcha			; Return to left margin
	lda	#asmprfx		; Assembly prefix
	jsr	dpycodaa		; Disassemble and display
	
;------------------------------------------------------------------------------
; Prompt for next instruction
;------------------------------------------------------------------------------
monasc09:
	lda	#SPACE
	ldx	#ascprmct - 1
	
@a:
	sta	ibuffer, X		; Prepare buffer for ...
	dex				; ... next instruction
	bpl	@a
	
	lda	#asmprfx		; Assemble code ...
	sta	ibuffer			; prompt prefix
	lda	addra + s_word		; Next instruction address bank
	jsr	binhex			; Convert to ASCII
	sta	ibuffer + apadrbkh	; Store bank MSN in buffer
	stx	ibuffer + apadrbkl	; Store bank LSN in buffer
	lda	addra + s_byte		; Next instruction address MSB
	jsr	binhex			; Convert to ASCII
	sta	ibuffer + apadrmbh	; Store address MSB MSN in buffer
	stx	ibuffer + apadrmbl	; Store address MSB LSN in buffer
	lda	addra			; Next instruction address LSB
	jsr	binhex			; Convert to ASCII
	sta	ibuffer + apadrlbh	; Store address LSB MSN in buffer
	stx	ibuffer + apadrlbl	; Store address LSB LSB in buffer
	lda	#ascprmct		; Effective input count
	jmp	moncea			; Re-enter input loop

;------------------------------------------------------------------------------
; Process assembly error
;------------------------------------------------------------------------------
monasc10:
	jsr	dpyerr			; indicate error and ...
	bra	monasc09		; ... prompt w/ same assembly address
	
;------------------------------------------------------------------------------
; mondsc: Disassemble Code
;
;	syntax: D [<addr1> [<addr2>]]
;------------------------------------------------------------------------------
mondsc:
; Migrating to gLCD
;	jsr	lcd_clear
;	pea	mm_mondsc
;	jsr	lcd_sprint
	
	shortr
	bcs	@a			; No parameters
	
	stz	flimflag		; Reset 8-bit mode
	jsr	facasize		; Check starting ...
	cmp	#s_dword			; ... address
	bcs	@e			; Out of range -> error
	
	jsr	facaddra		; Copy starting address
	jsr	getparm			; Get ending address
	bcc	@b			; We got it!
	
@a:
	jsr	clrfaca			; Clear .A
	
	longa				; 16-bit .A

	clc
	lda	addra			; Get start address
	adc	#n_mBytes		; Default number of bytes
	sta	faca			; Effective ending address
	
	shorta				; Returning to 8-bit .A

	lda	addra + s_word		; Get starting bank
	adc	#$00			; Mind bank boundary crossing
	sta	faca			; Effective ending bank
	bcs	@e			; End address > $FF/FFFF -> error
	
@b:
	jsr	facasize		; Check ending address range
	cmp	#s_dword		; 2 Bytes
	bcs	@e			; Out of range -> error
	
	jsr	facaddrb		; Copy ending address
	jsr	getparm			; Check for excess input
	bcc	@e			; error
	
	jsr	calccnt			; Calculate # bytes to dump
	bcc	@e			; end < start -> error
	
@c:	
	jsr	teststop		; See if display stop requested
	bcs	@d			; Stoppped
	
	jsr	newline			; Next line
	jsr	dpycod			; Display
	jsr	decdcnt			; Decrement byte count
	bcc	@c			; Until we are done ...
	
@d:
	jmp	monce			; back to main loop
	
@e:
	jmp	monerr			; Address range error
	
;------------------------------------------------------------------------------
; monjmp: Execute Code
;
;	syntax: G [<dp>]
;
;	If no address is specified, the current values in the PB & PC
;	shadow registers are used.
;------------------------------------------------------------------------------
monjmp:
; Migrating to gLCD
;	jsr	lcd_clear
;	pea	mm_monjmp
;	jsr	lcd_sprint
	
	jsr	setxaddr		; Set execution address
	bcs	monjmpab		; Out of range -> error
	
	jsr	getparm			; Check for excess input
	bcc	monjmpab		; Too much input -> error
	
	longa				; 16-Bit .A

	lda	reg_spx
	tcs				; Restore SP
	
monjmpaa:
	shorta				; 8-Bit .A

	lda	reg_pbx
	pha				; Restore PB
	
	longa				; 16-Bit .A

	lda	reg_pcx
	pha				; Restore PC
	
	shorta				; 8-Bit .A

	lda	reg_srx
	pha				; Restore SR
	lda	reg_dbx
	pha
	plb				; Restore PB
	
	longr				; 16-Bit registers .A, .X and .Y

	lda	reg_dpx
	tcd				; Restore DP
	lda	reg_ax			; Restore .C
	ldx	reg_xx			; Restore .X
	ldy	reg_yx			; Restore .Y
	rti				; Execute code
	
monjmpab:
	jmp	monerr			; Error
	
;------------------------------------------------------------------------------
; monjsr: Execute Code as Subroutine
;
;	syntax: J [<dp>]
;
;	If no address is specified the current values in the PB & PC
;	shadow registers are used.   An RTS at the end of the called
;	subroutine will return control to the monitor  provided  the
;	stack remains in balance.
;------------------------------------------------------------------------------
monjsr:
	pha
	phx
	
; Migrating to gLCD	
;	jsr	lcd_clear
;	pea	mm_monjsr
;	jsr	lcd_sprint

	plx
	pla
	
	jsr	setxaddr		; Set execution address
	bcs	monjmpab		; Out of range -> error
	
	jsr	getparm			; Check for excess input
	bcc	monjmpab		; Too much input -> error
	
	longa				; 16-Bit .A

	lda	reg_spx
	tcs				; Restore SP and ...
	jsr	monjmpaa		; ... call subroutine
	php				; Push SR
	
	longr				; 16-Bit registers .A, .X and .Y

	sta	reg_ax			; Save ...
	stx	reg_xx			; ... register ...
	sty	reg_yx			; ... returns
	
	shorti				; 8-Bit .X and .Y

	plx				; Get and save ...
	stx	reg_srx			; ... return SR
	tsc				; Get and save ...
	sta	reg_spx			; ... return SP
	tdc				; Get and save ...
	sta	reg_dpx			; ... DP pointer
	
	shorta				; 8-Bit .A
	
	phk				; Get and ...
	pla				; ... save ...
	sta	reg_pbx			; ... return PB
	phb				; Get and ...
	pla				; ... save ...
	sta	reg_dbx			; ... return DB
	pea	mm_rts			; "*RET"
	jmp	moncom			; Return to monitor
	
;------------------------------------------------------------------------------
; monchm: Change and/or Dump Memory
;
;	syntax: > [<addr> <operand> [<operand>]...]
;
;	> <addr> without operands will dump 16 bytes
;	of memory, starting at <addr>.
;------------------------------------------------------------------------------
monchm:
	bcs	@c			; No address given -> quit
	
; Migrating to gLCD
;	jsr	lcd_clear
;	pea	mm_monchm
;	jsr	lcd_sprint
	
	jsr	facasize		; Size address
	cmp	#s_dword
	bcs	@d			; Address out of range -> error
	
	jsr	facaddra		; Get starting address
	jsr	getpat			; Evaluate change pattern
	bcc	@a			; Entered
	
	bpl	@b			; Not entered
	
	bra	@d			; Evaluation error
	
@a:
	dey				; Next byte
	bmi	@b			; Done
	
	lda	auxbuf, Y		; Write pattern ...
	sta	[addra], Y		; ... to memory
	bra	@a			; Next
	
@b:
	jsr	newline			; Next line
	jsr	dpymem			; Regurgitate changes
	
@c:
	jmp	monce			; Back to command loop
	
@d:
	jmp	monerr			; Goto error handler
	
	
;------------------------------------------------------------------------------
; moncmp: Compare Memory
;
;	syntax: C <start> <end> <ref>
;------------------------------------------------------------------------------
moncmp:
	bcs	@c			; Start not given -> quit
	
	jsr	enddest			; get end and reference addresses
	bcs	@d			; Range or other error
	
	stz	xrtemp			; Column counter
	
@a:
	jsr	teststop		; Check for stop
	bcs	@c			; Abort
	
	lda	[addra], Y		; Get from reference location
	cmp	[operand]		; test against compare location
	beq	@b			; Match, don't display address
	
	jsr	dpycaddr		; Display current location
	
@b:
	jsr	nxtaddra		; Next address location
	bcs	@c			; Done
	
	longa				; 16-bit .A

	inc 	operand			; Bump bits 15-0
	
	shorta				; 8-bit .A

	bne	@a
	
	inc	operand + s_word	; Bump bits 23-16 (bank)
	bra	@a
	
@c:
	jmp	monce			; Return to command loop
	
@d:
	jmp	monerr
	
;------------------------------------------------------------------------------
; moncpy: Copy (Transfer) Memory
;
;	syntax: T <start> <end> <target>
;------------------------------------------------------------------------------
moncpy:
	bcs	@d			; Start not given -> quit
	
	jsr	enddest			; Get and set target address
	bcs	@e			; Range or other error
	
	longa				; 16-bit .A

	sec
	lda	addrb			; Ending address
	sbc	addra			; Starting address
	bcs	@e			; Start > End -> error
	
	sta	facb			; Bytes to copy
	
	shorta				; 8-Bit .A

	longi				; 16-bit .X and .Y

	lda	operand + s_word	; Target bank
	ldy	operand			; Target address
	cmp	addra + s_word		; Source bank
	
	longa				; 16-Bit .A

	bne	@b			; Can use forward copy
	
	cpy	addra			; Source address
	bcc	@b			; Can use forward copy
	
	bne	@a			; Must use reverse copy
	
	bra	@e			; Copy in-place -> error
	
@a:
	lda	facb			; Get bytes to copy
	pha				; Protect
	jsr	lodbnk			; Load banks
	jsr	cprvsup			; Do reverse copy setup
	pla				; Get bytes to copy
	tax				; Save a copy
	clc
	adc	operand			; Change target to ...
	tay				; ... target end
	txa				; Recover bytes to copy
	ldx	addrb			; Source end
	bra	@c
	
@b:
	lda	facb			; Get bytes to copy
	pha				; Protect
	jsr	lodbnk			; Load banks
	jsr	cpfwsup			; Do forward copy setup
	pla				; Recover bytes to copy
	ldx	addra			; Source start

@c:
	jmp	mcftwork		; Copy memory
@d:
	jmp	monce			; Return to command loop
	
@e:
	jmp	monerr			; Error
	
;------------------------------------------------------------------------------
; mondmp: Display Memory Range
;
;	syntax: M [<addr1> [<addr2>]]
;------------------------------------------------------------------------------
mondmp:
	shortr
	bcs	@a			; No parameters
	
	jsr	facasize		; Check address ...
	cmp	#s_dword		; ... range
	bcs	@e			; Out of range -> error
	
	jsr	facaddra		; Copy starting address
	jsr	getparm			; Get ending address
	bcc	@b			; Got it
	
@a:
	jsr	clrfaca			; Clear accumulator
	
	longa				; 16-bit .A

	clc
	lda	addra			; Starting address
	adc	#n_mBytes		; Default bytes
	sta	faca			; Effective ending address
	
	shorta				; 8-Bit .A

	lda	addra + s_word		; Starting bank
	adc	#$00
	sta	faca + s_word		; Effective ending bank
	bcs	@e			; Ending address > $FF/FFFF -> error
	
@b:
	jsr	facasize		; Check ending address ...
	cmp	#s_dword		; ... range
	bcs	@e			; Out of range -> error
	
	jsr 	facaddrb		; Copy ending address
	jsr	getparm			; Check for excess input
	bcc	@e			; Error
	
	jsr	calccnt			; Calculate bytes to dump
	bcc	@e			; end < start -> error
	
@c:
	jsr	teststop		; Test for user initiated break
	bcs	@d			; User pressed <Ctrl-C>
	
	jsr	newline			; Next line
	jsr	dpymem			; Display
	jsr	decdcnt			; Decrement byte count
	bcc	@c			; not done, yet ...
	
@d:
	jmp	monce			; Go to main loop

@e:
	jmp	monerr			; Handle error
	

;------------------------------------------------------------------------------
; monfil: Fill Memory
;
;	syntax: F <start> <end> <fill>
;
;	<start> & <end> must be in the same bank.
;------------------------------------------------------------------------------
monfil:
	shortr
	bcs	@a			; Start not given -> quit
	
	jsr	facasize		; Check size
	cmp	#s_dword		; 2 bytes?
	bcs	@b			; Out of range -> error
	
	jsr	facaddra		; Store start address
	jsr	getparm			; Evaluate end address
	bcs	@b			; Not entered -> error
	
	jsr	facasize		; Check size
	cmp	#s_dword		; 2 bytes?
	bcs	@b			; Out of range -> error
	
	lda	faca + s_word		; End bank
	cmp	addra + s_word		; Compare with start bank
	bne	@b			; Not the same -> error
	
	jsr	facaddrb		; Store end address
	
	longa				; 16-Bit .A

	sec
	lda	addrb			; Get end address
	sbc	addra			; Subtract start address
					; (addrb - addra = number of bytes)
	bcc	@b			; Start > end  -> error
	
	sta	facb			; Bytes to copy/fill
	
	shorta				; 8-Bit .A

	jsr	getparm			; Evaluate fill byte
	bcs	@b			; not entered -> error
	
	jsr	facasize		; Fill byte should be ...
	cmp	#s_word			; ... 8 bits
	bcs	@b			; It isn't -> error
	
	jsr	facaoper		; Store fill byte
	jsr	getparm			; There should be no more parameters
	bcc	@b			; Seems there are more -> error
	
	lda	operand			; Get fill byte
	sta	[addra]			; Store at first address location
	
	longr				; 16-bit .A, .X and .Y

	lda	facb			; Get byte count
	beq	@a			; Only one location, done...
	
	dea				; Zero align and ...
	pha				; ... protect
	
	shorta				; 8-Bit .A

	lda	addra + s_word		; Get start bank
	xba
	lda	addrb + s_word		; Get end bank
	jsr	cpfwsup			; Perform forward copy setup
	pla				; Recover fill count
	ldx	addra			; Fill-from start address
	txy
	iny				; Fill-to starting location
	jmp	mcftwork		; Fill memory
	
@a:
	jmp	monce			; Goto command executive
	
@b:
	jmp	monerr			; Goto error handler
	
;------------------------------------------------------------------------------
; monhnt: Search (Hunt) Memory
;
;	syntax: H <addr1> <addr2> <pattern>
;------------------------------------------------------------------------------
monhnt:
	shortr
	bcs	@e			; No start address given -> abort
	
	jsr	facasize		; Get size of starting address
	cmp	#s_dword		; Is it 16 bit wide?
	bcs	@f			; Address is out of range -> error
	
	jsr	facaddra		; Store starting address
	jsr	getparm			; Evaluate ending address
	bcs	@f			; No address -> error
	
	jsr	facasize		; Get size of ending address
	cmp	#s_dword		; Is it also 16 bits wide?
	bcs	@f			; Address is out of range -> error
	
	jsr	facaddrb		; Store ending address
	jsr	calccnt			; Calculate byte range
	bcs	@f			; Ending < starting address -> error
	
	jsr	getpat			; Evaluate search pattern
	bcs	@f			; Unparsable pattern -> error
	
	stz	xrtemp			; Clear column counter
	
@a:
	jsr	teststop		; Check for stop (<Ctrl-C>
	bcs	@e			; Abort... Abort... Abort...
	
	ldy	auxbufindex		; At which index in buffer is pattern

@b:
	dey
	bmi	@c			; Pattern matches
	
	lda	[addra], Y		; Get from memory
	cmp	auxbuf, Y		; test against pattern
	bne	@d			; Not found -> next location
	
	beq	@b			; Match, keep testing
	
@c:
	jsr	dpycaddr		; Display current address
	
@d:
	jsr	nxtaddra		; Get next address
	bcc	@a			; Not done, yet...
	
@e:
	jmp	monce			; Return to a prompt
	
@f:
	jmp	monerr			; Go to error handler
	
;------------------------------------------------------------------------------
; monenv: Convert Numeric Value
;
;	syntax: <radix><value>
;------------------------------------------------------------------------------
monenv:
	shortr
	jsr	getparmr		; Re-read and evaluate parameter(s)
	bcs	@b			; Nothing entered
	
	ldx	#$00			; Radix index
	ldy	#n_radix		; Number of available radices
	
@a:
	phy				; Save counter
	phx				; Save radix index
	jsr	newline			; Next line ...
	jsr	clearlin		; ... and clear line
	lda	#SPACE			; We are going to print <Space>s
	ldx	#halfTab		; Index 1/2 tab
	jsr	multspc			; Print the <Space>s
	plx				; Get radix index back, but ...
	phx				; ... put it backa	
	lda	radxtab, X		; Get radix
	jsr	binasc			; Convert to ASCII
	phy				; String address MSB
	phx				; String address LSB
	jsr	sprint			; Print
	plx				; Get index, again
	ply				; Get counter
	inx
	dey				; All radices handled?
	bne	@a			; No ...
	
@b:
	jmp	monce			; Get back to prompt
	
;------------------------------------------------------------------------------
; monchr: Change Registers
;
;	syntax: ; [PB [PC [.S [.C [.X [.Y [SP [DP [DB]]]]]]]]]
;
;	; with no parameters is the same as the R command.
;------------------------------------------------------------------------------
monchr:
	shortr
	bcs	@d			; No paramaters given, dump registers
					; and return
					
	ldy	#$00			; Registr counter
	sty	facc			; Initialize register index
	
@a:
	jsr	facasize		; Get parameter size
	cmp	rcvltab, Y		; Check against size table
	bcs	@e			; Out of range -> error
	
	lda	rcvltab, Y		; Determine number of bytes ...
	cmp	#s_word + 1		; ... to store
	ror	facc + s_byte		; Condition flag
	bpl	@b			; We are asking for an 8-bit register
	
	longa				; 16-Bit .A

					; As we are asking for a 16-bit register
					
@b:
	ldx	facc			; Get register index
	lda	faca			; Get parameter(s)
	sta	reg_pbx, X		; Put in shadow storage
	
	shorta				; 8-Bit .A requested
	
	asl	facc + s_byte		; Mode flagg to carry
	txa				; Register index
	adc	#s_byte			; At least 1 byte stored
	sta	facc			; Save new index
	jsr	getparm			; Get a parameter
	bcs	@d			; Enf Of Input
	
	iny				; Bump register count
	cpy	#n_regchv		; All registers processed?
	bne	@a			; NOpe, keep on going ...
	
@c:
	jsr	alert			; Excessive input
	
@d:
	jmp	monreg			; Display registers and changes
	
@e:
	jmp	monerr
	
;------------------------------------------------------------------------------
;   C U R R E N T L Y   N O T   I M P L E M E N T E D
;
; monxit: Exit To Operating Environment
;
;	Syntax: X
;
;   C U R R E N T L Y   N O T   I M P L E M E N T E D
;------------------------------------------------------------------------------
;monxit:
;	bcc	@b			; No parameter(s) allowed
;	
;	rep	#$20			; 16-bit .A
;	.a16
;	lda	vecbrki			; BRK indirect vector
;	cmp	#monbrk			; Did we intercept it?
;	bne	@a			; Nope, do not change it
;	
;	lda	vecbrkia		; Get original vector
;	sta	vecbrki			; Restore it
;	stz	vecbrkia		; Invalidate old vector
;	
;@a:
;	sep	#$10			; 8-bit .X and .Y
;	.i8
;	jml	vecexit			; Long jump to exit
;	
;@b:
;	jmp	monerr			; Go to error handler
;	
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;
; # # #               S T A R T   O F   S U B R O U T I N E S             # # #
;------------------------------------------------------------------------------
; mul8_8_16
; 
; Multiply two 8 bit values with 16 bit result
; [add1L] * [add2L] = [sum1H][sum1L]
;
; (routines found at http://6502org.wikidot.com/software-math-intmul
;------------------------------------------------------------------------------
mul8_8_16:
	ldx	#$08			; Process 8 bits
	lda	#$00
	lsr	add1L			; 8-bit factor 1
@l1:
	bcc	@l2
	clc
	adc	add2L			; 8-bit factor 2
@l2:
	ror
	ror	add1L
	dex
	bne	@l1
	sta	sum1H			; High byte of result
	lda	add1L			; Low byte of result
	sta	sum1L
	rts

;------------------------------------------------------------------------------
; mul8_8_8
;
; Multiply two 8 bit values with 8 bit result
; [add1L] * [add2L] = [sum1L]
;
; (routines found at http://6502org.wikidot.com/software-math-intmul
;------------------------------------------------------------------------------
mul8_8_8:
	ldx	#$08			; Process 8 bits
	lda 	#$00
@l1:
	asl
	asl	add1L			; Factor 1
	bcc	@l2
	clc
	adc	add2L			; Fsctor 2
@l2:
	dex
	bne	@l1
	sta	sum1L			; Product
	rts
;------------------------------------------------------------------------------
; div16_8_8
;
; Divide 16-bit valuem 8-bit quotient, 8-bit remainder
; [add1H][add1L] / [add2L] = [sum1L] mod [sum1H]
;
; (routines found at http://6502org.wikidot.com/software-math-intdiv
;------------------------------------------------------------------------------
div16_8_8:
	lda	add1H			; MSB of 16-bit numerator
	ldx	#$08
	asl	add1L			; LSB of numerator
@l1:
	rol
	bcs	@l2
	cmp	add2L
	bcc	@l3
@l2:
	sbc	add2L
	sec				; We need to set Carry when BCS @l2
					; above was taken
@l3:
	rol	add1L
	dex
	bne	@l1
	sta	sum1H			; Remainder
	lda	add1L			; 8-bit quotient
	sta	sum1L
	rts
	
;------------------------------------------------------------------------------
; div8_8_8
;
; Divide 8-bit value, 8-bit quotient, 8-bit remainder
; [add1L] / [add2L] = [sum1L] mod [sum1H]
;
; (routines found at http://6502org.wikidot.com/software-math-intdiv
;------------------------------------------------------------------------------
div8_8_8:
	lda	#$00
	ldx	#$08
	asl	add1L
@l1:
	rol
	cmp	add2L
	bcc	@l2
	sbc	add2L
@l2:
	rol	add1L
	dex
	bne	@l1
	sta	sum1H			; Remainder
	lda	add1L
	sta	sum1L			; Quotient
	rts
	
;------------------------------------------------------------------------------
; dpycaddr: Display Current Address in Columns
;------------------------------------------------------------------------------
dpycaddr:
	ldx	xrtemp			; Column count
	bne	@a			; Not at right side
	
	jsr	newline			; Next row
	ldx	#n_hccols		; Maximum number of columns
	
@a:
	cpx	#n_hccols		; Compare to maximum number of columns
	beq	@b			; Are we at left margin?
	
	lda	#HT			; <Tab>
	jsr	putcha			; Print  <Tab>
	
@b:
	dex				; One less column
	stx	xrtemp			; Save column counter
	jmp	prntladr		; Print reference address
	
;------------------------------------------------------------------------------
; dpycod: Disassemble and Display Code
;
;	This function disassembles & displays the machine code at  the  location
;	pointed to by ADDRA.  Upon return, ADDRA will point to the opcode of the
;	next instruction.   The entry point at DPYCODAA  should be called with a
;	disassembly prefix character loaded in .A.   If entered  at  DPYCOD, the
;	default character will be display at the beginning of each  disassembled
;	instruction.
;
;	The disassembly of immediate mode instructions that can take an 8 or  16
;	bit operand is affected by the bit pattern that is  stored  in  FLIMFLAG
;	upon entry to this function:
;
;	    FLIMFLAG: xx000000
;	              ||
;	              |+---------> 0:  8 bit .X or .Y operand
;	              |            1: 16 bit .X or .Y operand
;	              +----------> 0:  8 bit .A or BIT # operand
;	                           1: 16 bit .A or BIT # operand
;
;	FLIMFLAG is conditioned according to the operand of  the  most  recently
;	disassembled REP or SEP instruction.   Hence repetitive  calls  to  this
;	subroutine will usually result in the correct disassembly of 16 bit imm-
;	ediate mode instructions.
;------------------------------------------------------------------------------
dpycod:
	shortr
	lda	#disprfx		; Default prefix	
;------------------------------------------------------------------------------
; Alternate prefix display entry point
;------------------------------------------------------------------------------
dpycodaa:
	jsr	putcha			; Print prefix
	jsr	printspc		; Print <Space>
	jsr	prntladr		; Print long address
	jsr	printspc		; Print another <Space>
	jsr	getbyte			; Get opcode
	sta	opcode			; Save ...
	jsr	printbyt		; ... display as hex	
;------------------------------------------------------------------------------
; Decode mnemonic and addressing mode
;------------------------------------------------------------------------------
	ldx	opcode			; Current mnemonic
	lda	mnetabidx, X		; Get mnemonic index
	asl				; Double for ...
	tay				; ... mnemonic tabel offset
	
	longa				; 16-bit .A

	lda	mnetab, Y		; Copy encoded mnemonic to ...
	sta	mnepck			; ... working storage
	
	shorta				; Back to 8-bit .A

	jsr	instdata		; Extract mode and size data
	sta	vopsflag		; Save mode flags
	sty	admodidx		; Save mode index
	asl				; Variable immediate instruction?
	bcc	dpycod01
;------------------------------------------------------------------------------
; Determine immediate mode operand size
;------------------------------------------------------------------------------
	lda	opcode			; Current opcode
	bit	flimflag		; Operand display mode
	bpl	@a			; 8-bit .A & BIT immediate mode
	
	and	#aimmaska		; Determine if ...
	cmp	#aimmaskb		; .A or BIT immediate
	beq	@c			; Display 16-bit operand
	
	lda	opcode			; Not .A or BIT immediate
	
@a:
	bvc	dpycod01		; 8-Bit .X/.Y immediate mode
	ldy	#n_vopidx - 1		; Opcodes to test
	
@b:
	cmp	vopidx, Y		; looking for LDX #, CPY #, etc...
	beq	@d			; Disassemle a 16-bit operand
	
	dey
	bpl	@b			; Keep trying
	
	bra	dpycod01		; Not .X or .Y immediate
	
@c:
	lda	opcode			; Reload opcode

@d:
	inx				; 16-Bit operand
;------------------------------------------------------------------------------
; Get and Display Operand Bytes
;------------------------------------------------------------------------------
dpycod01:
	stx	iopsize			; Operand size ...
	inx				; ... plus opcode becomes ...
	stx	instsize		; ... instruction size
	stx	charcnt			; Total bytes to process
	lda	#n_opcols + 2		; Total operand columns plus WS
	sta	xrtemp			; Initialize counter
	jsr	clroper			; Clear operand
	ldy	iopsize			; Operand size
	beq	@b			; No operand
	
	ldx	#$00			; Operand index
	
@a:
	jsr	getbyte			; Get operand byte
	sta	operand, X		; Save
	phx				; Protect operand index
	jsr	printbyt		; Print operand byte
	dec	xrtemp			; 3 Columns used, 2 for ...
	dec 	xrtemp			; operand nibbles and ...
	dec 	xrtemp			; 1 for whitespace
	plx				; Get operand index
	inx				; Bump it
	dey
	bne	@a			; Next one...
	
@b:
	ldx	xrtemp			; Operand columns remaining
	jsr	multspc			; Space to mnemonic field
;------------------------------------------------------------------------------
; Display mnemonic
;------------------------------------------------------------------------------
	ldy	#s_mnemonic		; Size of ASCII mnemonic
	
@c:
	lda	#$00			; Initialize character
	ldx	#n_shfEncode		; Shifts to execute
@d:
	asl	mnepck			; Shift encoded mnemonic
	rol	mnepck + s_byte		
	rol
	dex
	bne	@d
	
	adc	#a_mnecvt		; Convert to ASCII and ...
	pha				; Push to stack
	dey
	bne	@c			; Continue with mnemonic
	
	ldy	#s_mnemonic
	
@e:
	pla				; Get mnemonic byte
	jsr	putcha			; Print it
	dey
	bne	@e
;------------------------------------------------------------------------------
; Display operand
;------------------------------------------------------------------------------
	lda	iopsize			; Operand size
	beq	clearlin		; Zero, disassembly finished
	
	jsr	printspc		; <Space> to operand field
	bit	vopsflag		; Check mode flags
	bvc	dpycod02		; not a branch
	jsr	offtarg			; Compute branch target
	ldx	instsize		; Effective instruction size
	dex
	stx	iopsize			; Effective operand size
	
dpycod02:
	stz	vopsflag		; Clear
	lda	admodidx		; Instruction addressing mode
	cmp	#am_move		; Block move instruction?
	bne	@a			; No...
	
	ror	vopsflag		; Yes
	
@a:
	asl				; Convert addressing mode to ...
	tax				; ... symbology table index
	
	longa				; 16-Bit .A

	lda	ms_lutab, X		; Addressing symbol pointer
	pha
	
	shorta				; 8-Bit .A

	ldy	#$00
	lda	(1 ,S), Y		; Get 1st character
	cmp	#SPACE
	beq	@b			; No addressing mode preamble
	
	jsr	putcha			; Print preamble
	
@b:
	lda	#c_hex
	jsr	putcha			; Operand displayed as hex
	ldy	iopsize			; Operand size = index
	
@c: 
	dey
	bmi	@d			; Done with operand
	
	lda	operand, Y		; Get operand byte
	jsr	dpyhex			; Print operand byte
	bit	vopsflag		; Block move?
	bpl	@c			; No
	
	stz	vopsflag		; Reset
	phy				; Protect operand index
	pea	ms_move
	jsr	sprint			; Display MVN/MVP operand reperator
	ply				; Recover operand index
	bra	@c			; Continue
	
@d:
	plx				; Symbology LSB
	ply				; Symbology MSB
	inx				; move past preamble
	bne	@e
	
	iny
	
@e:
	phy
	phx
	jsr	sprint			; Print preamble, if any
;------------------------------------------------------------------------------
; Condition immediate mode display format
;------------------------------------------------------------------------------
dpycod03:
	lda	operand			; Operand LSB
	and	#pfxmxmask		; Isolate m & x bits
	asl				; Shift to match ...
	asl				; FLIMFLAG aligment
	ldx	opcode			; Current instruction
	cpx	#opc_rep		; Was it REP?
	bne	@a			; No
	
	tsb	flimflag		; Set flag bits as required
	bra	clearlin
	
@a:
	cpx	#opc_sep		; Was it SEP
	bne	clearlin		; No, just exit
	
	trb	flimflag		; Clear flag bits as required
	
;------------------------------------------------------------------------------
; clearlin: Clear Display Line
;------------------------------------------------------------------------------
clearlin:
	rts
	
;------------------------------------------------------------------------------
; dpyibuf: Display Monitor Input Buffer Contents
;------------------------------------------------------------------------------
dpyibuf:
	pea	ibuffer
	bra	dpyerraa
	
;------------------------------------------------------------------------------
; dpymem: Display Memory
;
;	This function displays 16 bytes of memory as hex values & as
;	ASCII equivalents.  The starting address for the display is
;	in ADDRA & is expected to be a 24 bit address.  Upon return,
;	ADDRA will point to the start of the next 16 bytes.
;------------------------------------------------------------------------------
dpymem:
	shortr				; 8-Bit .A, .X and .Y
	
	stz	charcnt			; Reset
	lda	#memPrefix
	jsr	putcha			; Display prefix
	jsr	prntladr		; Print 24-bit address
	ldx	#$00			; String buffer index
	ldy	#n_dump			; # Bytes per line
	
@a:
	jsr	getbyte			; Get from RAM, also ...
	pha				; ... save for decoding
	phx				; Save string index
	jsr	printbyt		; Display as hex ASCII
	inc	charcnt			; Bytes displayed +1
	plx				; Recover string index and ...
	pla				; ... byte
	cmp	#SPACE			; Printable?
	bcc	@b			; No ...
	
	cmp	#DEL
	bcc	@c			; Is printable
	
@b:
	lda	#memSubChar		; Get substitute character
	
@c:
	sta	ibuffer, X		; Save character
	inx				; Bump index
	dey				; Byte count - 1
	bne	@a			; Not done, yet ...
	
	stz	ibuffer, X		; Terminate ASCII string
	lda	#memSepChar
	jsr	putcha			; Seperate ASCII from bytes
	jsr	dpyibuf			; Display ASCII equivalents
	rts
;------------------------------------------------------------------------------
; dpyerr: Display Error Signal
;------------------------------------------------------------------------------
dpyerr:
	pea	mm_err			; "*ERR*"

dpyerraa:
	jsr	sprint
	rts

;------------------------------------------------------------------------------
; gendbs: Generate Destructive <Backspace>
;------------------------------------------------------------------------------
gendbs:
	pea	dc_bs			; Destructive <Backspace>
	bra	dpyerraa
	
;------------------------------------------------------------------------------
; prntladr: Print 24-bit Current Address
;------------------------------------------------------------------------------
prntladr:
	php				; Protect register sizes
	
	shorta				; 8-Bit .A

	lda	addra + s_word		; Get blank byte and ...
	jsr	dpyhex			; ... display it
	
	longa				; 16-Bit .A

	lda	addra			; Get 16-bit address
	plp				; Restore register sizes
;------------------------------------------------------------------------------
; vvvvvv  Flow into vvvvvv

;------------------------------------------------------------------------------
; dpyhexw: Display Binary Word as Hex ASCII
;
;	Preparatory Ops: .C: word to display
;
;	Returned Values: .C: used
;	                 .X: used
;	                 .Y: entry value
;------------------------------------------------------------------------------
dpyhexw:
	php				; Protect register sizes
	
	longa				; 16-Bit .A

	pha				; Protect value
	
	shorta				; 8-Bit .A

	xba				; Get MSB of address and ...
	jsr	dpyhex			; ... print
	
	longa				; 16-bit .A

	pla				; Recover value

	shorta				; 8-Bit .A
					; Only LSB is now available in .A
	plp				; Reset register sizes

;------------------------------------------------------------------------------
; dpyhex: Display Binary Byte as Hex ASCII
;
;	Preparatory Ops: .A: byte to display
;
;	Returned Values: .A: used
;	                 .X: used
;	                 .Y: entry value
;------------------------------------------------------------------------------
dpyhex:
	jsr	binhex			; Convert to hex ASCII
	jsr	putcha			; Print MSN
	txa
	jmp	putcha			; Print LSN
	
;------------------------------------------------------------------------------
; multspc: Print Multiple Blanks
;
;	Preparatory Ops : .X: number of blanks to print
;
;	Register Returns: none
;
;	Calling Example : ldx #3
;	                  jsr multspc    ;print 3 spaces
;
;	Notes: This sub will print 1 blank if .X=0.
;------------------------------------------------------------------------------
multspc:
	txa
	bne	@a			; Blank count specified
	
	inx				; Default to 1 blank/<Space>
	
@a:
	lda	#' '

@b:
	jsr	putcha
	dex
	bne	@b
	
	rts
	
;------------------------------------------------------------------------------
; newline: Print Newline (<CR><LF>)
;------------------------------------------------------------------------------
newline:
	pea	dc_lf
	bra	dpyerraa
	
;------------------------------------------------------------------------------
; printbyt: Print a Byte With Leading <Space>
;------------------------------------------------------------------------------
printbyt:
	pha				; Protect byte
	jsr	printspc		; Print leading <Space>
	pla				; Restore .A and ...
	bra	dpyhex			; ... print byte
	
;------------------------------------------------------------------------------
; alert: Alert User With <Bell>
;------------------------------------------------------------------------------
alert:
	lda	#BELL
	bra	printcmn
	
;------------------------------------------------------------------------------
; printspc: Print a <Space>
;------------------------------------------------------------------------------
printspc:
	lda	#SPACE
	
;------------------------------------------------------------------------------
; printcmn: A Common Print Routine
;------------------------------------------------------------------------------
printcmn:
	jmp	putcha

;------------------------------------------------------------------------------
; sprint: Print Null-Terminated Character String
;
;	Preparatory Ops : SP+1: string address LSB
;	                  SP+2: string address MSB
;
;	Register Returns: .A: used
;	                  .B: entry value
;	                  .X: used
;	                  .Y: used
;        
;	MPU Flags: NVmxDIZC
;	           ||||||||
;	           |||||||+---> 0: okay
;	           |||||||      1: string too long (1)
;	           ||||+++----> not defined
;	           |||+-------> 1
;	           ||+--------> 1
;	           ++---------> not defined
;
;	Example: PER STRING
;	         JSR SPRINT
;	         BCS TOOLONG
;
;	Notes: 1) Maximum permissible string length including the
;	          terminator is 32,767 bytes.
;	       2) All registers are forced to 8 bits.
;	       3) DO NOT JUMP OR BRANCH INTO THIS FUNCTION!
;------------------------------------------------------------------------------
sprint:
	shorta				; 8-Bit .A

	longi				; 16-Bit .X and .Y

;------------------------------------------------------------------------------
_retaddr 	= 1			; Return address
_src		= _retaddr + s_word	; String address stack offset
;------------------------------------------------------------------------------
	ldy	#$0000
	clc				; Initial no error

@next:
	lda	(_src, S), Y		; Get a byte
	beq	@done			; If $00, we are done
;
	phy				; Save our 16-bit .Y
;
	jsr	putcha			; Print character
;
	longi				; Force proper register widths
	shorta
	ply				; Retrieve .Y (with proper width)
;
	iny				; Increment .Y
	bpl	@next			; Read next char if .Y < ($FFFF + 1)
;	
	sec				; String too long
	
@done:
	plx				; Pull RTS address
	ply				; Clear string pointer
	phx				; Replace RTS
	
	shorti				; 8-Bit .X and .Y

	rts
	
;------------------------------------------------------------------------------
; ascbin: Convert Null-Terminated ASCII Number String To Binary
;
;	Preparatory Ops: ASCII number string in IBUFFER
;
;	Returned Values: FACA: converted parameter
;	                   .A: used
;	                   .X: used
;	                   .Y: used
;	                   .C: 1 = conversion error
;	                   .Z: 1 = nothing to convert
;
;	Notes: 1) Conversion stops when a non-numeric char-
;	          acter is encountered.
;	       2) Radix symbols are as follows:
;
;	          % binary
;	          @ octal
;	          + decimal
;	          $ hexadecimal
;
;	          Hex is the default if no radix is speci-
;	          fied in the 1st character of the string.
;------------------------------------------------------------------------------
ascbin:
	shortr				; 8-Bit .A, .X and .Y

	jsr	clrfaca			; Clear .A
	stz	charcnt			; Zero char count
	stz	radix			; Initialize
;------------------------------------------------------------------------------
; Process radix if present
;------------------------------------------------------------------------------
	jsr	getcharw		; Get next non-WS char
	bne	@a			; Got something
	
	clc				; No more input
	rts
	
@a:		
	ldx	#n_radix - 1		; Number of radices
	
@b:
	cmp	radxtab, X		; Recognized radix?
	beq	@c			; Yes
	
	dex
	bpl	@b			; Try next
	
	dec 	ibufidx			; Reposition to previous character
	inx				; Not recognized, assume hex
	
@c:	
	cmp	#c_dec			; Decimal radix?
	bne	@d			; Nope ...
	
	ror	radix			; Flag decimal conversion
	
@d:
	lda	basetab, X		; Number bases table
	sta	range			; Set valid numeral range
	lda	bitsdtab, X		; Get bits per digit
	sta	bitsdig			; Store
	
;------------------------------------------------------------------------------
; Process numerals
;------------------------------------------------------------------------------
ascbin01:
	jsr	getchar			; Get next character
	
;	bne	@a			; Not End-Of-Input
;	jmp	ascbin03		; End-Of-Input
	beq	ascbin03		; Got End-Of-Input
	
@a:		
	cmp	#' '			; Blank, End-Of-Field (EOF)
	beq	ascbin03
	
	cmp	#','			; Comma, End-Of-Field (EOF)
	beq	ascbin03
	
	cmp	#HT			; <Tab>, End-Of-Field (EOF)
	beq	ascbin03
	
	jsr	nybtobin		; Change to binary

	bcs	ascbin04		; Not a recognized numeral
	
	cmp	range			; Check range
	bcs	ascbin04		; not valid for base
	
	sta	numeral			; Save processed numeral
	inc	charcnt			; Bump numeral count
	bit	radix			; Working in base 10
	
	bpl	@c			; No
	
;------------------------------------------------------------------------------
; Compute N * 2 for Decimal Conversion
;------------------------------------------------------------------------------
	ldx	#$00			; Index into .A
	ldy	#s_pfac / 2		; Iterations
	
	longa				; 16-bit .A

	clc
@b:		
	lda	faca, X			; N
	rol				; N = N * 2
	sta	facb, X
	inx
	inx
	dey
	bne	@b
	
	bcs	ascbin04		; Overflow -> error
	
	shorta				; 8-Bit .A
	 
;------------------------------------------------------------------------------
; Compute N * base for Binary, Octal, Hex
; or N * 8 for Decimal
;------------------------------------------------------------------------------
@c:		
	ldx	bitsdig			; Bits per digit
	
	longa				; 16-Bit .A
	
@d:
	asl	faca
	rol	faca + s_word
	bcs	ascbin04		; Overflow -> error

	dex
	bne	@d			; Next shift
	
	shorta				; 8-Bit .A

	bit	radix			; Check base
	bpl	ascbin02		; Not decimal
;------------------------------------------------------------------------------
; Compute N * 10 for Decimal (N * 8 + N * 2) ...
;------------------------------------------------------------------------------
	ldy	#s_pfac
	
	longa				; 16-Bit .A
	
@e:
	lda	faca, X			; N * 8
	adc	facb, X			; N * 2
	sta	faca, X			; Now N * 10
	inx
	inx
	dey
	bne	@e
	
	bcs	ascbin04		; Overflow -> error
	
	shorta				; 8-Bit .A

;------------------------------------------------------------------------------
; Add current numeral to partial result
;------------------------------------------------------------------------------
ascbin02:	
	lda	faca			; N
	adc	numeral			; N = N + D
	sta	faca
	ldx	#$01
	ldy	#s_pfac - 1
	
@a:
	lda	faca, X
	adc	#$00			; Account for Carry
	sta	faca, X
	inx
	dey
	bne	@a
	
	bcc 	ascbin01		; Next if no overflow
;	bcc	ascbin05		; Used to circumvent branch err
	
	bcs	ascbin04		; Overflow -> error
;------------------------------------------------------------------------------
; Now, finish up
;------------------------------------------------------------------------------
ascbin03:
	clc				; No error
	
ascbin04:
	shorta				; 8-Bit .A
					; Reset if necessary
	
	lda	charcnt			; Load the character count
	rts				; Done
	
ascbin05:
	jmp	ascbin01		; Next if no overflow
	
;------------------------------------------------------------------------------
; bcdasc: Convert BCD digit to ASCII
;
;	Preparatory Ops: .A: BCD digit, $00-$99
;
;	Returned Values: .A: ASCII MSD
;	                 .X: ASCII LSD
;	                 .Y: entry value
;------------------------------------------------------------------------------
bcdasc:
	jsr	bintonyb		; Extract nybbles
	pha				; Save tens
	txa
	ora	#btoamask		; Change units to ASCII
	tax				; Store
	pla				; Get tens
	ora	#btoamask		; Change to ASCII
	rts

;------------------------------------------------------------------------------
; bintonyb: Extract Binary Nybbles
;
;	Preparatory Ops: .A: binary value
;
;	Returned Values: .A: MSN
;	                 .X: LSN
;	                 .Y: entry value
;------------------------------------------------------------------------------
bintonyb:
	pha				; Save
	and	#bcdumask		; Extract LSN
	tax				; Save it
	pla
	lsr				; Extract MSN
	lsr
	lsr
	lsr
	rts
	
;------------------------------------------------------------------------------
; binasc: Convert 32-bit Binary to Null-Terminated ASCII Number String
;
;	Preparatory Ops: FACA: 32-bit operand
;	                   .A: radix character, w/bit 7 set to
;	                       suppress radix symbol in the
;	                       conversion string
;
;	Returned Values: ibuffer: conversion string
;	                      .A: string length
;	                      .X: string address LSB
;	                      .Y: string address MSB
;
;	Execution Notes: ibufidx & instsize are overwritten.
;------------------------------------------------------------------------------
binasc:
	shortr
	
	stz	ibufidx			; Initialize sting index
	stz	instsize		; Clear format flag
;------------------------------------------------------------------------------
; Evaluate radix
;------------------------------------------------------------------------------
	asl				; Extract format flag and ...
	ror	instsize		; ... save it
	lsr				; Extract radix character
	ldx	#n_radix - 1		; Total radices
	
@a:
	cmp	radxtab, X		; Recognized radix?
	
	beq	@b			; Yes
	
	dex
	bpl	@a			; Try next
	
	inx				; Assume hex
	
@b:
	stx	radix			; Save radix index for later
	bit 	instsize
	bmi	@c			; No radix symbol wanted
	
	lda	radxtab, X		; Radix table
	sta	ibuffer			; Prepend string
	inc	ibufidx			; Bump string index
	
@c:
	cmp	#c_dec			; Convert to decimal?
	bne	@d			; Nope...
		
	jsr	facabcd			; Convert operand to BCD
	lda	#$00
	bra	@g			; Skip binary stuff
;------------------------------------------------------------------------------
; Prepare for binary, octal or hex conversion
;------------------------------------------------------------------------------
@d:
	ldx	#$00			; Operand index
	ldy	#s_sfac - 1		; Workspace index
	
@e:
	lda	faca, X			; Copy operand to ...
	sta	facb, Y			; ... workspace in ...
	
	dey				; ... big endian order
	inx
	cpx	#s_pfac
	bne	@e
	
	lda	#$00
	tyx
	
@f:
	sta	facb, X			; Pad workspace
	
	dex
	bpl	@f
;------------------------------------------------------------------------------
; Set up conversion parameters
;------------------------------------------------------------------------------
@g:
	sta	facc			; Initialyze byte counter
	
	ldy	radix			; radix index
	lda	numstab, Y		; Numerals in string
	sta	facc + s_byte		; Set remaining numeral count
	
	lda 	bitsntab, Y		; Bits per numeral
	sta	facc + s_word		; Set
	
	lda	lzsttab, Y		; Leading zero threshold
	sta	facc + s_xword		; Set
	
;------------------------------------------------------------------------------
; Generate conversion string
;------------------------------------------------------------------------------
@h:	
	lda	#$00
	ldy	facc + s_word		; Bits per numeral
	
@i:
	ldx	#s_sfac - 1		; Workspace size
	clc				; Avoid starting carry
	
@j:
	rol	facb, X			; Shift out a bit ...
	dex				; from the operand or ...
	bpl	@j			; BCD conversion result
	
	rol				; Bits to .A
	
	dey
	bne	@i			; More bits to grab
	
	tay				; If numeral isn't zero
	bne	@k			; Skip leading zero tests
	
	ldx	facc + s_byte		; Remaining numerals
	cpx	facc + s_xword		; Leading zero threshold
	bcc	@k			; Below it, must convert
	
	ldx	facc			; Processed byte count
	beq	@m			; Discard leading zero
	
@k:
	cmp	#10			; Check range
	bcc	@l			; is 0-9
	
	adc	#a_hexdec		; Apply hex adjust
	
@l:
	adc	#'0'			; Change to ASCII
		
	ldy 	ibufidx			; String index
	sta	ibuffer, Y		; Save numeral in buffer
	inc	ibufidx			; Next buffer position
	inc 	facc			; Bytes = bytes + 1
	
@m:
	dec	facc + s_byte		; Numerals = numerals - 1
	
	bne	@h			; not done, yet ...
;------------------------------------------------------------------------------
; Terminate string and exit
;------------------------------------------------------------------------------
	ldx	ibufidx			; Printable string length
	stz	ibuffer, X		; Terminate string
	txa
	ldx	#<ibuffer		; Converted string
	ldy	#>ibuffer
	clc				; All okay
	rts

;------------------------------------------------------------------------------
; binhex: Convert Binary to Hex ASCII Characters
;
;	Preparatory Ops: .A: byte to convert
;
;	Returned Values: .A: MSN ASCII char
;	                 .X: LSN ASCII char
;	                 .Y: entry value
;------------------------------------------------------------------------------
binhex:
	jsr	bintonyb		; Generate binary values
	pha				; Save MSN
	txa
	jsr	@a			; Generate ASCII LSN
	tax				; Save
	pla				; Get original input
;------------------------------------------------------------------------------
; Convert nybble to hex ASCII equivalents
;------------------------------------------------------------------------------
@a:	cmp	#10			; Intentional decimal entry!
	bcc	@b			; In decimal range?
	
	adc	#k_hex			; Compensate for hex
	
@b:
	eor	#'0'			; Finalize nybble
	rts				; Done
	
;------------------------------------------------------------------------------
; clrfaca: Clear Floating Accumulator .A
;
;------------------------------------------------------------------------------
clrfaca:
	php				; Store processor status
	
	longa
	
	stz	faca
	stz	faca + s_word
	plp				; Restore processor status

	rts
	
;------------------------------------------------------------------------------
; clrfacb: Clear Floating Accumulator .B
;
;------------------------------------------------------------------------------
clrfacb:
	php				; Store processor status
	
	longa
	
	stz	facb
	stz	facb + s_word
	plp				; Restore processor status
	
	rts
	
;------------------------------------------------------------------------------
; facabcd: Convert FACA into BCD
;
;------------------------------------------------------------------------------
facabcd:
	ldx	#s_pfac - 1		; Primary accumulator size - 1
	
@a:
	lda	faca, X			; Value to be converted
	pha				; Preserve original
	dex
	bpl	@a			; Next
	
	ldx	#s_sfac - 1		; Workspace size
	
@b:
	stz	facb, X			; Clear final result storage
	stz	facc, X			; Clear scratchpad storage
	dex
	bpl	@b			; Next
	
	inc	facc + s_sfac - s_byte	
	sed				; Set decimal mode
	ldy	#m_bits - 1		; Bits to convert - 1
	
@c:
	ldx	#s_pfac - 1		; Operand size
	clc				; No carry at start
	
@d:
	ror	faca, X			; Grab LS bit in operand
	dex
	bpl	@d
	
	bcc	@f			; LS bit clear
	
	clc
	ldx	#s_sfac - 1
	
@e:
	lda	facb, X			; Partial result
	adc	facc, X			; Scratchpad
	sta	facb, X			; New partial result
	dex
	bpl	@e
	
	clc
	
@f:
	ldx	#s_sfac - 1
	
@g:
	lda	facc, X			; Scratchpad
	adc	facc, X			; Double and ...
	sta	facc, X			; ... save
	dex
	bpl	@g
	
	dey
	bpl	@c			; Next operand bit
	
	cld
	ldx	#0
	ldy	#s_pfac
	
@h:
	pla				; Operand
	sta	faca, X			; Restore
	inx
	dey
	bne	@h			; Next
	
	rts
	
;------------------------------------------------------------------------------
; nybtobin: Convert ASCII nybble to Binary
;
;------------------------------------------------------------------------------
nybtobin:
	shortr
	jsr	toupper			; Convert to upppercase if necessary
	sec
	sbc	#'0'			; Change to binary
	bcc	@b			; Not a numeral -> error
	
	cmp	#10
	bcc	@a			; Numeral is 0-9
	
	sbc	#a_hexdec + 1		; 10-15 => A-F
	clc				; No conversion error
	
@a:
	rts
	
@b:
	sec				; Conversion error
	rts
	
;------------------------------------------------------------------------------
; calccnt: Compute Byte Count from Address Range
;
;------------------------------------------------------------------------------
calccnt:
	jsr	clrfacb			; Clear .B accumulator
	
	longa				; 16-bit .A

	sec
	lda	addrb			; Ending address
	sbc	addra			; Starting address
	sta	facb			; Byte count
	
	shorta				; Back to 8-bit .A

	lda	addrb + s_word		; Handle banks
	sbc	addra + s_word
	sta	facb + s_word
	
;------------------------------------------------------------------------------
; clroper: Clear Operand
;
;------------------------------------------------------------------------------
clroper:
	phx
	ldx	#s_oper - 1
	
@a:
	stz	operand, X
	dex
	bpl	@a
	
	stz	eopsize
	plx
	rts
	
;------------------------------------------------------------------------------
; cpfwsup: Forward Copy Memory Setup
;
;------------------------------------------------------------------------------
cpfwsup:
	longr				; Full 16-bit registers .A, .X and .Y

	ldx	#opc_mvn		; "Move Next" opcode
	bra	cpsup
	
;------------------------------------------------------------------------------
; cprvsup: Reverse Copy Memory Setup
;
;------------------------------------------------------------------------------
cprvsup:
	longr				; Full 16-bit registers .A, .X and .Y
	
	ldx	#opc_mvp		; "Move Previous" opcode
	
;------------------------------------------------------------------------------
; cpsup: Copy Memory Setup
;
;------------------------------------------------------------------------------
cpsup:
	pha				; Save banks
	txa				; Protect ...
	xba				; ... opcode
	
	shorta				; 8-bit .A

	ldx	#cpcodeee - cpcode - 1	
	
@a:
	lda	f:cpcode, X		; Transfer code copy ...
	sta	mcftwork, X		; ... to workspace
	dex	
	bpl	@a			; Until done...
	
	xba				; Recover opcode
	sta	mcftopc			; Set it
	
	longa				; 16-bit .A

	pla				; Retrieve banks ...
	sta	mcftbnk			; ... and set them
	rts
	
;------------------------------------------------------------------------------
; decdcnt: Decrement Dump Count
;
;	Preparatory Ops: bytes to process in FACB
;	                 bytes processed in CHARCNT
;
;	Returned Values: .A: used
;	                 .X: entry value
;	                 .Y: entry value
;	                 .C: 1 = count = zero
;------------------------------------------------------------------------------
decdcnt:
	shorta				; 8-bit .A

	lda	#$00
	xba				; Clear .B
	lda	facb + s_word		; Count  MSW
	
	longa				; 16-bit .A

	sec
	ora	facb			; Count LSW
	beq	@b			; Result = 0, just quit
	
	lda	facb
	sbc	charcnt			; Bytes processed
	sta	facb
	
	shorta				; 8-Bit .A

	lda	facb + s_word
	sbc	#$00			; Handle borrow/carry
	bcc	@a			; Underflow

	sta	facb + s_word
	clc				; Count > 0
	rts
	
@a:
	sec
	
@b:
	shorta				; 8-Bit .A

	rts

;------------------------------------------------------------------------------
; enddest: Get 2nd & 3rd Addresses for Compare and Transfer
;
;------------------------------------------------------------------------------
enddest:
	jsr	facasize		; Check start ...
	cmp	#s_dword		; ... for range
	bcs	@a			; Out of range -> error

	jsr	facaddra		; Store start address
	jsr	getparm			; Get end address
	bcs	@a			; Not entered -> error
	
	jsr	facasize		; Check end ...
	cmp	#s_dword		; ... for range
	bcs	@a			; Out of range -> error
	
	jsr	facaddrb		; Store end address
	jsr	getparm			; Get destination
	bcs	@a			; Not entered -> error
	
	jsr	facasize		; Check destination ...
	cmp	#s_dword		; ... for range
	bcc	facaoper		; Store destination address
	
@a:
	rts				; Exit w/ error
	
;------------------------------------------------------------------------------
; facaddra: Copy FACA to ADDRA
;
;------------------------------------------------------------------------------
facaddra:
	shortr
	ldx	#s_xword - 1

@a:
	lda	faca, X
	sta	addra, X
	dex
	bpl	@a
	
	rts
	
;------------------------------------------------------------------------------
; facaddrb: Copy FACA to ADDRB
;
;------------------------------------------------------------------------------
facaddrb:
	shortr
	ldx	#s_xword - 1

@a:
	lda	faca, X
	sta	addrb, X
	dex
	bpl	@a
	
	rts
	
;------------------------------------------------------------------------------
; facaoper: Copy FACA to operand
;
;------------------------------------------------------------------------------
facaoper:
	shortr
	ldx	#s_oper - 1

@a:
	lda	faca, X
	sta	operand, X
	dex
	bpl	@a
	
	rts
	
;------------------------------------------------------------------------------
; facasize: Report Operand Size in FACA
;
;	Preparatory Ops: operand in FACA
;
;	Returned Values: .A: s_byte  (1)
;	                     s_word  (2)
;	                     s_xword (3)
;	                     s_dword (4)
;
;	Notes: 1) This function will always report
;	          a non-zero result.
;------------------------------------------------------------------------------
facasize:
	shortr				; 8-Bit registers .A, .X and .Y

	ldx	#s_dword - 1
	
@a:
	lda	faca, X			; Get byte
	bne	@b			; Done
	
	dex
	bne	@a			; Next
	
@b:
	inx				; count = index + 1
	txa
	rts
	
;------------------------------------------------------------------------------
; getparm: Get a Parameter
;
;	Preparatory Ops: null-terminated input in IBUFFER
;
;	Returned Values: .A: chars in converted parameter
;	                 .X: used
;	                 .Y: entry value
;	                 .C: 1 = no parameter entered
;------------------------------------------------------------------------------
getparmr:
	dec	ibufidx			; Reread previous character
	
getparm:
	phy				; Preserve .Y
	jsr	ascbin			; Convert parameter to binary
	bcs	@d			; Conversion error

	jsr	getcharr		; Reread last character
	bne	@a			; Not end-of-input
	
	dec	ibufidx			; Reindex to terminator
	lda	charcnt			; Get characters processed so far
	beq	@c			; None
	
	bne	@b			; Some
	
@a:
	cmp	#' '			; Recognize delimiter
	beq	@b			; End of parameter
	
	cmp	#','			; Recognized delimiter
	bne	@d			; Unknown delimiter
	
@b:	
	clc
	.byte	$24			; Use BIT to skip SEC below
	
@c:	
	sec
	
	ply				; Restore .Y
	lda	charcnt			; Get count
	rts				; Done

@d:
	pla				; Clean up stack
	pla
	pla
	jmp	monerr			; Abort w/ error
	
;------------------------------------------------------------------------------
; nxtaddra: Test and Increment Workign Address 'A'
;
;	Calling syntax: JSR NXTADDRA
;
;	Exit registers: .A: used
;	                .B: used
;	                .X: entry value
;	                .Y: entry value
;	                DB: entry value
;	                DP: entry value
;	                PB: entry value
;	                SR: NVmxDIZC
;	                    ||||||||
;	                    |||||||+---> 0: ADDRA < ADDRB
;	                    |||||||      1: ADDRA >= ADDRB
;	                    ||||||+----> undefined
;	                    |||+++-----> entry value
;	                    ||+--------> 1
;	                    ++---------> undefined
;------------------------------------------------------------------------------
nxtaddra:
	shorta				; 8-Bit .A

	lda	addra + s_word		; Bits 23-16
	cmp	addrb + s_word
	bcc	incaddra		; increment
	
	bne	@a			; Don't increment
	
	longa				; 16-bit .A

	lda	addra			; Bits 15-0
	cmp	addrb			; Condition flags
	
	shorta				; 8-Bit .A

	bcc	incaddra		; Increment
	
@a:
	rts
	
;------------------------------------------------------------------------------
; getbyte: Get a Byte from Memory
;
;------------------------------------------------------------------------------
getbyte:
	lda	[addra]			; Get a byte
	
;------------------------------------------------------------------------------
; incaddra: Increment Working Address 'A'
;
;	Calling syntax: JSR INCADDRA
;
;	Exit registers: .A: entry value
;	                .B: entry value
;	                .X: entry value
;	                .Y: entry value
;	                DB: entry value
;	                DP: entry value
;	                PB: entry value
;	                SR: NVmxDIZC
;	                    ||||||||
;	                    ++++++++---> entry value
;------------------------------------------------------------------------------
incaddra:
	php
	
	longa				; 16-bit .A

	inc	addra			; bump bits 15-0
	bne	@a
	
	shorta
	
	inc	addra + s_word		; Bump bits 23-16 (bank)
	
@a:
	plp
	rts
	
;------------------------------------------------------------------------------
; incoper: Increment Operand Address
;
;------------------------------------------------------------------------------
incoper:
	clc
	php
	
	longr				; 16-Bit .A

	pha
	inc	operand			; Handle base address
	bne	@a
	
	shorta				; 8-Bit .A

	inc	operand + s_word	; Handle bank
	
	longa				; 16-bit .A
	
@a:
	pla
	plp
	rts
	
;------------------------------------------------------------------------------
; instdata: Get Instruntion Size and Addressing Mode Data
;	Preparatory Ops: .X: 65C816 opcode
;
;	Returned Values: .A: mode flags
;	                 .X: operand size
;	                 .Y: mode index
instdata:
	shortr				; 8-bit registers .A, .X and .Y

	lda	mnetabam, X		; Addressing mode data
	pha				; Save mode flag bits
	pha				; Save data size
	and	#amodmask		; Extract mode index and ...
	tay				; Save
	pla				; Recover data
	and	#opsmask		; Mask mode field and ...
	lsr				; Extract operand size
	lsr
	lsr
	lsr
	tax				; Operand size
	pla				; Recover mode flags
	and	#vopsmask		; Discard mode and size fields
	rts
	
;------------------------------------------------------------------------------
; offtarg: Convert Branch Offset to Target Address
;
;	Preparatory Ops:    ADDRA: base address
;	                 INSTSIZE: instruction size
;	                  OPERAND: offset
;
;	Returned Values:  OPERAND: target address (L/H)
;	                       .A: used
;	                       .X: entry value
;                              .Y: entry value
offtarg:
	longa				; 16-Bit .A

	lda	addra			; Base address
	
	shorta				; 8-Bit .A

	lsr	instsize		; Bit 0 will be set if ...
	bcs	@a			; ... a long branch
	
	bit	operand			; Short forward or backward?
	bpl	@a			; Forward
	
	xba				; Expose address MSB
	dea				; Back a page
	xba				; Expose address LSB
	
@a:
	longa				; 16-Bit .A

	clc
	adc	operand			; Calculate target address
	sta	operand			; New operand
	shorta				; 8-Bit .A

	lda	#s_xword
	sta	instsize		; Effectice instruction size
	rts
	
;------------------------------------------------------------------------------
; setxaddr: Set Execution Address
;
;------------------------------------------------------------------------------
setxaddr:
	bcs	@a			; No address given
	
	jsr	facasize		; Check address ...
	cmp	#s_dword		; ... range
	bcs	@b			; Out of range
	
	longa				; 16-bit .A

	lda	faca			; Execution address
	sta	reg_pcx			; Set new .PC (Program counter) value
	
	shorta				; 8-Bit .A

	lda	faca + s_word
	sta	reg_pbx			; Set new .PB (Program Bank) value
	
@a:
	clc				; No error
	
@b:
	rts
	
;------------------------------------------------------------------------------
; targoff: Convert Branch Target Address to Branch Offet
;
;	Preparatory Ops:   ADDRA: instruction address
;	                 OPERAND: target address
;
;	Returned Values: OPERAND: computed offset
;	                      .A: effective operand size
;	                      .X: entry value
;                             .Y: entry value
;	                      .C: 1 = branch out of range
;
;	Execution notes: ADDRB is set to the branch base
;	                 address.
;------------------------------------------------------------------------------
targoff:
	stz	instsize + s_byte	; Always zero
	lda	instsize		; Instruction size will tell ...
	lsr				; ... if long or short branch
;------------------------------------------------------------------------------
_btype		= facc + 5		; Branch type flag
;------------------------------------------------------------------------------
	ror	_btype		; Set branch type
		
;------------------------------------------------------------------------------
; x0000000
; |
; +--------->	0: short
;		1: long
;------------------------------------------------------------------------------
	longa				; 16-Bit .A

	clc
	lda	addra			; Instruction address
	adc	instsize		; Instruction size
	sta	addrb			; Base address
	sec
	lda	operand			; Target address
	sbc	addrb			; base address
	sta	operand			; Offset
	
	shorta				; 8-Bit .A

	bcc	@d			; Backward branch
;------------------------------------------------------------------------------
; Process short forward branch
;------------------------------------------------------------------------------
	xba				; Offset MSB should be zero
	bne	@f			; If it isn't -> out of range
	
	xba				; Offset LSB should be $00 - $7F
	bmi	@f			; It isn't -> out of range
	
@a:
	lda	#s_byte			; Final instruction size
	clc				; Branch is in range
	rts
;------------------------------------------------------------------------------
; Process long forward branch
;------------------------------------------------------------------------------
@b:
	xba				; Offset MSB should be positive
	bmi	@f			; It isn't -> branch out of range
	
@c:
	lda	#s_word
	clc
	rts
	
;------------------------------------------------------------------------------
; Process backward branch
;------------------------------------------------------------------------------
@d:
	bit	_btype			; Long or short?
	bmi	@e			; Long ...
	
;------------------------------------------------------------------------------
; Process short backward branch
;------------------------------------------------------------------------------
	xba				; Offset MSB should be negative
	bpl	@f			; It isn't -> out of range
	
	eor	#%11111111		; Complement offset MSB 2s
	bne	@f			; Out of range
	
	xba				; Offset LSB should be $80 - $FF
	bmi	@a			; It is -> Branch in range
	
;------------------------------------------------------------------------------
; Process long backward branch
;------------------------------------------------------------------------------
@e:
	xba				; Offset MSB should be negative
	bmi	@c			; It is -> branch in range
	
@f:
	sec				; Range error
	rts

;------------------------------------------------------------------------------
; getcharr: Get a Character from Input Buffer
;
;------------------------------------------------------------------------------
getcharr:
	dec	ibufidx			; Move back a character
	
;------------------------------------------------------------------------------
; getchar: Get a Character from Input Buffer
;	Preparatory Ops : none
;
;	Register Returns: .A: character or <NUL>
;	                  .B: entry value
;	                  .X: entry value
;	                  .Y: entry value
;
;	MPU Flags: NVmxDIZC
;	           ||||||||
;	           |||||||+---> entry value
;	           ||||||+----> 1: <NUL> gotten
;	           |||||+-----> entry value
;	           ||||+------> entry value
;	           |||+-------> entry value
;	           ||+--------> entry value
;	           |+---------> not defined
;	           +----------> not defined
getchar:
	phx
	phy
	php				; Save register sizes
	
	shortr				; Force 8-bit registers .A, .X and .Y
	
	ldx	ibufidx			; Buffer index
	lda	ibuffer, X		; Get character
	inc	ibufidx			; Bump index
	plp				; Restore register widths
	ply
	plx
	xba				; Condition
	xba				; .Z
	rts
	
;------------------------------------------------------------------------------
; getpat: Get pattern for Memory Change or Search
;
;	Preparatory Ops: Null-terminated pattern in IBUFFER.
;
;	Returned Values: .A: used
;	                 .X: used
;	                 .Y: pattern length if entered
;	                 .C: 0 = pattern valid
;	                     1 = exception:
;	                 .N  0 = no pattern entered
;	                     1 = evaluation error
;
;	Notes: 1) If pattern is preceded by "'" the following
;	          characters are interpreted as ASCII.
;	       2) A maximum of 32 bytes or characters is
;	          accepted.  Excess input will be discarded.
getpat:
	stz	status			; Clear pattern type indicator
	ldy	#$00			; Pattern index
	jsr	getcharr		; Get last character
	beq	@g			; EOS (End-Of-String)
	
	ldx	ibufidx			; Current buffer index
	jsr	getcharw		; Get next
	beq	@g			; EOS
	
	cmp	#$27			; Single quote
	bne	@a			; Not ASCII input
	
	ror 	status			; Condition flag
	bra	@c			; balance of input if ASCII
	
@a:
	stx	ibufidx			; Restore buffer index
	
@b:
	jsr	getparm			; Evaluate numeric pattern
	bcs	@f			; Done w/ pattern
	
	jsr	facasize		; Size
	cmp	#s_word
	bcs	@g			; Not a byte -> error
	
	lda	faca			; Get byte and ...
	bra	@d			; ... store
	
@c:
	jsr	getchar			; Get ASCII character
	beq	@f			; Done with pattern
	
@d:
	cpy	#s_auxBuffer		; Pattern buffer full?
	beq	@e			; Yes
	
	sta	auxbuf, Y		; Store pattern
	iny
	bit	status
	bpl	@b			; Get next numeric value
	
	bra	@c			; Get next ASCII character
	
@e:
	jsr	alert			; Excess input
	
@f:
	sty	auxbufindex		; Save pattern size
	tya				; Condition .Z
	clc				; Pattern is valid
	rts
;------------------------------------------------------------------------------
; No pattern entered
;------------------------------------------------------------------------------
@g:
	rep	#%10000000
	nop				; Added two NOPs to be safe
	nop
	sec
	rts
;------------------------------------------------------------------------------
; Evaluation error
;------------------------------------------------------------------------------
@h:
	sep	#%10000001
	nop				; Added two NOPs to be safe?
	nop
	
	rts
	
;------------------------------------------------------------------------------
; getcharw: Get from Input Buffer, Discarding Whitespace
;
;	Preparatory Ops: Null-terminated input in IBUFFER.
;
;	Returned Values: .A: char or null
;	                 .X: entry value
;	                 .Y: entry value
;	                 .Z: 1 = null terminator detected
;
;	Notes: Whitespace is defined as a blank ($20) or a
;	       horizontal tab ($09).
getcharw:
	jsr	getchar			; Get from buffer
	beq	@a			; EOI (End-Of-Input)
	
	cmp	#SPACE
	beq	getcharw		; Discard whitespace
	
	cmp	#HT			; Also whitespace
	beq	getcharw
	
@a:
	clc
	rts
	
;------------------------------------------------------------------------------
; input: Interactive Input from Console Channel
;
;	Preparatory Ops: Zero IBUFIDX or load IBUFFER with default
;	                 input & set IBUFIDX to the number of chars
;	                 loaded into the buffer.
;
;	Returned Values: .A: used
;	                 .X: characters entered
;	                 .Y: used
;
;	Example: STZ IBUFIDX
;	         JSR INPUT
;
;	Notes: Input is collected in IBUFFER & is null-terminated.
;	       IBUFIDX is reset to zero upon exit.
input:
	ldx	ibufidx
	stz	ibuffer, X		; Be sure buffer is terminated
	jsr	dpyibuf			; Print default input if any
;	pea	dc_cn
;	jsr	sprint			; Enable cursor
	ldx	ibufidx			; Starting buffer index
;------------------------------------------------------------------------------
; Main input loop
;------------------------------------------------------------------------------
@a:
	jsr	getcha			; Poll for input
	bcc	@b			; Got something
	
; Maybe we can use WAI to wait for any IRQ (if using input IRQ)
;	wai
	bra	@a			; Try again
	
@b:
	cmp	#DEL			; Above ASCII range?
	bcs	@a			; Try again
	
	cmp	#HT			; <Tab>?
	bne	@c			; No
	
	lda	#SPACE			; Replace <Tab> with <Space>
	
@c:
	cmp	#SPACE			; Did we get a control character?
	bcc	@e			; Yes
;------------------------------------------------------------------------------
; Process QWERTY character
;------------------------------------------------------------------------------
	cpx	#s_ibuf			; Room in buffer?
	bcs	@d			; No
	
	sta	ibuffer, X		; Store character
	inx				; Bump buffer
	.byte	$2c			; Use BIT to skip next two bytes, echo
					; character stored

@d:
	lda	#BELL			; Alert user
	jsr	putcha
	bra	@a			; Get some some?
	
;------------------------------------------------------------------------------
; Process <CR>
;------------------------------------------------------------------------------
@e:
	cmp	#CR			; <CR>?
	bne	@f			; No
	
;	phx				; Protect input count
;	pea	dc_co
;	jsr	sprintf			; Cursor off
;	plx				; Recover input count
	stz	ibuffer, X		; Terminate input and ...
	stz	ibufidx			; ... reset buffer index
	rts				; Done
;------------------------------------------------------------------------------
; Process <BS>
;------------------------------------------------------------------------------
@f:
	cmp	#BS			; Backspace?
	bne	@a			; No
	
	txa
	beq	@a			; No input, ignore <BS>
	
	dex				; 1 less character
	phx				; Preserve (new) count
	jsr	gendbs			; Destructive <BS>
	plx				; Restore count
	bra	@a			; Get more input
	
;------------------------------------------------------------------------------
; lodbnk: Load Source and Destination banks
;------------------------------------------------------------------------------
lodbnk:
	shorta				; 8-Bit .A

	lda	operand + s_word	; Destination bank
	xba				; Make it MSB
	lda	addra + s_word		; Source bank is LSB
	rts
	
;------------------------------------------------------------------------------
; getcharc: Get a Character from Input Buffer and Convert Case
;
;	Preparatory Ops: Null-terminated input in IBUFFER.
;
;	Returned Values: .A: char or null
;	                 .X: entry value
;	                 .Y: entry value
;	                 .Z: 1 = null terminator detected
;------------------------------------------------------------------------------
getcharc:	
	jsr	getchar			; Get from buffer
	
;------------------------------------------------------------------------------
; toupper: Force Character to Upper Case
;
;	Preparatory Ops : .A: 8 bit character to convert
;
;	Register Returns: .A: converted character
;	                  .B: entry value
;	                  .X: entry value
;	                  .Y: entry value
;
;	MPU Flags: no change
;
;	Notes: 1) This subroutine has no effect on char-
;	          acters that are not alpha.
toupper:
	php				; Protect flags
	
	cmp	#a_asclcl		; Check character range
	bcc	@a			; Not lower case alpha character
	
	cmp	#a_asclch + s_byte	
	bcs	@a			; Not lower case alpha character
	
	and 	#a_lctouc		; Force upper case

@a:
	plp				; Restore flags
	
touppera:	
	rts
	
;------------------------------------------------------------------------------
; teststop: Test for Stop Key
;
;	Preparatory Ops: none
;
;	Returned Values: .A: detected keypress, if any
;	                 .X: entry value
;	                 .Y: entry value
;
;	MPU Flags: NVmxDIZC
;	           ||||||||
;	           |||||||+---> 0: normal key detected
;	           |||||||      1: <STOP> detected
;	           +++++++----> not defined
;
;	Example: jsr teststop
;	         bcs stopped
;
;	Notes: The symbol STOPKEY defines the ASCII
;	       value of the "stop key."
;------------------------------------------------------------------------------
teststop:
	jsr	getcha			; Poll console
	bcs	@a			; No input
	
	cmp	#stopkey
	beq	@b			
@a:
	clc
	
@b:
	rts
	
;------------------------------------------------------------------------------
; cpcode: Copy Memory Code
;
;	This code is transfered to workspace when a
;	copy or fill operation is to be performed.
;------------------------------------------------------------------------------
cpcode:
		phb			; Must preserve data bank
		nop			; Placeholder
		nop			; Placeholder
		nop			; Placeholder
		plb			; Restore databank
		jml	monce		; Return to command executive
cpcodeee	= *			; Placeholder - do not delete
	
monhlp:
	pea	mm_hlp			; Print help
	jsr	sprint
	
	sec
	jmp	monreg			; When returning, show registers
	
;------------------------------------------------------------------------------
; Command Processing Data Tables
;------------------------------------------------------------------------------
; Monitor commands
;------------------------------------------------------------------------------
mpctab:
	.byte	'A'			; Assemble code
	.byte	'C'			; Compare memory ranges
	.byte	'D'			; Disassemble code
	.byte	'F'			; Fill memory
	.byte	'G'			; Execute code
	.byte	'H'			; Search memory
	.byte	'J'			; Execute code as subroutine
	.byte	'L'			; Load Intel Hex file
	.byte	'M'			; Dump memory range
	.byte	'R'			; Dump registers
	.byte	'T'			; Copy memory range
;	.byte	'X'			; Exit from monitor
	.byte	'>'			; Change memory
	.byte	';'			; Change registers
	.byte	'?'			; Help
n_mpctab	= * - mpctab		; # entries in above table

;------------------------------------------------------------------------------
; Monitor command jump table
;------------------------------------------------------------------------------
mpcextab:
	.word	monasc - s_byte		; A - Assemble code
	.word	moncmp - s_byte		; C - Compare memory ranges
	.word	mondsc - s_byte		; D - Disassemble code
	.word	monfil - s_byte		; F - Fill memory
	.word	monjmp - s_byte		; G - Execute code
	.word	monhnt - s_byte		; H - Search memory
	.word	monjsr - s_byte		; J - Execute code as subroutine
;	.word	LOADS19 - s_byte	; L - Load S19 file
	.word	strtintel - s_byte	; L - Load Intel Hex file
	.word	mondmp - s_byte		; M - Dump memory range
	.word	monreg - s_byte		; R - Dump registers
	.word 	moncpy - s_byte		; T - Copy memory range
;	.word	monxit - s_byte		; X - Exit from mointor
	.word	monchm - s_byte		; > - Change memory
	.word	monchr - s_byte		; ; - Change registers
	.word	monhlp - s_byte		; ? - Help

;------------------------------------------------------------------------------
; Number Conversion
;------------------------------------------------------------------------------
basetab:				; Supported number bases
	.byte	16			;  - Hexadecimal
	.byte	10			;  - Decimal
	.byte	8			;  - Octal
	.byte	2			;  - Binary
bitsdtab:				; Bits per binary digit
	.byte	4
	.byte	3
	.byte	3
	.byte	1
bitsntab:				; Bits per ASCII character
	.byte	4
	.byte	4
	.byte	3
	.byte	1
lzsttab:				; Leading zero suppression thresholds
	.byte	3
	.byte	2
	.byte	9
	.byte	2
numstab:				; Bin to ASCII conversion numerals
	.byte	12
	.byte	12
	.byte	16
	.byte	48
radxtab:				; Supported radices
	.byte	c_hex			;  - Hexadecimal radix
	.byte	c_dec			;  - Decimal radix
	.byte	c_oct			;  - Octal radix
	.byte	c_bin			;  - Binary radix
n_radix		= * - radxtab		; # recognized radices

;------------------------------------------------------------------------------
; Shadow MPU Register Sizes
;------------------------------------------------------------------------------
rcvltab:
	.byte	s_mpupbrx + s_byte	; PB
	.byte	s_mpupcx  + s_byte	; PC
	.byte	s_mpusrx  + s_byte	; SR
	.byte	s_word    + s_byte	; .C
	.byte	s_word    + s_byte	; .X
	.byte	s_word    + s_byte	; .Y
	.byte	s_mpuspx  + s_byte	; SP
	.byte	s_mpudpx  + s_byte	; DP
	.byte	s_mpudbrx + s_byte	; DB
n_regchv	= * - rcvltab		; Total shadow registers

;------------------------------------------------------------------------------
; Assembler/Disassembler Data Tables
;------------------------------------------------------------------------------
mnetab:
	.word	mne_xba			;  0 - XBA
	.word	mne_lda			;  1 - LDA
 	.word	mne_pea			;  2 - PEA
 	.word	mne_pha			;  3 - PHA
 	.word	mne_pla			;  4 - PLA
 	.word	mne_bra			;  5 - BRA
 	.word	mne_ora			;  6 - ORA
 	.word	mne_sta			;  7 - STA
 	.word	mne_txa			;  8 - TXA
 	.word	mne_tya			;  9 - TYA
 	.word	mne_phb			; 10 - PHB
 	.word	mne_plb			; 11 - PLB
 	.word	mne_trb			; 12 - TRB
 	.word	mne_tsb			; 13 - TSB
 	.word	mne_sbc			; 14 - SBC
 	.word	mne_bcc			; 15 - BCC
 	.word	mne_adc			; 16 - ADC
 	.word	mne_tdc			; 17 - TDC
 	.word	mne_dec			; 18 - DEC
 	.word	mne_sec			; 19 - SEC
 	.word	mne_clc			; 20 - CLC
 	.word	mne_inc			; 21 - INC
 	.word	mne_tsc			; 22 - TSC
 	.word	mne_bvc			; 23 - BVC
 	.word	mne_tcd			; 24 - TCD
 	.word	mne_sed			; 25 - SED
 	.word	mne_phd			; 26 - PHD
 	.word	mne_cld			; 27 - CLD
 	.word	mne_pld			; 28 - PLD
 	.word	mne_and			; 29 - AND
 	.word	mne_xce			; 30 - XCE
 	.word	mne_bne			; 31 - BNE
 	.word	mne_wai			; 32 - WAI
 	.word	mne_pei			; 33 - PEI
 	.word	mne_sei			; 34 - SEI
 	.word	mne_cli			; 35 - CLI
 	.word	mne_bmi			; 36 - BMI
 	.word	mne_rti			; 37 - RTI
 	.word	mne_phk			; 38 - PHK
 	.word	mne_brk			; 39 - BRK
 	.word	mne_jml			; 40 - JML
 	.word	mne_rol			; 41 - ROL
 	.word	mne_bpl			; 42 - BPL
 	.word	mne_brl			; 43 - BRL
 	.word	mne_asl			; 44 - ASL
 	.word	mne_jsl			; 45 - JSL
 	.word	mne_rtl			; 46 - RTL
 	.word	mne_wdm			; 47 - WDM
 	.word	mne_mvn			; 48 - MVN
 	.word	mne_rep			; 49 - REP
 	.word	mne_sep			; 50 - SEP
 	.word	mne_php			; 51 - PHP
 	.word	mne_plp			; 52 - PLP
 	.word	mne_cmp			; 53 - CMP
 	.word	mne_jmp			; 54 - JMP
 	.word	mne_cop			; 55 - COP
 	.word	mne_nop			; 56 - NOP
 	.word	mne_stp			; 57 - STP
 	.word	mne_mvp			; 58 - MVP
 	.word	mne_beq			; 59 - BEQ
 	.word	mne_per			; 60 - PER
 	.word	mne_eor			; 61 - EOR
 	.word	mne_ror			; 62 - ROR
 	.word	mne_jsr			; 63 - JSR
 	.word	mne_lsr			; 64 - LSR
 	.word	mne_bcs			; 65 - BCS
 	.word	mne_tcs			; 66 - TCS
 	.word	mne_rts			; 67 - RTS
 	.word	mne_bvs			; 68 - BVS
 	.word	mne_txs			; 69 - TXS
 	.word	mne_bit			; 70 - BIT
 	.word	mne_clv			; 71 - CLV
 	.word	mne_tax			; 72 - TAX
 	.word	mne_ldx			; 73 - LDX
 	.word	mne_dex			; 74 - DEX
 	.word	mne_phx			; 75 - PHX
 	.word	mne_plx			; 76 - PLX
 	.word	mne_inx			; 77 - INX
 	.word	mne_cpx			; 78 - CPX
 	.word	mne_tsx			; 79 - TSX
 	.word	mne_stx			; 80 - STX
 	.word	mne_tyx			; 81 - TYX
 	.word	mne_tay			; 82 - TAY
 	.word	mne_ldy			; 83 - LDY
 	.word	mne_dey			; 84 - DEY
 	.word	mne_phy			; 85 - PHY
 	.word	mne_ply			; 86 - PLY
 	.word	mne_iny			; 87 - INY
 	.word	mne_cpy			; 88 - CPY
 	.word	mne_sty			; 89 - STY
 	.word	mne_txy			; 90 - TXY
 	.word	mne_stz			; 91 - STZ

s_mnetab	= * - mnetab		; Menmonic table size
n_mnemon	= s_mnetab / s_word	; Total # mnemonics

;------------------------------------------------------------------------------
; Mnemonic Lookup Indices in Opcode Order
;------------------------------------------------------------------------------
mnetabidx:
	.byte	mne_brkx		; $00  BRK
 	.byte	mne_orax		; $01  ORA (dp,X)
 	.byte	mne_copx		; $02  COP
 	.byte	mne_orax		; $03  ORA <offset>,S
 	.byte	mne_tsbx		; $04  TSB dp
 	.byte	mne_orax		; $05  ORA dp
 	.byte	mne_aslx		; $06  ASL dp
 	.byte	mne_orax		; $07  ORA [dp]
 	.byte	mne_phpx		; $08  PHP
 	.byte	mne_orax		; $09  ORA #
 	.byte	mne_aslx		; $0A  ASL A
 	.byte	mne_phdx		; $0B  PHD
 	.byte	mne_tsbx		; $0C  TSB abs
 	.byte	mne_orax		; $0D  ORA abs
 	.byte	mne_aslx		; $0E  ASL abs
 	.byte	mne_orax		; $0F  ORA absl
;
 	.byte	mne_bplx		; $10  BPL abs
 	.byte	mne_orax		; $11  ORA (<dp>),Y
 	.byte	mne_orax		; $12  ORA (dp)
 	.byte	mne_orax		; $13  ORA (<offset>,S),Y
 	.byte	mne_trbx		; $14  TRB dp
 	.byte	mne_orax		; $15  ORA dp,X
 	.byte	mne_aslx		; $16  ASL dp,X
 	.byte	mne_orax		; $17  ORA [dp],Y
 	.byte	mne_clcx		; $18  CLC
 	.byte	mne_orax		; $19  ORA abs
 	.byte	mne_incx		; $1A  INC A
 	.byte	mne_tcsx		; $1B  TCS
 	.byte	mne_trbx		; $1C  TRB abs
 	.byte	mne_orax		; $1D  ORA abs,X
 	.byte	mne_aslx		; $1E  ASL abs,X
 	.byte	mne_orax		; $1F  ORA absl,X
;
 	.byte	mne_jsrx		; $20  JSR abs
 	.byte	mne_andx		; $21  AND (dp,X)
 	.byte	mne_jslx		; $22  JSL absl
 	.byte	mne_andx		; $23  AND <offset>,S
 	.byte	mne_bitx		; $24  BIT dp
 	.byte	mne_andx		; $25  AND dp
 	.byte	mne_rolx		; $26  ROL dp
 	.byte	mne_andx		; $27  AND [dp]
 	.byte	mne_plpx		; $28  PLP
 	.byte	mne_andx		; $29  AND #
 	.byte	mne_rolx		; $2A  ROL A
 	.byte	mne_pldx		; $2B  PLD
 	.byte	mne_bitx		; $2C  BIT abs
 	.byte	mne_andx		; $2D  AND abs
 	.byte	mne_rolx		; $2E  ROL abs
 	.byte	mne_andx		; $2F  AND absl
;
 	.byte	mne_bmix		; $30  BMI abs
 	.byte	mne_andx		; $31  AND (<dp>),Y
 	.byte	mne_andx		; $32  AND (dp)
 	.byte	mne_andx		; $33  AND (<offset>,S),Y
 	.byte	mne_bitx		; $34  BIT dp,X
 	.byte	mne_andx		; $35  AND dp,X
 	.byte	mne_rolx		; $36  ROL dp,X
 	.byte	mne_andx		; $37  AND [dp],Y
 	.byte	mne_secx		; $38  SEC
 	.byte	mne_andx		; $39  AND abs,Y
 	.byte	mne_decx		; $3A  DEC A
 	.byte	mne_tscx		; $3B  TSC
 	.byte	mne_bitx		; $3C  BIT abs,X
 	.byte	mne_andx		; $3D  AND abs,X
 	.byte	mne_rolx		; $3E  ROL abs,X
 	.byte	mne_andx		; $3F  AND absl,X
;
 	.byte	mne_rtix		; $40  RTI
 	.byte	mne_eorx		; $41  EOR (dp,X)
 	.byte	mne_wdmx		; $42  WDM
 	.byte	mne_eorx		; $43  EOR <offset>,S
 	.byte	mne_mvpx		; $44  MVP sb,db
 	.byte	mne_eorx		; $45  EOR dp
 	.byte	mne_lsrx		; $46  LSR dp
 	.byte	mne_eorx		; $47  EOR [dp]
 	.byte	mne_phax		; $48  PHA
 	.byte	mne_eorx		; $49  EOR #
 	.byte	mne_lsrx		; $4A  LSR A
 	.byte	mne_phkx		; $4B  PHK
 	.byte	mne_jmpx		; $4C  JMP abs
 	.byte	mne_eorx		; $4D  EOR abs
 	.byte	mne_lsrx		; $4E  LSR abs
 	.byte	mne_eorx		; $4F  EOR absl
;
 	.byte	mne_bvcx		; $50  BVC abs
 	.byte	mne_eorx		; $51  EOR (<dp>),Y
 	.byte	mne_eorx		; $52  EOR (dp)
 	.byte	mne_eorx		; $53  EOR (<offset>,S),Y
 	.byte	mne_mvnx		; $54  MVN sb,db
 	.byte	mne_eorx		; $55  EOR dp,X
 	.byte	mne_lsrx		; $56  LSR dp,X
 	.byte	mne_eorx		; $57  EOR [dp],Y
 	.byte	mne_clix		; $58  CLI
 	.byte	mne_eorx		; $59  EOR abs,Y
 	.byte	mne_phyx		; $5A  PHY
 	.byte	mne_tcdx		; $5B  TCD
 	.byte	mne_jmlx		; $5C  JML absl
 	.byte	mne_eorx		; $5D  EOR abs,X
 	.byte	mne_lsrx		; $5E  LSR abs,X
 	.byte	mne_eorx		; $5F  EOR absl,X
;
 	.byte	mne_rtsx		; $60  RTS
 	.byte	mne_adcx		; $61  ADC (dp,X)
 	.byte	mne_perx		; $62  PER
 	.byte	mne_adcx		; $63  ADC <offset>,S
 	.byte	mne_stzx		; $64  STZ dp
 	.byte	mne_adcx		; $65  ADC dp
 	.byte	mne_rorx		; $66  ROR dp
 	.byte	mne_adcx		; $67  ADC [dp]
 	.byte	mne_plax		; $68  PLA
 	.byte	mne_adcx		; $69  ADC #
 	.byte	mne_rorx		; $6A  ROR A
 	.byte	mne_rtlx		; $6B  RTL
 	.byte	mne_jmpx		; $6C  JMP (abs)
 	.byte	mne_adcx		; $6D  ADC abs
 	.byte	mne_rorx		; $6E  ROR abs
 	.byte	mne_adcx		; $6F  ADC absl
;
 	.byte	mne_bvsx		; $70  BVS abs
 	.byte	mne_adcx		; $71  ADC (<dp>),Y
 	.byte	mne_adcx		; $72  ADC (dp)
 	.byte	mne_adcx		; $73  ADC (<offset>,S),Y
 	.byte	mne_stzx		; $74  STZ dp,X
 	.byte	mne_adcx		; $75  ADC dp,X
 	.byte	mne_rorx		; $76  ROR dp,X
 	.byte	mne_adcx		; $77  ADC [dp],Y
 	.byte	mne_seix		; $78  SEI
 	.byte	mne_adcx		; $79  ADC abs,Y
 	.byte	mne_plyx		; $7A  PLY
 	.byte	mne_tdcx		; $7B  TDC
 	.byte	mne_jmpx		; $7C  JMP (abs,X)
 	.byte	mne_adcx		; $7D  ADC abs,X
 	.byte	mne_rorx		; $7E  ROR abs,X
 	.byte	mne_adcx		; $7F  ADC absl,X
;
 	.byte	mne_brax		; $80  BRA abs
 	.byte	mne_stax		; $81  STA (dp,X)
 	.byte	mne_brlx		; $82  BRL abs
 	.byte	mne_stax		; $83  STA <offset>,S
 	.byte	mne_styx		; $84  STY dp
 	.byte	mne_stax		; $85  STA dp
 	.byte	mne_stxx		; $86  STX dp
 	.byte	mne_stax		; $87  STA [dp]
 	.byte	mne_deyx		; $88  DEY
 	.byte	mne_bitx		; $89  BIT #
 	.byte	mne_txax		; $8A  TXA
 	.byte	mne_phbx		; $8B  PHB
 	.byte	mne_styx		; $8C  STY abs
 	.byte	mne_stax		; $8D  STA abs
 	.byte	mne_stxx		; $8E  STX abs
 	.byte	mne_stax		; $8F  STA absl
;
 	.byte	mne_bccx		; $90  BCC abs
 	.byte	mne_stax		; $91  STA (<dp>),Y
 	.byte	mne_stax		; $92  STA (dp)
 	.byte	mne_stax		; $93  STA (<offset>,S),Y
 	.byte	mne_styx		; $94  STY dp,X
 	.byte	mne_stax		; $95  STA dp,X
 	.byte	mne_stxx		; $96  STX dp,Y
 	.byte	mne_stax		; $97  STA [dp],Y
 	.byte	mne_tyax		; $98  TYA
 	.byte	mne_stax		; $99  STA abs,Y
 	.byte	mne_txsx		; $9A  TXS
 	.byte	mne_txyx		; $9B  TXY
 	.byte	mne_stzx		; $9C  STZ abs
 	.byte	mne_stax		; $9D  STA abs,X
 	.byte	mne_stzx		; $9E  STZ abs,X
 	.byte	mne_stax		; $9F  STA absl,X
;
 	.byte	mne_ldyx		; $A0  LDY #
 	.byte	mne_ldax		; $A1  LDA (dp,X)
 	.byte	mne_ldxx		; $A2  LDX #
 	.byte	mne_ldax		; $A3  LDA <offset>,S
 	.byte	mne_ldyx		; $A4  LDY dp
 	.byte	mne_ldax		; $A5  LDA dp
 	.byte	mne_ldxx		; $A6  LDX dp
 	.byte	mne_ldax		; $A7  LDA [dp]
 	.byte	mne_tayx		; $A8  TAY
 	.byte	mne_ldax		; $A9  LDA #
 	.byte	mne_taxx		; $AA  TAX
 	.byte	mne_plbx		; $AB  PLB
 	.byte	mne_ldyx		; $AC  LDY abs
 	.byte	mne_ldax		; $AD  LDA abs
 	.byte	mne_ldxx		; $AE  LDX abs
 	.byte	mne_ldax		; $AF  LDA absl
;
 	.byte	mne_bcsx		; $B0  BCS abs
 	.byte	mne_ldax		; $B1  LDA (<dp>),Y
 	.byte	mne_ldax		; $B2  LDA (dp)
 	.byte	mne_ldax		; $B3  LDA (<offset>,S),Y
 	.byte	mne_ldyx		; $B4  LDY dp,X
 	.byte	mne_ldax		; $B5  LDA dp,X
 	.byte	mne_ldxx		; $B6  LDX dp,Y
 	.byte	mne_ldax		; $B7  LDA [dp],Y
 	.byte	mne_clvx		; $B8  CLV
 	.byte	mne_ldax		; $B9  LDA abs,Y
 	.byte	mne_tsxx		; $BA  TSX
 	.byte	mne_tyxx		; $BB  TYX
 	.byte	mne_ldyx		; $BC  LDY abs,X
 	.byte	mne_ldax		; $BD  LDA abs,X
 	.byte	mne_ldxx		; $BE  LDX abs,Y
 	.byte	mne_ldax		; $BF  LDA absl,X
;
 	.byte	mne_cpyx		; $C0  CPY #
 	.byte	mne_cmpx		; $C1  CMP (dp,X)
 	.byte	mne_repx		; $C2  REP #
 	.byte	mne_cmpx		; $C3  CMP <offset>,S
 	.byte	mne_cpyx		; $C4  CPY dp
 	.byte	mne_cmpx		; $C5  CMP dp
 	.byte	mne_decx		; $C6  DEC dp
 	.byte	mne_cmpx		; $C7  CMP [dp]
 	.byte	mne_inyx		; $C8  INY
 	.byte	mne_cmpx		; $C9  CMP #
 	.byte	mne_dexx		; $CA  DEX
 	.byte	mne_waix		; $CB  WAI
 	.byte	mne_cpyx		; $CC  CPY abs
 	.byte	mne_cmpx		; $CD  CMP abs
 	.byte	mne_decx		; $CE  DEC abs
 	.byte	mne_cmpx		; $CF  CMP absl
;
 	.byte	mne_bnex		; $D0  BNE abs
 	.byte	mne_cmpx		; $D1  CMP (<dp>),Y
 	.byte	mne_cmpx		; $D2  CMP (dp)
 	.byte	mne_cmpx		; $D3  CMP (<offset>,S),Y
 	.byte	mne_peix		; $D4  PEI dp
 	.byte	mne_cmpx		; $D5  CMP dp,X
 	.byte	mne_decx		; $D6  DEC dp,X
 	.byte	mne_cmpx		; $D7  CMP [dp],Y
 	.byte	mne_cldx		; $D8  CLD
 	.byte	mne_cmpx		; $D9  CMP abs,Y
 	.byte	mne_phxx		; $DA  PHX
 	.byte	mne_stpx		; $DB  STP
 	.byte	mne_jmpx		; $DC  JMP [abs]
 	.byte	mne_cmpx		; $DD  CMP abs,X
 	.byte	mne_decx		; $DE  DEC abs,X
 	.byte	mne_cmpx		; $DF  CMP absl,X
;
 	.byte	mne_cpxx		; $E0  CPX #
 	.byte	mne_sbcx		; $E1  SBC (dp,X)
 	.byte	mne_sepx		; $E2  SEP #
 	.byte	mne_sbcx		; $E3  SBC <offset>,S
 	.byte	mne_cpxx		; $E4  CPX dp
 	.byte	mne_sbcx		; $E5  SBC dp
 	.byte	mne_incx		; $E6  INC dp
 	.byte	mne_sbcx		; $E7  SBC [dp]
 	.byte	mne_inxx		; $E8  INX
 	.byte	mne_sbcx		; $E9  SBC #
 	.byte	mne_nopx		; $EA  NOP
 	.byte	mne_xbax		; $EB  XBA
 	.byte	mne_cpxx		; $EC  CPX abs
 	.byte	mne_sbcx		; $ED  SBC abs
 	.byte	mne_incx		; $EE  INC abs
 	.byte	mne_sbcx		; $EF  SBC absl
;
 	.byte	mne_beqx		; $F0  BEQ abs
 	.byte	mne_sbcx		; $F1  SBC (<dp>),Y
 	.byte	mne_sbcx		; $F2  SBC (dp)
 	.byte	mne_sbcx		; $F3  SBC (<offset>,S),Y
 	.byte	mne_peax		; $F4  PEA #
 	.byte	mne_sbcx		; $F5  SBC dp,X
 	.byte	mne_incx		; $F6  INC dp,X
 	.byte	mne_sbcx		; $F7  SBC [dp],Y
 	.byte	mne_sedx		; $F8  SED
 	.byte	mne_sbcx		; $F9  SBC abs,Y
 	.byte	mne_plxx		; $FA  PLX
 	.byte	mne_xcex		; $FB  XCE
 	.byte	mne_jsrx		; $FC  JSR (abs,X)
 	.byte	mne_sbcx		; $FD  SBC abs,X
 	.byte	mne_incx		; $FE  INC abs,X
 	.byte	mne_sbcx		; $FF  SBC absl,X

;------------------------------------------------------------------------------
; Instruction Addressing Modes and Sizes in Opcode Order...
;
;	    xxxxxxxx
;	    ||||||||
;	    ||||++++---> Addressing Mode
;	    ||||         ----------------------------------
;	    ||||          0000  dp, abs, absl, implied or A
;	    ||||          0001  #
;	    ||||          0010  dp,X, abs,X or absl,X
;	    ||||          0011  dp,Y or abs,Y
;	    ||||          0100  (dp) or (abs)
;	    ||||          0101  [dp] or [abs]
;	    ||||          0110  [dp],Y
;	    ||||          0111  (dp,X) or (abs,X)
;	    ||||          1000  (<dp>),Y
;	    ||||          1001  <offset>,S
;	    ||||          1010  (<offset>,S),Y
;	    ||||          1011  sbnk,dbnk (MVN or MVP)
;	    ||||          ---------------------------------
;	    ||||           #    = immediate
;	    ||||           A    = accumulator
;	    ||||           abs  = absolute
;	    ||||           absl = absolute long
;	    ||||           dbnk = destination bank
;	    ||||           dp   = direct (zero) page
;	    ||||           S    = stack relative
;	    ||||           sbnk = source bank
;	    ||||         ----------------------------------
;	    ||||
;	    ||++-------> binary-encoded operand size
;	    |+---------> 1: relative branch instruction
;	    +----------> 1: variable operand size...
;
;	    Variable operand size refers to an immediate mode instruction
;	    that can accept either an 8 or 16 bit operand.  During instr-
;	    uction assembly, an 8 bit operand can be forced to 16 bits by
;	    preceding the operand field with !,  e.g.,  LDA !#$01,  which
;	    will assemble as $A9 $01 $00.
;------------------------------------------------------------------------------
mnetabam:
	.byte	ops0|am_nam 		; $00  BRK
	.byte	ops1|am_indx		; $01  ORA (dp,X)
	.byte	ops1|am_nam 		; $02  COP
	.byte	ops1|am_stk 		; $03  ORA <offset>,S
	.byte	ops1|am_nam 		; $04  TSB dp
	.byte	ops1|am_nam 		; $05  ORA dp
	.byte	ops1|am_nam 		; $06  ASL dp
	.byte	ops1|am_indl		; $07  ORA [dp]
	.byte	ops0|am_nam 		; $08  PHP
	.byte	vops|am_imm 		; $09  ORA #
	.byte	ops0|am_nam 		; $0A  ASL A
	.byte	ops0|am_nam 		; $0B  PHD
	.byte	ops2|am_nam 		; $0C  TSB abs
	.byte	ops2|am_nam 		; $0D  ORA abs
	.byte	ops2|am_nam 		; $0E  ASL abs
	.byte	ops3|am_nam 		; $0F  ORA absl
;
	.byte	bop1|am_nam 		; $10  BPL abs
	.byte	ops1|am_indy		; $11  ORA (<dp>),Y
	.byte	ops1|am_ind 		; $12  ORA (dp)
	.byte	ops1|am_stky		; $13  ORA (<offset>,S),Y
	.byte	ops1|am_nam 		; $14  TRB dp
	.byte	ops1|am_adrx		; $15  ORA dp,X
	.byte	ops1|am_adrx		; $16  ASL dp,X
	.byte	ops1|am_indly 		; $17  ORA [dp],Y
	.byte	ops0|am_nam 		; $18  CLC
	.byte	ops2|am_nam 		; $19  ORA abs
	.byte	ops0|am_nam 		; $1A  INC A
	.byte	ops0|am_nam 		; $1B  TCS
	.byte	ops2|am_nam 		; $1C  TRB abs
	.byte	ops2|am_adrx		; $1D  ORA abs,X
	.byte	ops2|am_adrx		; $1E  ASL abs,X
	.byte	ops3|am_adrx		; $1F  ORA absl,X
;
	.byte	ops2|am_nam 		; $20  JSR abs
	.byte	ops1|am_indx		; $21  AND (dp,X)
	.byte	ops3|am_nam 		; $22  JSL absl
	.byte	ops1|am_stk 		; $23  AND <offset>,S
	.byte	ops1|am_nam 		; $24  BIT dp
	.byte	ops1|am_nam 		; $25  AND dp
	.byte	ops1|am_nam 		; $26  ROL dp
	.byte	ops1|am_indl		; $27  AND [dp]
	.byte	ops0|am_nam 		; $28  PLP
	.byte	vops|am_imm 		; $29  AND #
	.byte	ops0|am_nam 		; $2A  ROL A
	.byte	ops0|am_nam 		; $2B  PLD
	.byte	ops2|am_nam 		; $2C  BIT abs
	.byte	ops2|am_nam 		; $2D  AND abs
	.byte	ops2|am_nam 		; $2E  ROL abs
	.byte	ops3|am_nam 		; $2F  AND absl
;
	.byte	bop1|am_nam 		; $30  BMI abs
	.byte	ops1|am_indy		; $31  AND (<dp>),Y
	.byte	ops1|am_ind 		; $32  AND (dp)
	.byte	ops1|am_stky		; $33  AND (<offset>,S),Y
	.byte	ops1|am_adrx		; $34  BIT dp,X
	.byte	ops1|am_adrx		; $35  AND dp,X
	.byte	ops1|am_adrx		; $36  ROL dp,X
	.byte	ops1|am_indly 		; $37  AND [dp],Y
	.byte	ops0|am_nam 		; $38  SEC
	.byte	ops2|am_adry		; $39  AND abs,Y
	.byte	ops0|am_nam 		; $3A  DEC A
	.byte	ops0|am_nam 		; $3B  TSC
	.byte	ops2|am_adrx		; $3C  BIT abs,X
	.byte	ops2|am_adrx		; $3D  AND abs,X
	.byte	ops2|am_adrx		; $3E  ROL abs,X
	.byte	ops3|am_adrx		; $3F  AND absl,X
;
	.byte	ops0|am_nam 		; $40  RTI
	.byte	ops1|am_indx		; $41  EOR (dp,X)
	.byte	ops0|am_nam 		; $42  WDM
	.byte	ops1|am_stk 		; $43  EOR <offset>,S
	.byte	ops2|am_move		; $44  MVP sb,db
	.byte	ops1|am_nam 		; $45  EOR dp
	.byte	ops1|am_nam 		; $46  LSR dp
	.byte	ops1|am_indl		; $47  EOR [dp]
	.byte	ops0|am_nam 		; $48  PHA
	.byte	vops|am_imm 		; $49  EOR #
	.byte	ops0|am_nam 		; $4A  LSR A
	.byte	ops0|am_nam 		; $4B  PHK
	.byte	ops2|am_nam 		; $4C  JMP abs
	.byte	ops2|am_nam 		; $4D  EOR abs
	.byte	ops2|am_nam 		; $4E  LSR abs
	.byte	ops3|am_nam 		; $4F  EOR absl
;
	.byte	bop1|am_nam 		; $50  BVC abs
	.byte	ops1|am_indy		; $51  EOR (<dp>),Y
	.byte	ops1|am_ind 		; $52  EOR (dp)
	.byte	ops1|am_stky		; $53  EOR (<offset>,S),Y
	.byte	ops2|am_move		; $54  MVN sb,db
	.byte	ops1|am_adrx		; $55  EOR dp,X
	.byte	ops1|am_adrx		; $56  LSR dp,X
	.byte	ops1|am_indly 		; $57  EOR [dp],Y
	.byte	ops0|am_nam 		; $58  CLI
	.byte	ops2|am_adry		; $59  EOR abs,Y
	.byte	ops0|am_nam 		; $5A  PHY
	.byte	ops0|am_nam 		; $5B  TCD
	.byte	ops3|am_nam 		; $5C  JML absl
	.byte	ops2|am_adrx		; $5D  EOR abs,X
	.byte	ops2|am_adrx		; $5E  LSR abs,X
	.byte	ops3|am_adrx		; $5F  EOR absl,X
;
	.byte	ops0|am_nam 		; $60  RTS
	.byte	ops1|am_indx		; $61  ADC (dp,X)
	.byte	bop2|am_nam 		; $62  PER
	.byte	ops1|am_stk 		; $63  ADC <offset>,S
	.byte	ops1|am_nam 		; $64  STZ dp
	.byte	ops1|am_nam 		; $65  ADC dp
	.byte	ops1|am_nam 		; $66  ROR dp
	.byte	ops1|am_indl		; $67  ADC [dp]
	.byte	ops0|am_nam 		; $68  PLA
	.byte	vops|am_imm 		; $69  ADC #
	.byte	ops0|am_nam 		; $6A  ROR A
	.byte	ops0|am_nam 		; $6B  RTL
	.byte	ops2|am_ind 		; $6C  JMP (abs)
	.byte	ops2|am_nam 		; $6D  ADC abs
	.byte	ops2|am_nam 		; $6E  ROR abs
	.byte	ops3|am_nam 		; $6F  ADC absl
;
	.byte	bop1|am_nam 		; $70  BVS abs
	.byte	ops1|am_indy		; $71  ADC (<dp>),Y
	.byte	ops1|am_ind 		; $72  ADC (dp)
	.byte	ops1|am_stky		; $73  ADC (<offset>,S),Y
	.byte	ops1|am_adrx		; $74  STZ dp,X
	.byte	ops1|am_adrx		; $75  ADC dp,X
	.byte	ops1|am_adrx		; $76  ROR dp,X
	.byte	ops1|am_indly 		; $77  ADC [dp],Y
	.byte	ops0|am_nam 		; $78  SEI
	.byte	ops2|am_adry		; $79  ADC abs,Y
	.byte	ops0|am_nam 		; $7A  PLY
	.byte	ops0|am_nam 		; $7B  TDC
	.byte	ops2|am_indx		; $7C  JMP (abs,X)
	.byte	ops2|am_adrx		; $7D  ADC abs,X
	.byte	ops2|am_adrx		; $7E  ROR abs,X
	.byte	ops3|am_adrx		; $7F  ADC absl,X
;
	.byte	bop1|am_nam 		; $80  BRA abs
	.byte	ops1|am_indx		; $81  STA (dp,X)
	.byte	bop2|am_nam 		; $82  BRL abs
	.byte	ops1|am_stk 		; $83  STA <offset>,S
	.byte	ops1|am_nam 		; $84  STY dp
	.byte	ops1|am_nam 		; $85  STA dp
	.byte	ops1|am_nam 		; $86  STX dp
	.byte	ops1|am_indl		; $87  STA [dp]
	.byte	ops0|am_nam 		; $88  DEY
	.byte	vops|am_imm 		; $89  BIT #
	.byte	ops0|am_nam 		; $8A  TXA
	.byte	ops0|am_nam 		; $8B  PHB
	.byte	ops2|am_nam 		; $8C  STY abs
	.byte	ops2|am_nam 		; $8D  STA abs
	.byte	ops2|am_nam 		; $8E  STX abs
	.byte	ops3|am_nam 		; $8F  STA absl
;
	.byte	bop1|am_nam 		; $90  BCC abs
	.byte	ops1|am_indy		; $91  STA (<dp>),Y
	.byte	ops1|am_ind 		; $92  STA (dp)
	.byte	ops1|am_stky		; $93  STA (<offset>,S),Y
	.byte	ops1|am_adrx		; $94  STY dp,X
	.byte	ops1|am_adrx		; $95  STA dp,X
	.byte	ops1|am_adry		; $96  STX dp,Y
	.byte	ops1|am_indly 		; $97  STA [dp],Y
	.byte	ops0|am_nam 		; $98  TYA
	.byte	ops2|am_adry		; $99  STA abs,Y
	.byte	ops0|am_nam 		; $9A  TXS
	.byte	ops0|am_nam 		; $9B  TXY
	.byte	ops2|am_nam 		; $9C  STZ abs
	.byte	ops2|am_adrx		; $9D  STA abs,X
	.byte	ops2|am_adrx		; $9E  STZ abs,X
	.byte	ops3|am_adrx		; $9F  STA absl,X
;
	.byte	vops|am_imm 		; $A0  LDY #
	.byte	ops1|am_indx		; $A1  LDA (dp,X)
	.byte	vops|am_imm 		; $A2  LDX #
	.byte	ops1|am_stk 		; $A3  LDA <offset>,S
	.byte	ops1|am_nam 		; $A4  LDY dp
	.byte	ops1|am_nam 		; $A5  LDA dp
	.byte	ops1|am_nam 		; $A6  LDX dp
	.byte	ops1|am_indl		; $A7  LDA [dp]
	.byte	ops0|am_nam 		; $A8  TAY
	.byte	vops|am_imm 		; $A9  LDA #
	.byte	ops0|am_nam 		; $AA  TAX
	.byte	ops0|am_nam 		; $AB  PLB
	.byte	ops2|am_nam 		; $AC  LDY abs
	.byte	ops2|am_nam 		; $AD  LDA abs
	.byte	ops2|am_nam 		; $AE  LDX abs
	.byte	ops3|am_nam 		; $AF  LDA absl
;
	.byte	bop1|am_nam 		; $B0  BCS abs
	.byte	ops1|am_indy		; $B1  LDA (<dp>),Y
	.byte	ops1|am_ind 		; $B2  LDA (dp)
	.byte	ops1|am_stky		; $B3  LDA (<offset>,S),Y
	.byte	ops1|am_adrx		; $B4  LDY dp,X
	.byte	ops1|am_adrx		; $B5  LDA dp,X
	.byte	ops1|am_adry		; $B6  LDX dp,Y
	.byte	ops1|am_indly 		; $B7  LDA [dp],Y
	.byte	ops0|am_nam 		; $B8  CLV
	.byte	ops2|am_adry		; $B9  LDA abs,Y
	.byte	ops0|am_nam 		; $BA  TSX
	.byte	ops0|am_nam 		; $BB  TYX
	.byte	ops2|am_adrx		; $BC  LDY abs,X
	.byte	ops2|am_adrx		; $BD  LDA abs,X
	.byte	ops2|am_adry		; $BE  LDX abs,Y
	.byte	ops3|am_adrx		; $BF  LDA absl,X
;
	.byte	vops|am_imm 		; $C0  CPY #
	.byte	ops1|am_indx		; $C1  CMP (dp,X)
	.byte	ops1|am_imm 		; $C2  REP #
	.byte	ops1|am_stk 		; $C3  CMP <offset>,S
	.byte	ops1|am_nam 		; $C4  CPY dp
	.byte	ops1|am_nam 		; $C5  CMP dp
	.byte	ops1|am_nam 		; $C6  DEC dp
	.byte	ops1|am_indl		; $C7  CMP [dp]
	.byte	ops0|am_nam 		; $C8  INY
	.byte	vops|am_imm 		; $C9  CMP #
	.byte	ops0|am_nam 		; $CA  DEX
	.byte	ops0|am_nam 		; $CB  WAI
	.byte	ops2|am_nam 		; $CC  CPY abs
	.byte	ops2|am_nam 		; $CD  CMP abs
	.byte	ops2|am_nam 		; $CE  DEC abs
	.byte	ops3|am_nam 		; $CF  CMP absl
;
	.byte	bop1|am_nam 		; $D0  BNE abs
	.byte	ops1|am_indy		; $D1  CMP (<dp>),Y
	.byte	ops1|am_ind 		; $D2  CMP (dp)
	.byte	ops1|am_stky		; $D3  CMP (<offset>,S),Y
	.byte	ops1|am_nam 		; $D4  PEI dp
	.byte	ops1|am_adrx		; $D5  CMP dp,X
	.byte	ops1|am_adrx		; $D6  DEC dp,X
	.byte	ops1|am_indly 		; $D7  CMP [dp],Y
	.byte	ops0|am_nam 		; $D8  CLD
	.byte	ops2|am_adry		; $D9  CMP abs,Y
	.byte	ops0|am_nam 		; $DA  PHX
	.byte	ops0|am_nam 		; $DB  STP
	.byte	ops2|am_indl		; $DC  JMP [abs]
	.byte	ops2|am_adrx		; $DD  CMP abs,X
	.byte	ops2|am_adrx		; $DE  DEC abs,X
	.byte	ops3|am_adrx		; $DF  CMP absl,X
;
	.byte	vops|am_imm 		; $E0  CPX #
	.byte	ops1|am_indx		; $E1  SBC (dp,X)
	.byte	ops1|am_imm 		; $E2  SEP #
	.byte	ops1|am_stk 		; $E3  SBC <offset>,S
	.byte	ops1|am_nam 		; $E4  CPX dp
	.byte	ops1|am_nam 		; $E5  SBC dp
	.byte	ops1|am_nam 		; $E6  INC dp
	.byte	ops1|am_indl		; $E7  SBC [dp]
	.byte	ops0|am_nam 		; $E8  INX
	.byte	vops|am_imm 		; $E9  SBC #
	.byte	ops0|am_nam 		; $EA  NOP
	.byte	ops0|am_nam 		; $EB  XBA
	.byte	ops2|am_nam 		; $EC  CPX abs
	.byte	ops2|am_nam 		; $ED  SBC abs
	.byte	ops2|am_nam 		; $EE  INC abs
	.byte	ops3|am_nam 		; $EF  SBC absl
;
	.byte	bop1|am_nam 		; $F0  BEQ abs
	.byte	ops1|am_indy		; $F1  SBC (<dp>),Y
	.byte	ops1|am_ind 		; $F2  SBC (dp)
	.byte	ops1|am_stky		; $F3  SBC (<offset>,S),Y
	.byte	ops2|am_imm 		; $F4  PEA #
	.byte	ops1|am_adrx		; $F5  SBC dp,X
	.byte	ops1|am_adrx		; $F6  INC dp,X
	.byte	ops1|am_indly 		; $F7  SBC [dp],Y
	.byte	ops0|am_nam 		; $F8  SED
	.byte	ops2|am_adry		; $F9  SBC abs,Y
	.byte	ops0|am_nam 		; $FA  PLX
	.byte	ops0|am_nam 		; $FB  XCE
	.byte	ops2|am_indx		; $FC  JSR (abs,X)
	.byte	ops2|am_adrx		; $FD  SBC abs,X
	.byte	ops2|am_adrx		; $FE  INC abs,X
	.byte	ops3|am_adrx		; $FF  SBC absl,X

;------------------------------------------------------------------------------
; .X & .Y immediate mode opcodes...
;
;------------------------------------------------------------------------------
vopidx:
	.byte	opc_cpxi        	; CPX #
	.byte	opc_cpyi        	; CPY #
	.byte	opc_ldxi        	; LDX #
	.byte	opc_ldyi        	; LDY #
n_vopidx	= * - vopidx		; # of opcodes

;------------------------------------------------------------------------------
; Addressing Mode Symbology Lookup
;------------------------------------------------------------------------------
ms_lutab:
	.word	ms_nam			; No symbol
	.word	ms_imm			; #
	.word	ms_addrx		; <addr>, X
	.word	ms_addry		; <addr>, Y
	.word	ms_ind			; (<addr>)
	.word	ms_indl			; [<dp>]
	.word	ms_indly		; [<dp>], Y
	.word	ms_indx			; (<addr>, X)
	.word	ms_indy			; (<dp>), Y
	.word	ms_stk			; <offset>, S
	.word	ms_stky			; (<offset>, S), Y
	.word	ms_nam			; <sbank>, <dbank>
	
;------------------------------------------------------------------------------
; Addressing Mode Symbology String
;------------------------------------------------------------------------------
ms_nam:
	.byte	' ',$00			; No symbol
ms_addrx:
	.byte	" ,X", $00		; <addr>, X
ms_addry:
	.byte	" ,Y", $00		; <addr>, Y
ms_imm:
	.byte	'#', $00		; #
ms_ind:
	.byte	"()", $00		; (<addr>)
ms_indl:
	.byte	"[]", $00		; [<dp>]
ms_indly:
	.byte	"[],Y", $00		; [<dp>], Y
ms_indx:
	.byte	"(,X)", $00		; (<addr>, X)
ms_indy:
	.byte	"(),Y", $00		; (<dp>), Y
ms_move:
	.byte	",$", $00		; <sbank>, <dbank>
ms_stk:
	.byte	" ,S", $00		; <offset>, S
ms_stky:
	.byte	"(,S),Y", $00		; (<offset>, S), Y
	
;------------------------------------------------------------------------------
; Console Display Control Strings
;------------------------------------------------------------------------------
;dc_bf:
;	.byte	BF			; Enable reverse foreground
;	.byte	$00
	
dc_bs:
	.byte	BS			; Destructive backspace
	.byte	$00
	
;dc_cl:
;	.byte	CEOL			; Clear to End Of Line
;	.byte	$00
	
;dc_cn:
;	.byte	CN			; Cursor On
;	.byte	$00
	
;dc_co:
;	.byte	CO			; Cursor Off
;	.byte	$00
	
;dc_er:
;	.byte	ER			; Enable normal foreground
;	.byte	$00
	
dc_lf:
	.byte	LF			; Newline
	.byte	CR			; <CR>
	.byte	$00
	
;------------------------------------------------------------------------------
; Text Strings
;------------------------------------------------------------------------------
mm_brk:
	.byte	BELL
	.byte	CR, LF
	.byte	"*BRK"
	.byte 	$00
	
mm_entry:
	.byte	CR, LF
	.byte	"Supermon 816 "
	softvers
	.byte 	CR, LF, "(Implementation for Adria "
	bios_version
	.byte 	")", CR, LF, $00
	
mm_err:
	.byte	"*ERR ", $00
	
mm_prmpt:
	.byte 	CR, LF
	.byte	'.'
	.byte	$00
	
mm_regs:
	.byte	CR, LF
	.byte	"  PB  PC   NVmxDIZC  .C   .X   .Y   SP   DP  DB"
	.byte	CR, LF
	.byte	':', ' '
	.byte	$00
	
mm_rts:
	.byte	BELL
	.byte	CR, LF
	.byte	"*RTS"
	.byte	CR, LF
	.byte	$00
	
	
mm_hlp:
	.byte	CR, LF
	.byte	"Supermon 816 "
	softvers
	.byte	" Help",CR, LF
	.byte	CR, LF
	.byte	"A - Assemble code", CR, LF
	.byte	"C - Compare memory ranges", CR, LF
	.byte	"D - Disassemble code", CR, LF
	.byte	"F - Fill memory", CR, LF
	.byte	"G - Execute code (JMP)", CR, LF
	.byte	"H - Search memory", CR, LF
	.byte	"J - Execute code as subroutine (JSR)", CR, LF
	.byte	"L - Load code (Intel Hex)", CR, LF
	.byte	"M - Dump memory range", CR, LF
	.byte	"R - Dump registers", CR, LF
	.byte	"T - Copy memory range", CR, LF
	.byte	"> - Change memory", CR, LF
	.byte	"; - Change registers", CR, LF
	.byte	"? - Help (this)", CR, LF
	.byte	$00

; Descriptive strings for monitor routines (used with LCD)?
;		 0123456789ABCDEF
mm_monce:
	.asciiz	"Monitor"
mm_monerr:
	.asciiz	"**Error"
mm_monasc:
	.asciiz	"Assembler"
mm_mondsc:
	.asciiz	"Disassembler"
mm_monjmp:
	.asciiz	"JMP routine"
mm_monjsr:
	.asciiz	"JSR routine"
mm_monchm:
	.asciiz	"Change memory"
mm_moncmp:
	.asciiz "Compare momory"
mm_moncpy:
	.asciiz	"Copy memory"
mm_mondmp:
	.asciiz	"Dump memory"
mm_monfil:
	.asciiz	"Fill memory"
mm_monhnt:
	.asciiz	"Hunt memory"

;---------------------------------------
; Temporary used to determine zeropage (DP) usage
; Comment when not needed
;---------------------------------------
ZPUsage:
; hexintel
	.byte	DPL
	.byte	DPH
	.byte	RECLEN
	.byte	START_LO
	.byte	START_HI
	.byte	RECTYPE
	.byte	RECCHKSUM
	.byte	DLFAIL
	.byte	TEMP
; Monitor
	.byte	reg_pbx
	.byte	reg_pcx
	.byte	reg_srx
	.byte	reg_ax
	.byte	reg_xx
	.byte	reg_yx
	.byte	reg_spx
	.byte	reg_dpx
	.byte	reg_dbx
	.byte	addra
	.byte	addrb
	.byte	faca
	.byte	facax
	.byte	facb
	.byte	facc
	.byte	operand
	.byte	auxbufindex
	.byte	ibufidx
	.byte	bitsdig
	.byte	numeral
	.byte	radix
	.byte	admodidx
	.byte	charcnt
	.byte	instsize
	.byte	mnepck
	.byte	opcode
	.byte	status
	.byte	xrtemp
	.byte	eopsize
	.byte	flimflag
	.byte	vecbrkia
	.byte	iopsize
	.byte	range
	.byte	vopsflag
	.byte	mcftwork
	.byte	mcftopc
	.byte	mcftbnk
	.word	ibuffer
	.word	auxbuf
; (g)LCD specific
;	.word	dpy_data_buf
;	.byte	dpy_data_size
;	.word	dpy_data_max
;
; System wide vectors
	.word	VECTOR_BRK
	.word	VECTOR_NMI
	.word	VECTOR_INT
	.word	VECTOR_RES
	.word	VECTOR_CHAR_IN
	.word	VECTOR_CHAR_SCAN
	.word	VECTOR_CHAR_OUT
;---------------------------------------
; Used to calculate memory footprint of module
;---------------------------------------
_mon_end = *
_mon_size = _mon_end - SYSMONITOR
	.out .concat("Supermon816                       $", .sprintf("%04x", SYSMONITOR), "      $", .sprintf("%04x", _mon_end), "    $", .sprintf("%04x", _mon_size), "  (", .sprintf("%05d", _mon_size), ")")
;	.out .concat("-- Size of Monitor:	            $", .sprintf("%04x", _mon_size), " (", .sprintf("%05d", _mon_size), ")")

