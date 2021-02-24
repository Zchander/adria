  .org $3000
  .p816
  .smart
  
; some constants
ZERO 	=	$30
SPACE 	=	$20
COUNT   =	20 ; Primes left to find

; some zero page locations
;define CHROUT $0f ; output device (visual6502)
INDEX  	=	$06 ; Index in decimal
FLAGS  	=	$30 ; Prime flags (sieve)

  SEP	#$30		

  LDY   #COUNT              ; Set target count
  LDA   #2                  ; And starting index
  STA   INDEX
  SED                      ; Use decimal
repeat:
  TAX                      ; Is this number prime?
  LDA   FLAGS,X
  BNE checknext
  LDA   #SPACE                  ; Yes, print it

  PHA
  PHX
  PHY
  jsr	$FD0C
  PLY
  PLX
  PLA

  TXA
  LSR   A
  LSR   A
  LSR   A
  LSR   A
  BEQ skip  ; tens digit zero?
  ORA   #ZERO ; to ascii

  PHA
  PHX
  PHY
  jsr	$FD0C
  PLY
  PLX
  PLA

skip:
  TXA
  AND   #$0f  ; units digit
  ORA   #ZERO ; to ascii

  PHA
  PHX
  PHY
  jsr	$FD0C
  PLY
  PLX
  PLA
  
  DEY                          ; Found twenty?
  BEQ done                     ; Yes

  CLC
  TXA
marking:
  ADC   INDEX
  BCS clearcarry ; BREAK CS at 100 (decimal)
  TAX
  STA   FLAGS,X
  BNE marking ; UNTIL EQ ; until 100

clearcarry:
  CLC
checknext:       ; Check the next index
  LDA   INDEX
  ADC   #1 ; carry is clear
  STA   INDEX
  BNE repeat ; UNTIL EQ (to 100)

done:
  RTS
