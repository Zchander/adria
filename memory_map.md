$FFFF
$FFF0   |________Emulation mode interrupt vectors
$FFE0   |________Native mode interrupt vectors
$FD00   |________Reset routine(s)
$FCA0   |________Fixed jump table
$FC00   |________Interrupt Service Routine(s)
$D800   |________Supermon816 (adapted from original by BigDumbDinosaur)
$D000   |________Intel Hex Uploader (adapted from original by Ross Archer)       
$CD00   |________UART driver (inspired by and based on code by BigDumbDinosaur)
$C020   |________UART I/O registers (mapped to UART card using GAL)
$C000   |________Start of ROM
$BFFF   |________Supermon816 stack
        |        (growing down...)
        |
$A000   |________Bottom of stack (?), giving 8kByte of stack
$9FFF   |________Top of free RAM
        |
        |        (Giving 37.5 kByte of free RAM)
        |
$0A00   |________Free RAM
$09FF   |
        |
$0980   |________UART Channel B TxD buffer
$097F   |
$0900   |________UART Channel B RxD buffer
$08FF   |
$0880   |________UART Channel A TxD buffer
$087F   |
$0800   |________UART Channel A RxD buffer        
$07FF   |
        |
$02E6   |________Free RAM / RESERVED?
$02E4   |________Character scan routine vector
$02E2   |________Character input routine vector
        |        (Reserved)
$02DE   |________Software BRK vector
$02DC   |________Software BRK vector checksum
$02DA   |________Software INT/IRQ vector
$02D8   |________Software INT/IRQ vector checksum
$02D6   |________Software NMI vector
$02D4   |________Software NMI vector checksum
$02D2   |________Software Reset vector checksum
$02D0   |________Software Reset vector
$02EF   |
$0266   |________Free RAM
$0265   |
        |
$0246   |________(Supermon 816 auxbuffer)
$0245   |
        |
$0200   |________Keyboard buffer
$01FF   |
        |
$0100   |________Hardware stack
$00FF   |
        |
$0000   |________Zero Page
