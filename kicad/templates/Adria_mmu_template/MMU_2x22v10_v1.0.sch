EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Device:R R1
U 1 1 5FDE684C
P 10250 1150
F 0 "R1" V 10043 1150 50  0000 C CNN
F 1 "330" V 10134 1150 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P7.62mm_Horizontal" V 10180 1150 50  0001 C CNN
F 3 "~" H 10250 1150 50  0001 C CNN
	1    10250 1150
	0    1    1    0   
$EndComp
$Comp
L Connector_Generic:Conn_02x25_Odd_Even J1
U 1 1 5FDE6E57
P 9800 3450
F 0 "J1" H 9850 4750 50  0000 C CNN
F 1 "SLOT" H 9850 2150 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x25_P2.54mm_Horizontal" H 9800 3450 50  0001 C CNN
F 3 "~" H 9800 3450 50  0001 C CNN
	1    9800 3450
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0103
U 1 1 5FDE8BBD
P 10350 4900
F 0 "#PWR0103" H 10350 4650 50  0001 C CNN
F 1 "GND" H 10355 4727 50  0000 C CNN
F 2 "" H 10350 4900 50  0001 C CNN
F 3 "" H 10350 4900 50  0001 C CNN
	1    10350 4900
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0104
U 1 1 5FDE8E28
P 10650 1400
F 0 "#PWR0104" H 10650 1150 50  0001 C CNN
F 1 "GND" H 10655 1227 50  0000 C CNN
F 2 "" H 10650 1400 50  0001 C CNN
F 3 "" H 10650 1400 50  0001 C CNN
	1    10650 1400
	1    0    0    -1  
$EndComp
$Comp
L power:PWR_FLAG #FLG0101
U 1 1 5FDE9181
P 10600 4900
F 0 "#FLG0101" H 10600 4975 50  0001 C CNN
F 1 "PWR_FLAG" V 10550 4800 50  0000 C CNN
F 2 "" H 10600 4900 50  0001 C CNN
F 3 "~" H 10600 4900 50  0001 C CNN
	1    10600 4900
	-1   0    0    1   
$EndComp
$Comp
L power:PWR_FLAG #FLG0102
U 1 1 5FDE9363
P 9100 2000
F 0 "#FLG0102" H 9100 2075 50  0001 C CNN
F 1 "PWR_FLAG" V 9150 1950 50  0000 C CNN
F 2 "" H 9100 2000 50  0001 C CNN
F 3 "~" H 9100 2000 50  0001 C CNN
	1    9100 2000
	-1   0    0    -1  
$EndComp
Wire Wire Line
	9350 2250 9600 2250
Wire Wire Line
	9100 2000 9100 2250
Wire Wire Line
	9100 2250 9350 2250
Connection ~ 9350 2250
Wire Wire Line
	10600 4900 10600 4650
Wire Wire Line
	10600 4650 10350 4650
Wire Wire Line
	10350 4900 10350 4650
Connection ~ 10350 4650
Wire Wire Line
	10350 4650 10100 4650
$Comp
L Device:LED D1
U 1 1 5FDE66E8
P 9750 1150
F 0 "D1" H 9750 950 50  0000 C CNN
F 1 "PWR" H 9750 1050 50  0000 C CNN
F 2 "LED_THT:LED_D5.0mm_Horizontal_O1.27mm_Z3.0mm" H 9750 1150 50  0001 C CNN
F 3 "~" H 9750 1150 50  0001 C CNN
	1    9750 1150
	-1   0    0    1   
$EndComp
Wire Wire Line
	9100 900  9100 1150
Wire Wire Line
	9900 1150 10100 1150
Wire Wire Line
	10400 1150 10650 1150
Wire Wire Line
	10650 1150 10650 1400
Wire Wire Line
	10400 5400 10150 5400
Wire Wire Line
	9650 5500 9400 5500
Wire Wire Line
	10400 5600 10150 5600
Wire Wire Line
	10400 5500 10150 5500
Wire Wire Line
	9350 4350 9600 4350
Wire Wire Line
	8900 2950 9600 2950
Wire Wire Line
	8900 2850 9600 2850
Wire Wire Line
	8900 2750 9600 2750
Wire Wire Line
	8900 2650 9600 2650
Wire Wire Line
	8900 2450 9600 2450
Wire Wire Line
	8900 2350 9600 2350
Wire Wire Line
	9350 4050 9600 4050
Wire Wire Line
	9350 3950 9600 3950
Wire Wire Line
	9350 3850 9600 3850
Wire Wire Line
	9350 3750 9600 3750
Wire Wire Line
	9350 3550 9600 3550
Wire Wire Line
	9350 4450 9600 4450
Wire Wire Line
	9350 4550 9600 4550
Wire Wire Line
	9350 4650 9600 4650
Text Label 9350 2350 0    50   ~ 0
D7
Text Label 9350 2450 0    50   ~ 0
D6
Text Label 9350 2550 0    50   ~ 0
D5
Text Label 9350 2650 0    50   ~ 0
D4
Text Label 9350 2750 0    50   ~ 0
D3
Text Label 9350 2850 0    50   ~ 0
D2
Text Label 9350 3050 0    50   ~ 0
D0
Text Label 9350 3550 0    50   ~ 0
sysClk
Text Label 9350 4450 0    50   ~ 0
~IRQ
Text Label 9350 4350 0    50   ~ 0
~NMI
Text Label 9350 4650 0    50   ~ 0
~RESET
Text Label 9350 4550 0    50   ~ 0
~ABORT
Text Label 9350 3850 0    50   ~ 0
R~W
Text Label 9350 4050 0    50   ~ 0
VDA
Text Label 9350 3950 0    50   ~ 0
VPA
Text Label 10400 5600 2    50   ~ 0
~IOSEL0
Text Label 10400 5500 2    50   ~ 0
~IOSEL1
Text Label 10400 5400 2    50   ~ 0
~IOSEL2
Text Label 10400 5300 2    50   ~ 0
~IOSEL3
Text Label 9400 5600 0    50   ~ 0
~RAMSEL
Text Label 9400 5500 0    50   ~ 0
~ROMSEL
Text Label 9350 2950 0    50   ~ 0
D1
Entry Wire Line
	8800 2350 8900 2450
Entry Wire Line
	8800 2450 8900 2550
Entry Wire Line
	8800 2550 8900 2650
Entry Wire Line
	8800 2650 8900 2750
Entry Wire Line
	8800 2850 8900 2950
Entry Wire Line
	8800 2950 8900 3050
Wire Wire Line
	8900 3050 9600 3050
Entry Wire Line
	8800 2250 8900 2350
Wire Wire Line
	8900 2550 9600 2550
Entry Wire Line
	8800 2750 8900 2850
Entry Wire Line
	10750 4550 10850 4650
Entry Wire Line
	10750 4450 10850 4550
Entry Wire Line
	10750 4350 10850 4450
Entry Wire Line
	10750 4250 10850 4350
Entry Wire Line
	10750 4150 10850 4250
Entry Wire Line
	10750 4050 10850 4150
Entry Wire Line
	10750 3950 10850 4050
Entry Wire Line
	10750 3850 10850 3950
Text Label 10850 5100 1    50   ~ 0
A[0..23]
Entry Wire Line
	10750 3750 10850 3850
Entry Wire Line
	10750 3650 10850 3750
Entry Wire Line
	10750 3550 10850 3650
Entry Wire Line
	10750 3450 10850 3550
Entry Wire Line
	10750 3350 10850 3450
Entry Wire Line
	10750 3250 10850 3350
Entry Wire Line
	10750 3150 10850 3250
Entry Wire Line
	10750 3050 10850 3150
Entry Wire Line
	10750 2950 10850 3050
Entry Wire Line
	10750 2850 10850 2950
Entry Wire Line
	10750 2750 10850 2850
Entry Wire Line
	10750 2650 10850 2750
Entry Wire Line
	10750 2550 10850 2650
Entry Wire Line
	10750 2450 10850 2550
Entry Wire Line
	10750 2350 10850 2450
Entry Wire Line
	10750 2250 10850 2350
$Comp
L power:+5V #PWR0101
U 1 1 6034BDE6
P 9100 900
F 0 "#PWR0101" H 9100 750 50  0001 C CNN
F 1 "+5V" H 9115 1073 50  0000 C CNN
F 2 "" H 9100 900 50  0001 C CNN
F 3 "" H 9100 900 50  0001 C CNN
	1    9100 900 
	1    0    0    -1  
$EndComp
Wire Wire Line
	9350 1150 9350 2250
Wire Wire Line
	9100 1150 9350 1150
Wire Wire Line
	9600 1150 9350 1150
Connection ~ 9350 1150
Text Label 9350 3650 0    50   ~ 0
~PHI2
Text Label 9350 3750 0    50   ~ 0
PHI2
Wire Wire Line
	9350 3650 9600 3650
Wire Wire Line
	10400 5300 10150 5300
Wire Wire Line
	10100 3950 10750 3950
Wire Wire Line
	10100 4350 10750 4350
Wire Wire Line
	10100 4450 10750 4450
Wire Wire Line
	10100 4550 10750 4550
Wire Wire Line
	10100 4050 10750 4050
Wire Wire Line
	10100 4150 10750 4150
Wire Wire Line
	10100 4250 10750 4250
Wire Wire Line
	10100 3850 10750 3850
Wire Wire Line
	10100 3750 10750 3750
Wire Wire Line
	10100 3450 10750 3450
Wire Wire Line
	10100 3550 10750 3550
Wire Wire Line
	10100 3650 10750 3650
Wire Wire Line
	10100 3150 10750 3150
Wire Wire Line
	10100 3250 10750 3250
Wire Wire Line
	10100 3350 10750 3350
Wire Wire Line
	10100 3050 10750 3050
Wire Wire Line
	10100 2250 10750 2250
Wire Wire Line
	10100 2350 10750 2350
Wire Wire Line
	10100 2450 10750 2450
Wire Wire Line
	10100 2550 10750 2550
Wire Wire Line
	10100 2650 10750 2650
Wire Wire Line
	10100 2750 10750 2750
Wire Wire Line
	10100 2850 10750 2850
Wire Wire Line
	10100 2950 10750 2950
Text Label 10350 2250 2    50   ~ 0
A23
Text Label 10350 2350 2    50   ~ 0
A22
Text Label 10350 2450 2    50   ~ 0
A21
Text Label 10350 2550 2    50   ~ 0
A20
Text Label 10350 2650 2    50   ~ 0
A19
Text Label 10350 2750 2    50   ~ 0
A18
Text Label 10350 2850 2    50   ~ 0
A17
Text Label 10350 2950 2    50   ~ 0
A16
Text Label 10350 3050 2    50   ~ 0
A15
Text Label 10350 3150 2    50   ~ 0
A14
Text Label 10350 3250 2    50   ~ 0
A13
Text Label 10350 3350 2    50   ~ 0
A12
Text Label 10350 3450 2    50   ~ 0
A11
Text Label 10350 3550 2    50   ~ 0
A10
Text Label 10350 3650 2    50   ~ 0
A9
Text Label 10350 3750 2    50   ~ 0
A8
Text Label 10350 3850 2    50   ~ 0
A7
Text Label 10350 3950 2    50   ~ 0
A6
Text Label 10350 4050 2    50   ~ 0
A5
Text Label 10350 4150 2    50   ~ 0
A4
Text Label 10350 4250 2    50   ~ 0
A3
Text Label 10350 4350 2    50   ~ 0
A2
Text Label 10350 4450 2    50   ~ 0
A1
Text Label 10350 4550 2    50   ~ 0
A0
$Comp
L Connector:TestPoint TP1
U 1 1 60186C0D
P 10650 950
F 0 "TP1" H 10708 1068 50  0000 L CNN
F 1 "GND" H 10708 977 50  0000 L CNN
F 2 "TestPoint:TestPoint_Loop_D3.50mm_Drill1.4mm_Beaded" H 10850 950 50  0001 C CNN
F 3 "~" H 10850 950 50  0001 C CNN
	1    10650 950 
	1    0    0    -1  
$EndComp
Wire Wire Line
	10650 950  10650 1150
Connection ~ 10650 1150
Text Label 8800 2150 1    50   ~ 0
D[0..7]
Wire Wire Line
	9650 5600 9400 5600
Wire Wire Line
	9400 5400 9650 5400
Wire Wire Line
	9400 5300 9650 5300
Text Label 9400 5400 0    50   ~ 0
~WE
Text Label 9400 5300 0    50   ~ 0
~OE
$Comp
L Connector_Generic:Conn_02x05_Odd_Even J2
U 1 1 5FE7ED22
P 9850 5500
F 0 "J2" H 9900 5817 50  0000 C CNN
F 1 "additional" H 9900 5200 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x05_P2.54mm_Horizontal" H 9850 5500 50  0001 C CNN
F 3 "~" H 9850 5500 50  0001 C CNN
	1    9850 5500
	1    0    0    -1  
$EndComp
Wire Wire Line
	9400 5700 9650 5700
Wire Wire Line
	10400 5700 10150 5700
Text Label 9400 5700 0    50   ~ 0
~DEVSEL
Text Label 10400 5700 2    50   ~ 0
IOEXTRA
NoConn ~ 9600 3150
NoConn ~ 9600 3250
NoConn ~ 9600 3350
NoConn ~ 9600 3450
NoConn ~ 9600 4150
NoConn ~ 9600 4250
Wire Bus Line
	8800 1600 8800 2950
Wire Bus Line
	10850 2350 10850 5250
$EndSCHEMATC
