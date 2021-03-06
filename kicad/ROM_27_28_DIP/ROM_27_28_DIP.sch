EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "Adria 32 kByte (E)EPROM board"
Date "2021-01-14"
Rev "1.1"
Comp "Xander Maas"
Comment1 "Changed footprint of U1 to longpads"
Comment2 "Corrected reference of U2 to U1"
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Device:R R1
U 1 1 5FDE684C
P 10150 1900
F 0 "R1" V 9943 1900 50  0000 C CNN
F 1 "330" V 10034 1900 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P7.62mm_Horizontal" V 10080 1900 50  0001 C CNN
F 3 "~" H 10150 1900 50  0001 C CNN
F 4 "CCF07330RJKE36" V 10150 1900 50  0001 C CNN "manf#"
	1    10150 1900
	0    1    1    0   
$EndComp
$Comp
L Connector_Generic:Conn_02x25_Odd_Even J1
U 1 1 5FDE6E57
P 9700 4200
F 0 "J1" H 9750 5500 50  0000 C CNN
F 1 "SLOT" H 9750 2900 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x25_P2.54mm_Horizontal" H 9700 4200 50  0001 C CNN
F 3 "~" H 9700 4200 50  0001 C CNN
F 4 "2213R-50G" H 9700 4200 50  0001 C CNN "manf#"
	1    9700 4200
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0103
U 1 1 5FDE8BBD
P 10250 5650
F 0 "#PWR0103" H 10250 5400 50  0001 C CNN
F 1 "GND" H 10255 5477 50  0000 C CNN
F 2 "" H 10250 5650 50  0001 C CNN
F 3 "" H 10250 5650 50  0001 C CNN
	1    10250 5650
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0104
U 1 1 5FDE8E28
P 10550 2150
F 0 "#PWR0104" H 10550 1900 50  0001 C CNN
F 1 "GND" H 10555 1977 50  0000 C CNN
F 2 "" H 10550 2150 50  0001 C CNN
F 3 "" H 10550 2150 50  0001 C CNN
	1    10550 2150
	1    0    0    -1  
$EndComp
$Comp
L power:PWR_FLAG #FLG0101
U 1 1 5FDE9181
P 10500 5650
F 0 "#FLG0101" H 10500 5725 50  0001 C CNN
F 1 "PWR_FLAG" V 10450 5550 50  0000 C CNN
F 2 "" H 10500 5650 50  0001 C CNN
F 3 "~" H 10500 5650 50  0001 C CNN
	1    10500 5650
	-1   0    0    1   
$EndComp
$Comp
L power:PWR_FLAG #FLG0102
U 1 1 5FDE9363
P 9000 2750
F 0 "#FLG0102" H 9000 2825 50  0001 C CNN
F 1 "PWR_FLAG" V 9050 2700 50  0000 C CNN
F 2 "" H 9000 2750 50  0001 C CNN
F 3 "~" H 9000 2750 50  0001 C CNN
	1    9000 2750
	-1   0    0    -1  
$EndComp
Wire Wire Line
	9250 3000 9500 3000
Wire Wire Line
	9000 2750 9000 3000
Wire Wire Line
	9000 3000 9250 3000
Connection ~ 9250 3000
Wire Wire Line
	10500 5650 10500 5400
Wire Wire Line
	10500 5400 10250 5400
Wire Wire Line
	10250 5650 10250 5400
Connection ~ 10250 5400
Wire Wire Line
	10250 5400 10000 5400
$Comp
L Device:LED D1
U 1 1 5FDE66E8
P 9650 1900
F 0 "D1" H 9650 1700 50  0000 C CNN
F 1 "PWR" H 9650 1800 50  0000 C CNN
F 2 "LED_THT:LED_D5.0mm_Horizontal_O1.27mm_Z3.0mm" H 9650 1900 50  0001 C CNN
F 3 "~" H 9650 1900 50  0001 C CNN
F 4 "LTL2R3KRD-EM" H 9650 1900 50  0001 C CNN "manf#"
	1    9650 1900
	-1   0    0    1   
$EndComp
Wire Wire Line
	9000 1650 9000 1900
Wire Wire Line
	9800 1900 10000 1900
Wire Wire Line
	10300 1900 10550 1900
Wire Wire Line
	10550 1900 10550 2150
Wire Wire Line
	9250 4300 9500 4300
Wire Wire Line
	8800 3700 9500 3700
Wire Wire Line
	8800 3600 9500 3600
Wire Wire Line
	8800 3500 9500 3500
Wire Wire Line
	8800 3400 9500 3400
Wire Wire Line
	8800 3200 9500 3200
Wire Wire Line
	8800 3100 9500 3100
Wire Wire Line
	9250 4500 9500 4500
Wire Wire Line
	9250 5400 9500 5400
Text Label 9250 3100 0    50   ~ 0
D7
Text Label 9250 3200 0    50   ~ 0
D6
Text Label 9250 3300 0    50   ~ 0
D5
Text Label 9250 3400 0    50   ~ 0
D4
Text Label 9250 3500 0    50   ~ 0
D3
Text Label 9250 3600 0    50   ~ 0
D2
Text Label 9250 3800 0    50   ~ 0
D0
Text Label 9250 4300 0    50   ~ 0
sysClk
Text Label 9250 5400 0    50   ~ 0
~RESET
Text Label 9250 4600 0    50   ~ 0
R~W
Text Label 12050 5950 2    50   ~ 0
~IOSEL0
Text Label 12050 6050 2    50   ~ 0
~IOSEL1
Text Label 12050 6150 2    50   ~ 0
~IOSEL2
Text Label 12050 6250 2    50   ~ 0
~IOSEL3
Text Label 11900 5400 0    50   ~ 0
~RAMSEL
Text Label 9250 3900 0    50   ~ 0
~ROMSEL
Text Label 9250 3700 0    50   ~ 0
D1
Entry Wire Line
	8700 3100 8800 3200
Entry Wire Line
	8700 3200 8800 3300
Entry Wire Line
	8700 3300 8800 3400
Entry Wire Line
	8700 3400 8800 3500
Entry Wire Line
	8700 3600 8800 3700
Entry Wire Line
	8700 3700 8800 3800
Wire Wire Line
	8800 3800 9500 3800
Entry Wire Line
	8700 3000 8800 3100
Wire Wire Line
	8800 3300 9500 3300
Entry Wire Line
	8700 3500 8800 3600
Entry Wire Line
	10650 5300 10750 5400
Entry Wire Line
	10650 5200 10750 5300
Entry Wire Line
	10650 5100 10750 5200
Entry Wire Line
	10650 5000 10750 5100
Entry Wire Line
	10650 4900 10750 5000
Entry Wire Line
	10650 4800 10750 4900
Entry Wire Line
	10650 4700 10750 4800
Entry Wire Line
	10650 4600 10750 4700
Text Label 10750 5750 1    50   ~ 0
A[0..23]
Entry Wire Line
	10650 4500 10750 4600
Entry Wire Line
	10650 4400 10750 4500
Entry Wire Line
	10650 4300 10750 4400
Entry Wire Line
	10650 4200 10750 4300
Entry Wire Line
	10650 4100 10750 4200
Entry Wire Line
	10650 4000 10750 4100
Entry Wire Line
	10650 3900 10750 4000
Entry Wire Line
	10650 3800 10750 3900
Entry Wire Line
	10650 3700 10750 3800
Entry Wire Line
	10650 3600 10750 3700
Entry Wire Line
	10650 3500 10750 3600
Entry Wire Line
	10650 3400 10750 3500
Entry Wire Line
	10650 3300 10750 3400
Entry Wire Line
	10650 3200 10750 3300
Entry Wire Line
	10650 3100 10750 3200
Entry Wire Line
	10650 3000 10750 3100
Wire Wire Line
	9250 1900 9250 3000
Wire Wire Line
	9000 1900 9250 1900
Wire Wire Line
	9500 1900 9250 1900
Connection ~ 9250 1900
Text Label 9250 4400 0    50   ~ 0
~PHI2
Text Label 9250 4500 0    50   ~ 0
PHI2
Wire Wire Line
	9250 4600 9500 4600
Wire Wire Line
	9250 4400 9500 4400
Wire Wire Line
	10000 4700 10650 4700
Wire Wire Line
	10000 5100 10650 5100
Wire Wire Line
	10000 5200 10650 5200
Wire Wire Line
	10000 5300 10650 5300
Wire Wire Line
	10000 4800 10650 4800
Wire Wire Line
	10000 4900 10650 4900
Wire Wire Line
	10000 5000 10650 5000
Wire Wire Line
	10000 4600 10650 4600
Wire Wire Line
	10000 4500 10650 4500
Wire Wire Line
	10000 4200 10650 4200
Wire Wire Line
	10000 4300 10650 4300
Wire Wire Line
	10000 4400 10650 4400
Wire Wire Line
	10000 3900 10650 3900
Wire Wire Line
	10000 4000 10650 4000
Wire Wire Line
	10000 4100 10650 4100
Wire Wire Line
	10000 3800 10650 3800
Wire Wire Line
	10000 3000 10650 3000
Wire Wire Line
	10000 3100 10650 3100
Wire Wire Line
	10000 3200 10650 3200
Wire Wire Line
	10000 3300 10650 3300
Wire Wire Line
	10000 3400 10650 3400
Wire Wire Line
	10000 3500 10650 3500
Wire Wire Line
	10000 3600 10650 3600
Wire Wire Line
	10000 3700 10650 3700
Text Label 10250 3000 2    50   ~ 0
A23
Text Label 10250 3100 2    50   ~ 0
A22
Text Label 10250 3200 2    50   ~ 0
A21
Text Label 10250 3300 2    50   ~ 0
A20
Text Label 10250 3400 2    50   ~ 0
A19
Text Label 10250 3500 2    50   ~ 0
A18
Text Label 10250 3600 2    50   ~ 0
A17
Text Label 10250 3700 2    50   ~ 0
A16
Text Label 10250 3800 2    50   ~ 0
A15
Text Label 10250 3900 2    50   ~ 0
A14
Text Label 10250 4000 2    50   ~ 0
A13
Text Label 10250 4100 2    50   ~ 0
A12
Text Label 10250 4200 2    50   ~ 0
A11
Text Label 10250 4300 2    50   ~ 0
A10
Text Label 10250 4400 2    50   ~ 0
A9
Text Label 10250 4500 2    50   ~ 0
A8
Text Label 10250 4600 2    50   ~ 0
A7
Text Label 10250 4700 2    50   ~ 0
A6
Text Label 10250 4800 2    50   ~ 0
A5
Text Label 10250 4900 2    50   ~ 0
A4
Text Label 10250 5000 2    50   ~ 0
A3
Text Label 10250 5100 2    50   ~ 0
A2
Text Label 10250 5200 2    50   ~ 0
A1
Text Label 10250 5300 2    50   ~ 0
A0
$Comp
L Connector:TestPoint TP1
U 1 1 60186C0D
P 10550 1700
F 0 "TP1" H 10608 1818 50  0000 L CNN
F 1 "GND" H 10608 1727 50  0000 L CNN
F 2 "TestPoint:TestPoint_Loop_D3.50mm_Drill1.4mm_Beaded" H 10750 1700 50  0001 C CNN
F 3 "~" H 10750 1700 50  0001 C CNN
	1    10550 1700
	1    0    0    -1  
$EndComp
Wire Wire Line
	10550 1700 10550 1900
Connection ~ 10550 1900
Text Label 8700 3000 1    50   ~ 0
D[0..7]
NoConn ~ 9500 5300
NoConn ~ 9500 5200
NoConn ~ 9500 5100
NoConn ~ 9500 5000
NoConn ~ 9500 4900
Wire Wire Line
	9250 3900 9500 3900
NoConn ~ 9500 4000
NoConn ~ 9500 4100
NoConn ~ 9500 4200
Wire Wire Line
	9250 4700 9500 4700
Wire Wire Line
	9250 4800 9500 4800
Text Label 9250 4700 0    50   ~ 0
~OE
Text Label 9250 4800 0    50   ~ 0
~WE
Wire Wire Line
	4500 1650 3800 1650
Wire Wire Line
	4500 1750 3800 1750
Wire Wire Line
	4500 1850 3800 1850
Wire Wire Line
	4500 1950 3800 1950
Wire Wire Line
	4500 2150 3800 2150
Wire Wire Line
	4500 2250 3800 2250
Text Label 4050 2250 2    50   ~ 0
D7
Text Label 4050 2150 2    50   ~ 0
D6
Text Label 4050 2050 2    50   ~ 0
D5
Text Label 4050 1950 2    50   ~ 0
D4
Text Label 4050 1850 2    50   ~ 0
D3
Text Label 4050 1750 2    50   ~ 0
D2
Text Label 4050 1550 2    50   ~ 0
D0
Text Label 4050 1650 2    50   ~ 0
D1
Entry Wire Line
	4600 1550 4500 1650
Entry Wire Line
	4600 1650 4500 1750
Entry Wire Line
	4600 1750 4500 1850
Entry Wire Line
	4600 1850 4500 1950
Entry Wire Line
	4600 2050 4500 2150
Entry Wire Line
	4600 2150 4500 2250
Wire Wire Line
	4500 1550 3800 1550
Entry Wire Line
	4600 1450 4500 1550
Wire Wire Line
	4500 2050 3800 2050
Entry Wire Line
	4600 1950 4500 2050
Entry Wire Line
	2350 2950 2250 3050
Entry Wire Line
	2350 2850 2250 2950
Entry Wire Line
	2350 2750 2250 2850
Entry Wire Line
	2350 2650 2250 2750
Entry Wire Line
	2350 2550 2250 2650
Entry Wire Line
	2350 2450 2250 2550
Entry Wire Line
	2350 2350 2250 2450
Entry Wire Line
	2350 2250 2250 2350
Entry Wire Line
	2350 2150 2250 2250
Entry Wire Line
	2350 2050 2250 2150
Entry Wire Line
	2350 1950 2250 2050
Entry Wire Line
	2350 1850 2250 1950
Entry Wire Line
	2350 1750 2250 1850
Entry Wire Line
	2350 1650 2250 1750
Entry Wire Line
	2350 1550 2250 1650
Wire Wire Line
	3000 2150 2350 2150
Wire Wire Line
	3000 1750 2350 1750
Wire Wire Line
	3000 1650 2350 1650
Wire Wire Line
	3000 1550 2350 1550
Wire Wire Line
	3000 2050 2350 2050
Wire Wire Line
	3000 1950 2350 1950
Wire Wire Line
	3000 1850 2350 1850
Wire Wire Line
	3000 2250 2350 2250
Wire Wire Line
	3000 2350 2350 2350
Wire Wire Line
	3000 2650 2350 2650
Wire Wire Line
	3000 2550 2350 2550
Wire Wire Line
	3000 2450 2350 2450
Wire Wire Line
	3000 2950 2350 2950
Wire Wire Line
	3000 2850 2350 2850
Wire Wire Line
	3000 2750 2350 2750
Text Label 2750 2950 0    50   ~ 0
A14
Text Label 2750 2850 0    50   ~ 0
A13
Text Label 2750 2750 0    50   ~ 0
A12
Text Label 2750 2650 0    50   ~ 0
A11
Text Label 2750 2550 0    50   ~ 0
A10
Text Label 2750 2450 0    50   ~ 0
A9
Text Label 2750 2350 0    50   ~ 0
A8
Text Label 2750 2250 0    50   ~ 0
A7
Text Label 2750 2150 0    50   ~ 0
A6
Text Label 2750 2050 0    50   ~ 0
A5
Text Label 2750 1950 0    50   ~ 0
A4
Text Label 2750 1850 0    50   ~ 0
A3
Text Label 2750 1750 0    50   ~ 0
A2
Text Label 2750 1650 0    50   ~ 0
A1
Text Label 2750 1550 0    50   ~ 0
A0
Wire Wire Line
	2600 3250 3000 3250
Text Label 2600 3250 0    50   ~ 0
~ROMSEL
Text Label 2600 3350 0    50   ~ 0
~OE
Wire Wire Line
	2600 3350 3000 3350
$Comp
L power:GND #PWR05
U 1 1 5FE60397
P 3800 5200
F 0 "#PWR05" H 3800 4950 50  0001 C CNN
F 1 "GND" H 3805 5027 50  0000 C CNN
F 2 "" H 3800 5200 50  0001 C CNN
F 3 "" H 3800 5200 50  0001 C CNN
	1    3800 5200
	1    0    0    -1  
$EndComp
$Comp
L Device:C C1
U 1 1 5FE606C8
P 3500 5000
F 0 "C1" V 3248 5000 50  0000 C CNN
F 1 "100n" V 3339 5000 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D5.0mm_W2.5mm_P5.00mm" H 3538 4850 50  0001 C CNN
F 3 "~" H 3500 5000 50  0001 C CNN
F 4 "C322C104K5R5TA7301" V 3500 5000 50  0001 C CNN "manf#"
	1    3500 5000
	0    1    1    0   
$EndComp
Text Label 4600 1450 1    50   ~ 0
D[0..7]
Text Label 2250 4050 3    50   ~ 0
A[0..23]
Wire Wire Line
	3650 5000 3800 5000
Wire Wire Line
	3800 5000 3800 5200
Wire Wire Line
	3200 4850 3200 5000
Wire Wire Line
	3200 5000 3350 5000
$Comp
L power:VCC #PWR0102
U 1 1 5FEEC1C3
P 3200 4850
F 0 "#PWR0102" H 3200 4700 50  0001 C CNN
F 1 "VCC" H 3215 5023 50  0000 C CNN
F 2 "" H 3200 4850 50  0001 C CNN
F 3 "" H 3200 4850 50  0001 C CNN
	1    3200 4850
	1    0    0    -1  
$EndComp
$Comp
L power:VCC #PWR0105
U 1 1 5FEEC66F
P 9000 1650
F 0 "#PWR0105" H 9000 1500 50  0001 C CNN
F 1 "VCC" H 9015 1823 50  0000 C CNN
F 2 "" H 9000 1650 50  0001 C CNN
F 3 "" H 9000 1650 50  0001 C CNN
	1    9000 1650
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0101
U 1 1 5FEEE448
P 3400 3750
F 0 "#PWR0101" H 3400 3500 50  0001 C CNN
F 1 "GND" H 3405 3577 50  0000 C CNN
F 2 "" H 3400 3750 50  0001 C CNN
F 3 "" H 3400 3750 50  0001 C CNN
	1    3400 3750
	1    0    0    -1  
$EndComp
Wire Wire Line
	3400 3750 3400 3550
$Comp
L power:VCC #PWR0106
U 1 1 5FEA7713
P 3400 1150
F 0 "#PWR0106" H 3400 1000 50  0001 C CNN
F 1 "VCC" H 3415 1323 50  0000 C CNN
F 2 "" H 3400 1150 50  0001 C CNN
F 3 "" H 3400 1150 50  0001 C CNN
	1    3400 1150
	1    0    0    -1  
$EndComp
$Comp
L Memory_EPROM:27C256 U1
U 1 1 5FE8B73B
P 3400 2450
F 0 "U1" H 3150 3500 50  0000 C CNN
F 1 "27C256" H 3600 3500 50  0000 C CNN
F 2 "Package_DIP:DIP-28_W15.24mm_LongPads" H 3400 2450 50  0001 C CNN
F 3 "http://ww1.microchip.com/downloads/en/DeviceDoc/doc0014.pdf" H 3400 2450 50  0001 C CNN
F 4 "AT28C256-15PU;110-87-628-41-001101" H 3400 2450 50  0001 C CNN "manf#"
	1    3400 2450
	1    0    0    -1  
$EndComp
Wire Wire Line
	3400 1150 3400 1350
NoConn ~ 3000 3150
Wire Bus Line
	8700 2250 8700 3700
Wire Bus Line
	4600 800  4600 2150
Wire Bus Line
	2250 1650 2250 4550
Wire Bus Line
	10750 3100 10750 6000
$EndSCHEMATC
