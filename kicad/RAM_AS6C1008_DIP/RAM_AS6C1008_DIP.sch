EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "Adria RAM board"
Date "2021-01-04"
Rev "1.1"
Comp "Xander Maas"
Comment1 "Added pull-up R for CS2 (pin 30)"
Comment2 "Changed footprint of resistors for easier assembly"
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Device:R R1
U 1 1 5FDE684C
P 10150 1900
F 0 "R1" V 9943 1900 50  0000 C CNN
F 1 "330" V 10034 1900 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" V 10080 1900 50  0001 C CNN
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
Text Label 9250 3900 0    50   ~ 0
~RAMSEL
Text Label 11850 5750 0    50   ~ 0
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
	4600 1650 3900 1650
Wire Wire Line
	4600 1750 3900 1750
Wire Wire Line
	4600 1850 3900 1850
Wire Wire Line
	4600 1950 3900 1950
Wire Wire Line
	4600 2150 3900 2150
Wire Wire Line
	4600 2250 3900 2250
Text Label 4150 2250 2    50   ~ 0
D7
Text Label 4150 2150 2    50   ~ 0
D6
Text Label 4150 2050 2    50   ~ 0
D5
Text Label 4150 1950 2    50   ~ 0
D4
Text Label 4150 1850 2    50   ~ 0
D3
Text Label 4150 1750 2    50   ~ 0
D2
Text Label 4150 1550 2    50   ~ 0
D0
Text Label 4150 1650 2    50   ~ 0
D1
Entry Wire Line
	4700 1550 4600 1650
Entry Wire Line
	4700 1650 4600 1750
Entry Wire Line
	4700 1750 4600 1850
Entry Wire Line
	4700 1850 4600 1950
Entry Wire Line
	4700 2050 4600 2150
Entry Wire Line
	4700 2150 4600 2250
Wire Wire Line
	4600 1550 3900 1550
Entry Wire Line
	4700 1450 4600 1550
Wire Wire Line
	4600 2050 3900 2050
Entry Wire Line
	4700 1950 4600 2050
Entry Wire Line
	2250 3150 2150 3250
Entry Wire Line
	2250 3050 2150 3150
Entry Wire Line
	2250 2950 2150 3050
Entry Wire Line
	2250 2850 2150 2950
Entry Wire Line
	2250 2750 2150 2850
Entry Wire Line
	2250 2650 2150 2750
Entry Wire Line
	2250 2550 2150 2650
Entry Wire Line
	2250 2450 2150 2550
Entry Wire Line
	2250 2350 2150 2450
Entry Wire Line
	2250 2250 2150 2350
Entry Wire Line
	2250 2150 2150 2250
Entry Wire Line
	2250 2050 2150 2150
Entry Wire Line
	2250 1950 2150 2050
Entry Wire Line
	2250 1850 2150 1950
Entry Wire Line
	2250 1750 2150 1850
Entry Wire Line
	2250 1650 2150 1750
Entry Wire Line
	2250 1550 2150 1650
Wire Wire Line
	2900 2150 2250 2150
Wire Wire Line
	2900 1750 2250 1750
Wire Wire Line
	2900 1650 2250 1650
Wire Wire Line
	2900 1550 2250 1550
Wire Wire Line
	2900 2050 2250 2050
Wire Wire Line
	2900 1950 2250 1950
Wire Wire Line
	2900 1850 2250 1850
Wire Wire Line
	2900 2250 2250 2250
Wire Wire Line
	2900 2350 2250 2350
Wire Wire Line
	2900 2650 2250 2650
Wire Wire Line
	2900 2550 2250 2550
Wire Wire Line
	2900 2450 2250 2450
Wire Wire Line
	2900 2950 2250 2950
Wire Wire Line
	2900 2850 2250 2850
Wire Wire Line
	2900 2750 2250 2750
Wire Wire Line
	2900 3050 2250 3050
Wire Wire Line
	2900 3150 2250 3150
Text Label 2650 3150 0    50   ~ 0
A16
Text Label 2650 3050 0    50   ~ 0
A15
Text Label 2650 2950 0    50   ~ 0
A14
Text Label 2650 2850 0    50   ~ 0
A13
Text Label 2650 2750 0    50   ~ 0
A12
Text Label 2650 2650 0    50   ~ 0
A11
Text Label 2650 2550 0    50   ~ 0
A10
Text Label 2650 2450 0    50   ~ 0
A9
Text Label 2650 2350 0    50   ~ 0
A8
Text Label 2650 2250 0    50   ~ 0
A7
Text Label 2650 2150 0    50   ~ 0
A6
Text Label 2650 2050 0    50   ~ 0
A5
Text Label 2650 1950 0    50   ~ 0
A4
Text Label 2650 1850 0    50   ~ 0
A3
Text Label 2650 1750 0    50   ~ 0
A2
Text Label 2650 1650 0    50   ~ 0
A1
Text Label 2650 1550 0    50   ~ 0
A0
Wire Wire Line
	4300 2450 3900 2450
Wire Wire Line
	4300 2750 3900 2750
Text Label 4300 2450 2    50   ~ 0
~RAMSEL
Text Label 4300 2650 2    50   ~ 0
~OE
Text Label 4300 2750 2    50   ~ 0
~WE
Wire Wire Line
	4300 2650 3900 2650
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
Text Label 4700 1450 1    50   ~ 0
D[0..7]
Text Label 2150 4050 3    50   ~ 0
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
P 3400 3550
F 0 "#PWR0101" H 3400 3300 50  0001 C CNN
F 1 "GND" H 3405 3377 50  0000 C CNN
F 2 "" H 3400 3550 50  0001 C CNN
F 3 "" H 3400 3550 50  0001 C CNN
	1    3400 3550
	1    0    0    -1  
$EndComp
Wire Wire Line
	3400 3550 3400 3350
$Comp
L Memory_RAM:628128_DIP32_SSOP32 U1
U 1 1 5FE9C3A5
P 3400 2350
F 0 "U1" H 3050 3300 50  0000 C CNN
F 1 "AS6C1008" H 3650 3300 50  0000 C CNN
F 2 "Package_DIP:DIP-32_W15.24mm_Socket_LongPads" H 3400 2350 50  0001 C CNN
F 3 "http://www.futurlec.com/Datasheet/Memory/628128.pdf" H 3400 2350 50  0001 C CNN
F 4 "AS6C1008-55PCN;110-87-632-41-001101" H 3400 2350 50  0001 C CNN "manf#"
	1    3400 2350
	1    0    0    -1  
$EndComp
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
Wire Wire Line
	3900 2550 4500 2550
Wire Wire Line
	4500 2550 4500 1200
Wire Wire Line
	3400 1150 3400 1200
$Comp
L Device:R R2
U 1 1 5FF432D8
P 4000 1200
F 0 "R2" V 3950 1400 50  0000 C CNN
F 1 "1k" V 4000 1200 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal" V 3930 1200 50  0001 C CNN
F 3 "~" H 4000 1200 50  0001 C CNN
F 4 "CCF071K00GKE36" V 4000 1200 50  0001 C CNN "manf#"
	1    4000 1200
	0    1    1    0   
$EndComp
Wire Wire Line
	3850 1200 3400 1200
Connection ~ 3400 1200
Wire Wire Line
	3400 1200 3400 1350
Wire Wire Line
	4150 1200 4500 1200
Wire Bus Line
	8700 2250 8700 3700
Wire Bus Line
	4700 800  4700 2150
Wire Bus Line
	10750 3100 10750 6000
Wire Bus Line
	2150 1650 2150 4550
$EndSCHEMATC
