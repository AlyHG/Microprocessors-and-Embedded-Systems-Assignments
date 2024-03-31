 ;**************************************************************************************************   
;*
;*  Title:        LED Light Brightness Control (Pulse Width Modulation)
;*
;*  Objective:    CMPEN 472 HW 3 
;*
;*  Programmer:   Aly Ghallab
;*  Company:      The Pennsylvania State University
;*                Department of Computer Science and Engineering
;*
;*  Date:         September 13,2023
;* 
;*  Algorithm:    Simple Parallel I/0 use and time delay-loop demo
;*
;*  Register Use: A, B : Accumulators Stored ON/OFF counters 
;*                used for the Duty Cycle to control dimming
;*                
;*
;*  Program:      LED 1 has duty cycle with a period of 1 ms and a frequency of 1KHz
;*                Switch 1 is not Pressed (5% Dimming):  ON for 0.05 msec, OFF for 0.95 msec
;*                Switch 1 is Pressed (65% Dimming) :    ON for 0.65 msec, OFF for 0.35 msec
;*
;*  Note:         On CSM-12C128 board, switch 1 is at PORTB bit 0, and LED 4 is at bit 7
;*                This Program is Developed using CodeWarrior V5.2 Simulator Only
;*                One MUST set "Switch 1" at PORTB bit 0 as an Output (1), not an input (0)
;*                However when using the CSM-12C128 board, PORTB bit 0 must be set to an Input (0) 
;*
;*  Memory Use:   RAM locations from $3000 for data,
;*                RAM Locations from $3100 for program
;*
;*  Input:        Parameters hard-coded in the program - PORTB
;*                - Switch 1 at PORTB bit 0 ----> (This bit is set as an output in simulation Only)
;*                - Switch 2 at PORTB bit 1 ----> (Not Used)
;*                - Switch 3 at PORTB bit 2 ----> (Not Used)
;*                - Switch 4 at PORTB bit 3 ----> (Not Used)
;*
;*  Output:       LED 1 at PORTB bit 4
;*                LED 2 at PORTB bit 5
;*                LED 3 at PORTB bit 6
;*                LED 4 at PORTB bit 7
;*
;*  Observation:  This Program implements dimming via Pulse Width Modulation in which by pulsing
;*                the LEDs, a dimming affect is achieved controlling the average power delivered
;*                to a load in this case it was the LEDs. Hence One can Control the brightness of
;*                a PHYSICAL LED on a board by manipulating the ON/OFF time (duty cycle).    
;*                
;*                
;*
;**************************************************************************************************   
;* Parameter Declaration Section
;*
;* Export Symbols
       
             XDEF       pstart          ; export 'pstart' symbol
             ABSENTRY   pstart          ; for assembly entry point

;* Symbols and Macros
PORTA        EQU        $0000           ; i/o port A addresses
DDRA         EQU        $0002
PORTB        EQU        $0001           ; i/o port B addresses
DDRB         EQU        $0003        

;**************************************************************************************************    
;* Data Section: Addresses used [$3000 to $30FF] in RAM
;*
             ORG        $3000           ; Reserved RAM memory starting address 
                                        ; for Data for CMPEN 472 class
                                        
Counter_ON   DC.B       $0              ; Memory Location to Hold ON Count 
Counter_OFF  DC.B       $0              ; Memory Location to Hold OFF Count
                                        ; Both Counts Sum to 100 Giving Each LED A Period
                                        ; of 1ms, And a Frequency of 1 KHz
                                        
                                    
                                        ; Remaining Data Memory Space for Stack,
                                        ; Up to Program Memory Start

;**************************************************************************************************                                      
;* Program Section: Addresses used [$3100 to $3FFF] in RAM                                  


             ORG        $3100             ; Program start address, in RAM
pstart       LDS        #$3100            ; initialize the stack pointer


             LDAA       #%11110001        ; LED 1,2,3,4 at PORTS bit 4,5,6,7 FOR Simulation only
             STAA       DDRB              ; set PORTB bits 4.5.6.7 as output and bit 0 for switch 1

             LDAA       #%01000000        ; Turn On LED 3 Only
             STAA       PORTB           
             
mainLoop
             LDAA       PORTB             ; Check bit 0 of PORTB, (Switch 1)
             ANDA       #%00000001        ; if 0, Switch 1 is not Pressed (sw1_OFF)
             BNE        sw1_ON            ; if 1, Switch 1 is Pressed (sw1_ON)
             
sw1_OFF                                   ; 5 % Brightness

             LDAA       #$5                
             STAA       Counter_ON        ; Load ON Counter with 5
             
             LDAB       #95               ; Load OFF Counter with 95
             STAB       Counter_OFF
             JSR        led1_ON           ; Go through LED ON Cycles
             JSR        led1_OFF          ; Go Through LED OFF Cycles
             BRA        mainLoop          ; Check Switch, Loop indefinitely
             
sw1_ON                                    ; 65% Brightness
             LDAA       #65               
             
             STAA       Counter_ON        ; Load ON Counter with 65
             
             LDAB       #35               ; Load OFF Counter with 35
             STAB       Counter_OFF
             JSR        led1_ON           ; Go through LED ON Cycles
             JSR        led1_OFF          ; Go through LED OFF Cycles
             BRA        mainLoop          ; Check Switch, Loop indefinitely

;**************************************************************************************************    
;* Subroutine Section: Addresses used [$3100 to $3FFF] in RAM
;*             
;**************************  LED1 Turn On/OFF Subroutines **************************

led1_ON
            LDAA        Counter_ON        ; Load Value from Counter_ON into Accumulator A

on_Loop     BSET        PORTB,%00010000   ; Turn On LED 1
            JSR         delay10us         ; 10us Delay
            DECA                          ; Decrement Counter in Accumulator A
            BNE         on_Loop
            RTS                           ; Return from Subroutine

led1_OFF
            LDAB        Counter_OFF       ; Load Value from Counter_OFF into Accumulator B

off_Loop    BCLR        PORTB,%00010000   ; Turn OFF LED 1
            JSR         delay10us         ; 10us Delay
            DECB                          ; Decrement Counter in Accumulator B
            BNE         off_Loop
            RTS                           ; Return from Subroutine
                       
;-----------------------------------------------------------------------------------
              
;******************************** Delay Subroutines ********************************

; Since the objective is to have the LED pulse at 1000Hz, Let 100 represent the full
; Duty Cycle, hence 100* 10us = 1ms Period or 1000Hz Frequency
;
 
delay10us
             PSHA                         ; Save Current Value in Accumulator A
             LDAA       #58               ; 58 Loop Iterations are needed to cause a
                                          ; 10us Delay at 24MHz
                                        
d10usLoop    SUBA       #$01              ; Decrement Counter in Accumulator A
             BNE        d10usLoop         ; If A not equal to 0, continue Looping
             
             PULA                         ; Restore Value of A when Loop is Over 
             RTS                          ; Return from Subroutine         

;-----------------------------------------------------------------------------------
                                     
;*
;*  Add Additional Subroutines Here
;*
;**************************************************************************************************  
             end                        ; Last Line of A Program
