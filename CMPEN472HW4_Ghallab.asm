;**************************************************************************************************   
;*
;*  Title:        LED Light Brightness Control (Pulse Width Modulation)
;*
;*  Objective:    CMPEN 472 HW 4 
;*
;*  Programmer:   Aly Ghallab
;*  Company:      The Pennsylvania State University
;*                Department of Computer Science and Engineering
;* 
;*  Date:         September 25, 2023
;*  Algorithm:    Simple Parallel I/0 use and time delay-loop demo
;*
;*  Register Use: A, B : Accumulators Stored ON/OFF counters 
;*                       used for the Duty Cycle to control dimming
;*                X:     Used for 1ms delay Loop
;*
;*  Program:      LED 1 has duty cycle with a period of 200 ms and a frequency of 5Hz
;*                The goal of the program is to have LED 1 go from 0% Brightness to 
;*                100% Brightness in 0.1 Seconds (100 ms). Then go from 100% Brightness to 0%
;*                in 0.1 seconds (100 ms) in a loop indefinitely 
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
                                        
Counter_ON   DC.B       $0              ; Memory Location to Hold ON Count, initialized 0
Counter_OFF  DC.B       $64             ; Memory Location to Hold OFF Count intialized 100
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
             JSR        dimUP
             JSR        dimDOWN
             BRA        mainLoop
             

;**************************************************************************************************    
;* Subroutine Section: Addresses used [$3100 to $3FFF] in RAM
;*             
;**************************  LED1 Turn On/OFF Subroutines **************************

led1_ON
            LDAA        Counter_ON        ; Load Value from Counter_ON into Accumulator A
            BEQ         end_on            ; if Counter_ON = 0 Do nothing

on_Loop     BSET        PORTB,%00010000   ; Turn On LED 1
            JSR         delay1ms          ; 1ms Delay
            DECA                          ; Decrement Counter in Accumulator A
            BNE         on_Loop
end_on
            RTS                           ; Return from Subroutine
;-----------------------------------------------------------------------------------

led1_OFF

            LDAB        Counter_OFF       ; Load Value from Counter_OFF into Accumulator B
            BEQ         end_off           ; If Counter_OFF = 0, do nothing.
            
off_Loop    BCLR        PORTB,%00010000   ; Turn OFF LED 1
            JSR         delay1ms          ; 10ms Delay
            DECB                          ; Decrement Counter in Accumulator B
            BNE         off_Loop
end_off            
            RTS                           ; Return from Subroutine
            
;-----------------------------------------------------------------------------------

dimUP
            LDAA        #0                ; Counter_ON = 0
            STAA        Counter_ON        ; Starting at 0% Brightness
            LDAB        #100              ; Counter_OFF = 100
            STAB        Counter_OFF
dimUPLoop
            JSR         led1_ON           ;
            LDAA        Counter_ON
            INCA                          ; Increment Brightness
            STAA        Counter_ON        ; Increment Counter_ON
            
            JSR         led1_OFF          ; 
            LDAB        Counter_OFF
            DECB                          ;
            STAB        Counter_OFF       ; Decrement Counter_OFF
            
            CMPA        #100              ; Check if Brightness Reached 100%
            BNE         dimUPLoop         ; If Not 100% Repeat Loop
            
            RTS                           ; Return from Subroutine

;-----------------------------------------------------------------------------------

dimDOWN
            LDAA        #100              ; Counter_ON = 100
            STAA        Counter_ON        ; Starting at 100% Brightness
            LDAB        #0                ; Counter_OFF = 100
            STAB        Counter_OFF
dimDOWNLoop
            JSR         led1_ON           ;
            LDAA        Counter_ON
            DECA                          ; Decrement Brightness
            STAA        Counter_ON        ; Decrement Counter_ON
            
            JSR         led1_OFF          ; 
            LDAB        Counter_OFF
            INCB                          ;
            STAB        Counter_OFF       ; Increment Counter_OFF
            
            CMPB        #100              ; Check if Brightness Reached 0%
            BNE         dimDOWNLoop       ; If Not 0% Repeat Loop
            
            RTS                           ; Return from Subroutine

                            
                       
;-----------------------------------------------------------------------------------
              
;******************************** Delay Subroutines ********************************
 
delay1ms
             PSHX                         ; Save Current Value in Register X
             LDX        #5998             ; 5998 Loop Iterations are needed to cause a
                                          ; 1 ms Delay at 24MHz
                                        
d1msLoop     DEX                          ; Decrement Counter in Register in X
             BNE        d1msLoop          ; If A not equal to 0, continue Looping
             
             PULX                         ; Restore Value of A when Loop is Over 
             RTS                          ; Return from Subroutine         

;-----------------------------------------------------------------------------------
                                     
;*
;*  Add Additional Subroutines Here
;*
;**************************************************************************************************  
             end                        ; Last Line of A Program
