***********************************************************************
*
* Title:          SCI Serial Port and 7-segment Display at PORTB
*
* Objective:      CMPEN 472 Homework 5
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Oct. 02, 2023
*
* Programmer:     Aly Ghallab
*
* Company:        The Pennsylvania State University
*                 College of Engineering
*
* Program:        Simple SCI Serial Port I/O and Demonstration
*                 Typewriter program 4 LEDS, 
*                 and 7-Segment display, at PORTB
*                 
*
* Algorithm:      Simple Serial I/O use, typewriter
*
* Register use:	  A:                              Serial port data
*                 B:                              Indexing of Command Buffer to go through each Character
*                 D:                              Holds the baud rate of the SCI
*                 X:                              Stores Command Buffer where entire User Command is stored
*                 Condition Code Register (CCR):  Used by subroutines, comparision, and branching instructions
*                 
*                 SCIRS1 (SCI Status Register 1): Provides status flags related to serial Communication
*                 
*                 - TDRE (Transmit Data Register Empty) Bit: Indicates if transmit data register is empty 
*                                                            if set (1) means the transmit buffer is empty 
*                                                            and ready for the next byte/ASCII Character
*
*                 - RDRF (Recieve Data Register Full) Bit:   Indicates if the recieve data register is full 
*                                                            if set to 1, it means a new byte has been recieved
*                                                            and is ready to be read.
*
*                 SCIDRL (SCI Data Register Low):            Data Register for the SCI, When Transmitting you write the
*                                                            byte of data to this register. When recieving, this register
*                                                            Gets read.            
*
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
* Note:           If not in TypeWriter Mode you can only type 5 characters
*                 Before it defaults to invalid Command. Commands are Case
*                 Sensitive
*                 
*                 Here are the Commands:
*                 L1: LED 1 goes from 0% light level to 100% light level in 0.1 seconds
*                     (Dimming Effect Seen on Board only due to Pulse Width Modulation, On Simulator it
*                      is percieved as Blinking of LEDs)
*                 F1: LED 1 goes from 100% light level to 0% light level in 0.1 seconds
*                     (Dimming Effect Seen on Board only due to Pulse Width Modulation, On Simulator it
*                      is percieved as Blinking of LEDs)
*                 
*                 L2: Turn on LED2
*                 F2: Turn off LED2
*                 L3: Turn on LED3
*                 F3: Turn off LED3
*                 L4: Turn on LED4
*                 F4: Turn off LED4
*                 QUIT: Quit menu program, run 'Type writer' program.
*                 
* Output:         
*                 PORTB bit 7 to bit 4, 7-segment MSB   (7 segment display only active during TypeWriter Mode)
*                 PORTB bit 3 to bit 0, 7-segment LSB   (7 segment display only active during TypeWriter Mode)
*
*                 LED 1 at PORTB bit 4
*                 LED 2 at PORTB bit 5
*                 LED 3 at PORTB bit 6
*                 LED 4 at PORTB bit 7
*
* Observation:    In typewriter mode this program displays ASCII
*                 data on PORTB - 7-segment displays inputted by the user in terminal
*                 
*                 Otherwise it waits for user to enter valid command (shown above) to perform
*                 Subroutine, or procedure.
*
*                 User input is read through the Serial Communication Interface                 
*
***********************************************************************
* Parameter Declearation Section
*
* Export Symbols
            XDEF        pstart       ; export 'pstart' symbol
            ABSENTRY    pstart       ; for assembly entry point
  
* Symbols and Macros
PORTB       EQU         $0001        ; i/o port B addresses
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character

***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address 
                                     ;   for Data for CMPEN 472 class

Counter_ON   DC.B       $0              ; Memory Location to Hold ON Count, initialized 0
Counter_OFF  DC.B       $64             ; Memory Location to Hold OFF Count intialized 100
                                        ; Both Counts Sum to 100 Giving Each LED A Period
                                        ; of 1ms, And a Frequency of 1 KHz

msg1        DC.B        'Hello', $00
msg2        DC.B        'You may type below', $00

;**** Define a buffer to store the received command and its index: ****

CommandBuffer    DS.B        5          ; Reserve 5 bytes for the buffer
CommandIndex     DS.B        1           ; Reserve 1 byte for the index



; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program - before the last "END" line.
                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start

*
***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*
            ORG        $3100          ; Program start address, in RAM
pstart      LDS        #$3100         ; initialize the stack pointer

            LDAA       #%11111111     ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA       DDRB           ; as output

            LDAA       #%00000000
            STAA       PORTB          ; clear all bits of PORTB

            ldaa       #$0C           ; Enable SCI port Tx and Rx units
            staa       SCICR2         ; disable SCI interrupts

            ldd        #$0001        ; Set SCI Baud Register = $0001 =>   1.5M baud at 24MHz (for simulation)
;            ldd        #$000D       ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;            ldd        #$009C       ; Set SCI Baud Register = $009C =>   9600 baud at 24MHz
            std        SCIBDH        ; SCI port baud rate change

            ldx   #msg1              ; print the first message, 'Hello'
            jsr   printmsg
            
main_Loop   ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;  Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            ldx   #commands          ; Prints the menu message containing all
            jsr   printmsg           ; All possible Commands
            ldaa  #CR                ; Goes to Next Line then skips a line
            jsr   putchar            ;  For the sake of Readibility
            ldaa  #LF
            jsr   putchar
            ldaa  #CR
            jsr   putchar
            ldaa  #LF
            jsr   putchar
            
            ldx   #msg2              ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            LDAA  #0                 ; Clear the command index
            STAA  CommandIndex
            
            LDX   #CommandBuffer      ; Load Base Adress of Command Buffer into X Register
            

read_Command:
            JSR   getchar               ; Read a character
            CMPA  #$00                  ; If NULL Character go back to reading
            BEQ   read_Command
            JSR   putchar               ; If not NULL, char is displayed on the terminal window - echo print
            

store_Char:
            LDAB  CommandIndex          ; Load the current index into B
            CMPB  #5                    ; Check if we've reached the end of the buffer
            BGE   buffer_Full           ; If we're at or past the end, handle accordingly

            STAA  0,X                   ; Store the character in Accumulator A in the Command buffer in X Register
            INX                         ; Increment the address pointed by the X register to add bytes in next Buffer Location
            INCB                        ; Increment B (CommandIndex)
            STAB  CommandIndex          ; Update CommandIndex

            CMPA  #CR                   ; Check if it's a Carriage Return
            BEQ   execute_Command       ; If yes, proceed with command execution
            BRA   read_Command          ; If no, read next character
            
            

buffer_Full:
            ; Handle buffer full situation by stopping reading
            BRA   execute_Command       ; Return from subroutine
            
execute_Command:
            ; Here, compare the CommandBuffer with known commands:
            LDX   #CommandBuffer
            LDAA  , X                   ; Load the first character
            CMPA  #'L'                  ; Check if command begins with "L" 
            BNE   F_Commands            ; Else Check if command begins with "F"
            LDAA  1, X                  ; Load the second character
check_L1             
            CMPA  #'1'                  ; Check if Second Character = 1;
            BNE   check_L2
            
            LDAA  2, X
            CMPA  #CR                   ; Compare with Carriage Return
            BNE   Check_L1_Jump
            
            JSR   L1                    ; Dim Up Subroutine
            JMP   command_Executed

Check_L1_Jump
            JMP   invalid_Command       ; Branch was too far for relative addressing
                 
check_L2
            CMPA  #'2'                  ; Check if Second Character = 2;
            BNE   check_L3
            
            LDAA  2, X
            CMPA  #CR                   ; Compare with Carriage Return
            BNE   Check_L2_Jump
            
            BSET  PORTB,%00100000       ; Turn On LED 2
            JMP   command_Executed

Check_L2_Jump
            JMP   invalid_Command       ; Branch was too far for relative addressing
                 
check_L3
            CMPA  #'3'                  ; Check if Second Character = 3;
            BNE   check_L4
            
            LDAA  2, X
            CMPA  #CR                   ; Compare with Carriage Return
            BNE   Check_L3_Jump
            
            BSET  PORTB,%01000000       ; Turn On LED 3
            JMP   command_Executed
            
Check_L3_Jump
            JMP   invalid_Command       ; Branch was too far for relative addressing
            
check_L4
            CMPA  #'4'                  ; Check if Second Character = 4;
            BNE   invalid_Command       ; If not then it is an invalid Command
            
            LDAA  2, X
            CMPA  #CR                   ; Compare with Carriage Return
            BNE   invalid_Command
            
            BSET  PORTB,%10000000       ; Turn On LED 4
            BRA   command_Executed


F_Commands                              ; Check if First Character is F Otherwise Check For Quit
            CMPA  #'F'
            BNE   quit_Command
            LDAA  1, X                  ; Load the second character
check_F1             
            CMPA  #'1'                  ; Check if Second Character = 1;
            BNE   check_F2
            
            LDAA  2, X
            CMPA  #CR
            BNE   invalid_Command
            
            JSR   F1                    ; Dim Down Subroutine
            BRA   command_Executed
check_F2
            CMPA  #'2'                  ; Check if Second Character = 2;
            BNE   check_F3
            
            LDAA  2, X
            CMPA  #CR                   ; Compare with Carriage Return
            BNE   invalid_Command
            
            BCLR  PORTB,%00100000       ; Turn OFF LED 2
            BRA   command_Executed

check_F3
            CMPA  #'3'                  ; Check if Second Character = 3;
            BNE   check_F4
            
            LDAA  2, X
            CMPA  #CR                   ; Compare with Carriage Return
            BNE   invalid_Command
            
            BCLR  PORTB,%01000000       ; Turn OFF LED 3
            BRA   command_Executed
            
check_F4
            CMPA  #'4'                  ; Check if Second Character = 4;
            BNE   invalid_Command       ; If not then it is an invalid Command
            
            LDAA  2, X
            CMPA  #CR                   ; Compare with Carriage Return
            BNE   invalid_Command
            
            BCLR  PORTB,%10000000       ; Turn OFF LED 4
            BRA   command_Executed
            
            
            
quit_Command
            CMPA  #'Q'
            BNE   invalid_Command
            
            LDAA  1, X                  ; Load the second character
            CMPA  #'U'
            BNE   invalid_Command

            LDAA  2, X                  ; Load the third character
            CMPA  #'I'
            BNE   invalid_Command

            LDAA  3, X                  ; Load the fourth character
            CMPA  #'T'
            BNE   invalid_Command

            LDAA  4, X                  ; Load the fifth character (should be NULL)
            CMPA  #CR                   ; Compare with Carriage Return
            BNE   invalid_Command
            BRA   TypeWriter
            


invalid_Command
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;  Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar                       
            ldx   #msg4              ; print "Invalid Command"
            jsr   printmsg
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            BRA   command_Executed

command_Executed:
            
            LDAA  #0                 ; Clear the command buffer and index for the next command
            STAA  CommandIndex
            STAA  CommandBuffer
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;  Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar                       
            ldx   #msg3              ; print "Enter Your Command Below"
            jsr   printmsg
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            JMP   main_Loop          ; Return to the menu           
            
            

TypeWriter
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;  Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            ldx   #msg5              ; Prints message informing user that they're in
            jsr   printmsg           ;  Type Writer Mode
            ldaa  #CR                ; Goes to Next Line then skips a line
            jsr   putchar            ;  For the sake of Readibility
            ldaa  #LF
            jsr   putchar
            ldaa  #CR
            jsr   putchar
            ldaa  #LF
            jsr   putchar
            
type_Writer jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   type_Writer
                                     ;  otherwise - what is typed on key board
            jsr   putchar            ; is displayed on the terminal window - echo print

            staa  PORTB              ; show the character on PORTB

            cmpa  #CR
            bne   type_Writer        ; if Enter/Return key is pressed, move the
            ldaa  #LF                ; cursor to next line
            jsr   putchar
            bra   type_Writer


;subroutine section below

;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL           equ     $00
printmsg       psha                   ;Save registers
               pshx
printmsgloop   ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
               cmpa    #NULL
               beq     printmsgdone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx 
               pula
               rts
;***********end of printmsg********************


;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar        brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
               staa  SCIDRL                      ; send a character
               rts
;***************end of putchar*****************


;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************
getchar        brclr SCISR1,#%00100000,getchar7
               ldaa  SCIDRL
               rts
getchar7       clra
               rts
;****************end of getchar**************** 

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

L1
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

F1
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
                                     
;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip

msg3           DC.B    'Enter your command below:', $00
msg4           DC.B    'Error: Invalid command', $00
msg5           DC.B    'You are now in TypeWriter Mode', $00

commands    
            DC.B        'Here are all the possible Commands (NOTE: Commands are CASE SENSITIVE)',CR, LF,CR,LF
            DC.B        'L1: LED 1 goes from 0% light level to 100% light level in 0.1 seconds', CR, LF
            DC.B        'F1: LED 1 goes from 100% light level to 0% light level in 0.1 seconds', CR, LF
            DC.B        'L2: Turn on LED2', CR, LF
            DC.B        'F2: Turn off LED2', CR, LF
            DC.B        'L3: Turn on LED3', CR, LF
            DC.B        'F3: Turn off LED3', CR, LF
            DC.B        'L4: Turn on LED4', CR, LF
            DC.B        'F4: Turn off LED4', CR, LF
            DC.B        'QUIT: Quit menu program, run "Type writer" program.', $00
;-----------------------------------------------------------------------------------

            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled
