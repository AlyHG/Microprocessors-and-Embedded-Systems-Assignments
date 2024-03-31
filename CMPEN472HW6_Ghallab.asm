**********************************************************************************************************************************************
*
* Title:          SCI Serial Port and 7-segment Display at PORTB
*
* Objective:      CMPEN 472 Homework 6
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Oct. 09, 2023
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
**********************************************************************************************************************************************
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

CR          EQU         $0d          ; carriage return, ASCII 'Return' key
LF          EQU         $0a          ; line feed, ASCII 'next line' character

HEX         EQU         $24          ; ASCII for '$' to identify if an input
                                     ; is a hex value
                                     
PROMPT      EQU         $3E          ; ASCII Value for '>' to Prompt User input
SPACE       EQU         $20          ; ASCII Value for whitespace: ' ' 

**********************************************************************************************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address 
                                     ;   for Data for CMPEN 472 class

msg1        DC.B        'Hello', $00
msg2        DC.B        'You may type below', $00
msg_prompt  DC.B         PROMPT,' ',$00

;**** Define a buffer to store the received command and its index: ****

C_BUF            DS.B        13           ; Reserve 13 bytes for the buffer
C_IDX            DS.B        1            ; Reserve 1 byte for the C_BUF index
arg_IDX          DS.B        1            ; Reserve 1 byte argument index

ADDR             DS.W        2           ; Target address parsed from Command Buffer       (Reserved 1 Word (2 bytes) of Storage)
DATA             DS.W        2           ; Data to be shown or written to target address   (Reserved 1 Word (2 bytes) of Storage)

ADDR4            DS.B        5           ; Target address parsed from Command Buffer       (Reserved 5 bytes of Storage)
ADDR4_Len        DC.B        0
DATA4            DS.W        4           ; Data to be shown or written to target address   (Reserved 4 bytes of Storage)
DATA4_Len        DC.B        0
TEMP             DS.B        1           ; Temporary storage for calculations



;**** Flags ****

;cmd_flag         DC.B        0           ; Flag to indicate command type
                                           ; 0 = Undeclared
                                           ; 1 = Show
                                           ; 2 = Write

addr_hex         DC.B        0           ; Flag to indicate address value
                                           ; 0 = Address is not passed as hex value
                                           ; 1 = Address is passed as hex value

data_hex         DC.B        0           ; Flag to indicate data value
                                           ; 0 = Data is not passed as hex value
                                           ; 1 = Data is passed as hex value
                                           
addr_valid       DC.B        0           ; Flag to indicate address validity
                                           ; 0 = Address passed is Valid
                                           ; 1 = Address passed is not Valid

data_valid       DC.B        0           ; Flag to indicate data validity
                                           ; 0 = Data passed is Valid
                                           ; 1 = Data passed is not Valid
                                           
                                           
                                           
                                           
                                           
                                           

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

            LDX   #msg_prompt        ; msg_prompt = '> ' to prompt user input
            JSR   printmsg
            
    
            LDAA  #0                 ; Clear the command index
            STAA  C_IDX
            
            LDX   #C_BUF             ; Load Base Adress of Command Buffer into X Register
            

read_Command:
            JSR   getchar               ; Read a character
            CMPA  #$00                  ; If NULL Character go back to reading
            BEQ   read_Command  
            JSR   putchar               ; If not NULL, char is displayed on the terminal window - echo print
            

store_Char:
            LDAB  C_IDX                 ; Load the current index into B
            CMPB  #13                   ; Check if we've reached the end of the buffer
            BGE   buffer_Full           ; If we're at or past the end, handle accordingly

            STAA  0,X                   ; Store the character in Accumulator A in the Command buffer in X Register
            INX                         ; Increment the address pointed by the X register to add bytes in next Buffer Location
            INCB                        ; Increment B (CommandIndex)
            STAB  C_IDX                 ; Update CommandIndex

            CMPA  #CR                   ; Check if it's a Carriage Return
            BEQ   execute_Command       ; If yes, proceed with command execution
            BRA   read_Command          ; If no, read next character
            
            

buffer_Full:
            ; Handle buffer full situation by stopping reading
            BRA   execute_Command       ; Return from subroutine
            
execute_Command:
            JSR   parse_C_BUF
            LDX   #C_BUF
            
            
            ; Here, compare the CommandBuffer with known commands:
show        
            LDAA  , X                   ; Load the first character
            CMPA  #'S'                  ; Check if command begins with "S" 
            BNE   write                 ; Else Check if command begins with "W"
            
            
        
            
            





write                                   ; Check if First Character is 'W' Otherwise Check For Quit
            CMPA  #'W'
            BNE   quit_Command
            
            
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


invalid_address

invalid_data
            

command_Executed:
            
            LDAA  #0                 ; Clear the command buffer and index for the next command
            STAA  C_IDX
            STAA  C_BUF
            STAA  arg_IDX
            
            
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



;**************************  Command Buffer Parsing Subroutines **************************

; * Parses Command Buffer, inserting ASCII values into ADDR4 and DATA4
parse_C_BUF
              
            PSHY                     ; Save Registers
            PSHA
            PSHB
            
            LDX   #C_BUF             ;
            LDY   #ADDR4
            LDAB  #0                 ; Used to mark Length of input Address and Input Data
            
            LDAA  1,X
            INX
            
            CMPA  #'$'
            BNE   not_hex_addr

is_hex_addr
            LDAA  #1
            STAA  addr_hex
            BRA   addr4_parse

not_hex_addr
            STAA  ,Y
            INY
            INCB
            
            LDAA  #0
            STAA  addr_hex
 
            
addr4_parse 
            LDAA  1,X
            INX
            
            STAB  ADDR4_Len
            
            CMPA  #CR
            BEQ   end_parse
            CMPA  #SPACE
            BEQ   data4_parse
            
            STAA  ,Y
            INY
            INCB
            BRA   addr4_parse

            
            
data4_parse 
            LDY   #DATA4
            LDAB  #0
            
            LDAA  1,X
            INX
            
      
            CMPA  #'$'
            BNE   not_hex_val
            
is_hex_val
            LDAA  #1
            STAA  data_hex
            BRA   data4_Loop
 

not_hex_val
            
            STAA  ,Y
            INY
            INCB
            
            
            LDAA  #0
            STAA  data_hex

data4_Loop          
            LDAA  1,X
            INX
         
             
            CMPA  #CR
            BEQ   store_data
            
            STAA  ,Y
            INY
            INCB
            BRA   data4_Loop
            
store_data
            STAB  DATA4_Len
end_parse
            PULY
            PULA
            PULB
            RTS
            
           
;-----------------------------------------------------------------------------------

ADDR4_TO_HEX
    
    PSHY
    PSHX
    
    
    LDAB    ADDR4_Len        ; Load the length of the number into ACCB
    LDX     #ADDR4           ; Load the address of the ASCII values
    LDY     #ADDR            ; Load the address to store the final hex value
    STY     TEMP             ; Store Y to TEMP, Y will be used in loop
    CLRA                      ; Clear ACCA to be used for storing intermediate values
    STAA    addr_valid       ; Assuming valid input, will be set to 1 if invalid
    
ADDR4_HEX_LOOP
    DECB                      ; Decrement the counter B (length)
    BMI     CONVERSION_DONE   ; If all digits are processed, finish conversion
    
    LDAA    ,X                ; Load the ASCII character
    
    ; Check for invalid character
    CMPA    #'0'
    BLO     INVALID_CHAR
    CMPA    #'9'
    BLE     VALID_CHAR
    CMPA    #'A'
    BLO     INVALID_CHAR
    CMPA    #'F'
    BHI     INVALID_CHAR
    
VALID_CHAR
    JSR     CONVERT_DIGIT     ; Convert ASCII to hex
    
    LSLA                      ; Shift left to make space for next digit
    LSLA
    LSLA
    LSLA
    
    INX                       ; Move to the next ASCII character
    
    BRA     ADDR4_HEX_LOOP    ; Repeat the loop
    
INVALID_CHAR
    LDAA    #1                ; Indicate invalid input
    STAA    addr_valid
    BRA     CONVERSION_DONE   ; Jump to end of routine, bypassing the loop
    
    
 
 CONVERSION_DONE
    PULX
    PULY
    RTS


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
