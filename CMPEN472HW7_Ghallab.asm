**********************************************************************************************************************************************
*
* Title:          SCI Serial Port
*
* Objective:      CMPEN 472 Homework 7 - Elementary Calculator
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:           Oct. 18, 2023
*
* Programmer:     Aly Ghallab
*
* Company:        The Pennsylvania State University
*                 College of Engineering
*
* Program:        Simple SCI Serial Port I/O and Demonstration
*                 Elementary Calculator 
*                 
*                 
*
* Algorithm:      Simple Serial I/O use
*
* Register use:   A:                              Serial port data and Arithmetic Operations
*                 B:                              Indexing of Command Buffer to go through each Character and Arithmetic Operations
*                 D:                              Holds the baud rate of the SCI, Holds Values for Arithmetic Operations
*                 X,Y:                            Stores Arrays/Variables For Operations to be Performed
*                 Condition Code Register (CCR):  Used by subroutines, comparision, and branching instructions
*                                                 Set if Overflow Occured
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
* Note:           You can only type 9 characters (Up to 4 Characters for NUM1, Up to 4 Characters NUM2, 1 Operator)
*                 No Spaces are needed for the input.
*                 Before it defaults to invalid Input, Integers Must be positive and are limited to 4 digits each.
*                 You cannot Divide by 0, it will give an overflow error
*                 Output is also limited to 4 digits, otherwise it will give an overflow error
*                 
*                 
* Output:         
*                 Arithmetic Result of User Inputs
*
* Observation:   
*                 Division operation automatically applies a floor as Remainder is not considered
*                 Leading Zeros are taken care of in a subroutine below
*                 Max Value for any integer is 9999 in decimal 
*                 All Inputs are in decimal format, not hex or binary
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


MULT        EQU         $2A
PLUS        EQU         $2B
MINUS       EQU         $2D
DIV         EQU         $2F


MAX_VALUE   EQU         $270F        ; 9999 in Hex

**********************************************************************************************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address 
                                     ;   for Data for CMPEN 472 class

msg_prompt  DC.B         'ECALC',PROMPT,' ',$00

;**** Define a buffer to store the received command and its index: ****

C_BUF            DS.B        9           ; Reserve 13 bytes for the buffer
C_IDX            DS.B        1            ; Reserve 1 byte for the C_BUF index
arg_IDX          DS.B        1            ; Reserve 1 byte argument index

NUM1             DS.B        2           ; Input 1 (Converted to 2 byte Decimal Number)
NUM2             DS.B        2           ; Input 2 (Converted to 2 byte Decimal Number)
NUM3             DS.B        2           ; Input 2 (Converted to 2 byte Decimal Number)

OP               DS.B        1           ; Operator ASCII character


NUM1_ASCII       DS.B        4           ; ASCII characters of Input 1
NUM1_DIGITS      DS.B        4           ; Individual Digits of Input 1 (Decimal)
NUM1_Len         DC.B        0           ; Length of Input 1 (Characters)

NUM2_ASCII       DS.B        4           ; ASCII characters of Input 2
NUM2_DIGITS      DS.B        4           ; Individual Digits of Input 2 (Decimal)
NUM2_Len         DC.B        0           ; Length of Input 2 (Characters)

NUM3_ASCII       DS.B        4           ; Data in ASCII to be shown or written to target address   (Reserved 4 bytes of Storage)

TEMP             DC.B        0           ; Temp variable
                                                                          

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
            ;ldx   #msg1              ; print the first message, 'Hello'
            ;jsr   printmsg
            
            
main_Loop   
            CLRA                     ; Clear Accumulators and Variables
            CLRB           
            
            LDX   #NUM1_ASCII
            STD   2,X+
            STD   ,X
            
            LDX   #NUM2_ASCII
            STD   2,X+
            STD   ,X
            
            LDX   #NUM3_ASCII
            STD   2,X+
            STD   ,X
           
            STD   NUM1
            STD   NUM2
            STD   NUM3
            
            
            LDX   #C_BUF              ; Clear Command Buffer
            STD   2,X+
            STD   2,X+
            STD   2,X+
            STD   2,X+
            STAA  X   
            
            
            STAA  NUM1_Len            ; Clearing Counters
            STAA  NUM2_Len
            STAA  C_IDX
            
              
            ldaa  #CR                ; move the cursor to beginning of the line
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
            
            ;ldx   #msg2              ; print the second message
            ;jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            LDX   #msg_prompt        ; msg_prompt = '> ' to prompt user input
            JSR   printmsg
        
            
            LDX   #C_BUF             ; Load Base Adress of Command Buffer into X Register            



read_Command:
            JSR   getchar               ; Read a character
            CMPA  #$00                  ; If NULL Character go back to reading
            BEQ   read_Command  
            JSR   putchar               ; If not NULL, char is displayed on the terminal window - echo print
            

store_Char:
            LDAB  C_IDX                 ; Load the current index into B
            CMPB  #9                    ; Check if we've reached the end of the buffer
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
            JSR   parse_C_BUF           ; Parse Input Command for Address and Data (ASCII)
            
            LDX   #NUM1_ASCII           ; Take ASCII Values of NUM 1 and convert to 2 byte decimal value
            LDY   #NUM1_DIGITS
            LDAA  NUM1_Len
            JSR   NUM_TO_DEC
            STD   NUM1
            
            LDX   #NUM2_ASCII           ; Take ASCII Values of NUM 2 and convert to 2 byte decimal value
            LDY   #NUM2_DIGITS
            LDAA  NUM2_Len
            JSR   NUM_TO_DEC
            STD   NUM2
            
            LDAA  OP                    ; Check Operator ASCII value to determine arithmetic operation
            CMPA  #MULT
            BEQ   multiplication
            CMPA  #PLUS
            BEQ   addition
            CMPA  #MINUS
            BEQ   subtraction
            CMPA  #DIV
            BEQ   division
            BRA   invalid_input         ; Invalid Input if operator doesnt match 
            
addition
          LDD  NUM1               ; Load NUM1 into ACC D
          ADDD NUM2               ; Subtract NUM1 + NUM2 -> ACC D
          STD  NUM3               ; Store Result in NUM3
          LBVS  overflow_error     ; If Overflow bit Set 1 then Branch to overflow error
          CPD  #MAX_VALUE         ; If greater than max value 9999 then throw error
          BHI  overflow_error
          
          CPD  #0                 ; If Result is negative, throw an erro
          BLT  overflow_error
           
          JSR  NUM3_TO_ASCII      ; Convert NUM3 to ASCII characters to be displayed on terminal
          
          LBRA  result


subtraction
          LDD  NUM1               ; Load NUM1 into ACC D
          SUBD NUM2               ; Subtract NUM1 - NUM2 -> ACC D
          STD  NUM3               ; Store Result in NUM3
          BVS  overflow_error     ; If Overflow bit Set 1 then Branch to overflow error
          CPD  #MAX_VALUE         ; If greater than max value 9999 then throw error
          BHI  overflow_error
          
          CPD  #0                 ; If Result is negative, throw an erro
          BLT  overflow_error
           
          JSR  NUM3_TO_ASCII      ; Convert NUM3 to ASCII characters to be displayed on terminal
          BRA  result


multiplication
          LDD  NUM1               ; Load Numbers into Registers
          LDY  NUM2
          EMUL                    ; (D)x(Y)-> Y:D  (LSB 2 Bytes in D)
          CPY  #0                 ; If Y not equal zero, hence Result is more than 2 bytes therefore overflow
          BNE  overflow_error
          STD  NUM3
          
          CPD  #MAX_VALUE         ; If greater than max value 9999 then throw error
          BHI  overflow_error
          
          CPD  #0                 ; If Result is negative, throw an erro
          BLT  overflow_error  
          
          JSR  NUM3_TO_ASCII      ; Convert NUM3 to ASCII characters to be displayed on terminal
          BRA  result


division
          LDD  NUM1               ; Load Numbers into Registers
          LDX  NUM2
          CPX  #0
          BEQ  invalid_input      ; Wont Allow Division By 0
          
          IDIV                    ; (D)x(X)-> X  Remainder in D
          BVS  overflow_error     ; If Overflow bit Set 1 then Branch to overflow error
          STX  NUM3
          
          CPX  #MAX_VALUE         ; If greater than max value 9999 then throw error
          BHI  overflow_error
          
          CPX  #0                 ; If Result is negative, throw an erro
          BLT  overflow_error  
          
          JSR  NUM3_TO_ASCII      ; Convert NUM3 to ASCII characters to be displayed on terminal
          BRA  result


invalid_input
            LDAA  #CR                ; move the cursor to beginning of the line
            JSR   putchar            ;  Cariage Return/Enter key
            LDAA  #LF                ; move the cursor to next line, Line Feed
            JSR   putchar                       
            LDX   #input_error        ; print Invalid Data Input Error Message
            JSR   printmsg
            LBRA   main_Loop
            
overflow_error     
            LDAA  #CR                ; move the cursor to beginning of the line
            JSR   putchar            ;  Cariage Return/Enter key
            LDAA  #LF                ; move the cursor to next line, Line Feed
            JSR   putchar                       
            LDX   #of_error           ; print Overflow Error Message
            JSR   printmsg
            LBRA   main_Loop


result
            LDAA  #CR                ; move the cursor to beginning of the line
            JSR   putchar            ;  Cariage Return/Enter key
            LDAA  #LF                ; move the cursor to next line, Line Feed
            JSR   putchar                       
            
            LDY   #NUM1_ASCII        ; Print First Input
            JSR   Trim_Zeros
            JSR   printnum
            
            LDAA  OP                 ; Print Operator
            JSR   putchar
            
            LDY   #NUM2_ASCII         ; Print Second Input
            JSR   Trim_Zeros
            JSR   printnum
            
            LDAA   #'='
            JSR   putchar
            
            LDY   #NUM3_ASCII         ; Print Third Input
            JSR   Trim_Zeros
            JSR   printnum

            
            LBRA  main_Loop


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
               beq     printmsgdone   ;end of string yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx 
               pula
               rts
;***********end of printmsg********************

;***********printnum***************************
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
;     (String is terminated with NULL=$00 
;     or After 4 Characters, whichever first)
;**********************************************
printnum      psha                   ; Save registers
               pshx
               ldab    #4             ; Load the maximum number of characters to B

printnumloop   ldaa    1,X+           ; Pick up an ASCII character from string
                                       ;   pointed by X register
                                       ; then update the X register to point to
                                       ;   the next byte
               cmpa    #NULL
               beq     printnumdone   ; end of string yet?
               
               jsr     putchar        ; if not, print character and do next
               
               decb                   ; Decrement counter
               cmpb    #0
               beq     printnumdone   ; If counter is zero, we're done

               bra     printnumloop

printnumdone   pulx 
               pula
               rts
;***********end of printnum********************

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

; Subroutine that Parses Command Buffer for user inputted Address and Data Value

parse_C_BUF

            LDX   #C_BUF             ; Load Command Buffer to be Parsed
            LDY   #NUM1_ASCII        ; Load Starting Address of NUM1_ASCII Array           
            LDAB  #0                 ; Store Counter in B to keep track of Length of NUM1
input1      
            LDAA  1,X+               ; Load Byte Pointed to by X Register then Increment X Register
            
                  
            CMPA    #'0'             ; Check if Character is a digit
            BLT     not_digit
            CMPA    #'9'
            BGT     not_digit

            STAA    1,Y+             ; Store the digit in the NUM1_ASCII array
            INCB                     ; Increment Length of Number stored in ACC B
            
            CMPB    #4
            BGT     error_handling   ; If length of Number is greater than 4 then its an invalid Input
            
            BRA     input1           ; Go to Next Character in Buffer and repeat

not_digit
            ; Since Character is not a digit check if its an operator
            ;Check if it's an operator (let's assume +, -, *, / are the operators)
            
            CMPA    #'+'
            BEQ     is_operator
            CMPA    #'-'
            BEQ     is_operator
            CMPA    #'*'
            BEQ     is_operator
            CMPA    #'/'
            BEQ     is_operator

            ; If here, it's neither a digit nor an operator, hence an error
            BRA     error_handling

is_operator
            STAA    OP               ; If its an operator store it in OP variable
            
 
            LDY     #NUM2_ASCII        ; Since we have our operator, The rest of the digits in the buffer will be NUM2
            STAB    NUM1_Len         ; Hence We store the number in ACC B to NUM1_Len
            LDAB    #0               ; Reset Length to keep track of NUM2
            
input2      
            LDAA  1,X+               ; Iterate through Buffer
            
            CMPA    #CR              ; If Carriage Return then Parse is Complete 
            BEQ     parse_complete   
                  
            CMPA    #'0'             ; Check if ASCII values are digits
            BLT     error_handling   ; If not digits there are no expected any operators, hence invalid Input
            CMPA    #'9'
            BGT     error_handling
   
            STAA    1,Y+             ; Store the digit in the NUM1_ASCII array
            INCB                     ; Increment the index B
            
            CMPB    #4               ; If length of NUM2 is greater than 4, then invalid input
            BGT     error_handling
            
            
            BRA     input2           ; Go to next character in buffer and repeat

parse_complete
            STAB    NUM2_Len         ; When parse is complete, save the length of NUM2
            RTS                      ; Return from Subroutine
            
error_handling
            LBRA     invalid_input    
;----------------------------------------------------------------------------------- 

NUM_TO_DEC                      ;Converts Number in variable held by Register X
                                ;to Decimal Value using Length of Array stored in ACC A
                                ; Stores Individual Decimal Values in Register Y
            
            PSHA
            DECA                ; ACC A = Num_Length
            STAA    TEMP        ; Temporary Counter variable = NUM_Len -1 for exponent
            PULA
            
            PSHY                ; Save Starting Address of NUM_DIGITS

ASCII_Coversion                                     ;Takes NUM_ASCII and converts them to Numbers in NUM_DIGITS
            LDAB    1,X+                            ; Load ASCII value to ACC B
            JSR     ASCII_TO_NUM                    ; Convert ASCII value to Number
            STAB    1,Y+                            ; Store Number in NUM_DIGITS
            DECA                                    ; Decrement Length (Characters Left)
            CMPA    #0                              ; If no Characters Left Exit Loop
            BNE     ASCII_Coversion
                                                    ; CLR Accumulators
            CLRA
            CLRB
            PULY                                    ; Restore Starting Address of NUM_Digits
            TFR     Y,X                             ; Copy Address into X register for EMUL
            LDAB    1,X+                            ; Load First Number into ACC B
            PSHA                                    ; Save Accumulator A
            
            ; Check if TEMP = 0 before entering the exponent loop
            LDAA    TEMP                            ; Check if Temp = 0 , which means Length = 1 therefore
            BEQ     skip_exponent                   ; No Multiplication Required, if so skip exponent loop

exponent_loop
            PULA
            LDY    #10
            EMUL              
            ADDB   1,X+
            PSHA
            LDAA   TEMP
            DECA   
            STAA   TEMP
            CMPA   #0
            BNE    exponent_loop
            
skip_exponent
            PULA
            RTS




;----------------------------------------------------------------------------------- 
NUM3_TO_ASCII
            LDY     #NUM3_ASCII+3 ; Point X to the last position of the array
            LDAA    #4             ; Counter for 4 digits
            STAA    TEMP
            CLRA
CONVERT_LOOP
            LDD     NUM3           ; Load the decimal number into D
            LDX     #10
            IDIV                    ; Divide by 10, quotient in X, remainder in D
            STX     NUM3            ; Store the quotient 
            
            ;TFR     D,B            ; Transfer remainder to B
            ADDB    #$30           ; Convert to ASCII
            STAB    ,Y             ; Store the ASCII character
            DEY                    ; Move to the next position in the array
            
            
            LDAA    TEMP
            DECA                   ; Decrement the counter
            STAA    TEMP
                       
            CMPA    #0
            BNE     CONVERT_LOOP   ; Loop until all digits are processed
            
           
            RTS                    ; Return
;----------------------------------------------------------------------------------- 
; Subroutine for handling leading zeros during print

Trim_Zeros
            LDAB  #4                  ; Counter for max array size

Trim_Loop
            LDAA  ,Y                  ; Load ASCII character from memory at Y
            CMPA  #$30                ; Compare with ASCII '0'
            BEQ   IsZero              ; If it's zero, check the next
            CMPA  #0
            BEQ   end_array
            

            TFR   Y,X                 ; Transfer Y to X if it's not zero
            BRA   PrintResult         

IsZero
            DECB                      ; Decrement counter (B register)
            CMPB  #0
            BEQ   AllZeros            ; If counter reaches 0, all characters are zeros
            INY                       ; Move to the next character
            BRA   Trim_Loop            ; Loop back

AllZeros
                                       ; Go back to the last zero
            TFR Y,X
            RTS
PrintResult
            RTS

end_array
            DEY
            TFR Y,X
            RTS
;----------------------------------------------------------------------------------- 


ASCII_TO_NUM

            ; Convert ASCII char in ACC B to number
            ; Check if it is a valid hex character (0-9 only since we are dealing with integer inputs)
            
            
            SUBB    #$30        ; Subtract ASCII value of '0'
            CMPB    #$0
            LBLT     invalid_input   ; If ASCII value is less than 0, then its not valid.
            CMPB    #$09        ; Compare with 9
            BLE     RETURN    ; If less than or equal to 9, it's a number, and it's valid

IS_VALID
            ; The character is valid (0-9, A-F). If it's A-F, adjust the numerical value
            CMPB    #$09        ; Compare with 9
            BLE     RETURN      ; If less than or equal to 9, subroutine done

RETURN     
            RTS                 ; Return to caller

 
;-----------------------------------------------------------------------------------                                     
;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip

msg3           DC.B    'Enter your command below:', $00
msg4           DC.B    'Error: Invalid command', $00

equals         DC.B    ' = ', $00

input_error     DC.B    'Inputs must be integers (Greater than 0) up to 4 digits in length, Decimal Format',CR, LF,CR,LF 
                DC.B     'Division by 0 is not allowed',$00
                
of_error        DC.B    'Overflow occured',$00


commands    
            DC.B        'Enter Two Positive Integers Up to 4 Digits (Decimal)',CR, LF,CR,LF
            DC.B        'No Spaces when Entering Commands Format: <NUM1><OPERATOR><NUM2><ENTER_KEY>',CR, LF,CR,LF
            DC.B        'Allowed Operations are Multiplication (*), Division(/),Addition(+) and Subtraction(-)', CR, LF,CR,LF
            DC.B        'Division by 0 is not allowed and will throw an input error', CR, LF,CR,LF
            DC.B        'Inputs and Output can be a max of 4 digits otherwise overflow will occur, causing an error.', $00
;-----------------------------------------------------------------------------------    

            END         ; End of Program
                    