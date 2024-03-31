;*******************************************************
;* CMPEN 472, 2022 Spring 
;* Homework 10: Timer Interrupt Sample Program, 
;* MC9S12C128 Program (set to MC9S12C32 for Simulation/Debug)
;* CodeWarrior Simulator/Debug edition, not for CSM-12C128 board
;*  November 10, Aly Ghallab
;* 
;* This program is a 2048 data transfer program running on the 
;*   CodeWarrior Debugger/Simulator as follows: 
;*   1. Program starts with print messages on the simulator Terminal,
;*      an intro message at 1.5M baud (this program will not work 
;*      on the CSM-12C128 board - 1.5M baud too fast).
;*   2. Then user may hit any key, it's a typewriter program at 1.5M baud.
;*      But hitting the Enter key will terminate the typewriter mode with 
;*      the instruction message print.
;*   3. Two messages are (1) start terminal data capture into a file and
;*      (2) hit Enter key for the 2048 data transfer to begin. 
;*   4. At this time, user setup the Terminal Output file, data capture to a file.
;*   5. User hits an Enter key to send 2048 data, to the Terminal and
;*      the data saved in to a file named RxData3.txt  which may be looked at 
;*      or plotted using Excel sheet.
;*   6. User may repeat the step 3 above as many times as he/she like.
;*      User plots or prints the data to verify the correct data transmit.
;* 
;* We assumed 24MHz bus clock and 4MHz external resonator clock frequency.  
;*******************************************************
; export symbols
              XDEF        Entry       ; export 'Entry' symbol
              ABSENTRY    Entry       ; for assembly entry point

; include derivative specific macros
PORTA         EQU         $0000
PORTB         EQU         $0001
DDRA          EQU         $0002
DDRB          EQU         $0003
  
; symbols/addresses


SCIBDH        EQU         $00c8        ; Serial port (SCI) Baud Rate Register H
SCIBDL        EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2        EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1        EQU         $00cc        ; Serial port (SCI) Status Register 1
SCIDRL        EQU         $00cf        ; Serial port (SCI) Data Register
;*   CodeWarrior project MUST specify MC9S12C32 chip for the terminal simulation to work.

CRGFLG        EQU         $0037        ; Clock and Reset Generator Flags
CRGINT        EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL        EQU         $003B        ; Real Time Interrupt Control

TIOS          EQU         $0040        ; Timer Input Capture (IC) or Output Compare (OC) select
TIE           EQU         $004C        ; Timer interrupt enable register
TCNTH         EQU         $0044        ; Timer free runing main counter
TSCR1         EQU         $0046         ; Timer system control 1
TSCR2         EQU         $004D        ; Timer system control 2
TFLG1         EQU         $004E        ; Timer interrupt flag 1
TC6H          EQU         $005C        ; Timer channel 2 register

DATAmax       EQU         2048         ; Data count maximum, 2048 constant

CR            EQU         $0d          ; carriage return, ASCII 'Return' key
LF            EQU         $0a          ; line feed, ASCII 'next line' character
SPACE         EQU         $20          ; ASCII Value for whitespace: ' ' 
**************************************************************************************
*           Data Section: address used [ $3000 to $30FF ] RAM memory                 *
**************************************************************************************
              ORG    $3000             ; RAMStart defined as $3000 in MC9S12C128 chip                                                                           
;-----------------     Numerical Clock Values (System)    -----------------

timeh           DS.B   1                 ; Hour
timem           DS.B   1                 ; Minute
times           DS.B   1                 ; Second

DIGIT_timeh     DS.B   2                 ; Hour
DIGIT_timem     DS.B   2                 ; Minute
DIGIT_times     DS.B   2                 ; Second

;-------------------    ASCII Clock Values (System)    -------------------

ASCII_timeh     DS.B   2                 ; Hour
ASCII_timem     DS.B   2                 ; Minute
ASCII_times     DS.B   2                 ; Second

;-----------------     Numerical Clock Values (From Set Command)   -----------------

t_timeh         DS.B   1               ; Hour
t_timem         DS.B   1               ; Minute
t_times         DS.B   1               ; Second

t_DIGIT_timeh   DS.B   2               ; Hour
t_DIGIT_timem   DS.B   2               ; Minute
t_DIGIT_times   DS.B   2               ; Second

;-------------------    ASCII Clock Values (From Set Command)     -------------------

t_ASCII_timeh   DS.B   2               ; Hour
t_ASCII_timem   DS.B   2               ; Minute
t_ASCII_times   DS.B   2               ; Second

;--------------------       Counters      -----------------------

ctr2p5m         DS.W   1                   ; interrupt counter for 2.5 mSec. of time
t_ctr           DC.B   0                   ; Temp Counter used for iteration

;-------------------     Command Buffer  ----------------------

C_BUF           DS.B        11        ; Reserve 11 bytes for the buffer
C_IDX           DS.B        1         ; Reserve 1 byte for the C_BUF index

Op              DS.B        1         ; Reserve 1 byte for the Operation ASCII                                       ; character
display         DC.B        's'         ; 7-Segment Display Mode

                                          
;-------------------     Timer Interrupt Variables    ----------------------

ctr125u     DS.W   1            ; 16bit interrupt counter for 125 uSec. of time
BUF         DS.B   6            ; character buffer for a 16bit number in decimal ASCII
CTR         DS.B   1            ; character buffer fill count

offset      DC.B   0
ctr_3       DC.B   0
ctr_4       DC.B   0

flag_100    DC.B   0            ; 0 = Regular Frequency Wave, 1 = 100Hz Wave
logic_flag  DC.B   0            ; 0 = up  , 1 = down
;-----------------     Menus & Prompts    --------------------

msg1            DC.B   'Hello', $00
msg2            DC.B   'You may type below', $00

clock_header    DC.B    'Clock> ',$00
cmd_header      DC.B    '     CMD> ',$00



;*************************************************************************************
;*                           Interrupt Vector Section                                *
;*************************************************************************************
            ORG    $FFF0             ; RTI interrupt vector setup for the simulator
;            ORG    $3FF0             ; RTI interrupt vector setup for the CSM-12C128 board
            DC.W   rtiisr


            ORG     $FFE2       ; Timer channel 6 interrupt vector setup, on simulator
            DC.W    oc6isr
**************************************************************************************
*        Program Section: addresses used [ $3100 to $3FFF ] RAM memory               *
**************************************************************************************
            ORG  $3100
Entry
            LDS   #Entry           ; initialize the stack pointer

            ldd   #$0001           ; For SIMULATION, Set SCI Baud Register = $0001 => 1.5M baud at 24MHz
            std   SCIBDH           ; SCI port baud rate change
            ldaa  #$0C             ; Enable SCI port Tx and Rx units
            staa  SCICR2           ; disable SCI interrupts

            LDAA   #%11111111   ; Set PORTA and PORTB bit 0,1,2,3,4,5,6,7
            STAA   DDRA         ; all bits of PORTA as output
            STAA   PORTA        ; set all bits of PORTA, initialize
            STAA   DDRB         ; all bits of PORTB as output
            STAA   PORTB        ; set all bits of PORTB, initialize
            
            bset   RTICTL,%00011001 ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                    ;      4MHz quartz oscillator clock
                                    
            bset   CRGINT,%10000000 ; enable RTI interrupt
            bset   CRGFLG,%10000000 ; clear RTI IF (Interrupt Flag)

            ldx    #0
            stx    ctr2p5m          ; initialize interrupt counter with 0.
            stx    timeh
            stx    timem
            stx    times
                 
            cli                     ; enable interrupt, global
                          
            LDAA  #'0'              ; Intialize Clock to 00:00:00
            LDAB  #'0'
            STD   ASCII_timeh
            STD   ASCII_timem
            STD   ASCII_times
            
            CLRA
            CLRB
                        
            LDX   #commands
            JSR   printmsg
            JSR   nextline   
            JSR   nextline       
            JSR  get_time           ; Print Time


main_loop            
            LDX  #C_BUF
            JSR  print_time
read_Command
            ;JSR  get_time
            
            
            JSR   getchar               ; Read a character
            CMPA  #$00                  ; If NULL Character go back to reading
            BEQ   read_Command 
            
            CMPA  #CR
            BEQ   store_Char 
            JSR   putchar               ; If not NULL, char is displayed on the terminal window - echo print

store_Char
            LDAB  C_IDX                 ; Load the current index into B
            CMPB  #11                   ; Check if we've reached the end of the buffer
            BGE   buffer_Full           ; If we're at or past the end, handle accordingly

            STAA  1,X+                   ; Store the character in Accumulator A in the Command buffer in X Register
                                         ; Increment the address pointed by the X register to add bytes in next Buffer Location
            INCB                        ; Increment B (CommandIndex)
            STAB  C_IDX                 ; Update CommandIndex

            CMPA  #CR                   ; Check if it's a Carriage Return
            
            BEQ   execute_Command       ; If yes, proceed with command execution
            BRA   read_Command          ; If no, read next character
            

buffer_Full                             ; Handle buffer full situation by stopping reading                             
            BRA   execute_Command       ; Return from subroutine

execute_Command                         ; Here, compare the CommandBuffer with known commands:
            
            JSR   parse_C_BUF           ; Parse Command Buffer into individual arguments
            LDX   #C_BUF  
            LDAA  , X                   ; Load the first character

set_time                                ; Check if CMD is 't' for Set Time            
            CMPA  #'t'                  ; Check if command begins with "t"
            LBNE   hour_display
            
            
                                        
            LDX   #t_ASCII_timeh        ; Convert Hour ASCII characters to binary value
            LDY   #t_DIGIT_timeh
            JSR   ASCII_TO_NUM
            
            CMPB  #0                    ; Check if value is within range [0-23]
            LBLT  invalid_input
            CMPB  #23
            LBGT  invalid_input
            STAB  t_timeh
            
            
            LDX   #t_ASCII_timem        ; Convert Minute ASCII characters to binary value
            LDY   #t_DIGIT_timem
            JSR   ASCII_TO_NUM
            
            CMPB  #0                    ; Check if value is within range [0-59]
            LBLT  invalid_input
            CMPB  #59
            LBGT  invalid_input
            STAB  t_timem
            
            LDX   #t_ASCII_times        ; Convert Seconds ASCII characters to binary value
            LDY   #t_DIGIT_times
            JSR   ASCII_TO_NUM
            
            CMPB  #0                    ; Check if value is within range [0-59]
            LBLT  invalid_input
            CMPB  #59
            LBGT  invalid_input
            STAB  t_times
            
            
            LDAA  t_timeh               ; Once all values are determined Valid Update Clock
            STAA  timeh                 ; Update Hour Integer Value
            LDX   t_ASCII_timeh
            STX   ASCII_timeh           ; Update Hour ASCII value 
            LDX   t_DIGIT_timeh
            STX   DIGIT_timeh
            
            LDAA  t_timem               ; Update Minute Integer Value
            STAA  timem
            LDX   t_ASCII_timem
            STX   ASCII_timem           ; Update Minute ASCII value 
            LDX   t_DIGIT_timem
            STX   DIGIT_timem
            
            LDAA  t_times               ; Update Second Integer Value
            STAA  times                            
            LDX   t_ASCII_times
            STX   ASCII_times            
            LDX   t_DIGIT_times         ; Update Seconds ASCII values 
            STX   DIGIT_times
            LBRA  cmd_executed
            
hour_display            
            CMPA  #'h'                  ; Check if command begins with "h"
            BNE   minute_display
            STAA  display
            
            LDAB  timeh                 ; Load Hour Integer Value
            STAB  PORTB                 ; Store Value and Display in 7 segment Display
       
            LBRA   cmd_executed
            
            
minute_display
            CMPA  #'m'                  ; Check if command begins with "m"
            BNE   seconds_display  
            STAA  display
            
            LDAB  timem                 ; Load Minute Integer Value
            STAB  PORTB                 ; Store Value and Display in 7 segment Display
            
            LBRA   cmd_executed
            
            
seconds_display
            CMPA  #'s'                  ; Check if command begins with "s"
            BNE   quit
            
            STAA  display
            
            LDAB  times                 ; Load Seconds Integer Value
            STAB  PORTB                 ; Store Value and Display in 7 segment Display
            
            LBRA   cmd_executed
            
quit
            CMPA  #'q'                  ; Check if command begins with "q"
            LBNE  invalid_input               ; If not then invalid input                       
            STAA  display
            
TypeWriter
            JSR   nextline           ; move the cursor to beginning of the line
                                     ;  Cariage Return/Enter key
                                     ; move the cursor to next line, Line Feed
            LDX   #type_writer_msg   ; Prints message informing user that they're in
            JSR   printmsg           ;  Type Writer Mode
            JSR   nextline           ; Goes to Next Line then skips a line
                                     ;  For the sake of Readibility
            JSR   nextline 
            
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


invalid_input                       
            LDX   #input_error       ; print Invalid Data Input Error Message
            JSR   printmsg
            LBRA  cmd_executed

            
invalid_time                       
            LDX   #time_error       ; print Invalid Data Input Error Message
            JSR   printmsg
            LBRA  cmd_executed

invalid_cmd                       
            LDX   #cmd_error       ; print Invalid Data Input Error Message
            JSR   printmsg
            LBRA  cmd_executed

cmd_executed                         ; Once Command Executed Clear Buffer and Variables and restart Loop
            JSR   clear_C_BUF
            
            LDAA  #0
            STAA  flag_100
            STAA  ctr_3
            ;STAA  ctr_4
            STAA  ctr125u
            JSR   nextline 
            LBRA  main_loop
            

**************************************************************************************
*                               Subroutine Section                                   *
**************************************************************************************

;*********** RTI interrupt service routine ***************
rtiisr      bset   CRGFLG,%10000000 ; clear RTI Interrupt Flag - for the next one
            ldx    ctr2p5m          ; every time the RTI occur, increase
            inx                     ;    the 16bit interrupt count
            stx    ctr2p5m
            JSR    get_time
rtidone     RTI
;***********end of RTI interrupt service routine********

;***********Timer OC6 interrupt service routine***************
oc6isr
            ldd   #3000              ; 125usec with (24MHz/1 clock)
            addd  TC6H               ;    for next interrupt
            std   TC6H               ; 
            bset  TFLG1,%01000000    ; clear timer CH6 interrupt flag, not needed if fast clear enabled
            ldd   ctr125u
            ldx   ctr125u
            inx                      ; update OC6 (125usec) interrupt counter
            stx   ctr125u
            
            
            ldaa  Op
            CMPA  #1
            BEQ   st_100
            CMPA  #2            
            BEQ   triangle
            CMPA  #3
            BEQ   square
            CMPA  #4
            BEQ   square_100 
            LBRA   print_wave
st_100
            ldy   #3
            EMUL  
            STAB   ctr_3
            clra
            JSR   pnum10
            RTI
            
triangle
            LDAA  logic_flag
            CMPA  #1
            BEQ   t_down
            
t_up        
            LDAB  ctr_4
            INCB
            STAB  ctr_4
            DECB
            CMPB  #255
            BNE   print_wave
            LDAA  #1
            STAA  logic_flag
            BRA   print_wave
            
t_down      
            LDAB   ctr_4      
            DECB
            STAB   ctr_4
            CMPB   #0
            BNE    print_wave
            STAB   logic_flag
            BRA    print_wave

square
            LDAA  logic_flag
            CMPA  #1
            BEQ   sq_hi

sq_lo       LDAA  ctr_4
            INCA
            STAA  ctr_4
            DECA
            LDAB  #0
            CMPA  #255
            BNE   print_wave
            LDAA  #1
            STAA  logic_flag
            BRA   print_wave
sq_hi     
            LDAB   #255
            LDAA   ctr_4      
            DECA
            STAA   ctr_4
            CMPA   #0   
            BNE    print_wave
            LDAA   #0
            STAA   logic_flag
            BRA    print_wave

square_100
            LDAA  logic_flag
            CMPA  #1
            BEQ   sq_hi100

sq_lo100    LDAA  ctr_4
            INCA
            STAA  ctr_4
            DECA
            LDAB  #0
            CMPA  #79               ; 100Hz/31.25 = 3.2       256/3.2 = 80
            BNE   print_wave
            LDAA  #1
            STAA  logic_flag
            BRA   print_wave
sq_hi100     
            LDAB   #255
            LDAA   ctr_4      
            DECA
            STAA   ctr_4
            CMPA   #0   
            BNE    print_wave
            LDAA   #0
            STAA   logic_flag
            BRA    print_wave

            
print_wave  clra                     ;   print ctr125u, only the last byte 
            jsr   pnum10             ;   to make the file RxData3.txt with exactly 2048 data 
oc2done     RTI

;***********end of Timer OC6 interrupt service routine********

;***************StartTimer6oc************************
;* Program: Start the timer interrupt, timer channel 6 output compare
;* Input:   Constants - channel 6 output compare, 125usec at 24MHz
;* Output:  None, only the timer interrupt
;* Registers modified: D used and CCR modified
;* Algorithm:
;             initialize TIOS, TIE, TSCR1, TSCR2, TC2H, and TFLG1
;**********************************************
StartTimer6oc
            PSHD
            LDAA   #%01000000
            STAA   TIOS              ; set CH6 Output Compare
            STAA   TIE               ; set CH6 interrupt Enable
            LDAA   #%10000000        ; enable timer, Fast Flag Clear not set
            STAA   TSCR1
            LDAA   #%00000000        ; TOI Off, TCRE Off, TCLK = BCLK/1
            STAA   TSCR2             ;   not needed if started from reset

            LDD    #3000            ; 125usec with (24MHz/1 clock)
            ADDD   TCNTH            ;    for first interrupt
            STD    TC6H             ; 

            BSET   TFLG1,%01000000   ; initial Timer CH6 interrupt flag Clear, not needed if fast clear set
            LDAA   #%01000000
            STAA   TIE               ; set CH6 interrupt Enable
            PULD
            RTS
;***************end of StartTimer2oc*****************


;***********pnum10***************************
;* Program: print a word (16bit) in decimal to SCI port
;* Input:   Register D contains a 16 bit number to print in decimal number
;* Output:  decimal number printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Keep divide number by 10 and keep the remainders
;     Then send it out to SCI port
;  Need memory location for counter CTR and buffer BUF(6 byte max)
;**********************************************
pnum10          pshd                   ;Save registers
                pshx
                pshy
                clr     CTR            ; clear character count of an 8 bit number

                ldy     #BUF
pnum10p1        ldx     #10
                idiv
                beq     pnum10p2
                stab    1,y+
                inc     CTR
                tfr     x,d
                bra     pnum10p1

pnum10p2        stab    1,y+
                inc     CTR                        
;--------------------------------------

pnum10p3        ldaa    #$30                
                adda    1,-y
                jsr     putchar
                dec     CTR
                bne     pnum10p3
                jsr     nextline
                puly
                pulx
                puld
                rts
;***********end of pnum10********************
          

;***********printHx***************************
; prinHx: print the content of accumulator A in Hex on SCI port
printHx     psha
            lsra
            lsra
            lsra
            lsra
            cmpa   #$09
            bhi    alpha1
            adda   #$30
            jsr    putchar
            bra    low4bits
alpha1      adda   #$37
            jsr    putchar            
low4bits    pula
            anda   #$0f
            cmpa   #$09
            bhi    alpha2
            adda   #$30
            jsr    putchar
            rts
alpha2      adda   #$37
            jsr    putchar
            rts
;***********end of printhx***************************
 
;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* C
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                jsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
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
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                       ; send a character
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

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            rts
;****************end of nextline***************

;****************delay1ms**********************
delay1ms:   pshx
            ldx   #$1000           ; count down X, $8FFF may be more than 10ms 
d1msloop    nop                    ;   X <= X - 1
            dex                    ; simple loop
            bne   d1msloop
            pulx
            rts
;****************end of delay1ms***************
;***********************get_time**********************************
;* Program: Increment and Print Time if 1 second has passed
;* Input:   ctr2p5m variable
;* Output:  ctr2p5m variable and time with Command buffer printed 
;*          on the terminal connected to SCI port
;*
;* Registers modified: CCR
;* Algorithm:
;*      Check for if 1 second has passed, if not increment ctr2p5m
;*      If 1 second passed increment time and reset ctr2p5m
;*****************************************************************
get_time
            psha
            pshx
            
            ldx    ctr2p5m          ; check for 0.5 sec
;            cpx    #200            ; 2.5msec * 200 = 0.5 sec
            cpx    #40              ; 2.5msec * 40 = 0.1 sec
            blo    done_time        ; NOT yet

            ldx    #0               ; interval is up,
            stx    ctr2p5m          ; clear counter to restart
            JSR    increment_time
            ;JSR    nextline
            ;JSR    print_time
        
        
            LDAB   display                  ; Check Display Mode then print hour, second, or time
            CMPB   #'h'
            BNE    disp_min
            LDAB   timeh
            STAB   PORTB
  
disp_min      
            CMPB   #'m'
            BNE    disp_sec
            LDAB   timem
            STAB   PORTB
        
disp_sec
            CMPB   #'s'
            BNE    done_time
            LDAB   times            
            STAB   PORTB
                       
done_time                           
            pulx                    ; Restore Registers
            pula
            rts
;**********************end of get_time****************************

;********************increment_time*******************************
;* Program: Increment Time
;* Input:   times, timem, timeh  time variables 
;* Output:  Updated timeh, timem, times time variables         
;*
;* Registers modified: CCR
;* Algorithm:
;*      Increase current time by 1 second.
;*    
;*****************************************************************
increment_time
            psha                      ; Save current registers
            pshb
            pshx
            
            LDAA    #0
                                      
inc_secs    LDAB    times
            CMPB    #59               ; Check if seconds = 59
            BEQ     inc_minutes       ; If yes increment minutes
            INCB                      ; If not increment by 1
            
            STAB    times             ; Increment seconds ASCII value by 1
            LDY     #ASCII_times+1
            JSR     NUM_TO_ASCII
            
            BRA     done_inc          ; Exit Subroutine
            
inc_minutes
            STAA    times             ; Reset Seconds to 0
            LDAB    #'0'              
            LDX     #ASCII_times
            STAB    1,X+
            STAB    ,X
            
            LDAB    timem             ; Check if minutes = 59
            CMPB    #59               ; If yes increment hours
            BEQ     inc_hours         ; If not increment by 1
            INCB
            
            STAB    timem             ; Increment minute ASCII value by 1
            LDY     #ASCII_timem+1
            JSR     NUM_TO_ASCII
            
            BRA     done_inc          ; Exit Subroutine
            
inc_hours
            STAA    timem             ; Reset minutes to 0
            LDAB    #'0'
            LDX     #ASCII_timem
            STAB    1,X+
            STAB    ,X
            
            LDAB    timeh             ; Check if hours = 23
            CMPB    #23               ; If yes increment day
            BEQ     inc_day
            INCB
            
            STAB    timeh             ; If not then increment hours
            LDY     #ASCII_timeh+1    ; Increment ASCII value
            JSR     NUM_TO_ASCII
            
            BRA     done_inc          ; Exit Subroutine  

inc_day                               ; If hour is 23 
            STAA    timeh             ; Set hour to 0
            LDAB    #'0'              ; Set Hour ASCII values to '0'
            LDX     #ASCII_timeh
            STAB    1,X+
            STAB    ,X
            
done_inc    PULA                      ; Finish Subroutine
            PULB                      ; Restore Registers
            PULX
            RTS                       ; Return from Subroutine

;******************end of increment_time**************************

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
;     Repeat twice for a total of 2 bytes
;**********************************************
printnum       psha                   ; Save registers
               pshx
               ldab    #2             ; Load the maximum number of characters to B

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

;***********end of printmsg********************

;***********printbuf***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;*          These Characters make the user inputted command in
;*          Command Buffer
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat for length of C_BUF
;**********************************************
printbuf        psha                   ;Save registers
                pshx
                
                LDX     #C_BUF         ; Load Base Address of Command Buffer        
                ldab    C_IDX          
                
printbufloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpb    #0
                beq     printbufdone   ;end of strint yet?
                jsr     putchar        ;if not, print character and do next
                decb
                bra     printbufloop
printbufdone    pulx 
                pula
                rts
;***********end of printbuf********************

;***********print_time***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;*     Pick up 1 byte from memory where X register is pointing
;*     Send it out to SCI port. 
;*     Update X register to point to the next byte
;*     Repeat until the byte data $00 is encountered
;*       (String is terminated with NULL=$00)
;*     Check display character then load appropriate value into
;*     PORTB to display on 7 segment display
;************************************************
print_time    
        PSHX

        LDX   #clock_header             ; Print 'Clock> '
        JSR   printmsg

        LDX   #ASCII_timeh              ; Print 'hh'
        JSR   printnum
        
        LDAA  #':'                      ; Print ':'
        JSR   putchar

        LDX   #ASCII_timem              ; Print 'mm'
        JSR   printnum                  

        LDAA  #':'                      ; Print ':'
        JSR   putchar

        LDX   #ASCII_times              ; Print 'ss'
        JSR   printnum   

        LDX   #cmd_header               ; Print 'CMD> '
        JSR   printmsg
        JSR   printbuf                  ; Print Command Buffer
        
        
done_print        
        PULX
        RTS
        
;****************end of print_time***************         
;-------------------------------------------------------------------------------------------------------------------------------
;* Program: Convert ASCII characters to Binary Number to be used for arithmetic
;*             
;* Input:   X:  Starting Address of ASCII array of Characters
;*          Y:  Starting Address of Where Digits Will be stored  
;* Output:  D:  Binary Value
;*
;* Algorithm:   Converts ASCII array in memory pointed by Register X to individual
;*              Binary Digits in array pointed to by Y register, 
;*              The Binary Digits are then put through an Exponent Loop then summed
;*              in ACC D using EMUL {(D x Y) -> Y:D} and ADD. Result is in ACC D.
;* 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - 
ASCII_TO_NUM                             ;Converts Number in variable held by Register X
                                         ;to Decimal Value using Length of Array stored in ACC A
                                         ; Stores Individual Decimal Values in Register Y
            
            LDAA    #1                   ; All values on clock are 2 digits long (hh:mm:ss)
                                         ; ACC A = Num_Length
            STAA    t_ctr                ; Temporary Counter variable = NUM_Len -1 for exponent
            LDAA    #2
            
            PSHY                         ; Save Starting Address of NUM_DIGITS

ASCII_Coversion                          ;Takes NUM_ASCII and converts them to Numbers in NUM_DIGITS
            LDAB    1,X+                 ; Load ASCII value to ACC B
            JSR     ASCII_TO_DIGIT       ; Convert ASCII value to DIGIT
            STAB    1,Y+                 ; Store Number in NUM_DIGITS
            DECA                         ; Decrement Length (Characters Left)
            CMPA    #0                   ; If no Characters Left Exit Loop
            BNE     ASCII_Coversion      ; Otherwise, Continue Looping
                                         ; CLR Accumulators
            CLRA
            CLRB
            PULY                         ; Restore Starting Address of NUM_Digits
            TFR     Y,X                  ; Copy Address into X register for EMUL
            LDAB    1,X+                 ; Load First Number into ACC B
            PSHA                         ; Save Accumulator A
            
                                         ; Check if t_ctr = 0 before entering the exponent loop
            LDAA    t_ctr                ; Check if t_ctr = 0 , which means Length = 1 therefore
            BEQ     skip_exponent        ; No Multiplication Required, if so skip exponent loop

exponent_loop
            PULA
            LDY    #10
            EMUL              
            ADDB   1,X+
            PSHA
            LDAA   t_ctr
            DECA   
            STAA   t_ctr
            CMPA   #0
            BNE    exponent_loop
            
skip_exponent
            PULA
            RTS

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 
ASCII_TO_DIGIT                           ; Convert ASCII char in ACC B to DIGIT
                                         ; Check if it is a valid hex character 
                                         ; (0-9 only since we are dealing with integer inputs) 
            
            
            SUBB    #$30                 ; Subtract ASCII value of '0'
            CMPB    #$0
            LBLT    invalid_input        ; If ASCII value is less than 0, then its not valid.
            CMPB    #$09                 ; Compare with 9
            BLE     RETURN               ; If less than or equal to 9, it's a number, and it's valid

IS_VALID
                                         ; The character is valid (0-9, A-F). If it's A-F, adjust the numerical value
            CMPB    #$09                 ; Compare with 9
            BLE     RETURN               ; If less than or equal to 9, subroutine done

RETURN     
            RTS                          ; Return to caller
;-------------------------------------------------------------------------------------------------------------------------------   
;* Program: Convert Binary Number to ASCII Value to be used for display
;*             
;* Input:   X:  Holds Divisor (#10) and Quotient for IDIV
;*          Y:  Last Address of ASCII array of Characters
;*          B:  Integer Input to be converted  
;* Output:  D:  Binary Value
;*
;* Algorithm:   Converts Integer input stored ACC B to ASCII value using IDIV with
;*              10 as divisor then adds ASCII bias. Result is stored in Address pointed  
;*              to by Y Register. Then Y pointer is decremented to point to previous 
;*              memory location. Thus working from LSB to MSB.
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- -  
NUM_TO_ASCII

            CLRA  
            LDX     #10            ; Decimal Number Already in ACC B before subroutine is called
            IDIV                   ; Divide by 10, quotient in X, remainder in D
            ADDB    #$30           ; Convert to ASCII
            STAB    ,Y             ; Store the ASCII character
            DEY                    ; Move to the next position in the array
            TFR     X,B
            ADDB    #$30           ; Convert to ASCII
            STAB    ,Y             ; Store the ASCII character
            RTS
;-----------------------------------------------------------------------------------   

;**************************  Command Buffer Parsing Subroutines **************************
;Subroutine that Parses Command Buffer for user inputted Address and Data Value

parse_C_BUF

            LDX   #C_BUF             ; Load Command Buffer to be Parsed
            LDY   #Op                ; Load Address where operation ASCII character is stored  ; (t, q, h, m, s)
            
            
Op_parse    LDAA  1,X+
            CMPA  #'t'
            BEQ   time_parse         ; If command is t, parse time inputs
            CMPA  #'q'
            BEQ   valid_op
            CMPA  #'h'
            BEQ   valid_op
            CMPA  #'m'
            BEQ   valid_op
            CMPA  #'s'
            BEQ   valid_op
            CMPA  #'g'               ; If command begins with 'g' then its a wave gen command
            BEQ   wave_parse
            LBRA  invalid_input

valid_op
            STAA  ,Y                 ; Store Command Value in Base Address of OP value
            LDAA  1,X+               ; Load Next Character
            CMPA  #CR                ; After q,h,m,s commands Carriage Return is expected, otherwise invalid input
            LBNE  invalid_cmd 
            LBRA  parse_complete     
            
            
time_parse                           ; if Op = 't'
            STAA  ,Y                 ; Store Command Value in Base Address of OP value
            LDAA  1,X+               ; Load Next Character
            CMPA  #SPACE             ; If it is Space then prepare to parse time input
            LBNE  invalid_time          
            LDY   #t_ASCII_timeh       ; Begin Parse of Hour Input
            LDAB  #0                 ; Store Counter in B to keep track of Length of Inputs

hour_parse      
            
            LDAA    1,X+             ; Load Byte Pointed to by X Register then Increment X Register    
            CMPA    #'0'             ; Check if Character is a digit
            
            BLT     hour_not_digit
            CMPA    #'9'
            BGT     hour_not_digit

            STAA    1,Y+             ; Store the digit in the ASCII_timeh array
            INCB                     ; Increment Length of Number stored in ACC B
            
            CMPB    #2
            LBGT    invalid_time     ; If length of Number is not 2 then its an invalid Input
            
            BRA     hour_parse        ; Go to Next Character in Buffer and repeat

hour_not_digit
            JSR     input_not_digit   ; Checks if Colon and previous input is 2 characters otherwise, invalid input error
                                      ; If valid then proceed
end_hour_parse
            LDAB    #0                ; Reset Length Counter to begin counting length of minute input 
            LDY     #t_ASCII_timem      ; Since we have two hour characters and colon, next characters will be minutes
              
            
minute_parse      
            LDAA    1,X+               ; Load Byte Pointed to by X Register then Increment X Register           
            CMPA    #'0'             ; Check if Character is a digit
            BLT     minute_not_digit
            CMPA    #'9'
            BGT     minute_not_digit

            STAA    1,Y+             ; Store the digit in the ASCII_timem array
            INCB                     ; Increment Length of Number stored in ACC B
            
            CMPB    #2
            LBGT    invalid_time    ; If length of Number is greater than 2 then its an invalid Input
            BRA     minute_parse      ; Go to Next Character in Buffer and repeat

minute_not_digit
            JSR     input_not_digit   ; Checks if Colon and previous input is 2 characters otherwise, invalid input error
                                      ; If valid then proceed           
end_minute_parse
            LDAB    #0                ; Reset Length Counter to begin counting length of second(s) input 
            LDY     #t_ASCII_times      ; Since we have two minute characters and colon, next characters will be seconds            
            
seconds_parse      
            LDAA    1,X+               ; Load Byte Pointed to by X Register then Increment X Register           
            CMPA    #'0'             ; Check if Character is a digit
            BLT     seconds_not_digit
            CMPA    #'9'
            BGT     seconds_not_digit

            STAA    1,Y+             ; Store the digit in the ASCII_timem array
            INCB                     ; Increment Length of Number stored in ACC B
            
            CMPB    #2
            LBGT    invalid_time     ; If length of Number is greater than 2 then its an invalid Input
            BRA     seconds_parse      ; Go to Next Character in Buffer and repeat

seconds_not_digit
            CMPB    #2                ; If Length of previous input isnt 2 characters then invalid input
            LBNE    invalid_time      
            CMPA    #CR
            LBNE    invalid_time      ; If character after seconds input isnt Enter Key then invalid command
            RTS


wave_parse
gen_st

            LDAA  1,X+
            CMPA  #'w'                ; Check for gw command
            BNE   gen_t               ; If next char not 'w' check for 't'
            LDAA  1,X+                ; If 'gw' in buffer and next character is not carriage return
            CMPA  #CR
            BNE   gen_st100           ; Check for '2'
            
            JSR     nextline
            ldx     #msg_st          ; print '> Generating Sawtooth Wave (8000Hz) for 2048 points'
            jsr     printmsg
            jsr     nextline

            jsr     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters
       
            JSR     enable_interrupt
            JSR     wave_gen
            JSR     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters
            

            LBRA     cmd_executed
            
gen_st100
            CMPA    #'2'             ; If char not = 2 then invalid
            LBNE     invalid_input
            LDAA    ,X
            CMPA    #CR              ; If not gw2 not succeeded by CR then invalid
            LBNE     invalid_input
           
            JSR     nextline
            ldx     #msg_st100       ; print '> Generating Sawtooth Wave (100Hz) for 2048 points'
            jsr     printmsg
            jsr     nextline
            jsr     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters
           
           
           
            LDAA    #1
            STAA    Op
            
            JSR     enable_interrupt
            JSR     wave_gen
            JSR     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters
            LBRA     cmd_executed            
            
gen_t
            CMPA    #'t'
            BNE     gen_sq            
            LDAA    1,X+
            CMPA    #CR
            LBNE     invalid_input
            
            JSR     nextline
            ldx     #msg_t           ; print '> Generating Triangle Wave (8000Hz) for 2048 points'
            jsr     printmsg
            jsr     nextline
            jsr     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters
            
            LDAA    #2
            STAA    Op
            
            JSR     enable_interrupt
            JSR     wave_gen
            JSR     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters
            
            
            LBRA     cmd_executed


gen_sq
            CMPA    #'q'
            LBNE     invalid_input          
            LDAA    1,X+
            CMPA    #CR
            BNE     gen_sq100
            
            JSR     nextline
            ldx     #msg_sq          ; print '> Generating Square Wave (8000Hz) for 2048 points'
            jsr     printmsg
            jsr     nextline
            jsr     delay1ms         ; flush out SCI serial port 
            
            LDAA    #3
            STAA    Op
            
            JSR     enable_interrupt
            JSR     wave_gen
            JSR     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters                         ; wait to finish sending last characters
            
            LBRA     cmd_executed 
            
            
gen_sq100   
            CMPA    #'2'
            LBNE     invalid_input
            LDAA    ,X
            CMPA    #CR
            LBNE     invalid_input
            
            JSR     nextline
            ldx     #msg_sq100        ; print '> Generating Triangle Wave (100Hz) for 2048 points'
            jsr     printmsg
            jsr     nextline
            jsr     delay1ms         ; flush out SCI serial port 
            
            JSR     nextline
            jsr     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters
            LDAA    #4
            STAA    Op
            
            JSR     enable_interrupt
            JSR     wave_gen
            LBRA    cmd_executed           
            
parse_complete
            RTS                      ; Return from Subroutine
;-------------------------------------------------------------------------------------------------------------------------------                 
input_not_digit
           CMPB    #2                ; If Length of previous input isnt 2 characters then Colon shouldnt be there
           LBNE    invalid_time      ; Hence Invalid Input
           CMPA    #':'
           LBNE    invalid_time     
           RTS                       ; If input valid, return from subroutine and continue
;-------------------------------------------------------------------------------------------------------------------------------
clear_C_BUF
           PSHA                     ; Save Accumulators 
           PSHB
           
           CLRA                     ; Clear Accumulators
           CLRB
           STAA  C_IDX              ; Clear Command Index Variable
            
           LDX   #C_BUF             ; Clear Command Buffer
           STD   2,X+               ; Clear Bytes 0-1
           STD   2,X+               ; Clear Bytes 2-3
           STD   2,X+               ; Clear Bytes 4-5
           STD   2,X+               ; Clear Bytes 6-7
           STD   2,X+               ; Clear Bytes 8-9
           STAA   ,X                ; Clear Byte 10
           
           PULA                     ; Restore Accumulators
           PULB
           RTS                      ; Return from Subroutine

;-------------------------------------------------------------------------------------------------------------------------------  
enable_interrupt
           ldx     #0
           stx     ctr125u
           jsr     StartTimer6oc
           ;CLI                      ; Interrupt enable, for Timer OC6 interrupt start
           RTS
           
end_wave_gen
            LDAA    #%00000000
            STAA    TIE               ; disable OC6 interrupt

            jsr     nextline
            jsr     nextline

            ldx     #msg5            ; print '> Done!  Close Output file.'
            jsr     printmsg
            jsr     nextline

            ldx     #msg6            ; print '> Ready for next data transmission'
            jsr     printmsg
            jsr     nextline
            RTS


wave_gen
            ldd     ctr125u
            cpd     #DATAmax         ; 2048 bytes will be sent, the receiver at Windows PC 
            bhs     end_wave_gen     ;   will only take 2048 bytes.
            bra     wave_gen         ; set Terminal Cache Size to 10000 lines, update from 1000 lines
            RTS


;-------------------------------------------------------------------------------------------------------------------------------  

msg3        DC.B   '> Be sure to start saving Terminal data: open Output file = RxData3.txt', $00
msg4        DC.B   '> When ready, hit Enter/Return key for sawtooth wave, 1024 point print.', $00
msg5        DC.B   '> Done!  You may close the Output file.', $00
msg6        DC.B   '> Ready for next data transmission, hit Enter key.',  $00

input_error     DC.B    '     Error> Invalid Input Format',$00
time_error      DC.B    '     Error> Invalid time format. Correct example => 00:00:00 to 23:59:59',$00
cmd_error       DC.B    '     Error> Invalid command. (h for hour display, m for minute display, s for seconds display and q for quit)', $00 
  
type_writer_msg DC.B    'Wave Generator and Clock stopped and Typewrite program started You may type below.', $00
 
msg_st      DC.B   'Generating Sawtooth Wave (8000Hz) for 2048 points...', $00
msg_st100   DC.B   'Generating Sawtooth Wave (100Hz) for 2048 points...',  $00
msg_t       DC.B   'Generating Triangle Wave (8000Hz) for 2048 points...', $00
msg_sq      DC.B   'Generating Square Wave (8000Hz) for 2048 points...',   $00
msg_sq100   DC.B   'Generating Square Wave (100Hz) for 2048 points...',  $00

commands    
            DC.B        'Here are all the possible Commands (NOTE: Commands are CASE SENSITIVE) No Unnecessary Spaces',CR, LF,CR,LF
            DC.B        'Nothing is Displayed on 7 segment Display until one of the Display Commands (h,m,s) is entered or user is in TypeWriter Mode', CR, LF,CR,LF
            DC.B        't: Command Format: t <hh>:<mm>:<ss><Enter-key(CR)>', CR, LF
            DC.B        'h: Display hour on 7 segment display', CR, LF
            DC.B        'm: Display minutes on 7 segment display', CR, LF
            DC.B        's: Display seconds on 7 segment display', CR, LF
            DC.B        'q: Quit menu program, run "Type writer" program.', $00

* Add any subroutines here
*
            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled