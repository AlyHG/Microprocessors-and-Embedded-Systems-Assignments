*******************************************************************************************
* Aly Ghallab
* CMPEN 472 HW 1
* Date: August 28,2023
*
* Comments : This program is developed and simulated using CodeWarrior development Software
* Program Info: Allocate an Array in Memory (222 Locations) then loop through it to fill
*               the array with '*'
*
*******************************************************************************************
* Parameter Declaration Section
*
* Export Symbols
        XDEF        pgstart; export 'pgstart' symbol
        ABSENTRY    pgstart; for assembly entry point
*        
* Symbols and Macros
*
PORTA   EQU         $0000 ;i/o port addresses
PORTB   EQU         $0001
DDRA    EQU         $0002
DDRB    EQU         $0003
*
*******************************************************************************************
* Data Section
*
        ORG         $3000 ;Reserved Memory Starting Address
here    DS.B        $DE   ;222 Memory Locations Reserved
count   DC.B        $DE   ;constant, star count = 222
*
*******************************************************************************************
* Program Section
*
        ORG         $3100 ;Program Start Address in RAM
pgstart ldaa        #'*'  ;load '*' into accumulator A
        ldab        count ;load star counter into B
        ldx         #here ;load address pointer into X
loop    staa        0,x   ;put a star
        inx               ;point to next location
        decb              ;decrease counter
        bne         loop  ;if not done, repeat
done    bra         done  ;task complete
                          ;do nothing
*
* Add any subroutines here
*
        END               ;last line of a file