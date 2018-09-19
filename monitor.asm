;Z80 Computer Monitor V0.5 Billy J.

org     0000

buffer           equ     $5000  ;bottom of 255 byte input buffer
scratch          equ     $5100

ledout           equ     $00
switchin         equ     $20

uartio           equ     $40
uartio_ier       equ     $41
uartio_lcr       equ     $43

ld      sp, $5F00
call    uart_init

main:
          ld      hl, mon_msg
          call    uart_prnt_str

main_loop:
          ld      hl, prompt                    ;display prompt ">"
          call    uart_prnt_str

          call    uart_get_string

          ld      hl, help                      ;check for valid commands
          call    string_comp
          jp      z,help_co

          ld      hl, peek
          call    string_comp
          jp      z,peek_co

          ld      hl, poke
          call    string_comp
          jp      z,poke_co

          ld      hl, load
          call    string_comp
          jp      z,load_co

          ld      hl, exec
          call    string_comp
          jp      z,exec_co

          ld      hl, clear
          call    string_comp
          jp      z,clear_co

          ld      a, (buffer)                    ;handle invalid command
          cp      $FF                           ;check for blank command
          jp      z, main_loop
          ld      hl, invalid_a                 ;display invalid msg.
          call    uart_prnt_str
          ld      hl, buffer
          call    uart_prnt_str
          ld      hl, invalid_b
          call    uart_prnt_str

          jp      main_loop

; Initialize UART
uart_init:
          push    af
          ld      a,$00          ; Disable all interrupts
          out     (uartio_ier),a        ; Send to Interrupt Enable Register
          ld      a,$80          ; Mask to set DLAB on
          out     (uartio_lcr),a        ; Send to Line Control Register
          ld      a,$0C            ; Divisor of 12 = 9600 bps with 1.8432 MHz clock
          out     (uartio),a        ; Set LSB of divisor
          ld      a,$00            ; This will be the MSB of the divisior
          out     (uartio_ier),a        ; Send to the MSB register
          ld      a,$03          ; 8 bits, 1 stop, no parity (and clear DLAB)
          out     (uartio_lcr),a        ; Write new value to LCR
          pop     af
          ret

; Wait for a byte from the UART, and save it in A
uart_rx:
                call    uart_rx_ready
                in      a,($40)
                ret

; Returns when UART has received data
uart_rx_ready:
                push    af
uart_rx_ready_loop:
                in      a,($45)        ; fetch the conrtol register
                bit     0,a             ; bit will be set if UART has data
                jp      z,uart_rx_ready_loop
                pop     af
                ret

; Sends byte in A to the UART
uart_tx:
                call    uart_tx_ready
                out     (0x40),a
                ret

; Returns when UART is ready to receive
uart_tx_ready:
                push    af
uart_tx_ready_loop:
                in      a,(0x45)        ; fetch the control register
                bit     5,a             ; bit will be set if UART is ready
                jp      z,uart_tx_ready_loop
                pop     af
                ret

;Print string starting at (HL), stop printing at $FF
uart_prnt_str:
                push af
uart_prnt_str_loop:
                 ld    a, (hl)
                 cp    $FF
                 jp    z,uart_end_prnt_str	    ;Jump if end byte is found
                 call  uart_tx
                 inc   hl                     		;Increment pointer to next char
                 jp    uart_prnt_str_loop
uart_end_prnt_str:
                  pop  af
                  ret


;store string from UART
uart_get_string:
              push af
              ld   hl, buffer
get_string_spcl:                         ;Special character handling
              call uart_rx
              cp   $0D                   ;Enter
              jp   z, get_string_enter
              cp   $7F                   ;Backspace
              jp   z, get_string_bkspc
              cp   $1B                   ;Escape
              jp   z, get_string_escape
              ld   b, a                   ;save character
              ld   a, $FF                 ;load end of buffer lsb(e.g. $50FF)
              ld   c, l                   ; compare with current location
              sub  c
              jp   z, get_string_overflow
              ld   a, b                    ;restore character
              call uart_tx               ;echo character
              ld   (hl), a               ;Store char in ram
              inc  hl

              jp   get_string_spcl
get_string_enter:
              ld   (hl), $FF             ;Add EOS char
              pop af
              ret
get_string_bkspc:
              ld   d, h
              ld   e, l
              dec  de
              ld   a, (de)
              cp   $FF                   ;checks to see if location before current char is beginning
              jp   z, get_string_spcl
              dec  hl
              ld   a, $7F                ;decrement to previous character, echo backspace
              call uart_tx
              jp   get_string_spcl
get_string_escape:                       ;need alternate escape handling
              jp   main
get_string_overflow:
              ld      hl, overflow_msg
              call    uart_prnt_str
              jp   main_loop

;compare string starting at HL to string in input buffer (DE)
string_comp:
              ld   de, buffer
string_comp_start:
              ld   a, (de)              ;compare characters
              cp   (hl)
              jp   z, string_comp_next
              cp    $20                  ;check for Space
              jp    z, string_comp_space
              jp   string_comp_fail
string_comp_next:
              ld   a, (hl)
              cp   $FF                  ;check for end of string
              jp   z, string_comp_no_args
              inc  hl
              inc  de
              jp string_comp_start
string_comp_fail:
              ld   a, $FF       ;clear Z flag
              cp   $AA
              ret
string_comp_no_args:
              ld      a, $00                ;clear argument memory
              ld      (scratch), a
              ld      (scratch+1), a
              ld      (scratch+2), a
              set      7, a                ;set invalid / h flagpeek
              ld      (scratch+3), a
string_comp_pass:                       ;set Z flag
              cp   a
              ret
string_comp_space:
              ld   a, (hl)
              cp   $FF                  ;check for end of string
              jp   z, arg_handler
              jp   string_comp_fail
;converts and stores arguments in ram
arg_handler:
          ld      a, $00                ;clear argument memory
          ld      (scratch), a
          ld      (scratch+1), a
          ld      (scratch+2), a
          ld      (scratch+3), a
arg_handler_principal:
          inc     de                     ;increment DE (to character in buffer after space), copy to HL
          ld      h, d
          ld      l, e                   ;first argument will be a 4 digit hex number
          call    is_hex
          jp      nz, arg_handler_invalid
          call    ascii_to_hex          ;get MSD, store in scratch ram 0
          ld      (scratch), a
          inc     hl
          call    is_hex
          jp      nz, arg_handler_invalid
          call    ascii_to_hex          ;get LSD, store in scratch ram  1
          ld      (scratch+1), a
          inc     hl
          ld      a, (hl)
          cp      $20                  ;check for Space
          jp      z, arg_handler_additional
          jp      arg_handler_end
arg_handler_additional:
          inc   hl
          ld    a, (hl)
          cp    $2D                     ;check for tac (-)
          jp    z, arg_handler_additional_2
          jp    arg_handler_invalid
arg_handler_additional_2:
          inc   hl
          ld    a, (hl)
          cp    $6E                               ;n
          jp    z, arg_handler_additional_n
          cp    $4E                               ;N
          jp    z, arg_handler_additional_n
          cp    $74                               ;t
          jp    z, arg_handler_additional_t
          cp    $54                                ;T
          jp    z, arg_handler_additional_t
          cp    $6F                                ;o
          jp    z, arg_handler_additional_o
          cp    $4F                                  ;O
          jp    z, arg_handler_additional_o
          cp    $68                                ;h
          jp    z, arg_handler_additional_h
          cp    $48                                 ;H
          jp    z, arg_handler_additional_h
          jp    arg_handler_invalid
arg_handler_additional_n:
          ld      a, (scratch+3)        ;get arg flags
          set     0, a                  ;set n flag
          ld      (scratch+3), a        ;write arg flags
          inc     hl
          call    is_hex
          jp      nz, arg_handler_invalid
          call    ascii_to_hex          ;get value, store in scratch ram 2
          ld      (scratch+2), a
          jp      arg_handler_additional_3
arg_handler_additional_t:
          ld      a, (scratch+3)        ;get arg flags
          set     1, a                  ;set t flag
          ld      (scratch+3), a        ;write arg flags
          jp      arg_handler_additional_3
arg_handler_additional_o:
          ld      a, (scratch+3)        ;get arg flags
          set     2, a                  ;set o flag
          ld      (scratch+3), a        ;write arg flags
          jp      arg_handler_additional_3
arg_handler_additional_h:
          ld      a, (scratch+3)        ;get arg flags
          set     7, a                  ;set h flag, same as invalid flag
          ld      (scratch+3), a        ;write arg flags
          jp      arg_handler_additional_3

arg_handler_additional_3:
          inc   hl
          ld    a, (hl)
          cp    $20                  ;check for Space
          jp    z, arg_handler_additional
          cp    $FF
          jp    z, arg_handler_end
          jp    arg_handler_invalid
arg_handler_end:
          jp    string_comp_pass
arg_handler_invalid:
          ld      a, (scratch+3)        ;get arg flags
          set     7, a                  ;set invalid flag, same as h flag
          ld      (scratch+3), a        ;write arg flags
          ld    hl, invalid_arg_msg
          call  uart_prnt_str
          jp    string_comp_pass


;Make sure  2 bytes at (HL)  are ascii encoded hex numbers
is_hex:
        ld      d, h
        ld      e, l
        call    is_hex_2
        jp      nz, is_hex_fail
        inc     de
is_hex_2:
        ld      a, (de)
        cp 	$30              ;0-9 lower bound
        jp      c, is_hex_fail
        cp	$40		;0-9 upper bound+1
        jp	c,  is_hex_pass
        cp 	$41              ;A-F lower bound
        jp      c, is_hex_fail
        cp	$47		;A-F upper bound+1
	jp	c,  is_hex_pass
        cp 	$61              ;a-f lower bound
        jp      c, is_hex_fail
        cp	$67		;a-f upper bound+1
        jp	c,  is_hex_pass
is_hex_fail:
        ld      a, $FF       ;clear Z flag
        cp      $AA
        ret
is_hex_pass:
        cp      a            ;set Z flag
        ret

;Convert ascii in HL to corresponding hex value
ascii_to_hex:
        ld      b, $00       ;will store entire byte, a 2 digit hex value
        call    to_hex_1
        rlca                  ;shit to high side of a
        rlca
        rlca
        rlca
        ld      b, a          ;store MSD in b
        inc     hl
to_hex_1:                    ;handles A-F, a-f
        ld      a, (hl)
        ld      c, a
        and     $70           ;check for A-F, a-f
        cp      $40
        jp      c, to_hex_2
        ld      a, c
        and     $0F
        add     a, $09
        jp      to_hex_end
to_hex_2:                     ;handles 0-9
        ld      a, c
        and     $0F
to_hex_end:
        or      b            ;commbine MSD and LSD, value in A, inc HL
        ret


;Convert binary number in A to ascii hex representation, stored in DE
hex_to_ascii:
        ld      h, a            ;store a in h
        call    hex_to_1
        ld      d, a
        ld      a, h
        call    hex_to_2
        ld      e, a
        ret
hex_to_1:                       ;work with MSD
        rra
        rra
        rra
        rra
hex_to_2                        ;work with LSD
        or      $F0
        daa
        add     a, $A0
        adc     a, $40
        ret



;Commands
help_co:
          ld      hl, help_msg
          call    uart_prnt_str
          jp      main_loop

peek_co:
          ld      a, (scratch+3)         ;get arg flags
          bit     7, a
          jp      nz, peek_co_invalid
          bit     0, a
          jp      nz, peek_co_1          ;if dump count 0 (not specified), make it 1
          ld      a, $01
          ld      (scratch+2), a
peek_co_1
          call    newline
          ld      a, (scratch+2)
peek_co_2:
          cp      $00                   ;check dump counter
          jp      z, main_loop
          ld      a, (scratch)          ;copy  address to HL
          ld      h, a
          ld      a, (scratch+1)
          ld      l, a
          ld      a, (scratch+3)
          bit     2, a
          jp      nz,   peek_co_io
          ld      b, (hl)               ;copy value in location (HL) to b
peek_co_3:
          inc     hl                    ;increment address
          ld      a, h
          ld      (scratch), a
          ld      a, l
          ld      (scratch+1), a
          ld      a, b
          call    hex_to_ascii
          ld      a, d
          call    uart_tx               ;print MSD
          ld      a, e
          call    uart_tx                ;print LSD
          ld      a, $20
          call    uart_tx                ;print Space
          ld      a, (scratch+2)        ;decrement dump counter
          dec     a
          ld      (scratch+2), a
          jp      peek_co_2
peek_co_io:
          ld      c, l
          in      a, (c)
          ld      b, a
          jp      peek_co_3
peek_co_invalid:
          ld      hl, peek_co_help_msg
          call    uart_prnt_str
          jp      main_loop

poke_co:
          ld      a, (scratch+3)         ;get arg flags
          bit     7, a
          jp      nz, poke_co_invalid
          bit     1, a
          jp      nz, poke_co_autojump
          bit     0, a
          jp      z, poke_co_invalid    ;must have value
          call    poke_co_write
          jp      main_loop
poke_co_write:
          ld      a, (scratch)          ;copy  address to HL
          ld      h, a
          ld      a, (scratch+1)
          ld      l, a
          ld      a, (scratch+3)
          bit     2, a
          jp      nz,   poke_co_io
          ld      a, (scratch+2)        ;copy value to A
          ld      (hl), a               ;write value
          ret
poke_co_io:
          ld      c, l
          in      a, (c)
          ld      a, (scratch+2)        ;copy value to A
          out      (c), a               ;write value
          ret
poke_co_autojump:
          ld    hl, poke_co_autojump_msg
          call  uart_prnt_str
poke_co_autojump_2:
          call  newline
          call  poke_co_print_address
          call  poke_co_get_value
          call  poke_co_write
          call  inc_Scratch01           ;address
          jp    poke_co_autojump_2
poke_co_print_address:
          ld      a, (scratch)
          call    hex_to_ascii
          ld      a, d
          call    uart_tx               ;print MSD
          ld      a, e
          call    uart_tx                ;print LSD
          ld      a, (scratch+1)
          call    hex_to_ascii
          ld      a, d
          call    uart_tx               ;print MSD
          ld      a, e
          call    uart_tx                ;print LSD
          ld      a, $3A                ;":"
          call    uart_tx
          ret
poke_co_get_value:
          call uart_rx
          cp   $1B                   ;Escape
          jp   z, main_loop
          call uart_tx
          ld   (buffer), a
          call uart_rx
          cp   $1B                   ;Escape
          jp   z, main_loop
          call uart_tx
          ld   (buffer+1), a
          ld    hl, buffer
          call  ascii_to_hex
          ld    (scratch+2), a
          ret
poke_co_invalid:
          ld      hl, poke_co_help_msg
          call    uart_prnt_str
          jp      main_loop


load_co:
          ld      a, $00
          ld      (scratch+6), a        ;for storing checksum
          ld      a, (scratch+3)         ;get arg flags
          bit     7, a
          jp      nz, load_co_invalid
          bit     0, a
          jp      nz, load_co_invalid
          bit     1, a
          jp      nz, load_co_invalid
          bit     2, a
          jp      nz, load_co_invalid
load_co_2:
          ld      hl, load_co_msg
          call    uart_prnt_str
          ld      hl, buffer
load_co_3:
          call  uart_rx
          cp    $1B                   ;Escape
          jp    z, main_loop
          call  uart_tx               ;echo character
          ld    (hl), a               ;Store char in ram
          ld    a, $03                 ;load end of buffer
          ld    c, l                   ; compare with current location
          sub   c
          jp    z, load_co_4
          inc   hl
          jp    load_co_3
load_co_4:
          ld    hl, buffer
          call  ascii_to_hex
          ld    (scratch+4), a
          inc    hl
          call  ascii_to_hex
          ld    (scratch+5), a
          call  newline
load_co_5:
          ld      a, (scratch)          ;copy  address to HL
          ld      h, a
          ld      a, (scratch+1)
          ld      l, a
          call    uart_rx               ;recieve and write value
          ld      (hl), a
          ld      b, a
          ld      a, (scratch+6)
          add     a, b
          ld      (scratch+6), a
          ld      a, (scratch+4)        ;print byte countdown
          call    hex_to_ascii
          ld      a, d
          call    uart_tx
          ld      a, e
          call    uart_tx
          ld      a, (scratch+5)
          call    hex_to_ascii
          ld      a, d
          call    uart_tx
          ld      a, e
          call    uart_tx
          ld      a, $0D
          call    uart_tx
          call    inc_Scratch01       ;inc address counter, dec byte counter
          call    dec_Scratch45
          ld      a, $00              ; check for byte counter 0
          cp      h
          call    z, load_co_end_check
          jp      load_co_5
load_co_end_check:
          cp      l
          jp      z, load_co_end
          ret
load_co_end:
          ld      hl,  load_co_checksum_msg
          call    uart_prnt_str
          ld      a, (scratch+6)        ;print checksum
          call    hex_to_ascii
          ld      a, d
          call    uart_tx
          ld      a, e
          call    uart_tx
          jp      main_loop
load_co_invalid
          ld      hl, load_co_help_msg
          call    uart_prnt_str
          jp      main_loop

exec_co:
          ld      hl, exec
          call    uart_prnt_str
          jp      main_loop

clear_co:
          ld      hl, clear_str
          call    uart_prnt_str
          jp      main_loop

;Functions
inc_Scratch01:
          ld      a, (scratch)          ;copy  address to HL
          ld      h, a
          ld      a, (scratch+1)
          ld      l, a
          inc     hl
          ld      a, h
          ld      (scratch), a
          ld      a, l
          ld      (scratch+1), a
          ret
dec_Scratch45:
          ld      a, (scratch+4)          ;copy  address to HL
          ld      h, a
          ld      a, (scratch+5)
          ld      l, a
          dec     hl
          ld      a, h
          ld      (scratch+4), a
          ld      a, l
          ld      (scratch+5), a
          ret

;Special string commands
newline:
        ld      hl, newline_msg
        call    uart_prnt_str
        ret

;Command Strings
help:
                  db      "help",$FF
peek:
                  db      "peek",$FF
poke:
                  db      "poke",$FF
load:
                  db      "load",$FF
exec:
                  db      "exec",$FF
clear:
                  db      "clear",$FF

peek_co_help_msg:
                  db      $0A,$0D,"~Usage~",$0A,$0D,$0A,$0D,"  peek XXXX -nAA -t -o -h",$0A,$0D,"XXXX   Address to peek.",$0A,$0D,"AA     Number of bytes to dump.*",$0A,$0D,"-t     Format as a table.*",$0A,$0D,"-o     Peek from I/O device.*",$0A,$0D,"-h     Display this help message.*",$0A,$0D,"  *  -  Optional",$FF
poke_co_help_msg:
                  db      $0A,$0D,"~Usage~",$0A,$0D,$0A,$0D,"  poke XXXX -nAA -t -o -h",$0A,$0D,"XXXX   Address to poke.",$0A,$0D,"AA     Value of byte to write.",$0A,$0D,"-t     Auto jump to next address.*",$0A,$0D,"-o     Poke to I/O device.*",$0A,$0D,"-h     Display this help message.*",$0A,$0D,"  *  -  Optional",$FF
poke_co_autojump_msg:
                  db      $0A,$0D,"Enter value to write to specified address. ESC to cancel.",$FF
load_co_help_msg:
                  db      $0A,$0D,"~Usage~",$0A,$0D,$0A,$0D,"  load XXXX",$0A,$0D,"XXXX   Start address of load.",$0A,$0D,$FF
load_co_msg:
                  db      $0A,$0D,"Number of bytes to load:",$FF
load_co_checksum_msg:
                  db      $0A,$0D,"Checksum:",$FF
;Strings
mon_msg:
                  db      $1B,$63,"Z80 Computer Monitor",$0A,$0D,"   v0.5  Billy J.",$0A,$0A,$0D,$FF
newline_msg:
                  db      $0A,$0D,$FF
prompt:
                  db      $0A,$0D,$3E,$FF
help_msg:
                  db      $0A,$0D,"~Commands~",$0A,$0D,$0A,$0D,"peek:  Display contents of memory location.",$0A,$0D,"poke:  Alter contents of memory location.",$0A,$0D,"load:  Save program from serial port to memory.",$0A,$0D,"exec:  Execute program stored in memory.",$0A,$0D,"clear: Clear the screen.",$0A,$0D,"help:  Display this help message.",$0A,$0D,$FF
clear_str:
                  db      $1B,$63,$FF
invalid_a:
                  db      $0A,$0D,$22,$FF
invalid_b:
                  db      $22," is not recognized as a valid command.",$FF
overflow_msg:
                  db      $0A,$0D,"Input buffer overflow",$FF
invalid_arg_msg:
                  db      $0A,$0D,"Invalid Arguments",$FF


