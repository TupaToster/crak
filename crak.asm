.286
.model tiny

framePos        equ     160d * 10d + 60d    ; initial pos
frameLenX       equ     20d                 ; the working area x len
frameLenY       equ     2d                  ; the working area y len
XLine           equ     1Fcdh               ; horizontal line symbol
YLine           equ     1Fbah               ; vertical line symbol
LTA             equ     1Fc9h               ; left top angle
RTA             equ     1Fbbh               ; right top angle
LBA             equ     1Fc8h               ; left bottom angle
RBA             equ     1Fbch               ; right bottom angle
FillStyle       equ     1Fh                 ; a byte for style
FieldStyle      equ     78h                 ; style byte for passwd field
mult            equ     151d                ; hash multiplier
passHash        equ     00d6h               ; hashed password (asmLol)

.code
org 100h
locals @@

EXIT    macro
        nop
        mov ax, 4c00h
        int 21h
        nop
        endm

debugger        macro           ; basically a debugger that prints 1 to some point on the screen when called
                nop
                push es bx
                xor bx, bx
                mov bx, 0b800h
                mov es, bx
                mov bx, 160d*2 + 20d
                mov es:[bx], 0ce31h
                pop bx es
                nop
                endm

start:          call ClearScr

                call DrawFrame

                call GetPasswd

                EXIT

; -------------------------------------
; Reads password from input until enter is pressed
; -------------------------------------
; Expects : none

; Exit : none

; Need : none

; Destroys : ax, di, bx, es
; ====================================

GetPasswd       proc

                lea di, buffer
                mov bx, 0b800h
                mov es, bx
                mov bx, framePos + 322d

@@Next:         mov ah, 07h
                int 21h
                mov byte ptr [di], al
                mov byte ptr es:[bx], 12h
                add bx, 2d
                add di, 2
                cmp al, 0Dh
                jne @@Next

                call getBufferHash

                cmp ax, passHash
                je @@granta
                call NoAccess
@@granta:       call Access

                endp

; ------------------------------------
; Hashes the contents of buffer straight up to 'enter' symbol
; ------------------------------------
; Expects : buffer containing anything

; Exit : hash in ax

; Needs : none

; Destroys : ax, si, bx, cx
; ====================================
getBufferHash   proc            ; при 6-значном пароле который может состоять из любых симболов таблицы размером в ~100
                                ; при выборе множителя из таблицы размером 10 рандомно при каждом запуске
                                ; подбор пароля по известному хэшу займет примерно 100^6 * 10/ (10^9) = 1000 сек или 15 минут примерно.
                                ; Это делает такой метод приемлемым для простого взлома так сказать
                                ; но я это пока не имплементирую тк не время ещё
                                ; ну и тут ещё рандом может ролять но это мне влом прикидывать

                lea si, buffer
                mov bx, mult
                xor ax, ax
                mov cx, 127d
@@Next:         cmp byte ptr [si], 0dH
                je @@break
                mul bx
                add al, byte ptr [si]
                add si, 2
                loop @@Next

@@break:        lea si, buffer
                add si, 250d
                cmp [si], 0C0FEh
                jne @@buffOverflow
                add si, 2d
                cmp [si], 4DEDh
                jne @@buffOverflow
                jmp @@nobuffOverfl
@@buffOverflow: call NoAccess
@@nobuffOverfl: 


                ret
                endp

; ------------------------------
; access granted lol
; ------------------------------
; Needs : none

; Exit : access

; Expects : none

; Destroys : ax, bx, es, cx, si
; ==============================

Access          proc

                mov bx, 0b800h
                mov es, bx
                mov bx, framePos + 1
                mov ax, 0afh * 100h
                mov cl, frameLenX + 1
                mov ch, frameLenY + 1
                mov dl, frameLenX + 2

@@FillStyle:    mov byte ptr es:[bx], ah
                add bx, 2d
                cmp cl, 0
                ja @@skipNL
                add bx, 160d - 2 * (frameLenX + 2)
                dec ch
                mov cl, dl
@@skipNL:       loop @@FillStyle

                mov byte ptr es:[bx], ah

                mov bx, framePos + 162d
                lea si, cool
                xor ax, ax

@@PrintText:    cmp byte ptr [si], '$'
                je @@stopPrint

                mov al, byte ptr [si]
                inc si
                mov byte ptr es:[bx], al
                add bx, 2d
                jmp @@PrintText

@@stopPrint:    EXIT

                ret
                endp

; -------------------------------
; un-grants access
; -------------------------------
; Needs : none

; Exit : no access

; Destroys : ax, bx, cx, es, si

; Expects : none
; ===============================

NoAccess        proc

                mov bx, 0b800h
                mov es, bx
                mov bx, framePos + 1d
                mov ax, 0cfh * 100h
                mov cl, frameLenX + 1
                mov ch, frameLenY + 1
                mov dl, frameLenX + 2

@@FillStyle:    mov byte ptr es:[bx], ah
                add bx, 2d
                cmp cl, 0
                ja @@skipNL
                add bx, 160d - 2 * (frameLenX + 2)
                dec ch
                mov cl, dl
@@skipNL:       loop @@FillStyle

                mov byte ptr es:[bx], ah

                mov bx, framePos + 162d
                lea si, nocool
                xor ax, ax

@@PrintText:    cmp byte ptr [si], '$'
                je @@stopPrint

                mov al, byte ptr [si]
                inc si
                mov byte ptr es:[bx], al
                add bx, 2d
                jmp @@PrintText

@@stopPrint:    EXIT

                ret
                endp

; ------------------------------------
; Gets singel byte char into al
; ------------------------------------
; Expects : none

; Exit : char in al

; Needs : none
; ====================================
GetChar         proc

                endp

; -------------------------------------
; Draws the frame and its contents
; -------------------------------------
; Expects : none

; Exit : none

; Needs : none

; Destroys : ax, bx, cx, dx, si
; =====================================
DrawFrame	proc

                mov bx, framePos        ; LTA pos

                mov cx, frameLenX       ; x length to ocunter
		mov es:[bx], LTA        ; draw LTA
		add bx, 2d              ; delta right
                mov ax, XLine           ; mov XLine to ax for faster drawing

		call DrawX		; Draw upper horizontal line

		mov es:[bx], RTA	; Draw top right angle

                mov bx, framePos + (frameLenY + 1) * 160d; set es:[bx] to LBA
		mov es:[bx], LBA 	; Bottom left angle
		add bx, 2d

		mov ax, XLine		; Set horizontal line symbol
                mov cx, frameLenX       ; resets counter to len x

		call DrawX		; Draw low horizontal line

		mov es:[bx], RBA	; Bottom right angle

                mov bx, framePos + 160d        ; reset draw pos + 1 row down
		mov ax, YLine	        ; Set current draw symbol to vert line
                mov cx, frameLenY       ; set counter to len y

		call DrawY		; draw left line

                mov bx, framePos + 2 * (frameLenX + 1) + 160d
                mov ax, YLine
                mov cx, frameLenY

                call DrawY

                mov bx, framePos + 162d
                mov cl, frameLenX - 1
                mov ch, frameLenY - 1
                xor ax, ax
                mov ah, FillStyle
                mov dl, frameLenX

@@FillStyle:    mov es:[bx], ax
                add bx, 2d
                cmp cl, 0
                ja @@skipNL
                add bx, 160d - 2 * frameLenX
                dec ch
                mov cl, dl
@@skipNL:       loop @@FillStyle

                mov es:[bx], ax

                mov bx, framePos + 162d
                lea si, msg

@@PrintText:    cmp byte ptr [si], '$'
                je @@stopPrint

                mov al, byte ptr [si]
                inc si
                mov byte ptr es:[bx], al
                add bx, 2d
                jmp @@PrintText

@@stopPrint:    xor cx, cx
                mov cx, frameLenX
                mov bx, framePos + 2d + frameLenY * 160d
                mov ax, 87dbh

@@fillField:    mov es:[bx], ax
                add bx, 2d
                loop @@fillField

                ret
                endp
; -------------------------------------
; Clears the screen
; -------------------------------------
; Expects : none

; Exit : none

; Destroys : bx, ax, cx, es
; ------------------------------------

ClearScr	proc

                mov bx, 0b800h
                mov es, bx
		mov al, 0dbh
		xor ah, ah
		xor bx, bx
		mov cx, 80d * 23d
@@Next:		mov es:[bx], ax
		add bx, 2
		loop @@Next

                xor ax, ax

		ret
		endp
; -------------------------------------

; -------------------------------------
; Draws horizontal line
; -------------------------------------
; Expects : ES -> video seg

; Exit : es:[bx] - position after the last drawn symbol

; Needs : BX (starting pos), AX (symbol to draw), CX (count)

; Destroys : cx
; -------------------------------------

DrawX		proc
@@Next:		mov es:[bx], ax
		add bx, 2d
		loop @@Next
		ret

		endp


; -------------------------------------
; Draws vertical line
; -------------------------------------
; Expects : ES -> video seg

; Exit : bx - position after the last drawn symbol

; Needs : BX (starting pos), AX (symbol to draw), CX (count)

; Destroys : cx
; -------------------------------------

DrawY		proc
@@Next:		mov es:[bx], ax
		add bx, 160d
		loop @@Next
		ret

		endp


.data

cool    db      '   Access Granted$'
nocool  db      '  Access Un-Granted$'
buffer  dw      125 dup (0DEDh), 0C0FEh, 4DEDh
msg     db      'Insert password:$'

end             start