EXTERNDELAY = 3

.MODEL SMALL
.STACK 100H
.DATA

   ; score dw 0
    scor_msg db "Score : $"


    WLCM      DB  0AH,0DH,"                                                  "
              DB  0AH,0DH,"                                                  "
              DB  0AH,0DH,"                             WELCOME TO           "
              DB  0AH,0DH,"                                                  "
              DB  0AH,0DH,"                                                  "
              DB  0AH,0DH,"                            BRICK BREAKER         "
              DB  0AH,0DH,"                                                  "
              DB  0AH,0DH,"                                                 $"

    str2      DB  0AH,0DH,"                                                                  "
              DB  0AH,0DH,"                                                                  "
              DB  0AH,0DH,"         Designed BY:                                             "
              DB  0AH,0DH,"                                                                  "
              DB  0AH,0DH,"                                                                  "
              DB  0AH,0DH,"              Awais Naeem                                         "
              DB  0AH,0DH,"                                                                  "
              DB  0AH,0DH,"          All rights reserved@2022                                "
              DB  0AH,0DH,"                                                                  "
              DB  0AH,0DH,"                                                                 $"


    str3      DB  0AH,0DH,"                                                                       "
              DB  0AH,0DH,"                                                                       "
              DB  0AH,0DH," IT IS A SINGLE PLAYER GAME. YOU HAVE THREE LEVELS WHICH MEANS YOU     "
              DB  0AH,0DH,"                                                                       "
              DB  0AH,0DH,"    MUST HAVE TO COMPLETE 3 LEVELS FOR CHANCE OF WINNING BUT AT THE    "
              DB  0AH,0DH,"                                                                       "
              DB  0AH,0DH,"     SAME TIME HAVE THE SAME CHANCE OF LOSING. IF YOU LOSE 3 LIVES     "
              DB  0AH,0DH,"                                                                       "
              DB  0AH,0DH,"                        OF YOUR BALLS THEN                             "
              DB  0AH,0DH,"                                                                       "
              DB  0AH,0DH,"                             GAME OVER :(                              "
              DB  0AH,0DH,"                                                                       $"

    str5      DB  0AH,0DH,"                                                                   "
              DB  0AH,0DH,"                           CONTROLS                                "
              DB  0AH,0DH,"                                                                   "
              DB  0AH,0DH,0AH,0DH
              DB  0AH,0DH,"                                                                   "
              DB  0AH,0DH,"   USE THE LEFT AND RIGHT ARROW KEYS TO MOVE THE BAR FROM IT'S     "
              DB  0AH,0DH,"                                                                   "
              DB  0AH,0DH,"         CURRENT POSITION AND SPACEBAR TO SHOOT THE BALL           "
              DB  0AH,0DH,"                                                                  $"


    str6      DB 0AH,0DH,"                           YOU WIN:}        "
              DB  0AH,0DH,"                                           "
              DB 0AH,0DH,"                          THANK YOU         "
              DB 0AH,0DH,"                             FOR            "
              DB 0AH,0DH,"                       Wasting Yr Time      "
              DB  0AH,0DH,"                                           "
              DB  0AH,0DH,"                                           $"

    LOSE_MSG  DB  0AH,0DH,"                                            "
              DB  0AH,0DH,"                                            "
              DB 0AH,0DH,"                           YOU LOSE          "
              DB 0AH,0DH,"                     BETTER LUCK NEXT TIME   "
              DB  0AH,0DH,"                                            $"

    ITEM0     DB  0AH,0DH,"                                            "
              DB  0AH,0DH,"                                            ",0AH,0DH,'$'
    ITEM1     DB 'NEW GAME      ',0AH,0DH,'$'
    ITEM2     DB 'DESIGNED BY  ',0AH,0DH,'$'
    ITEM3     DB 'ABOUT         ',0AH,0DH,'$'
    ITEM4     DB 'HIGHSCORES    ',0AH,0DH,'$'
    ITEM5     DB 'INSTRUCTIONS  ',0AH,0DH,'$'
    ITEM6     DB 'QUIT GAME     ',0AH,0DH,'$'
    ITEM7     DB "                                             "
              DB 0AH,0DH,"                                              $"

    SELECTED DW 0     ; currenlty selected menu item.
    MENU_COUNT DW 6   ; number of items in menu.
    UP1      EQU     48h
    DOWN1    EQU     50h

    ; all menu items should be
    ; entered in this address array:
    ITEMS_ADR DW ITEM1, ITEM2,ITEM3, ITEM4,ITEM5, ITEM6,ITEM7

    SELECTOR        DB '                       >  $'
    EMPTY_SELECTOR  DB '                           $'

    ;MAIN GAME
    entername db "Please Enter your Name$"
 ;nae db 'brick$'
 buff        db  26        ;MAX NUMBER OF CHARACTERS ALLOWED (25).
            db  ?         ;NUMBER OF CHARACTERS ENTERED BY USER.
            db  26 dup(0)
score db '     Score: $'
scoreCount dw 0

lives db '   Lives: '
livesCount dw 03,03,03

ending db ' $'
count dw 04
level db 49
count1 db 51

ballY dw 163
ballX dw 158
ballLeft db 1
ballUp db 0

color db 3

startx dw ?
starty dw ?

endx dw ?
endy dw ?

begin db 0

ballspeed dw 2

strikerX dw 140 ;bat
strikerY dw 170

innerDelay db 0

boundaryEnd dw 250
boundaryStart dw 30

levelshow db 'Level  '
levelshow1 db 49
levelending db '$'

bool db 48

county dw 48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48
countx dw 48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48
;county db 48

batsize dw 40

brick1x dw 45,85,125,165,205,245,45,85,125,165,205,245,45,85,125,165,205,245;idhr udhr
brick1y dw 30,30,30,30,30,30,50,50,50,50,50,50,70,70,70,70,70,70;upar nechay
brick2x dw 45,85,125,165,205,245,45,85,125,165,205,245,45,85,125,165,205,245;idhr udhr
brick2y dw 25,25,25,25,25,25,45,45,45,45,45,45,65,65,65,65,65,65;upar nechay
;brick2y dw 30,30,30,30,30,30,50,50,50,50,50,50,70,70,70,70,70,70;upar nechay

; dir1 db "c:\test1", 0
;dir2 db "test2", 0
dir3 db "newname", 0

file1 db "c:\test1\file1.txt", 0
file2 db "c:\test1\newfile.txt", 0
file3 db "t1.txt", 0

handle dw ?

;text db "lazy dog jumps over red fox."
text_size = $ - offset buff+2
;text2 db "h!"
text2_size = $ - offset scoreCount

.CODE
redrawStriker macro visColor

mov color, visColor
call drawStriker
endm

redrawBall macro visColor

    mov color, visColor
    call drawball
endm

BuildBrick macro  A, B
    push ax
    push bx
    mov ax, A
    mov bx, B
    call AddBrick
    pop bx
    pop ax
endm

DestroyBrick macro  A, B
    push ax
    push bx
    mov ax, A
    mov bx, B
    call RemoveBrick
    call beep
    inc scoreCount
    call DrawLivesScores
    pop bx
    pop ax
endm

BrickCollision MACRO X,Y
local copper
    push ax
    push bx
    push cx
    push dx
    push si
    mov ax, ballY
    mov bx, ballX
    mov cx, X
    mov dx, Y
   add dx,7
 ;  sub cx ,7

    cmp dx, ballY
    jl copper
   sub dx, 7 ;BALL RETURN
    cmp ballY, dx
    jl copper

    mov dx, X

    cmp ballX, dx
    jl copper
    add dx, 30;BALL RETURN
    cmp dx, ballX
    jl copper



;call check
call check1



    copper:
    mov bool,48
     pop si
    pop dx
    pop cx
    pop bx
    pop ax


endm
checklevel proc



checklevel endp




MAIN PROC

MOV AX,@DATA
MOV DS,AX
            mov dx,offset entername
            mov ah,9
            int 21h
        mov dx, 10     ;Instructions to take first input on next line to string
mov ah, 2
int 21h
            mov ah, 0Ah ;SERVICE TO CAPTURE STRING FROM KEYBOARD.
            mov dx, offset buff
            int 21h

;CHANGE CHR(13) BY '$'.
            mov si, offset buff + 1 ;NUMBER OF CHARACTERS ENTERED.
            mov cl, [ si ] ;MOVE LENGTH TO CL.
            mov ch, 0      ;CLEAR CH TO USE CX.
            inc cx ;TO REACH CHR(13).
            add si, cx ;NOW SI POINTS TO CHR(13).
            mov al, '$'
            mov [ si ], al ;REPLACE CHR(13) BY '$'.

    CALL WELCOME_SCR
    CALL MAIN_MENU
    CALL SET_DISPLAY_MODE

    MOV AH,0CH
                 CHK_MENU:
                    CMP AH,01
                  ;  JNE SHOW
                       ; CALL SCORE
                        MENU_SHOW:
                            CALL MAIN_MENU
                          ;  JMP SHOW
    END_GAME:
        MOV AH,4CH
        INT 21H
MAIN ENDP


DELAYER PROC
    PUSH BX

        MOV BX,199H
        DELAYER_LOOP:
                    DEC BX
                    JNZ DELAYER_LOOP
    POP BX
    RET
    DELAYER ENDP


SET_DISPLAY_MODE    PROC
    MOV AH,0
    MOV AL,13H
    INT 10H


    MOV AH,0CH
    INT 10H
    RET
SET_DISPLAY_MODE ENDP








MAIN_menu PROC
    PUSH    AX      ; store registers...
    PUSH    BX      ;
    PUSH    CX      ;
    PUSH    DX      ;



    START:
        PRINT_MENU:

            CALL CLEAR_SCREEN
            MOV AH,9
            LEA DX,ITEM0
            INT 21H

            MOV BX, 0
            MOV CX, MENU_COUNT
            PRINT_ITEMS:
                MOV AX,BX
                SHR AX, 1   ; DIVIDE BY 2.
                CMP AX, SELECTED
                JNE NOT_SEL

                LEA DX, SELECTOR
                MOV AH, 09H
                INT 21H

                JMP CONT
            NOT_SEL:
                LEA DX, EMPTY_SELECTOR
                MOV AH, 09H
                INT 21H
            CONT:
                MOV DX, ITEMS_ADR[BX]
                MOV AH, 09H
                INT 21H

                ADD BX, 2  ; NEXT ITEM.
            LOOP PRINT_ITEMS



            MOV AH,9
            LEA DX,ITEM7
            INT 21H


            CHECK_FOR_KEY:

            MOV     AH,01H
            INT     16H
            JZ      NO_KEY

            MOV     AH,00H
            INT     16H

            CMP     AL, 1BH    ; ESC - KEY?
            JE      NOT_ENTER  ;

            CMP     AH, UP1
            JNE     NOT_UP
            SUB     SELECTED, 1
            NOT_UP:

            CMP     AH, DOWN1
            JNE     NOT_DOWN
            ADD     SELECTED, 1
            NOT_DOWN:


            ; enter pressed?
            CMP    AH, 1CH
            JNE    NOT_ENTER
                CMP SELECTED, 0
                JNE NOT_ITEM_0
                CALL CLEAR_SCREEN
                JMP START_GAME
            NOT_ITEM_0:
                CMP SELECTED, 1
                JNE NOT_ITEM_1
                CALL CLEAR_SCREEN
                LEA DX, STR2
                MOV AH, 9
                INT 21H
                MOV AH,1
                INT 21H
                JMP START
            NOT_ITEM_1:
                CMP SELECTED, 2
                JNE NOT_ITEM_2
                CALL CLEAR_SCREEN
                LEA DX, STR3
                MOV AH, 9
                INT 21H
                MOV AH,1
                INT 21H
                JMP START
            NOT_ITEM_2:
                CMP SELECTED,3
                JNE NOT_ITEM_3
                CALL CLEAR_SCREEN
                CALL SCORE_DISP
                MOV AH,1
                INT 21H
                JMP START
            NOT_ITEM_3:
                CMP SELECTED, 4
                JNE NOT_ITEM_4
                CALL CLEAR_SCREEN
                LEA DX, STR5
                MOV AH, 9
                INT 21H
                MOV AH,1
                INT 21H
                JMP START
            NOT_ITEM_4:
                CMP SELECTED,5
                JNE NOT_ITEM_5
                CALL CLEAR_SCREEN
                LEA DX, STR6
                MOV AH, 9
                INT 21H
                MOV AH,1
                INT 21H
                JMP stop_prog
            NOT_ITEM_5:
                ; WAIT FOR KEY:
                MOV    AH, 00
                INT    16H
            NOT_ENTER:



            ; CHECK IF OUT OF MENU BOUNDS:
            CMP    SELECTED, -1
            JNLE    OK1
            MOV    SELECTED, 0
            OK1:
                MOV AX, MENU_COUNT
                CMP    SELECTED, AX
                JNGE OK2
                DEC    AX
                MOV    SELECTED, AX
            OK2:
                JMP PRINT_MENU

            NO_KEY:
                JMP CHECK_FOR_KEY


        STOP_PROG:
            MOV AH,4CH
            INT 21H
    START_GAME:
   call setVideoMode

    call drawBoundary
    mov si,0
     mov cx,18
     l1:
     BuildBrick brick1x[si], brick1y[si]
     add si,2
     loop l1
    redrawStriker 10
    redrawBall 7
    call DrawLivesScores

    call gameLoop

    mov ah,4ch
    int 21h
MAIN_MENU ENDP
filehandle proc
mov ah, 3ch
mov cx, 0
mov dx, offset file3
int 21h
jc err2
mov handle, ax
; seek:
mov ah, 42h
mov bx, handle
mov al, 0
mov cx, 0
mov dx, 10
int 21h
; write to file:
mov ah, 40h
mov bx, handle
mov dx, offset buff+2
mov cx, text_size
int 21h
; seek:
mov ah, 42h
mov bx, handle
mov al, 0
mov cx, 0
mov dx, 2
int 21h
; write to file:
mov ah, 40h
mov bx, handle
mov dx, offset scoreCount
mov cx, text2_size
int 21h
; close c:\emu8086\MyBuild\t1.txt
mov ah, 3eh
mov bx, handle
int 21h
err2:
;nop
ret
filehandle endp


WELCOME_SCR PROC
    PUSH    AX      ; store registers...
    PUSH    BX      ;
    PUSH    CX      ;
    PUSH    DX      ;

        CALL CLEAR_SCREEN

        LEA DX,WLCM
        MOV AH,9
        INT 21H

        MOV AH,1
        INT 21H

    POP     DX      ; re-store registers...
    POP     CX      ;
    POP     BX      ;
    POP     AX      ;

RET
WELCOME_SCR ENDP





YOU_LOSE PROC
    PUSH    AX      ; store registers...
    PUSH    BX      ;
    PUSH    CX      ;
    PUSH    DX      ;

        MOV AH,0
        MOV AL,2
        INT 10H
        call SCORE_DISP
        CALL CLEAR_SCREEN
        MOV AH,9
        LEA DX,LOSE_MSG
        INT 21H

        MOV AH,1
        INT 21H

    POP     DX      ; re-store registers...
    POP     CX      ;
    POP     BX      ;
    POP     AX      ;

RET
YOU_LOSE ENDP




CLEAR_SCREEN PROC
        PUSH    AX      ; store registers...
        PUSH    DS      ;
        PUSH    BX      ;
        PUSH    CX      ;
        PUSH    DI      ;

        MOV     AX, 40h
        MOV     DS, AX  ; for getting screen parameters.
        MOV     AH, 06h ; scroll up function id.
        MOV     AL, 0   ; scroll all lines!
        MOV     BH, 07  ; attribute for new lines.
        MOV     CH, 0   ; upper row.
        MOV     CL, 0   ; upper col.
        MOV     DI, 84h ; rows on screen -1,
        MOV     DH, [DI] ; lower row (byte).
        MOV     DI, 4Ah ; columns on screen,
        MOV     DL, [DI]
        DEC     DL      ; lower col.
        INT     10h

        ; set cursor position to top
        ; of the screen:
        MOV     BH, 0   ; current page.
        MOV     DL, 0   ; col.
        MOV     DH, 0   ; row.
        MOV     AH, 02
        INT     10h

        POP     DI      ; re-store registers...
        POP     CX      ;
        POP     BX      ;
        POP     DS      ;
        POP     AX      ;

        RET
CLEAR_SCREEN ENDP


SCORE_DISP PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    call filehandle
        CALL CLEAR_SCREEN

        MOV AH,9
        LEA DX,SCOR_MSG
        INT 21H

        MOV Ax,scoreCount
        MOV BX,10D
        XOR CX,CX
        SAV_DEC:
            XOR DX,DX
            DIV BX
            INC CX
            PUSH DX
            OR AX,AX
            JNZ SAV_DEC

            MOV AH,2

        OUT_DEC:
            POP DX
            ADD DX,30H
            INT 21H
            LOOP OUT_DEC
       MOV AH,1
       INT 21H
    POP DX
    POP CX
    POP BX
    POP AX
    RET
SCORE_DISP ENDP

proj proc
    cmp countx[si],49
    je l9
    BuildBrick brick2x[si] ,brick2y[si]
    mov ax,brick2x[si]
    mov brick1x[si],ax
    mov ax,brick2y[si]
    mov brick1y[si],ax
    inc countx[si]
    l9:
    ret
    proj endp

check1 proc
cmp bool,48
jne return30
;jmp copper
l20:
call switcher
 DestroyBrick brick1x[si], brick1y[si]
    add  brick1y[si],300
    call proj2
    mov bool,48
return30:
ret
check1 endp
check proc
cmp levelshow1,50
jne l30
mov dx,97
cmp dx, ballY
    jl l30
   sub dx, 7 ;BALL RETURN
    cmp ballY, dx
    jl l30
    mov dx, 45
    cmp ballX, dx
    jl l30
    add dx, 30;BALL RETURN
    cmp dx, ballX
    jl l30
    mov bool,49

call switcher

l30:
ret
check endp
 proj3 proc
    cmp county[si],50
    je l12
    BuildBrick brick2x[si], brick2y[si]
    mov ax,brick2x[si]
    mov brick1x[si],ax
    mov ax,brick2y[si]
    mov brick1y[si],ax
    add brick2x[si],5
    add brick2y[si],5
    inc county[si]
    l12:
    ret
    proj3 endp
proj1 proc
  mov si,0
     mov cx,18
     l11:
     add brick2y[si],5
     BuildBrick brick2x[si], brick2y[si]
    mov ax,brick2x[si]
    mov brick1x[si],ax
    mov ax,brick2y[si]
    mov brick1y[si],ax
    sub brick2y[si],5
     add si,2
     loop l11
      redrawBall 0
   mov ballY, 163
mov ballX, 158-4
call baller
    redrawBall 3
 sub batsize,10
 redrawStriker 10
  add ballspeed,1
    inc levelshow1
    cmp levelshow1,51
    jne return3
    BuildBrick 45, 90
    BuildBrick 245 ,90
    return3:
ret
proj1 endp

proj2 proc
   cmp scoreCount, 18
    jl return
    je copper1
    cmp scoreCount,54
    jl return1
    je copper2
    cmp scoreCount ,108
    jl return2
    call endgame
    mov ah,4ch
    int 21h
copper1:
call proj1
jmp return
copper2:
call proj1
jmp return
return:
ret
return1:
call proj
ret
return2:
call proj3
ret
proj2 endp
endgame proc
                call SCORE_DISP
                CALL CLEAR_SCREEN
                LEA DX, STR6
                MOV AH, 9
                INT 21H
                MOV AH,1
                INT 21H
endgame endp


DrawLivesScores proc
    push dx
    push ax

    mov dh, 1 ;row
    mov dl, 3 ;col
    mov ah, 2
    int 10h
    mov dx, offset buff + 2      ; lea dx, nae
    mov ah, 9
    int 21h
    lea dx, score
    mov ah, 9
    int 21h

    call printScore
    lea dx,lives
    mov ah,9
   int 21h
 mov dh, 15 ;row
    mov dl, 15 ;col
    mov ah, 2
    int 10h
 lea dx, levelshow
    mov ah, 9
    int 21h
    pop ax
    pop dx
    ret
    DrawLivesScores endp

printScore proc
    push ax
    push bx
    push cx
    push dx

    mov cx,0

    mov ax,scoreCount
    ll:
    mov bx,10
    mov dx,0
    div bx
    push dx
    inc cx
    cmp ax,0
    jne ll

    l2:
    pop dx
    mov ah,2
    add dl,'0'
    int 21h
    loop l2

    pop dx
    pop cx
    pop bx
    pop ax

    ret
    printScore endp

sleep proc

mov cx,111111111111111b

l:
loop l
ret
sleep endp

drawball proc
    push bx
    mov bx, ballX
    mov startx, bx
    add bx, 4
    mov endx,   bx
    mov bx, ballY
    mov starty, bx
    add bx, 4
    mov endy,   bx

    pop bx

    call draw
ret
drawball endp

CollisionStriker proc
    push ax
    push bx
    push cx
    push dx

    mov dx, ballY
    cmp dx, 165 ; striker surface check
    jl bhaag
    cmp dx, 170 ; striker missed
    jg fail



    mov cx,strikerX
    mov ax, ballX
    cmp ax, cx
    jl bhaag
    add cx , 40
    cmp ax, cx
    jg bhaag

    mov ballUp, 1
    jmp bhaag


    fail:
    mov begin,0
    mov si,count
    mov livesCount[si],0
    sub count,2
    dec count1
    cmp count1,48
    je khatam
    push ax
    push bx
    push cx
    push dx


    redrawBall 0

    mov ax, strikerX
    mov ballX,ax
    add ballX,18

    mov ballY,  163

    redrawBall 3
    mov ballUp, 1     ;monis
    mov ballLeft,0



    pop dx
    pop cx
    pop bx
    pop ax

    call DrawLivesScores
    jmp bhaag



    khatam:
    ;call DrawLivesScores
    call YOU_LOSE
    mov ah,4ch
    int 21h

    bhaag:

    pop dx
    pop cx
    pop bx
    pop ax
    ret
    CollisionStriker endp


switcher:
    cmp ballUp, 0
    je DownT
    jne UpT
    UpT:
   inc ballUp
    ret
    DownT:
   dec ballUp
    ret

AddBrick proc
    push ax
    push bx
    mov startx, ax
    add color,9
    mov ax, bx
    mov bx, startx

    add bx, 30

    mov endx,bx

    mov starty, ax

    mov bx,starty

    add bx,7
    mov endy,bx

    call draw
    pop bx
    pop ax
    ret
    AddBrick endp

RemoveBrick proc

    push ax
    push bx
    push cx
    push dx

    mov startx, ax
    mov color, 0
    mov ax, bx
    mov bx, startx

    add bx, 30

    mov endx,bx

    mov starty, ax

    mov bx,starty

    add bx,7
    mov endy,bx

    call draw

    pop dx
    pop cx
    pop bx
    pop ax
    ret
    RemoveBrick endp

Collisionwall proc

    mov bx, ballX
    mov cx, ballY

    checkLeftRight:
    cmp bx, 25; max left
    jl goRight
    cmp bx, 290; Max Right
    jg goLeft
    jmp checkUpDown
    goRight:
    mov ballLeft, 0
    jmp checkUpDown;
    goLeft:
    mov ballLeft, 1
    checkUpDown:

    cmp cx, 24;max top
    jl goDown
    cmp cx, 184;max bottom
    jg goUp

    jmp noInput1
    goUp:
    mov ballUp,1
    jmp noInput1
    goDown:
    mov ballUp, 0

    noInput1:
    ret
    Collisionwall endp

gameOver proc

    mov ah,4ch
    int 21h
    gameOver endp

ajeebse:
ret
baller proc

	inc innerDelay
	cmp innerDelay, EXTERNDELAY
	jne ajeebse
	mov innerDelay, 0
    redrawBall 0

	mov bx,ballX
	cmp ballLeft, 1
	je Left
	jne Right

	Left:
	sub bx,  ballspeed
	jmp P2;
	Right:
	add bx, ballspeed

	P2:
	mov ballX,  bx
	mov bx, ballY
	cmp ballUp, 1
	je Up
	jne Down
	Up:
    sub bx, ballspeed
	jmp P3
	Down:
    add bx, ballspeed
	P3:
    mov ballY,  bx

    redrawBall 3

ret
baller endp


repeat1:
gameLoop:
   CALL    checkKeyboard
   cmp begin,1
   jne repeat1



   call Collisionwall
   call CollisionStriker
     mov si,0
     mov cx,18
     l3:
     BrickCollision brick1x[si],brick1y[si]
     add si,2
     loop l3

   CALL baller
   CALL sleep
   JMP     gameLoop

exit:
    mov ah, 4ch
    int 21h

checkKeyboard proc
    mov     ah,     1h
    int     16h         ; check keypress
    jz      noInput     ; no keypress
    mov     ah,     0h
    int     16h
    cmp     ax,     4D00h
    je      rightKey
    cmp     ax,     4B00h
    je      leftKey
    cmp     al,     27D
    je      exit
    cmp     ax,     3920h ;space to begin
    je      beg
    jne     noInput

    beg:
    mov begin,1

    noInput:
    ret

    rightKey:
    mov bx, boundaryEnd
    cmp     strikerX, bx ;max right limit
    jg      noInput
    redrawStriker 0
    add     strikerX, 5
    redrawStriker 10
    cmp begin,0
    jz moveBallRight
    jmp     noInput

    leftKey:
    mov bx, boundaryStart
    cmp     strikerX, bx ;max left limit
    jl      noInput
    redrawStriker 0
    sub     strikerX, 5
    redrawStriker 10
    cmp begin,0
    jz moveBallLeft
    jmp     noInput


    moveBallLeft:
    redrawBall 0
    sub     ballX, 5
    redrawBall 3
    jmp     noInput


    moveBallRight:
    redrawBall 0
    add    ballX, 5
    redrawBall 3
    jmp     noInput

checkKeyboard endp

draw proc
    push ax
    push cx
    push dx

    mov dx,starty
    mov cx,startx
    mov ah,0ch
    mov al,color
    c3:
    inc cx
    int 10h
    cmp cx,endx
    jne c3

    mov cx,startx
    inc dx
    cmp dx,endy
    jne c3

    pop dx
    pop cx
    pop ax
    ret
draw endp

drawStriker proc;bat
    push bx
    push cx

    mov bx, strikerX
    mov cx, strikerY
    mov startx,bx
    add bx, batsize ;Bat size
    mov endx,bx
    mov starty,cx
    mov endy,175
    call draw

    pop cx
    pop bx
    ret
    drawStriker endp

drawBoundary proc
    mov color,21
    ;------TOP------------
    mov startx,20
    mov endx,300
    mov starty,20
    mov endy,23
    call draw
    ;------RIGHT------------
    mov startx,297
    mov endx,300
    mov starty,7
    mov endy,180
    call draw
    ;------LEFT------------
    mov startx,20
    mov endx,23
    mov starty,7
    mov endy,180
    call draw
    ;------BOTTOM------------
    mov startx,20;start row
   mov endx,300;end row
    mov starty,177
    mov endy,180
    call draw

    ret
    drawBoundary endp


setVideoMode proc

    mov ah, 0   ; set display mode function.
    mov al, 13h ; mode 13h = 320x200 pixels, 256 colors.
    int 10h

    ret
    setVideoMode endp

beep proc
        push ax
        push bx
        push cx
        push dx
        mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
        mov     ax, 5000      ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 2          ; Pause for duration of note.
pause1:
        mov     cx, 65535
pause2:
        dec     cx
        jne     pause2
        dec     bx
        jne     pause1
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.

        pop dx
        pop cx
        pop bx
        pop ax

ret
beep endp

END MAIN

