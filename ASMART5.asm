;ASCII art
;Graficzne przedsawienie RSA 1024

.186					;dla pusha, popa 

data1 segment
       tab_count    db  0
       tab_offset   dw  128 dup(?)                       ;random
       tab_arg      db  300 dup('$')					;tablica z parsera. Jest tutaj niezmieniony, sparsowany input
       tab_converted_arg db 16 dup(?)
       tab_wart     db  153 dup(0)						;tutaj przechowywane sa finalne wartosci wypisywane na ekran
       enter1        db  13,10,"$"
       tab_len  dw 150 dup(?)     ;dl argumentow
       ascii_symbols    db  " ",".","o","+","=","*","B","O","X","@","%","&","#","/","^"
       ;line        db "+", 17 dup("-"), "+", 13 , 10, "$" ;linia start/koniec
       not_good_args db "Argumenty nie maja zakladanej dlugosci","$"
       not_enough_args  db "Brak wystarczajacej ilosci argumentow","$"
       no_args      db  "Brak argumentow",13,10,"$"
       _end     db  "End of graphics",13,10,"$"
	   f_too_long db "Pierwszy argument ma dlugosc inna niz 1",13,10,"$"
	   pos_y db 8 ;aby pozniej nie trzeba bylo sie bawic z dzieleniem itp
	   flag_for_first_carry db 0						;Carry flag dla 1 pary. 
	   bad_first db "Bledny pierwszy argument, powinno byc 0 lub 1"
	   border_u db "+---[RSA 1024]----+$"	; top 
		border_d db "+-----------------+$"	; bottom
data1 ends

code1 segment
start:

    mov ax , seg wstosu       ;inicjalizacja stosu podobna do tego co robilismy na wykladach
    mov ss , ax
    mov sp , offset wstosu
    call into_array               ;segment parsera. Na tym etapie elementy sa tylko i wylacznie dodane do tablicy (nie sa sprawdzone)
    mov ax , data1                ;na cały program
    mov ds , ax
    call check_args               ;tutaj sprawdzam jedynie dlugosc oraz to czy sa 2 oraz czy dlugosc jest ok oraz czy 1 jest ok
    call compare1
    call print_ascii
    
kill_program:
    mov   ah, 4ch
    int   21h
    ret
;-------------------------Obsluga bledu------------------------------------------
check_args proc
    mov cl,ds:[tab_count]		;sprawdzenie czy dostalismy 2 arg
    cmp cl , 2
    jne error_count
	cmp ds:[tab_arg+1],"$"		;czy po 1 jest spacja?
	jne first_too_long
    cmp ds:[tab_len+2] , 32		;oraz czy sa poprawnej dlugosci
    jne is_not_valid			;jak nie to error 
    cmp ds:[tab_arg],"1"		;czy to są argumenty pokroju 0/1?
    ja bad_first_arg
    ret
error_count:
    mov ax, seg not_enough_args
    mov dx,ax
    mov dx, offset not_enough_args
    mov ah,9h
    int 21h
    call kill_program
first_too_long:
    mov ax, seg f_too_long
    mov dx,ax
    mov dx, offset f_too_long
    mov ah,9h
    int 21h
    call kill_program
bad_first_arg:
    mov ax, seg bad_first
    mov dx,ax
    mov dx, offset bad_first
    mov ah,9h
    int 21h
    call kill_program	
check_args endp
;-------------------------------------------------------------------------------------
is_not_valid proc
   mov ax,seg not_good_args      ;blad zwiazany z brakiem odpowiednich argumentow
   mov ds,ax
   mov dx,offset not_good_args
   mov ah,9h
   int 21h
   call kill_program
is_not_valid endp;

;-------------------------------------
check_if_valid proc      ;zgodnie z trescia zadania znaki duze nie sa poprawne.
  cmp ah , '0'
  jb  is_not_valid      ;jump if bellow
  cmp	ah, '9'
  jna is_number ;jump if not above ---> daje 0<=ah<=9 cyfry
  cmp	ah, 'f'
  ja  is_not_valid      ;jump if above
  cmp ah , 'a'
  jnb is_letter  ;jump if not bellow --> daje a<=ah<=f litery (male)
is_number:
    sub ah , '0'
    ret
is_letter:
    sub ah , 'a'
    add ah , 10
    ret
check_if_valid endp
;------------------------Czesc wyswietlajaca------------------------------
print_bottom proc
   mov dx,offset border_d
   mov ah,9h
   int 21h
   call kill_program
   ret
print_bottom endp
;-------------------------------------------------------------------------
print_up proc
   mov dx,offset border_u
   mov ah,9h
   int 21h
   mov dx, offset enter1
   int 21h
   ret
print_up endp
;--------------------------------------------------------------------------------
print_ascii proc
   mov ch , 9                           
   mov di , 0
   mov dx , 0
   call print_up
Print_loop_1:
  mov cl , 17
;przerwanie 2h->print charow.
Print_loop_2:
  cmp di , si ; koniec
  je print_E
  cmp byte ptr ds:[tab_wart + di] , 14
  ja above_14
  cmp di , 76                             ;czy to nie jest poczatkowa/koncowa pozycja?
  je print_S
  mov bl , byte ptr ds:[tab_wart + di]
  mov dl , byte ptr ds:[ascii_symbols + bx ]   ;ascii[tab_wart]
  mov ah , 2h
  int 21h
  go_back_in_print_loop:
  inc di                                      ;go next
  dec cl
  cmp cl , 0
  jne print_loop_2                            ;next line
  mov dx ,  offset enter1
  mov ah , 9h
  int 21h
  dec ch
  cmp ch , 0                                   ;loop
  jne print_loop_1
  call print_bottom                        ;end
  
above_14:
  mov dl , byte ptr ds:[ascii_symbols + 14]     ;sprawdzenie tej opcji, gdzie ilosc przejsc > 14
  mov ah , 2h
  int 21h
  jmp go_back_in_print_loop
    
print_E:
  mov dl , "E"
  mov ah , 2h
  int 21h
  jmp go_back_in_print_loop
print_S:
  mov dl , 'S'
  mov ah , 2h
  int 21h
  jmp go_back_in_print_loop
print_ascii endp
;---------------------------------------------------------------------
convert proc         ;tworzenie tablicy, sklejanie 2 liter tak aby byla zachowana kolejnosc czytania bitow z zadania 
    pusha				;wynik zapisuje w tab_converted_arg
    mov cl , 16		;licznik po argumentach do "loopa", jest 16 cyft w zapisie binarnym
    mov si , 0		;licznik po tablicy
    mov di , word ptr ds:[tab_offset +2] ;drugi argument
convert_etiq:
    mov ah , byte ptr ds:[di]            ;1 litera
    inc di
    mov al , byte ptr ds:[di] ; druga litera
    inc di
    call check_if_valid ;dostaje w ah jest 16-tkowy daje wynik w ah
    mov bl , ah
    shl bl , 4        ;przesuwamy na starszą 4 bitów
    mov ah , al
    call check_if_valid
    add bl , ah
    mov byte ptr ds:[tab_converted_arg +si] , bl	;zmodyfikowanie tablicy, offset dodalem wczesniej do ds 
	;go next:
    inc si
    dec cl
    cmp cl , 0
	;loop it:
    jne convert_etiq
    popa
    ret
convert endp
;-----------------------------------------------------------------------------------------
compare1 proc
    call convert
    cmp ds:[tab_arg] , '1'          ;jesli jest 0 to jest bez modyfikacji, jesli 1 to z.
    je modyf
back_to_beg_in_compare1:
    mov si , 76                     ;srodek
    mov di , 0                      ;"pointer po tablicy"
    mov ch , 16                     ;licznik
;bedziemy pobierac znaki z tab_converted_arg oraz poprzez polecenie shl (przesuniecie bitu) wydobywac z nich kolejne ruchy.
;pierwszy bit to gora/dol, drugi to lewo/prawo
loop_in_compare1:
        mov al , byte ptr ds:[tab_converted_arg + di]                ;wczytanie arg z tablicy
        mov cl ,4                                                    ;1 arg=4 cyfry w binarnym
    move_in_compare_for_first:
        shr al , 1        ;pierwszy znak to ruch gora/dol
        jc move_right
        jmp move_left
    move_in_compare_for_second:              ;drugi znak to ruch lewo/prawo
        shr al , 1
        jc  move_down
        jmp move_up
    end_of_pair:
        inc byte ptr ds:[tab_wart+si]
        dec cl
        cmp cl , 0
        jne move_in_compare_for_first
    ;koniec wew pętli
    inc di      ;nastepny ind
    dec ch      ;zmniejszamy licznik
    cmp ch ,0   ;koniec?
    jne loop_in_compare1
    ret

;z racji tego, ze robilismy ruchy na os x i y to mozemy zrobic 1 porownanie i stwierdzic czy jest mozliwe. Do tego warunek "slizgania sie" jest zachowany.
move_up:
    cmp si , 17					;nie mozemy sie ruszyc w gore jesli jestesmy na 0-wym polu
    jb  end_of_pair
    sub si,17
    jmp end_of_pair
move_left:
    cmp ds:[pos_y],0    
    je  move_in_compare_for_second
	;jne: (mozna wykonac ruch)
    dec si
	dec ds:[pos_y]
    jmp move_in_compare_for_second

move_down:
    cmp si , 135
    ja end_of_pair
    add si,17
    jmp end_of_pair

move_right:
    cmp ds:[pos_y],16                             
    je  move_in_compare_for_second
    inc si
	inc ds:[pos_y]
    jmp move_in_compare_for_second

modyf:
    call modify_array         ;opcjonalna modyfikacja
    jmp back_to_beg_in_compare1 ; powrot do glonwej czesci
compare1 endp
;-------------------------------------
;Modyfikacja polegala na rotacji jednego bitu.
;procedura polega na przekonwertowaniu o 1 bit calej tablicy. Pozniej z niej bedziemy wyciagac argsy
;przy modyfikacji przesuwam bity zaczynajac od poczatku az do konca. Jesli jest carry w poczatkowym to dodaje go do zmiennej globalnej
;dodatkowo po przejsciu calej operacji dodaje ta zmienna do ostatniej wartosci w tablicy
modify_array proc
    pusha
    mov cx,15
	mov si,0
	xor ax,ax
mov_args:
	xor ax,ax
	shl ds:[tab_converted_arg+si],1
	jc CF_eq_1
go_next:
	inc si
	cmp si,16
	je its_last
	jmp mov_args
	ret
CF_eq_1:
	cmp si,0
	je inc_first
	cmp si,0
	jne inc_others
inc_first:
	inc ds:[flag_for_first_carry]
	jmp go_next
inc_others:
	inc ds:[tab_converted_arg+si-1]
	jmp go_next
inc_last:
	inc ds:[tab_converted_arg+15]
	popa
	ret
its_last:
	cmp ds:[flag_for_first_carry],1
	je inc_last
	popa
	ret
modify_array endp
;-------------------------------------------------
end_of_program proc
    mov ax, seg _end
    mov dx,ax
    mov dx, offset _end
    mov ah,9h
    int 21h
    call kill_program
end_of_program endp
;--------------------------------------------------------------------------------
;PARSER SEGMENT
;----------------------------------------------------------------------------------
loop_to_coppy proc
    push ax
    push cx                 ;potrzebuje jedynie akumulatora i cx wiec nie ma sensu pushowania wszystkiego
begin:
    mov al, ds:[si]         ;wpisanie argumentu do al oraz seria porownan
    inc si
    call is_white           ;funkcja is_white zawiera serie porownan. korzystam z niej 2 razy wiec warto przeksztalcic i tylko pisac call
    cmp bh,1                ;dodatkowo bh to taka "flaga" if bh == 1 jest bialy else nie jest
    je  flag
    mov ah,al
    mov es:[di],al          ;fizyczne dodanie do tablicy pewnego znaku =/= bialego
    inc es:[tab_len+bx]
    inc di
    cmp al,0
    je return               ;sprawdzenie czy to przypadkiem nie ostatni bo wczesniej przeszedlem na kolejny
    jmp begin
flag:
    dec si
    inc di
    dec es:[tab_len+bx]
return:
    pop ax
    pop cx                  ;jak wychodze to popuje ax,cx
    ret
loop_to_coppy endp
;--------------------------------------------------------------------------------------
is_white:                   ;sprawdzilem sobie jaki rejestr jest dostepny i nie uzywany bez pop/push
                            ;bylo to bh wiec zrobilem sobie z niego licznik
                            ;jesli == 0 to nie jest to bialy znak
                            ;else jest
    xor bh,bh
    cmp al,20h              ;space
    je flag_1
    cmp al,09h              ;tab
    je flag_1
    cmp al,0Ah              ;end of line
    je flag_1
    cmp al,0Ch              ;page break
    je flag_1
    cmp al,0Dh              ;Carriage return
    je flag_1
    ret
flag_1:
    inc bh
    ret
    
erasewhite proc             ;sprawdzam czy jest bialy. Z racji tego, ze uzywam 2 razy porownania do bialego to jest funkcja
loop2:
    dec cl 
    mov al,ds:[si]          ;od razu wczytuje pierwsza litere bez poczatku linii
    inc si                  ;inc po przeniesienu do al zeby nie robic ifow
    call is_white
    cmp bh,1
    je loop2
    dec si
return2:
    ret
erasewhite endp    
;----------------------------------------------------------------------------------------  
  
;Plan jest prosty:
;Wczytujemy argumety, sprawdzamy czy jest to bialy znak czy tez nie. 
;Jesli tak to pomijamy
;Jesli znaki sie skoncza czyli al==0, to koniec i wypisujemy
;teoretycznie moznaby jeszcze w tablicy zapisywac od razu enter i z nim drukowac
;ale na potrzeby nastepnego zadania zostawilem tak jak jest    
into_array proc             ;PSP -> DS:80h (ile znakow), DS:81h(spacja), DS:82h(ciag znakow)
    push cx
    push ax
    push si
    ;mov ah, 51h
    ;int 21h                 ;upewnienie, �e mamy PSP
    mov cl,ds:[80h]         ;przerzucenie ilosci znakow do licznika
    cmp cl,1                ;puste linie/linie z samych bialych znakow
    je error                ;chwilowo korzystam z cl a pozniej bedzie to al bo AX jest zajete
    mov si, 82h             ;pobranie argumentow
    cmp si,0
    je error
    mov ax, seg tab_arg
    mov es,ax               ;po potrzebuje to zrobic poprzez akumulator, nie moge bezposrednio
    mov di,offset tab_arg  ;no i offset do dx, bo przy wypisywaniu wartosci koszystamy z ds:dx
    
Loop_in_into_array:
    call erasewhite
    cmp al,0                ;wole sprawdzac po al czy sie skonczylo, mozna jeszcze po cl
    je exit    
    mov bl, es:[tab_count]    ;nie wiem czemu tu musi byc bl a ponizej bx w sensie 8 i 16 bitowe rejestry
    shl bx,1                           ;bo nie moge przez inc bo nie dziala dla 1 wywolania.
    mov es:[tab_offset+bx],di ;dodanie do tablicy offsetu, dodanie wartosci w linijce 82
    inc es:[tab_count]        ;tab_count ++
    call loop_to_coppy
    jmp loop_in_into_array
exit:
    pop cx                  ;bo pushowalem na poczatku procedury. 1 push = 1 pop
    pop ax
    pop si    
    ret
into_array endp
;----------------------------------------------------------------------------------------
error proc
   mov ax,seg no_args
   mov ds,ax
   mov dx,offset no_args
   mov ah,9
   int 21h
   call kill_program
error endp
;-----------------------------------------------------------------------------------------
code1 ends


stos1 segment stack
    dw 256 dup(?)
    wstosu  dw  ?
stos1 ends

end start
