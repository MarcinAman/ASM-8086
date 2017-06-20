;Marcin Aman
;Rysowanie Krzywej Kocha

.186					;dyrektywa dla pusha, popa
.387
data1 segment
      ;parser:
       tab_count    db  0
       tab_offset   dw  128 dup(?)                       ;w sumie wartosci randomowe moga zaostac i tak je zmieniam
       tab_arg      db  300 dup('$')					;tablica z parsera. Jest tutaj niezmieniony, sparsowany input
       tab_len  db 150 dup(?)     ;dl argumentow
       no_args db "Brak argumentow!",13,10,"$";
       ;errory z krzywej:
       error_number db "Niewlasciwa ilosc argumentow.",13,10,"$";
       double_d db "Drugi argument (internum) powinien byc z przedzialu [1,4].",13,10,"$";
       len_d db "Wartosc drugiego parametru nie jest z przedzialu [1,100].",13,10,"$";
       ;zmienne z krzywej:
       iternum db 0
       len dw 0 ;moze byc bajtowe, bo 1 bajt to 8 bitow, czyli 2^8-1 wartosci == 255 wartosci. Daje to przedzial [0,255]
       lsystem_1 db 1793 dup(0) ;gdyz zalozylem, ze max iterum == 4 wiec 7*4^n=1793
       lsystem_2 db 1793 dup(0)
       first_line db 'F','+','+','F','+','+','F',0
       filled db 'F','-','F','+','+','F','-','F'
       ;LSYSTEM				db		1793 dup(0)
       result_in  db 0
       intx				dw		20
     	 inty				dw		60
       three dw 3
       ten dw 10d
data1 ends

code1 segment
start:

    mov ax , seg wstosu       ;inicjalizacja stosu podobna do tego co robilismy na wykladach
    mov ss , ax
    mov sp , offset wstosu
    call into_array               ;segment parsera. Na tym etapie elementy sa tylko i wylacznie dodane do tablicy (nie sa sprawdzone)
    mov ax , data1                ;na cały program
    mov ds , ax
    call check_args
    call generateString
    call graph_init
    call create_a_flake
    xor ax,ax
    int 16h   ;oczekiwanie na klawisz

    mov ax,3
    int 10h   ;wyjscie z trybu graficznego

    call kill_program

kill_program proc
    mov   ah, 4ch
    int   21h
kill_program endp

check_args proc
;WORKS
;1) maja byc 2 Argumenty
;2) pierwszy argument (internum) ma byc z przedzialu 1,4
;3) drugi argument (len) z przedzialu 1,100

    pusha
    mov al,2d
    cmp ds:[tab_count],al ;czy sa 2 argument
    jne error_number_args
    ;segment internum
    mov al,ds:[tab_arg+1]
    cmp al,'$'  ;wprowadzona liczba internum jest dwucyfrowa, error
    jne error_double_digit
    mov al, ds:[tab_arg]
    sub al,'0'                 ;wtedy 0 bedzie 0 ascii. Analogicznie dla innych liczb
    cmp al,4d
    ja error_double_digit       ;jump if above
    cmp al,1d
    jb error_double_digit       ;jump if bellow
    mov ds:[iternum],al        ;jest juz z przedzialu [1,4]
    ;segment len
    xor ax,ax
    mov al, 3d
    cmp ds:[tab_len+2],al
    ja error_len
    xor cx,cx
    xor ax,ax
    xor bx,bx
first_len_dig:
    ;uzywam al do przechowywania 1 cyfry
    mov al,ds:[tab_arg+2]
    sub al,'0'
    ;cmp al,1d
    ;jne error_len
    mov cl,ds:[tab_len+2]
    dec cl
    cmp cl,0d
    je end_of_inputing_digs
second_len_dig:
    ;w ch jest druga cyfra
    mov ch,ds:[tab_arg+3]
    sub ch,'0'
    dec cl
    cmp cl,0d
    je len_is_2d
third_len_dig:
    ;w cl jest 3 liczba, nie potrzebuje juz licznika.
    mov cl,ds:[tab_arg+4]
    sub cl,'0'
    ;mamy wszystkie cyfry, teraz trzeba to zlaczyc w 1 liczbe
    ;w al nadal mamy 1 cyfre, setek
    mov bl,100d ;mnozenie przez 100d
    mul bl      ;opis funkcji mul znajduje sie ponizej
    push ax   ;kopia
    xor ax, ax  ;wyzerowany index bo nie wykorzystuje calego a jedynie polowe
    mov al,ch   ;dodanie 2 cyfry,dziesiatek
    mov bl,10d  ;mnozenie przez 10d
    mul bl
    push ax   ;kopia
    xor ch,ch
    mov bx,cx ;3 cyfra (jednosci)
    pop ax    ;sciagam cyfre dziesiatek
    add bx,ax ;dodaje do jednosci
    pop ax    ;sciagam cyfre setek
    add bx,ax ;dodaje do poprzedniej sumy
    mov ax,bx ;przesuwam sobie wynik do ax
    jmp end_of_inputing_digs    ;jump na koniec
len_is_2d:
    ;opis funkcji mul:
    ;jesli argument jest typu byte (jak tutaj)
    ;ax=al*bl, bl moze byc zastapiony poprzez inny operand
    ;jesli argument jest wordem
    ;DX:AX = AX*operand
    ;Zrodlo:
    ;http://www.electronics.dit.ie/staff/tscarff/8086_instruction_set/8086_instruction_set.html#MUL
    xor bx,bx
    mov bl,10d  ;mnozenie przez 10d
    mul bl
    xor cl,cl
    add al,ch   ;dodaje do cyfry jednosci
    jmp end_of_inputing_digs  ;jump na koniec

end_of_inputing_digs:
    cmp ax,100d
    ja error_len
    cmp ax,0d
    je error_len
    mov ds:[len],ax
    popa
    ret

;errory:
error_len:
    popa
    call error_len_f

error_double_digit:   ;error odpowiada za przypadek, gdy internum jest z poza przedzialu [1,4]
    popa
    call error_double_digit_f
error_number_args:
    popa
    call error_number_args_f

check_args endp
;------------------------------------------------------
error_number_args_f proc
    mov dx,offset error_number
    mov ah,9h
    int 21h
    call kill_program
error_number_args_f endp

error_double_digit_f proc
    mov dx,offset double_d
    mov ah,9h
    int 21h
    call kill_program
error_double_digit_f endp

error_len_f proc
  mov dx,offset len_d
  mov ah,9h
  int 21h
  call kill_program
error_len_f endp
;-------------------------------------------------------
;WORKS
generateString	proc
;first_line db 'F','+','+','F','+','+','F',0
;filled db 'F','-','F','+','+','F','-','F'
  pusha
;zdecydowalem sie na podejscie rekrencyjne ze wzgledu na fakt, iz trzeba by bylo kopiowac z tablicy do tablicy stringu
;przy stringach dochodzacych do 4000 znakow jest to czasochlonne
  xor bx,bx
  xor cx,cx
  xor dx,dx
  mov ax,seg lsystem_1
  mov ds,ax
  mov dx,offset lsystem_1;
  mov cl,ds:[iternum];
  mov al,'+'
  mov ah,'-'
  ;pierwsze f
  call insert_F
  mov ds:[lsystem_1+bx],al;dwa plusy
  inc bx
  mov ds:[lsystem_1+bx],al
  inc bx
  ;drugie f
  call insert_F
  mov ds:[lsystem_1+bx],al
  inc bx
  mov ds:[lsystem_1+bx],al
  inc bx
  call insert_F
  ;koniec pierwszego, poczatkowego stringu

  jmp end_of_generate

  insert_F:
  cmp cl,0
  je end_of_insert_f
  dec cl
  call insert_F
  mov ds:[lsystem_1+bx],ah
  inc bx
  call insert_F
  mov ds:[lsystem_1+bx],al
  inc bx
  mov ds:[lsystem_1+bx],al
  inc bx
  call insert_F
  mov ds:[lsystem_1+bx],ah
  inc bx
  call insert_F
  inc cl
  ret

end_of_insert_f:
  push ax   ;bo brakuje mi rejestrow a potrzebuje finalnie dodac do tablicy F-y
  mov al,'F'
  mov ds:[lsystem_1+bx],al
  inc bx
  pop ax
  ret
end_of_generate:
  popa
  ret

generateString	endp
;----------------------------------------------------------------------------
graph_init proc
  pusha
  xor ah,ah     ;grafika 320x200, 256 kolorow
  mov al,13h
  int 10h
  popa
  ret
graph_init endp

create_a_flake proc
  pusha
  xor bx,bx
;przygotowanie FPU:
  finit
  fldpi ;wstaw na wierzcholek pi, st(0)=pi
  fidiv ds:[three] ;st(0)=st(0)/3, potrzebuje do tego zmiennej
  fldz            ;push +0.0
  fild ds:[inty]  ;push inty
  fild ds:[intx]  ;push intx
;Rejestry koporcesora wygladaja nastepujaco:
;st(0)=x,st(1)=y,st(2)=rad,st(3)=pi/3, pozostale pozostaja wolne (max 8)
;katy sa w radianach
creating_flake_loop:
  mov al,ds:[lsystem_1+bx]
  inc bx
  cmp al,0
  je finish_in_flake
  cmp al, 'F'
  je linie
  cmp al,'+'
  je plus
  cmp al,'-'
  je minus
  jmp creating_flake_loop

linie:
  call draw_line
  jmp creating_flake_loop
plus:
  call add_rads
  jmp creating_flake_loop
minus:
  call dec_rads
  jmp creating_flake_loop

finish_in_flake:
  popa
  ret
create_a_flake endp

add_rads proc
;procedura ma dodac do rad wartosc pi/3
;stan rejestrow:
;st(0)=x,st(1)=y,st(2)=rad,st(3)=pi/3
;wiec potrzebujemy st(2)+=st(3)
  pusha       ;mozemy jedynie wykonywac operacje na wierzcholku, czyli st(0)
  fxch st(2)  ;wiec zmieniam st(2) z st(0) jest: 0->rad, 1->y, 2->x, 3->pi/3
  fadd st(0),st(3)  ;dodaje do st(0), st(2) jest: 0->rad+pi/3, 1->y, 2->x, 3->pi/3
  fxch st(2)  ;ponownie zamieniam jak w kroku 1
  ;zostaje:
  ;0->x, 1->y, 2->rad+pi/3,3->pi/3
  popa
  ret
add_rads endp

dec_rads proc
  pusha
  fxch st(2)
  fsub st(0),st(3)  ;zamiast dodawnia jakie mialem w funkcji add_rads teraz mam odejmowanie.
  fxch st(2)  ;algorytm pozostaje analogiczny
  popa
  ret
dec_rads endp

draw_line				proc
  ; procedura rysuje linie o dlugosci LEN pod katem rad

  pusha

  mov bl,byte ptr ds:[LEN]

  fld st(2)						; zaladuj liczbe z pamieci (w tym przypadku robimy kopie kata)
                  ; STAN: [rad, x, y, rad, PI/3 ]
  fsincos							; st(0) = cosinus [rad], st(1) = sinus [rad]
  ;warto korzystac z fsincos a nie z fsin i fcos bo oszczedza nam to czas
                  ; STAN: [cos, sin, x, y, rad, PI/3 ]

  fincstp							; zwiekszam wskaznik stosu dwa razy, czyli
  fincstp							; st(5) -> st(3), st(4) -> st(2) etc...

  ; finalnie jest:
  ; [ x, y, rad, PI/3, cos, sin ]

lineLoop:
  cmp bl,0d						;jesli juz wszystko to koniec
  je lineEnd

  fadd st(0), st(6) 				; x += cos(rad)
  fist word ptr ds:[intx] 		; new x do pamieci

  fxch st(1) 						; swap st(0), st(1)
                  ; STAN: [y, x, rad, PI/3, cos, sin]

  fadd st(0), st(7) 				; y += sin(rad)
  fist word ptr ds:[inty] 		; new y do zmiennej

  fxch st(1) 						; przywroc kolejnosc x,y
                  ; STAN: [ x, y, rad, PI/3, cos, sin ]

  ; pobierz wspolrzedne do rejestrow
  mov cx, word ptr ds:[intx]
  mov dx, word ptr ds:[inty]

  ; nie rysuj poza ekranem
  cmp cx, 320 - 1
  ja skipdraw
  cmp dx, 200 - 1
  ja skipdraw

  mov al,0Fh      ;kolor
  mov ah,0Ch
  int 10h 						; Zapisz pixel w kolorze AL, w cx jest x, w dx jest y.
;Komentarz do przerwania do 10h:
;ze wzgledu na dosyc ograniczona ilosc pikseli moge sobie pozwolic na rysownie "przez bios" a nie bezposrednio do karty
;graficznej (ES:DI = 0A000:320*y+x).

skipdraw:
;jesli wyjdzie poza ekran nie wysylam przerwaniem 10h a skipuje ten poziom rysowania.
  dec bl
  jmp lineLoop

lineEnd:
  ffree st(7) ; pozbadz sie sinusa i cosinusa
  ffree st(6)

  ; wyjscie: [x, y, rad, PI/3], czyli stan poczatkowy z nowymi x,y

  popa
  ret
draw_line				endp

;-----------------------------------------------------------------------------------
;NOTATKI z Koprocesora:
;przechowuje dane w 8 80bitowych rejestrach indeksowanych od st(0) do st(7). st(0) to wierzchołek
;Funkcje:
;FILD: ładowanie liczb całkowitych na wiechrzołek(obsługuje dw,dd,dq)
;Jak damy kilka razy fild to one ułoża sie normalnie w stos, zgodnie z kolejnoscia
;FBLD: załadowanie 10bajtowej liczby o kodzie BCD (dt), czyli duze liczby w formacie binarnym.
;FLD: ładowanie wszystkich, rozpoznawalnych typów rzeczywistych
;(mozna powielic dana wartosc poprzez st(2), wtedy mamy ja zarowno na st(2) jak i st(0))
;UWAGA: liczby musza byc z kropka aby byly traktowane jako zmienno przecinkowe
;FLDPI -> laduje pi
;FST zdejmowanie ze stosu, pozniej laduje ona w argumencie
;fst liczba - > liczb = st(0) nie zdejmuje
;fstp to samo tylko ze zdejmowaniem
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
    je error_no_args                ;chwilowo korzystam z cl a pozniej bedzie to al bo AX jest zajete
    mov si, 82h             ;pobranie argumentow
    cmp si,0
    je error_no_args
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
;-----------------------------------------------------------------------------------------
error_no_args proc
    mov dx,offset no_args
    mov ah,9h
    int 21h
    call kill_program
error_no_args endp
;-----------------------
code1 ends


stos1 segment stack
    dw 1024 dup(?) ;1 word == 16 bitów == 2 bajty, 2x1024 - 2 (wierzocholek) = 2046 bajtow
    wstosu  dw  ?
stos1 ends

end start
