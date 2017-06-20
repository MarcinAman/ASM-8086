
; You may customize this and other start-up templates;
; The location of this template is c:\emu8086\inc\0_com_template.txt
;https://www.maximintegrated.com/en/app-notes/index.mvp/id/27
;https://www.lammertbies.nl/comm/info/crc-calculation.html


;Algorytm:
;http://www.sunshine2k.de/articles/coding/crc/understanding_crc.html
;dodatkowo: http://www.ross.net/crc/download/crc_v3.txt
;https://4programmers.net/Delphi/Gotowce/CRC_-_Sumy_kontrolne_plik%C3%B3w
.186

data1 segment
       tab_len  db 6 dup(?)
	   tab_count    db  0
       tab_offset   dw  128 dup(?)                       ;w sumie wartosci randomowe moga zaostac i tak je zmieniam
       tab_wart     db  128 dup('$')                     ;wypelniam "$". Pozniej tablica wyglada mniej wiecej tak:
	   enter1        db  13,10,"$"                       ;'a','l','a','$','m','a',$ itd
       no_args      db  "Brak argumentow",13,10,"$"
	   working_case	db 0									;0->pierwszy kejs z -v, 1->drugi kejs
	   wrong_args 	db	"Argumenty nie spelniaja zalozen zadania",13,10,"$"
	   wrong_second db "Drugi argument nie spelnia zalozen, rozny od -v",13,10,"$"
	   file_open_error_1 db "Nie udalo sie otworzyc pliku 1",13,10,"$"
     file_open_error_2 db "Nie udalo sie otworzyc pliku 2",13,10,"$"
	   file_save_error db "Nie udalo sie zapisac do pliku",13,10,"$"
	   file_close_error	db	"Nie udalo sie zamknac pliku",13,10,"$"
     file1_reading_error db "Blad podczas czytania z pliku 1",13,10,"$";
     file_create_error db "Nie udalo sie utworzyc pliku",13,10,"$";
	   file1	db 	20 dup(0)							;wiecej?
	   file2	db	20	dup(0)
	   buffer	db 	300 dup(0)
	   handler dw	?
      handler2 dw ?
	   position_in_buffer	dw		0
	   buffer_size	dw		300
     buffer_left dw  0
     crc16_lo db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 000h, 0c1h, 081h, 040h, 001h, 0c0h, 080h, 041h
	db 001h, 0c0h, 080h, 041h, 000h, 0c1h, 081h, 040h

crc16_hi db 000h, 0c0h, 0c1h, 001h, 0c3h, 003h, 002h, 0c2h
	db 0c6h, 006h, 007h, 0c7h, 005h, 0c5h, 0c4h, 004h
	db 0cch, 00ch, 00dh, 0cdh, 00fh, 0cfh, 0ceh, 00eh
	db 00ah, 0cah, 0cbh, 00bh, 0c9h, 009h, 008h, 0c8h
	db 0d8h, 018h, 019h, 0d9h, 01bh, 0dbh, 0dah, 01ah
	db 01eh, 0deh, 0dfh, 01fh, 0ddh, 01dh, 01ch, 0dch
	db 014h, 0d4h, 0d5h, 015h, 0d7h, 017h, 016h, 0d6h
	db 0d2h, 012h, 013h, 0d3h, 011h, 0d1h, 0d0h, 010h
	db 0f0h, 030h, 031h, 0f1h, 033h, 0f3h, 0f2h, 032h
	db 036h, 0f6h, 0f7h, 037h, 0f5h, 035h, 034h, 0f4h
	db 03ch, 0fch, 0fdh, 03dh, 0ffh, 03fh, 03eh, 0feh
	db 0fah, 03ah, 03bh, 0fbh, 039h, 0f9h, 0f8h, 038h
	db 028h, 0e8h, 0e9h, 029h, 0ebh, 02bh, 02ah, 0eah
	db 0eeh, 02eh, 02fh, 0efh, 02dh, 0edh, 0ech, 02ch
	db 0e4h, 024h, 025h, 0e5h, 027h, 0e7h, 0e6h, 026h
	db 022h, 0e2h, 0e3h, 023h, 0e1h, 021h, 020h, 0e0h
	db 0a0h, 060h, 061h, 0a1h, 063h, 0a3h, 0a2h, 062h
	db 066h, 0a6h, 0a7h, 067h, 0a5h, 065h, 064h, 0a4h
	db 06ch, 0ach, 0adh, 06dh, 0afh, 06fh, 06eh, 0aeh
	db 0aah, 06ah, 06bh, 0abh, 069h, 0a9h, 0a8h, 068h
	db 078h, 0b8h, 0b9h, 079h, 0bbh, 07bh, 07ah, 0bah
	db 0beh, 07eh, 07fh, 0bfh, 07dh, 0bdh, 0bch, 07ch
	db 0b4h, 074h, 075h, 0b5h, 077h, 0b7h, 0b6h, 076h
	db 072h, 0b2h, 0b3h, 073h, 0b1h, 071h, 070h, 0b0h
	db 050h, 090h, 091h, 051h, 093h, 053h, 052h, 092h
	db 096h, 056h, 057h, 097h, 055h, 095h, 094h, 054h
	db 09ch, 05ch, 05dh, 09dh, 05fh, 09fh, 09eh, 05eh
	db 05ah, 09ah, 09bh, 05bh, 099h, 059h, 058h, 098h
	db 088h, 048h, 049h, 089h, 04bh, 08bh, 08ah, 04ah
	db 04eh, 08eh, 08fh, 04fh, 08dh, 04dh, 04ch, 08ch
	db 044h, 084h, 085h, 045h, 087h, 047h, 046h, 086h
	db 082h, 042h, 043h, 083h, 041h, 081h, 080h, 040h

hi db 00h ; high byte of CRC
lo db 00h ; low byte of CRC
;X db 00h
crc dw 0
crc_str db 4 dup(?),0
flag_for_file_end db  0
outbuffer db 0,0,0,0,0
input_generated db  "Pomyslnie wygenerowano kod CRC16",13,10,"$"
crc_checked_suc db  "Werfikacja CRC16 pomyslna",13,10,"$"
crc_checked_fail  db  "Weryfikacja CRC16 niepomyslna",13,10,"$"
second_crc db 4 dup(0),0
data1 ends

code1 segment
start:
    mov ax , seg wstosu       ;inicjalizacja stosu podobna do tego co robilismy na wykladach
    mov ss , ax
    mov sp , offset wstosu
    call into_array
    call check_input
    call fill_names
    call open_files
    call make_crc
    call save_crc
  ;test:
  ;  call save_crc
  ;  call close_files
  ;  call kill_program
  ;test_end

    cmp ds:[working_case],1 ; input i Output
    je working_case_1
    call cin_second_crc
    call compare_2_crc
    call close_files
    call kill_program
  working_case_1:
    call close_files
    call input_generated_suc
    call kill_program
;----------------------Errors segment------------------------------
;------------------------------------------------------------------
;WORKS
check_input proc
	pusha
	mov ax,seg tab_wart
	mov es,ax
	xor ax,ax
	cmp es:[tab_count],3
	je first_input
	cmp es:[tab_count],2
	je second_input
	jmp error_wrong_args
first_input:
	;?
	mov di,0
	mov al, es:[tab_wart+di]
	cmp al,"-"
	jne error_wrong_second_parameter
	inc di;
	mov al, es:[tab_wart+di]
	cmp ax, "v"
	jne error_wrong_second_parameter
	popa
	ret
second_input:
	inc es:[working_case]
	popa
	ret
check_input endp
;------------------------------------------------
;WORKS
error_wrong_second_parameter proc
	mov ax, seg wrong_second
	mov ds,ax
	mov dx,offset wrong_second
	mov ah,9h
	int 21h
	call kill_program
error_wrong_second_parameter endp
error_wrong_args proc
  popa ;bo nie ma tego w poprzedniej funkcji a bylo pusha
	mov ax, seg wrong_args
	mov ds,ax
	mov dx, offset wrong_args
	mov ah,9h
	int 21h
	call kill_program
error_wrong_args endp
error_while_saving proc
	mov ax, seg file_save_error
	mov ds,ax
	mov dx, offset file_save_error
	mov ah,9h
	int 21h
	call kill_program
error_while_saving endp
error_while_opening_file1 proc
  popa
	mov ax, seg file_open_error_1
	mov ds,ax
	mov dx, offset file_open_error_1
	mov ah,9h
	int 21h
	call kill_program
error_while_opening_file1 endp
error_while_opening_file2 proc
  popa
	mov ax, seg file_open_error_2
	mov ds,ax
	mov dx, offset file_open_error_2
	mov ah,9h
	int 21h
	call kill_program
error_while_opening_file2 endp
error_while_closing_file proc
	mov ax, seg file_close_error
	mov ds,ax
	mov dx, offset file_close_error
	mov ah,9h
	int 21h
	call kill_program
error_while_closing_file endp
error_while_reading_file1 proc
  mov ax, seg file1_reading_error
  mov ds,ax
  mov dx, offset file1_reading_error
  mov ah,9h
  int 21h
error_while_reading_file1 endp
error_while_creating proc
  mov ax, seg file_create_error
  mov ds,ax
  mov dx, offset file_create_error
  mov ah,9h
  int 21h
error_while_creating endp

;---------------------------------------------------------------------
;WORKS
fill_names proc
	pusha
	xor cx,cx
	xor ax,ax
	xor di,di
  xor bx,bx
	cmp es:[working_case],1
	je second_case
first_case:
	mov al,byte ptr es:[tab_len]
  mov bl,al
  inc bx
first_case_loop_first_arg:
	mov al,es:[tab_wart+bx]
	mov es:[file1+di],al
	inc bx
	inc di
	cmp al, "$"
	jne first_case_loop_first_arg
  mov al,0
  mov es:[file1+di-1],al
	mov di,0
	;inc bx
first_case_loop_second_arg:
	mov al,es:[tab_wart+bx]
	mov es:[file2+di],al
	inc di
	inc bx
	cmp al,"$"
	jne first_case_loop_second_arg
  mov al,0
  mov es:[file2+di-1],al
	popa
	ret
second_case:
	xor bx,bx
	xor di,di
second_case_loop_first_arg:
	mov al, es:[tab_wart+bx]
	mov es:[file1+di],al
	inc di
	inc bx
	cmp al,"$"
	jne second_case_loop_first_arg
  mov al,0
  mov es:[file1+di-1],al
	mov di,0
	;inc bx
second_case_loop_second_arg:
	mov al,es:[tab_wart+bx]
	mov es:[file2+di],al
	inc bx
	inc di
	cmp al,"$"
	jne second_case_loop_second_arg
  mov al, 0
  mov es:[file2+di-1],al
	popa
	ret
fill_names endp
;----------------------------------------------------------------------------------
;Przerwania na plikach:
;3Ch ->utworzenie
;3Eh ->zamkniecie
;3Dh-> otworzenie (0->odczyt, 1->zapis, 2 -> both)
;WORKS
open_files proc ;first case -> with -v. 2x read-only al==0
  ;second case -> first for read, second for write-only al ==0 and al == 1
  pusha
	xor ax,ax;
	xor bx,bx;
	cmp es:[working_case],1
	je second_case_open
first_case_open:
  mov ax,es
  mov ds,ax
  mov dx,offset file1
  xor al,al     ;al==0, do odczytu
  mov ah,3dh
  int 21h
  mov ds:[handler],ax
  jc error_while_opening_file1
  mov dx, offset file2
  xor al, al
  mov ah, 3dh
  int 21h
  mov word ptr es:[handler2],ax
  jc error_while_opening_file2
  popa
  ret
second_case_open:
	mov ax,seg file1
	mov ds,ax
	mov dx,offset file1
	xor al,al		;do odczytu, 1->zapis, 2->both
	mov ah,3dh		;otwarcie
	int 21h
	mov ds:[handler],ax				;handler->identyfikator pliku
	jc error_while_opening_file1
  mov dx, offset file2
  mov ah,3Ch ;utworzenie nowego
  mov cl,2
  int 21h
  jc error_while_creating
  mov al,1
  mov ah, 3dh
  int 21h
  mov word ptr es:[handler2],ax
  jc error_while_opening_file2
;TEST:
;  mov ah, 3fh
;  mov bx, es:[handler]
;  mov cx, es:[buffer_size]
;  mov dx, offset buffer
;  int 21h
;  mov ah, 9h
;  int 21h
;  mov ah, 40h
;  mov bx, es:[handler2]
;  mov cx, es:[buffer_size]
;  int 21h
;  call kill_program
;WORKS
  popa
  ret
open_files endp
;----------------------------------------------
close_files proc
  mov bx,ds:[handler]
  mov ah,3eh
  int 21h
  jc error_while_closing_file			;CF==0 -> ok, CF==1 not ok
  mov bx,ds:[handler2]
  mov ah,3eh
  int 21h
  jc error_while_closing_file			;CF==0 -> ok, CF==1 not ok
  ret
close_files endp
;--------------------------------------------------------
save_crc proc
;nazewnictwo:
;w number jest dodanie tak aby wyszlo ascii
;w end jest dodanie do outbuffer, ktory jest outputem.
pusha
    xor ax ,ax
    mov al,ds:[lo]
    mov dx, 0
    mov bx, 16
    div bx    	;div dx:ax przez 16 (bx), wynik calkowity w ax, reszta w dx
    ;dziele przez 16 aby otrzymac 2 hexy, czyli jeden a ax, drugi w dx.
    ;analogicznie jak dzielenie przez 10 w dziesietnym systemie.
    cmp dx,10
    jl number_for_3  ;jump if less
    add dx,'A'  ;to ascii
    sub dx ,10
    mov ds:[outbuffer + 3] , dl ;wkladam je od konca.
    jmp end_for_3
number_for_3:
    add dx,'0'
    mov ds:[outbuffer + 3] , dl
end_for_3:
    cmp ax,10
    jl number_for_2
    add ax,'A'
    sub ax ,10
    mov ds:[outbuffer + 2] , al ;az do poczatku
    jmp end_for_2
number_for_2:
    add ax,'0'
    mov ds:[outbuffer + 2] , al
end_for_2:
    xor ax ,ax
    mov al, ds:[hi] ;bierzemy teraz analogicznie hi oraz robimy dokladnie to samo
    mov dx, 0       ;roznica jedynie polega na tym, ze tutaj mamy dostepne pola o indeksach 0-1
    mov bx, 16
    div bx
    cmp dx,10
    jl number_for_1
    add dx,'A'
    sub dx ,10
    mov ds:[outbuffer + 1] , dl
    jmp end_for_1
number_for_1:
    add dx,'0'
    mov ds:[outbuffer + 1] , dl
end_for_1:
    cmp ax,10
    jl number_end
    add ax,'A'
    sub ax ,10
    mov ds:[outbuffer] , al
    jmp end_end
number_end:
    add ax,'0'
    mov ds:[outbuffer] , al
end_end:
    cmp ds:[working_case],0
    je end_1
    mov dx , offset outbuffer
    mov bx , ds:[handler2]
    mov cx, 5
    mov ah,40h
    int 21h
    popa
    ret
end_1:
  popa
  ret
save_crc endp
;----------------------------------------------
kill_program:
    mov   ah, 4ch
    int   21h
    ret

;w al jest input charow z pliku
;algorytm:
;X=lo xor ax
;nowe hi = crc_hi[X]
;nowe lo = crc_lo[X] xor hi
make_crc proc
  crcloop:
    call getchar ; gets char into ax and sets finishedreading if required
    cmp ds:[flag_for_file_end],1
    je crcdone
  pusha
  xor cx,cx
  xor dx,dx

  mov cl,ds:[lo]
  xor cx,ax	; =X
  mov bx,cx	;kopia
  mov dl,ds:[crc16_hi+bx] ;nowe hi = crc_hi[X]

  mov al,ds:[crc16_lo+bx] ;
  xor cx,cx
  mov cl,ds:[hi]
  xor ax, cx ;nowe lo =crc16_lo[I] XOR hi

  mov ds:[hi],dl  ;zapis
  mov ds:[lo],al
  popa
  jmp crcloop
  crcdone:
  ret
make_crc endp

;WORKS
;dostajemy tablice buffer i pobieramy z niej znaki
;sa 3 mozliwe przypadki:
;1)jest znak w buffer
;2)nie ma znaku w buffer ale jest jeszcze do pobrania z pliku
;3)nie ma znaku w buffer i nie ma go do pobrania z pliku

; przypadku 2 pobieramy znak z pliku funkcja loadbuffer
;w przypadku 3 nic nie robimy bo skonczylismy
getchar proc
    ;nie pushuje ax bo w al zwracam sobie char
    push dx
    push cx
    push bx
    mov dx,ds:[buffer_left]
    cmp dx,0
    je need_chars       ;case 2/3
still_have_input:     ;case 1
    xor ax,ax
    xor bx,bx
    mov bx,ds:[position_in_buffer]
    mov al,ds:[buffer+bx]
    inc bx
    mov ds:[position_in_buffer],bx
    mov bx,ds:[buffer_left]
    dec bx
    mov ds:[buffer_left],bx
    jmp return_in_getchar
need_chars:
  call loadbuffer
  cmp ds:[flag_for_file_end],1
  jne still_have_input
  mov cx,ds:[buffer_left]
  cmp cx,0
  jne still_have_input
return_in_getchar:
  pop bx
  pop cx
  pop dx
	ret
getchar endp

;WORKS
loadbuffer proc
    ;przerwanie 3Fh:
    ;bx-> uchwyt, w tym przypadku handler
    ;cx -> liczba bajtów do przeczytania czyli buffer_size
    ;ds:dx wskaznik na buffor
    ;output: w ax dostajemy liczbe wczytanych znakow
    ;cf==1 jesli cos poszlo nie tak
    mov bx,ds:[handler]
  	mov cx, ds:[buffer_size]
  	mov dx,offset buffer
    mov ah,3Fh
  	int 21h
    jc error_in_load_buffer
    mov cx, 0
  	mov ds:[position_in_buffer], cx
  	cmp ax,0
  	je eof_is_here
    more_to_load:
  	mov ds:[buffer_left],ax ;number of bytes loaded to buffer
    ret
eof_is_here:
  mov ds:[flag_for_file_end],1
  jmp more_to_load
error_in_load_buffer:
  popa
  call error_while_reading_file1
loadbuffer endp
;WORKS
cin_second_crc proc
  pusha
  mov ah,3fh
  mov bx, es:[handler2]
  mov cx,4
  mov dx, offset second_crc
  int 21h
  ;mov ah,9h
  ;int 21h
  popa
  ret
cin_second_crc endp
;---------------------------------------
compare_2_crc proc
  pusha
  mov ax,seg second_crc
  mov es,ax
  mov bx,4
for_in_compare:
  mov al,es:[second_crc+bx]
  mov ah,es:[outbuffer+bx]
  cmp al,ah
  jne error_while_comparing
  dec bx
  cmp bx,0
  jne for_in_compare

  popa
  call crc_check_succes
  ret

error_while_comparing:
  popa
  call crc_check_failed
  ret
compare_2_crc endp

;Komunikaty:
;-----------------------------------------
input_generated_suc proc
  mov ax, seg input_generated
  mov ds, ax
  mov dx, offset input_generated
  mov ah, 9h
  int 21h
  ret
input_generated_suc endp
crc_check_succes proc
  mov ax, seg crc_checked_suc
  mov ds, ax
  mov dx, offset crc_checked_suc
  mov ah, 9h
  int 21h
  ret
crc_check_succes endp
crc_check_failed proc
  mov ax, seg crc_checked_fail
  mov ds, ax
  mov dx, offset crc_checked_fail
  mov ah, 9h
  int 21h
  ret
crc_check_failed endp
;-----------------------Parser Segment------------------------------------
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
    mov ax, seg tab_wart
    mov es,ax               ;po potrzebuje to zrobic poprzez akumulator, nie moge bezposrednio
    mov di,offset tab_wart  ;no i offset do dx, bo przy wypisywaniu wartosci koszystamy z ds:dx

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
;---------------------------------------------------------------------------------------
stos1 segment stack
    dw 256 dup(?)
    wstosu  dw  ?
stos1 ends

end start
