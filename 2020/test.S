extern printf
extern getchar
extern exit

section .text
global _start

;;; undef for part 1
%define part2
	
%macro printd 1					; Print a number
	mov rdi, fmt_str
	mov rsi, %1
	call printf
%endmacro

_start:
    mov rdi, hello
    call printf
.start:
	call getchar				; Read a char
	cmp eax, -1
	jz .end						; If rax = (int32)-1, end of input

	cmp al, 10					; If al = \n
	je .nl
	
	mov byte [lastc], al 		; Set The last char to al

	sub rax, 97					; rax -= 'a'
	inc byte [decl + rax]		; Increment declarations for this char

	jmp .start 					; Loop again

.nl:							; Newline found
	cmp byte [lastc], 10		; If its a single newline just ignore it
	je .group					; If second newline, this is the end of a group
	mov byte [lastc], al
	inc byte [in_group]			; Another person in the group
	jmp .start
.group:
	mov rcx, 26
	xor rdx, rdx
	mov al, byte [in_group]
.loop:
%ifndef part2	
	cmp byte [decl + rcx-1], 0	; Is this declaration >0
	setne dl					; Set dl if declaration not zero
%else
	cmp byte [decl + rcx-1], al ; Did everyone in the group answer yes?
	sete dl
%endif
	add dword [sum], edx		; Increment sum if declaration not zero
	mov byte [decl + rcx-1], 0	; Clear declarations 
	loop .loop	 				; Loop until counter = 0
	
	mov dword [in_group], 0		; Clear # people in group
	jmp .start					; Loop again
.end:
	printd [sum]				; Print the sum
    xor rdi, rdi				; Exit 0
    call exit

section .data
    hello db "Advent of Code day 6", 10, 0
	fmt_str db "%d", 10, 0
	sum dd 0 					; Sum of declarations
	in_group db 0 				; Number of people in current group
	lastc db 0					; Last char
section .bss
	decl resb 26				; Declarations (one per letter)
