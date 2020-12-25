extern printf
extern scanf
extern exit

section .text
global _start
	
%macro printd 1					; Print a number
	mov rdi, fmt_str
	mov rsi, %1
	call printf
%endmacro

%macro abs 1
	mov rax, %1
	mov rbx, rax
	neg rbx
	cmovg rax, rbx
%endmacro
	
_start:
    mov rdi, hello
    call printf
.start:
	mov rdi, scan_str
	mov rsi, char
	mov rdx, arg
	call scanf
	cmp eax, 2					; If less than 2 groups scanned, exit
	jne .end

	;; Store argument in rax
	mov rax, [arg]
	
	cmp byte [char], 'F'
	je .forward
	cmp byte [char], 'L'
	je .rotate
	cmp byte [char], 'R'
	je .rotate
	
	cmp byte [char], 'N'
	je .north
	cmp byte [char], 'S'
	je .south
	cmp byte [char], 'E'
	je .east
	cmp byte [char], 'W'
	je .west
	
	jmp .start 					; Loop

.south:
	neg rax		 				; Negate argument
.north:
	add [wn], rax
	jmp .start

.west:
	neg rax
.east:
	add [we], rax
	jmp .start
	
.forward: 						; Move forward
	mov r9, rax					; Save rax
	mul qword [we]	 			; rax = arg * we
	add [se], rax
	mov rax, r9
	mul qword [wn]
	add [sn], rax
	jmp .start

.rotate:						; Rotate waypoint
	xor rdx, rdx
	;; Arg already in rax
	mov rcx, 90					; Divide by 90 degrees
	div rcx
	
	mov rcx, rax 				; counter = quotient (loop N times)
.loop:
	mov rsi, [we]
	mov rdi, [wn]
	
	cmp byte [char], 'L'
	je .is_left
	neg rsi
	jmp .after_dirs
.is_left:
	neg rdi
.after_dirs:
	mov [we], rdi
	mov [wn], rsi

	loop .loop
	
	jmp .start
	
.end: 							; Print positions
	abs qword [se]
	mov rdx, rax
	abs qword [sn]
	add rax, rdx
	printd rax

	xor rdi, rdi
	call exit

section .data
    hello db "Advent of Code day 12", 10, 0
	fmt_str db "part2 = %lld", 10, 0
	scan_str db "%c%d", 10, 0 	; scanf format string
	char dq 0					; Opcode char, padding for printf
	arg dq 0					; Argument
	se dq 0 					; Ship east
	sn dq 0 					; Ship north
	we dq 10	 				; Waypoint east
	wn dq 1 					; Waypoint north
