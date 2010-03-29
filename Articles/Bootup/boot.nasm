[bits 16]
[org 0x7c00] ; Be aware that the code is going to be loaded at 0x7c00

mov ax,cs
mov ds,ax
mov ax,0xb800
mov es,ax

mov cx,[ds:len]
mov si,hello
mov di,0

l1:
mov ah,[ds:si]
mov [es:di],ah
inc si
inc di
mov byte [es:di],0x4f
inc di
loop l1


hello:
	db "HELLO WORLD"  
len: 	dw $-hello

jmp $

times (510-($-$$)) db 0
dw 0xaa55 ; Indicating the sector is a boot sector

times ((512*18*80*2)-($-$$)) db 0
