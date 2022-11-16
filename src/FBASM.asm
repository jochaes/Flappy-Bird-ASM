; multi-segment executable file template.
; mount c c:\emu8086\mybuild
data segment
    ancho dw 320
    largo dw 200
    
    bird_x dw ?
    bird_y dw ?
    bird_w dw ?
    bird_h dw ?
    bird_name db "bird.img",0
    bird_img db 533 dup(?) 
    bird_v dw 1
    bird_j dw 12
    
    filename_dir dw ?
    w_dir dw ?
    h_dir dw ? 
    buff_dir dw ?
    x_dir dw ?
    y_dir dw ?
    y_val dw ?
    handle dw ?
    
    ultimo_s db ?  
    
    ;las usa el rectangulo
    w dw ?
    h dw ?
    x dw ?
    y dw ?
    c db ? 
    
    ;movimientoObs
    pipe_x dw ?
    pipe_y dw ?
    
    ;obstaculos
    Pw dw ?  
    Ph dw ?
    Px dw ?
    Py dw ?
    Pl dw ?
    Pc db ? 
    
    ;apertura
    apx dw ?
    apy dw ?
ends

stack segment
    dw   128  dup(0)
ends

code segment
    jmp start
    leer_imagen proc   
        ;INT 21h / AH= 3Dh - open existing file.
        mov al, 0
    	mov dx,  filename_dir
    	mov ah, 3dh
    	int 21h
    	jc err
    	mov handle, ax
    	;INT 21h / AH= 42h - SEEK - set current file position.
        
        mov al, 0
    	mov bx, handle
    	mov cx, 0
    	mov dx, 0
    	mov ah, 42h
    	int 21h ; seek... 
    	jc err
    	
    	
    	;INT 21h / AH= 3Fh - read from file. 
    	mov bx, handle
    	mov dx, w_dir
    	mov cx, 1
    	mov ah, 3fh
    	int 21h ; read from file...
    	jc err
    	
    	;INT 21h / AH= 3Fh - read from file. 
    	mov bx, handle
    	mov dx, h_dir
    	mov cx, 1
    	mov ah, 3fh
    	int 21h ; read from file... 
    	jc err
    	
    	;calcular tamano de la imagen
    	;para saber cuandos bytes leer
    	;w x h  = cantidad de bytes
    	mov di, w_dir
    	mov ax, [di] 
    	mov di, h_dir
    	mov bx, [di]
    	mul bx
    	mov cx, ax 
    	
    	;INT 21h / AH= 3Fh - read from file. 
    	mov bx, handle
    	mov dx, buff_dir
    	mov ah, 3fh
    	int 21h ; read from file... 
    	jc err
    	
    	;INT 21h / AH= 3Eh - close file. 
    	mov bx, handle
    	mov ah, 3eh
    	int 21h ; close file... 
    	jnc ok
    	
    	err:
    	    ;hacer algo
	    
	    ok:
	ret    
    endp 
    
    DIBUJAR_RECT_DATOS MACRO xm, ym, wm, hm, cm
        mov ax, xm
        mov x, ax
        mov ax, ym
        mov y, ax
        mov ax, wm
        mov w, ax
        mov ax, hm
        mov h, ax
        mov al, cm
        mov c, al
        call draw_rect
    ENDM
    draw_rect proc
        ;requiere un w, h, x, y, c
        ;bx <- contador para medir ancho
        ;cuando se alcance el ancho -> se cambia de linea
        ;cuando se hayan pintado todos los pixeles se termina
        mov bx, 0
        mov ax, w
        mul h
        mov cx, ax
        ciclo_rect:
            ;calcular donde pintar
            ;posicion de memoria grafica
            ; 320 x 200
            ; recorrer el rectangulo por ancho
            ; 
            mov ax, y
            mul ancho
            add ax, x
            add ax, bx
            mov di, ax
            mov al, c
            mov es:[di], al
            inc bx
            cmp bx, w
            jne sig_px_rect
            mov bx, 0
            inc y
            sig_px_rect:
                loop ciclo_rect 
             
        ret    
    endp 
     
     
    DIBUJAR_OBSTACULOS_DATOS MACRO xm,ym,lm,cm
        mov ax, xm
        mov Px, ax
        mov pipe_x,ax
        mov ax, ym
        mov Py, ax
        mov pipe_y,ax
        mov ax, 30
        mov Pw, ax
        mov ax, 150
        mov Ph, ax
        mov ax,lm
        mov Pl,ax
        mov al, cm
        mov Pc, al
        call draw_obstaculos
    ENDM
    
    draw_obstaculos proc
       ;requiere un largo(w)
       ;requiere un w, h, x, y, c
        ;bx <- contador para medir ancho
        ;cuando se alcance el ancho -> se cambia de linea
        ;cuando se hayan pintado todos los pixeles se termina

        mov ax,Py      
        mov apy,ax
        
        mov ax,Px
        mov apx,ax
        
        
        mov bx, 0
        mov ax, Pw
        mul Ph
        mov cx, ax
        ciclo_obst:
            ;calcular donde pintar
            ;posicion de memoria grafica
            ; 320 x 200
            ; recorrer el rectangulo por ancho
            ; 
            mov ax, Py
            mul ancho
            add ax, Px
            add ax, bx
            mov di, ax
            mov al, Pc
            mov es:[di], al
            inc bx
            cmp bx, Pw
            jne sig_px_obst
            mov bx, 0
            inc Py
            sig_px_obst:
                loop ciclo_obst
        
   
        ;requiere un largo(w)
        ;requiere un w, h, x, y, c
        ;bx <- contador para medir ancho
        ;cuando se alcance el ancho -> se cambia de linea
        ;cuando se hayan pintado todos los pixeles se termina
        
        ;Calculamos size de la apertura
        mov bx, 0 
        
        mov ax,Pl
        mov apy,ax
        
        mov ax,bird_h
        add ax,10  
        
        mov Ph,ax  
        mov ax, Pw
        mul Ph
        mov cx, ax 
        
        ciclo_apertura:
            ;calcular donde pintar
            ;posicion de memoria grafica
            ; 320 x 200
            ; recorrer el rectangulo por ancho
            ; 
            mov ax, apy
            mul ancho
            add ax, apx
            add ax, bx
            mov di, ax
            mov al, 54
            mov es:[di], al
            inc bx
            cmp bx, Pw
            jne sig_px_apertura
            mov bx, 0
            inc apy
            sig_px_apertura:
                loop ciclo_apertura  
        ret    
    endp
    
    draw_img proc
        ;usa los valores de las direcciones de memoria
        ;para pintar las imagenes
        ;calcular cuantos px hay que pintar
        ;bx<-recorrer el ancho
        ;verificar el px
        ; si el color es 0 lo ignoramos 
        ; si no lo pintamos calculando la posicion de memoria grafica
        mov di, w_dir
    	mov ax, [di] 
    	mov di, h_dir
    	mov bx, [di]
    	mul bx
    	mov cx, ax
    	mov si, buff_dir
    	mov di, y_dir
        mov dx, [di]  ; dx <- posicion y de la imagen (calcular linea)
        mov y_val, dx
        mov bx, 0
        ciclo_draw_img:
            cmp [si], 0
            je pint_sig_px
            mov ax, y_val
            mul ancho
            mov di, x_dir
            mov dx, [di]
            add ax, dx
            add ax, bx
            mov di, ax
            mov al, [si]
            mov es:[di], al
            pint_sig_px:
                inc bx
                inc si
                mov di, w_dir
    	        mov ax, [di]
    	        cmp bx, ax
    	        jne continuar_loop
                mov bx, 0
                inc y_val
            continuar_loop:
                loop  ciclo_draw_img
        
        ret    
    endp

    salto proc
        ;verificar que se apreto alguna letra 
        MOV AH,01h
        int 16h      
        
        ;verificar cual letra fue 
        MOV AH,00h
        int 16h
         
        ;verifcar que sea espacio
        cmp al,20h; "espacio"
        JE pressSpace
            
        ;metodo para que el pajaro salte
        pressSpace:
            DIBUJAR_RECT_DATOS bird_x, bird_y, bird_w, bird_h, 54
            
            mov ax, bird_y
            sub ax, bird_j
            add ax,bird_h
            cmp ax,bird_h
            jge subir
            add ax,bird_j   
            subir:
                sub ax,bird_h 
                mov bird_y, ax
                lea ax, bird_y
                mov y_dir, ax
                lea ax, bird_x
                mov x_dir, ax
                call draw_img 
        
        mov ah, 01h
        int 16h
        jz dibujar_escena  
             
    endp
start:
; set segment registers:
    ; set segment registers:
    mov ax, data
    mov ds, ax
    mov ax, stack
    mov ss, ax
    mov ax, 0xA000
    mov es, ax
    
    ;iniciar modo grafico
    mov ax, 0x0013
    int 10h
    
    lea ax, bird_name
    mov filename_dir, ax
    
    lea ax, bird_w
    mov w_dir, ax
    
    lea ax, bird_h
    mov h_dir, ax
    
    lea ax, bird_img
    mov buff_dir, ax
    
    call leer_imagen                       
    
    mov bird_x, 100
    mov bird_y, 50
    DIBUJAR_RECT_DATOS 0, 0, ancho, 150, 54 
    DIBUJAR_RECT_DATOS 0, 150, ancho, 50, 10 
    DIBUJAR_OBSTACULOS_DATOS 220,0,40,3 
   
    
    dibujar_escena:
        ;INT 21h / AH=0Ch - flush keyboard buffer and read standard input.
        mov ah, 0ch
        mov al, 0
        int 21h
        
        ;INT 21h / AH=2Ch - get system time;
        ;return: CH = hour. CL = minute. DH = second. DL = 1/100 seconds.  
        mov ah, 0x2C
        int 21h
        mov ultimo_s, dl
        esperar:
            int 21h
            cmp dl, ultimo_s
            je esperar
               
        
        ;primero borrar la imagen anterior  
        DIBUJAR_RECT_DATOS bird_x, bird_y, bird_w, bird_h, 54
        ;DIBUJAR_RECT_DATOS 0, 0, ancho, 150, 54              
        DIBUJAR_OBSTACULOS_DATOS Px,0,40,54
        
        
        ;Movimiento pajaro
        mov ax, bird_y
        add ax, bird_v 
        add ax,bird_h
        cmp ax,150
        jle mover
        sub ax,bird_v   
            mover:
                sub ax,bird_h
                mov bird_y, ax
                lea ax, bird_y
                mov y_dir, ax
                lea ax, bird_x
                mov x_dir, ax
                call draw_img  
        
        ;Movimiento Obstaculo
        xor ax,ax 
        mov Py,ax
        mov ax, Px
        dec ax
        mov Px, ax 
        ;add ax,Pw
        cmp ax,0
        jg moverObstaculo
        mov ax, ancho
        sub ax,Pw
        mov Px,ax                        
            moverObstaculo:
                DIBUJAR_OBSTACULOS_DATOS Px,Py,40,3
               
        ;Otras verificaciones    
        mov ah, 01h
        int 16h
        jz dibujar_escena 
        cmp al,20h
        je salto
        cmp al, 'q'
        jne  dibujar_escena
        
        
    
    
    ; wait for any key....    
    mov ah, 0x10
    int 16h
    ;activar modo text
    mov ax, 0x0003
    int 10h
    mov ax, 4c00h ; exit to operating system.
    int 21h           
ends

end start ; set entry point and stop the assembler.
