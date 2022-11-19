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
    ;Archivos 
    HandleE dw ?    ; Handle para el archivo de entrada 

    Buffy db ?      ; procesaremos caracter por caracter                
    buff  db "$"
     
    NombreE db "SCORE.TXT",0
    msgerror db "Error al procesar$"  
    msgPause db "Presione una Tecla Para Continuar...",13,"$"
    
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
    nivel db 0
    
    ;Score
    msgvidas db "",03,"$"
    msgret   db "",13,"$"
    msgGameOver db "Fin del Juego",10,13,"$"
    msgscrfin db "Puntuacion:",10,13,"$"
    
    msgscr db 10,13,"Top 10 de Puntuaciones", 10,13,"$"
    hrt dw 3 ;3 Vidas
    scr db 0 ;Score
    choquef db 0 ;Si hay Colision
    choqueseg db ?  
    ctrSeg    dw ?
    coloIn    db 54
    scr2      db ?

ends

stack segment
    dw   128  dup(0)
ends

code segment
    jmp start 
    
;********************************************************    
;Imprime el numero que este en AX 
    printAX proc
    ; imprime a la salida estandar un numero que supone estar en el AX
    ; supone que es un numero positivo y natural en 16 bits.
    ; lo imprime en decimal.  
    
        push AX
        push BX
        push CX
        push DX
    
        xor cx, cx
        mov bx, 10
    ciclo1PAX: xor dx, dx
        div bx
        push dx
        inc cx
        cmp ax, 0
        jne ciclo1PAX
        mov ah, 02h
    ciclo2PAX: pop DX
        add dl, 30h
        int 21h
        loop ciclo2PAX
    
        pop DX
        pop CX
        pop BX
        pop AX
        ret
    printAX endP    
;########################################################    
    
                                            
                                            
    
;********************************************************
;Imprime lo que venga en el Buffer
;El buffer contiene el byte que acaba de leer del archivo 
;Si encuentra un 0, coloca un salto de linea
;Si encunetra otro numero, entonces lo pinta en pantalla
    printBuff proc
        PUSH dX
        PUSH ax
        xor dx,dx
        
        
        ; El archivo esta compuesto por bytes, existen bytes de puntuacion 
        ;  y bytes que indican cuando un byte ya se ha leido.
        ;El archivo esta escrito de la siguinete manera 
        ; 01 00 01 00 01 00 01 00 01 00 01 00 01 00 01 00 01 00 
        ; Los bytes pares guardan siempre 0 y los bytes impares guardan la puntuacion
    
        
        ;Nos damos cuenta si en el buffer viene un 0 
        mov dl, Buffy 
        cmp dl, 0
        jne imprima
            
            ;Si es 0 entonces escribe un salto de linea 
            mov ah, 9
            mov dx, offset Buffy
            mov Buffy, 0Ah
            int 21h  
            ; y un retorno de carry   
            mov Buffy, 0Dh
            int 21h
            jmp salPmsg
       
        imprima:
            ;Antes de imprimir el numero verifica que 
            ; si puede guardar el score actual en esa posicion.
            ;Osea si el Score>Buffer lo guarda en esa posicion en el archivo 
            ; ya que el score actual es mayor que lo que esta leyendo 
            
            mov al, scr      ;Mueve el score actual al AL 
            mov ah, Buffy    ;Mueve el numero que lee del archivo al AH
            cmp al,ah        
            jle printNumScr  ;Si el Buffer es mayor lo imprime 
            
            ;Pero si el score es mayor
            ; vamos a sobre escribir el numero del archivo con el nuevo score   
            
            ;Para esto primero hay que mover el puntero a la posiicon anterior, 
            ; ya que cuando se lee un byte del archivo, el puntero queda en la posicion siguiente, 
            ; y si se intenta sobreescribir, va a escribir sobre el 00 y no sobre la puntuacion 
            ; que esta en el archivo. 
             
            mov bx, HandleE ;Mueve el Handle al BX
                            ;Sobre Escribe el numero si es menor al scr 
                            ;Moverse una posicion atras
            ;Vamos a utilizar la funcion SEEK 42h de la 21h, con al el 1, lo que nos devuelve el el ax, la posicion 
            ; actual del puntero 
            mov ah, 42h     
            mov al, 1
            mov cx,0
            mov dx,0
            int 21h         ;Nos devuelve la posicion actual del puntero 
                            ; osea si estamos tratando de sobreescribir el byte 0, esto nos devuelve la posicion 1, osea el siguiente byte, 
                            ; por eso devemos devolvernos una posicion atras para sobreescribir el byte 0 del archivo. 
            
            
            dec ax          ;Decrementamos el AX, osea nos devolvemos una posicion 
            mov dx, ax      ;Indicamos en el DX, el offset que se debe mover el puntero, desde el origen hasta lo que tenga AX,
                            ; en nuestro ejemplo, le indicamos que se va a mover 0 bytes desde el origen    
            mov ah, 42h
            mov al,0
            int 21h         ;Cambia el puntero del archivo a la posicion anterior 
            
            ;Ahora, como vamos a ingresar un nuevo highscore, devemos desplazar la lista hacia abajo 
            ;Por lo tanto, guardamos el score anterior y en la siguiente iteracion lo guardamos como un nuevo score,
            ; pero en la siguiente posicion de la lista
            mov al, scr     ;Mueve el Score actual al AL
            mov ah, Buffy   ;Mueve el Buffer al AH 
            ;mov scr2, ah
            mov scr, ah     ;Guardamos el Score anterior en local para escribirlo en la siguiente pasada 
            mov Buffy, al   ;Guardamos el Score actual en el buffer 
            mov ah, 40h     ;Utilizamos la funcion para guardar 
            mov cx, 1       ;Indicamos que solo vamos a guaradr un byte
            lea dx, Buffy   ;Posicion del Buffer en memoria 
            mov bx, HandleE ;Handle del archivo 
            int 21h         ;Guardamos el nuevo hig score en el archivo 
            
            
            printNumScr:
           
            ;Mandamos a imprimir el numero que esta en el buffer 
            xor ax,ax
            mov al,Buffy             
            call printAX
        
        salPmsg:
        pop ax
        POP dX
        ret
    endp
;########################################################    
    
                                            
                                            
    
;********************************************************
;Abre el archivo con el nombre guardao en el HandleE
;Lo abre en modo 2, lectura y escritura, ya que lo vamos a ir
; modificando sobre la marcha 
    abrirFile proc
        push ax
        push dx
        
        mov ah, 3Dh   ; abrir el archivo de score 
        mov al, 2     ; codigo 2 para abrirlo como lectura escritura
        lea dx, nombreE
        int 21h
        mov handleE, ax 
        
        pop dx
        pop ax
        ret
    endp
;########################################################    
    
                                            
                                            
    
;********************************************************
;Funcion para cerrar el archivo, se ejecuta antes de cerrar el programa 
    cerrarFile proc 
    ;Cierra el archivo 
        push ax
        push bx
        
        mov ah, 3Eh ; cerrar el archivo de entrada
        mov bx, handleE
        int 21h
        
        pop bx
        pop ax
        ret
        
    endp
    
;########################################################    
;Imprime un mensaje en pantalla
;Debe venir la "variable" en el dx    
    printMSG proc
        push dx
        push ax
       ;Si existe algun error muestra un mensaje  
        
        mov ah, 09h
        int 21h
            
        pop ax
        pop dx
        ret
    endp                                          
                                            
    
;********************************************************
;Cuando el pajarito pierde las 3 vidas, se llama este procedimiento 
; que se encarga de imprimir el Score Board y ademas lo actualiza,
; cuando el score actual es un highscore   
    printScore proc 
        xor ax,ax
        xor cx,cx
        xor dx,dx
        xor bx,bx
        
        ;Abre el Archivo que contiene los 10 scores mas altos 
        ; El archivo esta compuesto por bytes, existen bytes de puntuacion 
        ;  y bytes que indican cuando un byte ya se ha leido.
        ;El archivo esta escrito de la siguinete manera 
        ; 01 00 01 00 01 00 01 00 01 00 01 00 01 00 01 00 01 00 
        ; Los bytes pares guardan siempre 0 y los bytes impares guardan la puntuacion
        
        ;Aca se abre el archivo
        call abrirFile 
        proceso:
    
          mov ah, 3Fh       ;Leemos el caracter del archivo
          mov cx, 1         ;Cada vez que se lee, el puntero se posiciona en el siguiente byte 
          lea dx, Buffy
          mov bx, handleE
          int 21h
          jnc esfin  
          jmp error
        esfin:  
          cmp ax, 1   ; averigua si llego al final del archivo
          jne cerrar      
     

        escribir:
        ;Escribe en pantalla         
        call printBuff       
        
        jmp proceso
        
        cerrar:      
            call cerrarFile 
            jmp salirPscr
            
        error:
            ;Si existe algun error muestra un mensaje
            lea dx, msgerror
            call printMSG   
    
        salirPscr:
    
       ret
        
    endp    
;########################################################    
                                                  
    
;********************************************************   
;Procedimiento que lee la informacion de la imagen
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
;########################################################    
    
                                            
                                            
;********************************************************     
;********************************************************
;Macro y  Procedimiento que dibujan secciones de la pantalla   
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
    
    ;Procedimiento que dibuja en pantalla
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
;########################################################    
    
                                            
                                            
;********************************************************     
;********************************************************
;Macro y  Procedimiento que dibuja obstaculos en pantalla     
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
                                        
                                            
    ;Procedimiento para dibujar Obstaculos
    draw_obstaculos proc
        push DX
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
            cmp ax, Pl
            jl  verde    ;Si es menor que PL lo pinta de verde
                         ;Fijarse si es menor que PL + bird_h + 10
            mov dx, PL
            add dx, bird_h
            add dx, 10
            cmp ax, dx 
            
            jg verde:    ;Si es mayor que PL + bird_h +10 lo pinta verde
            mov Pc,54    ;Si no, lo pinta azul
            jmp seguir
            
            verde:       ;Cammbia el color a verde 
                mov Pc,3
            seguir:
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
        pop DX  
        ret    
    endp
;########################################################    
    
                                            
                                            
;********************************************************
;Procedimiento que dibuja la imagen del bird    
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
        cmp al,57h; "W"
        JE pressSpace  
        cmp al,77h; "w"   
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
    
    sleep proc 
        PUSH AX
        PUSH CX
        PUSH DX
        
        ;INT 21h / AH=2Ch - get system time;
        ;return: CH = hour. CL = minute. DH = second. DL = 1/100 seconds.
        mov ah, 0x2C
        int 21h
        mov ultimo_s, dL
        ;add ultimo_s, 2 
        esperar:
            int 21h
            cmp dl, ultimo_s
            je esperar
        
        POP DX
        POP CX
        POP AX
        RET
    ENDP                                                 
;########################################################    
    
    
;********************************************************
;Procedimiento que revisa los eventos de choque con los pipes 
 
    rev_col proc
        PUSH AX
        PUSH BX
        Push CX          
        
        ;Revisa si el pajarito pega con el suelo                  
        mov ax, bird_y
        add ax, bird_v 
        add ax,bird_h
        cmp ax,149
        jle suiz
        jmp choque
                 
        ;Revisa la colision superior izquierda
        suiz:
        mov ax, bird_x
        add ax, bird_w
        cmp ax, Px
        jl salga
        ;Si es mayor hay que ver si esta entre [Px, Px+30]
        mov bx, Px
        add bx, 30
        cmp ax,bx
        je sumPto
        jg salga
        ;Aca sabemos que esta entre [Px, Px+30]
        ;Entonces hay que ver si colisiona arriba o abajo
        xor ax,ax
        xor bx,bx
        mov bx, Pl
        sub bx, 5
        mov ax, bird_y
        cmp ax,bx
        jg verabj
        jmp choque 
        ;Si es mayor, hay que ver 
        
        ;Verifica si choca abajo
        verabj:
        xor ax,ax
        xor bx,x
        mov bx,Pl
        add bx,bird_h
        add bx,10
        mov ax, bird_y
        cmp ax,bx
        jl salga
        
        choque:
        ;Decrementar 1 vida
        
       
        inc choquef    ;Activa la Bandera de Choque
        mov ctrSeg, 40 ;Va a esperar 2 segundos antes de volver a revizar la colision 
        dec hrt        ;Decrementa 1 Vida  
        jnz corazones
        
        ;Pantalla de Score
        
        ;activar modo text
        mov ax, 0x0003
        int 10h        
        
        lea dx,msgGameOver
        call printMSG
        
        lea dx, msgscrfin
        call printMSG
        
        xor ax,ax
        mov al, scr
        call printax
        
        lea dx, msgscr
        call printMSG
        
        
        
             
        call printScore ;Imprime el Score
        
        mov ax, 4c00h ; exit to operating system.
        int 21h      
        
        jmp salga
        ;Sumar un punto al Score
        sumPto:
        inc scr
        
        corazones:
        xor cx,cx
        
        ;Imprime los corazones 
        mov cx, hrt
        printVidas:
        lea dx, msgvidas
        call printMSG 
        loop printVidas
        lea dx, msgret
        call printMSG
        salga:
        
        POP CX
        POP BX
        POP AX
        RET 
    ENDP 
;########################################################
                                                         
                                                         
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
    
    ;--------------------------------------------------------
    ;Dibuja todo la primera vez 
    DIBUJAR_RECT_DATOS 0, 0, ancho, 150, 54 
    DIBUJAR_RECT_DATOS 0, 150, ancho, 50, 10 
    DIBUJAR_OBSTACULOS_DATOS 220,0,40,3
    ;--------------------------------------------------------
     
    
    dibujar_escena:
        ;INT 21h / AH=0Ch - flush keyboard buffer and read standard input.
        mov ah, 0ch
        mov al, 0
        int 21h                                                  
        
        ;--------------------------------------------------------        
        ;Revisa colision
 
        mov al, choquef ;Si la bandera choquef esta activada, salta la inmunidad
        cmp al, 1
        je inmunidad    ;Salta a la Inmunidad
        
        call rev_col    ;Si la bandera esta en 0 revisa colision
        jmp noInm
         
      
        inmunidad:
        ;Captura el segundo, lo campara con el que esta guardado
        ; si este llega a ser diferente, acaba de pasar un segundo
        ;Cuando pasa un segundo, se decrementa el contador de segundos
        ;Cuando este contador llegue a cero cambia la bandera 
        ; indicando que se vuelve a revizar si existe colision
        ;Mientras que la bandera este activada no se revisan colisiones
        
        mov coloIn,34         ;Cambia el color de fondo del Bird
        mov ah, 0x2C
        int 21h               ;Guarda el Segundo en Dh 
        cmp choqueseg, dh     ;Si el segundo en choqueseg es diferente al de DH
        jle restaunSeg        ; resta un segundo al contador
        mov choqueseg, dh     ;Guarda el segundo en choqueseg
        jmp salirCon
        restaunSeg:
            dec ctrSeg        ;Decrementa el contador
            jnz salirCon      ;No cambia la bandera hasta que el contador sea cero
            dec choquef       ;Cambia la bandera, ya no hay inmunidad
            mov coloIn,54     ;Cambia el color de fondo del bird
        salirCon:
        noInm:
        ;--------------------------------------------------------
        
        
        
        ;--------------------------------------------------------
        ;Cada frame pinta el pajarito en azul
        DIBUJAR_RECT_DATOS bird_x, bird_y, bird_w, bird_h, coloIn
        ;Cada frame "barre" el pipe
        mov ax, Px
        add ax, 30
        DIBUJAR_RECT_DATOS ax, 0, Pw, 150, 54
        ;--------------------------------------------------------
                                                                 
                                                                 
        ;--------------------------------------------------------
        ;Cada frame se detiene un 1/100 segundo
        call sleep
        ;--------------------------------------------------------
        
        
        ;--------------------------------------------------------
        ;Se mueve el bird hacia arriba o hacia abajo
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
        ;--------------------------------------------------------
          
          
        ;--------------------------------------------------------
        ;Mueve el pipe un pixel a la izquierda
        ;Movimiento Obstaculo
        xor ax,ax 
        mov Py,ax
        mov ax, Px
        dec ax
        mov Px, ax 
        cmp ax,0              ;Si lleguo al final de la pantalla
        jge moverObstaculo    ;Lo Pinta de nuevo al lado derecho
        mov ax, ancho
        sub ax,Pw
        mov Px,ax
        ;Random del PL
        
        mov dl, nivel
        mov ax, 50
        mul dx
        cmp ax, 150
        jge resNivel  
        mov Pl, ax
        inc nivel
        jmp moverObstaculo        
        resNivel:
        mov nivel, 0
        mov ax,0
        mov Pl,ax       
                                
            moverObstaculo:
                DIBUJAR_OBSTACULOS_DATOS Px,Py,Pl,3    
        ;--------------------------------------------------------
        
            
        
        ;--------------------------------------------------------       
        ;Otras verificaciones    
        mov ah, 01h
        int 16h
        jz dibujar_escena 
        cmp al,57h    ;W
        je salto         
        cmp al,77h    ;w
        JE salto
        cmp al,51h    ;Q
        jne  dibujar_escena
        ;Pausa
        
        lea dx, msgPause
        call printmsg
        
        mov ah, 0ch
        mov al, 0
        int 21h
         
        mov ah, 7
	    int 21h
	    DIBUJAR_RECT_DATOS 0, 0, ancho, 150, 54
	    jmp dibujar_escena
        ;--------------------------------------------------------
 
    
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
