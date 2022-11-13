.model tiny 

.data
    msg db "Proyecto Flapy Bird!!!!!",10,13,"$"
.code
    .startup
    mov dx, offset msg
    mov ah, 09h
    int 21h
      
    mov ah, 00h
    int 16h
    .exit
end    