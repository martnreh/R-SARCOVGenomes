#Funciones de actividad previa. El presente archivo permite reutilizar
Genera_ADN <- function(n){
    
    fuente <- c("A", "T","G", "C")
    secuencia <- vector()
    
    for(i in 1:n){
        secuencia <- c(secuencia, sample(fuente,1))
        
    }
    return(secuencia)
}

Size_ADN <- function(ADN){
    return(length(ADN))
}

Split_Secuencia <- function(Secuencia){
    
    return(unlist(strsplit(as.character(Secuencia),"")))
    
}

Genera_Complementaria <- function(Hebra, ARN = FALSE  , void = TRUE, Print = FALSE){
    
    if(ARN == FALSE){
        complemento <- c("A","T","G","C")
        names(complemento) <- c("T","A","C","G")  
    }else{
        complemento <- c("A","U","G","C")
        names(complemento) <- c("U","A","C","G")
    }
    
    
    hebra_complemento <- vector()
    
    for(i in 1:length(Hebra) ){
        
        for(j in 1:length(complemento)){
            
            if(Hebra[i] == complemento[j]){
                hebra_complemento[i] <- names(complemento)[j]
            }
        }
    }
    if(Print == TRUE){
    print(paste(c("Hebra directa: ","5'-",Hebra,"-3'"),collapse = ""))
    print(paste(c("Hebra complemento: ","3'-",hebra_complemento,"-5'"),collapse = ""))
    
    }
    if(void == FALSE){
    return(hebra_complemento)
    }
    
}

Invertir_Hebra <- function(Hebra, complementaria = FALSE, void = TRUE){
    
    inversa <- rev(Hebra)
    
    if(complementaria == FALSE){
        print( paste(c("Hebra directa original: ","5'-",Hebra,"-3'"),collapse = ""))
        
        print(paste(c("Hebra directa inversa 3'-",inversa, "-5'"),collapse = ""))
        
    }else{
        print( paste(c("Hebra complementaria original: ","3'-",Hebra,"-5'"),collapse = ""))
        print(paste(c("Hebra complementaria inversa","5'-",inversa, "-3'"),collapse = ""))
    }
    
    if(void == FALSE){
    (return(inversa))
    }
    
}
Porcentajes_ADN <- function(ADN , ARN = FALSE, Nombre = "", Print=FALSE){
    
    if(Print ==TRUE ) cat("\n","\n ","Tama?o de Sequencia ",Nombre,": ", Size_ADN(ADN),"\n")
    
    
    conteo <- c(0,0,0,0)
    
    if(ARN == FALSE){
        names(conteo) <- c("A", "T","G", "C")
        
    }else{
        
        names(conteo) <- c("A", "U","G", "C") 
    }
   
    
    for(i in ADN){
        
        conteo[i] <- conteo[i] + 1
    }
    
    
    
    
    if(Print ==TRUE )  print(conteo)
    for(i in 1:4){
        conteo[names(conteo)[i]] <- (conteo[names(conteo)[i]])*100/length(ADN)
        
    }
    
    if(Print ==TRUE )  cat("\n","Porcentajes (%): ","\n", conteo)
   
    return(conteo)
}

TRADUCE_SECUENCIA <- function(seq, Print = FALSE, ARN = FALSE){
    
    if(ARN == FALSE){
        resultado <- replace(seq, seq == "T", "U")
    }else{
        resultado <- replace(seq, seq == "U", "T")
    }
    
    if(Print == TRUE){
        
        print("Antes: ")
        show(seq)
        print("Ahora: ")
        show(resultado)
    }
    return(resultado)
}

Elimina_N <- function(seq){
    
    referencia <- c("A", "T", "U", "G", "C")
    Solo_Bases <- vector()
    pos <- 1
    
    for(i in 1:length(seq) ){
        
        if( seq[i] %in% referencia ){
            
            Solo_Bases[pos] <- seq[i]
            pos <- pos + 1
            
        }else{
            next()
        }
    }
    
    return(Solo_Bases)
    
}
#------------------------------------
Cuenta_Bases <- function(secuencia, ARN = FALSE){
    conteo <- c(0,0,0,0)
    
    if(ARN == FALSE){
        names(conteo) <- c("A", "T","G", "C")
        
    }else{
        
        names(conteo) <- c("A", "U","G", "C") 
    }
    
    for(i in secuencia){
        
        conteo[i] <- conteo[i] + 1
    }
    
    return(conteo)
    
}



