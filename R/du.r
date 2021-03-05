## -*- coding: utf-8 -*-

## private functions

#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_vacuo <- function(x) {
    length(x) == 0
}

#' length
#' @description vector has length greater than zero?
#' @param x vector
#' @return logical
#' @author eddy castellón
filled <- function(x) {
    length(x) > 0
}

#' character type
#' @description vector is of character type and has elements?
#' @param x vector
#' @return logical
filled_char <- function(x) {
    is.character(x) && length(x)
}

#' numeric mode
#' @description vector is of numeric mode and has elements?
#' @param x vector
#' @return logical
filled_num <- function(x) {
    is.numeric(x) && length(x)
}

#' integer type
#' @description vector is of integer type and has elements?
#' @param x vector
#' @return logical
filled_int <- function(x) {
    is.integer(x) && length(x)
}

#' Número-entre
#' @description Comprueba si un número está entre los límites de un
#'     intervalo
#' @param x numeric
#' @param x1 numeric: límite inferior
#' @param x2 numeric: límite superior
#' @param inclusive logical: con igualdad a uno de los límites?; FALSE
#'     por omisión
#' @return logical
#' @export
num_entre <- function(x, x1 = numeric(), x2 = numeric(),
                      inclusive = FALSE) {
    stopifnot("arg. x inválido" = filled_num(x),
              "arg. x1 inválido" = filled_num(x1),
              "arg. x2 inválido" = filled_num(x2),
              "args. x, x1 incomp" = length(x) == length(x1),
              "args. x, x2 incomp" = length(x) == length(x2))
    
    tf <- x > x1 & x < x2
    if (inclusive) {
        tf  <- tf | x == x1 | x == x2
    }

    tf
}

#' Concatenar-int
#' @description Concatenar enteros
#' @param x integer: entero al inicio
#' @param y integer: entero al final
#' @param desplaza integer: número de posiciones decimales
#'     "a la izquierda" que es desplazado el primer número
#' @return integer
#' @export
#' @examples
#' concatenar_int(2, 3) #-> 203
#' concatenar_int(2, 3, 3) #-> 2003
concatenar_int <- function(x, y, desplazar = 2L) {
    stopifnot(exprs = {
        filled_num(x)
        filled_num(y)
        filled_num(desplazar) && desplazar > 0})

    desplazar <- as.integer(desplazar)
    as.integer(x) * (10 ^ desplazar) + as.integer(y)
}


#' Remover espacios
#' @description Remueve todos lo espacios, final de línea o tabulador,
#'     de una frase
#' @param x frase
#' @export
sin_sp <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    gsub("[[:space:]]+", "", x)
}

## no requiere stringr

#' Sin tilde
#' @description Sustituye letras con acento por las equivalentes sin
#'     acento
#' @param x palabra con o sin acentos
#' @export
sin_tilde <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    vv <- c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u",
            "Á"="A", "É"="E", "Í"="I", "Ó"="O", "Ú"="U")
    while (any((mm <- regexpr("[ÁáÉéÍíÓóÚú]", x)) > -1L)){
        regmatches(x, mm) <- vv[regmatches(x, mm)]
    }
    x
}

#' Sin diéresis
#' @description Sustituye letras con diéresis por las equivalentes sin
#'     diéresis
#' @param x palabra con o sin diéresis
#' @export
sin_dieresis <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    vv <- c("ü"="u", "Ü"="U")
    while (any((mm <- regexpr("[Üü]", x)) > -1L)){
        regmatches(x, mm) <- vv[regmatches(x, mm)]
    }
    x
}

#' String-ASCII
#' @description Devuelve la secuencia de letras minúsculas, sin
#'     espacios, acentos o diéresis, de una frase
#' @param x character
#' @return character
#' @keywords internal
#' @examples
#' str_ascii("  éEmn üs") #-> "eemnus"
str_ascii <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    tolower(x) %>% sin_sp() %>%
        sin_tilde() %>% sin_dieresis()
}

solo_letras_ascii <- function(x = character()) str_ascii(x)
