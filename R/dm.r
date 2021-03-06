## -*- coding: utf-8 -*-

## TODO: agregar otros datos pertinentes de los departamentos y
## municipios, tales como nombre de la cabecera departamental
## (municipal) y sus coordenadas, superficie, y otras de interés.

#' Departamento-municipio
#' @description Conectar con la base de datos de departamentos y
#'     municipios
#' @details Los nombres y códigos de los departamentos (dpt,
#'     departamento) y municipios (mun, municipio) están almacenados
#'     en una base de datos SQLite. Los códigos son enteros que
#'     corresponden a la nomenclatura oficial; los códigos de
#'     municipio formados por la concatenación del código del
#'     departamento el código del municipio.
#' @param x character: ruta de la base de datos; si se omite, se lee
#'     de la variable de ambiente DBDEPMUN
#' @return objeto SQLiteConnection o NULL
#' @examples con_dm("c:/path-dir/depmun.sqlite")
#' @keywords internal
#' @author eddy castellón
con_dm <- function(x = character()) {
    stopifnot("arg. x inadmisible" = is.character(x))

    if (is_vacuo(x)) {
        x <- Sys.getenv("DBDEPMUN")
    }
    
    cn <- try(DBI::dbConnect(RSQLite::SQLite(), x),
              silent = TRUE)
    if (inherits(cn, "try-error")) {
        warning("\n... base de datos ", x, " no existe !!!",
                call. = FALSE)
        cn <- NULL
    }
    
    invisible(cn)
}

#' Consultas
#' @description Consulta la base de datos de municipios
#' @param qry character: expresión de consulta SQL
#' @param dbf character: ruta de acceso a la base de datos; por
#'     omisión, la toma de la variable-ambiente DBDEPMUN
#' @return data.frame o NULL
#' @export
qry_dm <- function(qry = character(), dbf = character()) {
    stopifnot("arg. qry inadmisible" = filled_char(qry),
              "admite una consulta" = length(qry) == 1)
    
    db <- con_dm(dbf)
    if (is.null(db)) {
        x <- NULL
    } else {
        ## qry es SQL legítima?
        x <- DBI::dbGetQuery(db, qry)
        DBI::dbDisconnect(db)
    }
    invisible(x)
}

#' Departamentos
#' @description Nombres de los departamentos
#' @param codigos integer: códigos de los departamentos requeridos. Es
#'     opcional. Si se omite se devuelven todos los nombres
#' @param inabr logical: incluir abreviatura en el resultado? TRUE por
#'     omisión
#' @param dbf character: ruta de la base de datos; si se omite, se
#'     obtiene de la variable-ambiente DBDEPMUN
#' @param locale logical: convierte al "encoding" local los nombres
#'     con encoding UTF-8; es TRUE por defecto
#' @return NULL, data.frame
#' @examples
#' departamentos(c(5, 50))
#' @export
#' @author eddy castellón
departamentos <- function(codigos = numeric(), inabr = TRUE,
                          dbf = character(), locale = TRUE) {
    if (inabr) {
        ss <- "select dpt,abr,"
    } else {
        ss <- "select dpt,"
    }
    
    cc <- paste(ss,"departamento",
                "from departamento")

    if (filled_num(codigos)) {
        cc <- paste(cc, "where dpt in(",
                    paste(codigos, collapse = ","), ")")
    }
    cc <- paste(cc, "order by departamento")

    x <- qry_dm(cc, dbf)
    if ( locale ) {
        x["departamento"] <- iconv(x$departamento, "UTF-8", "")
    }

    invisible(x)
}

#' abr-departamento
#' @description Abreviaturas de los departamentos
#' @param codigos integer: códigos de los departamentos requeridos. Es
#'     opcional. Si se omite se devuelven todas
#' @param dbf character: ruta de la base de datos; si se omite, se
#'     obtiene de la variable-ambiente DBDEPMUN
#' @param iso logical: devolver abreviatura ISO?; FALSE por omisión
#' @return data.frame
#' @export
#' @examples
#' abr_departamentos(c(5, 50))$abr #-> "NS" "MG"
#' abr_departamentos(c(5, 50), iso = TRUE)$abr #-> "NI-NS", "NI-MN"
abr_departamentos <- function(codigos = numeric(), dbf = character(),
                              iso = FALSE) {

    if ( iso ) {
        ss <- "select dpt,iso as abr"
    } else {
        ss <- "select dpt,abr"
    }
    
    cc <- paste(ss,
                "from departamento",
                "order by dpt")

    if (filled_num(codigos)) {
        cc <- paste(cc, "where dpt in(",
                    paste(codigos, collapse = ","), ")")
    }

    x <- qry_dm(cc, dbf)
    invisible(x)
}

#' Municipios
#' @description devuelve data.frame con los códigos y nombres de los
#'     municipios y los departamentos a los que pertenecen
#' @param dpt numeric: códigos de los departamentos; por omisión,
#'     todos los municipios
#' @param dbf character: ruta de la base de datos; si se omite, se
#'     obtiene de la variable-ambiente DBDEPMUN
#' @param locale logical: convierte al "encoding" local, los nombres
#'     con encoding UTF-8; es TRUE por defecto
#' @return data.frame o NULL
#' @examples
#' municipios()
#' @export
#' @author eddy castellón
municipios <- function(dpt = integer(), dbf = character(),
                       locale = TRUE) {
    stopifnot("arg. dbf inadmisible" = is.character(dbf),
              "arg. dpt inadmisible" = is.numeric(dpt))
    if (is_vacuo(dpt)) {
        cc <- paste("select a.mun, a.municipio, b.departamento",
                    "from municipio a, departamento b",
                    "where a.dpt = b.dpt",
                    "order by a.mun")
    } else {
        cc <- paste("select mun, municipio from municipio",
                    "where dpt in(",
                    Reduce(function(...)paste(..., sep = ","), dpt),
                    ")")
    }
    
    x <- qry_dm(cc, dbf)
    if ( locale ) {
        x["municipio"] <- iconv(x$municipio, "UTF-8", "")
        if ( is.element("departamento", names(x)) ) {
            x["departamento"] <- iconv(x$departamento, "UTF-8", "")
        }
    }

    invisible(x)
}

#' Validar-Dpto-Muni
#' @description Comprueba si el código o nombre del departamento o
#'     municipio está registrado en la base de datos
#' @param x código (numeric) o nombre (character)
#' @param dfm data.frame con el código (integer) y el nombre
#'     (character) del departamento o municipio
#' @param ccod character: nombre de la columna del data.frame con los
#'     códigos del departamento o municipio; por omisión "mun"
#' @param cnom character: nombre de la columna del data.frame con los
#'     nombres de los departamentos o municipios; por omisión
#'     "municipio"
#' @return logical
#' @examples
#' mun <- municipios()
#' es_dm(505, mun)
#' es_dm("jalapa", mun, cnom = "municipio")
#' @keywords internal
es_dm <- function(x, dfm, ccod = "mun", cnom = "municipio") {
    stopifnot("arg. x inválido" = filled_num(x) || filled_char(x),
              "arg. dfm inválido" = is.data.frame(dfm))
    nc <- names(dfm)
    stopifnot("arg. ccod inválido" = is.element(ccod, nc),
              "arg. cnom inválido" = is.element(cnom, nc),
              "arg. dfm inválido" = is.integer(dfm[[ccod]]),
              "arg. dfm inválido" = is.character(dfm[[cnom]]))

    if (is.numeric(x)) {
        mm <- dfm[[ccod]]
    } else {
        mm <- solo_letras_ascii(dfm[[cnom]])
        x  <- solo_letras_ascii(x)
    }
    is.element(x, mm)
}

#' Municipio-válido
#' @description Comprueba si el código o nombre del municipio está en
#'     la base de datos de municipios
#' @param x código (numeric) o nombre del municipio (character)
#' @param dbf character: ruta de la base de datos de los municipios;
#'     por omisión, tomado de la variable-ambiente DBDEPMUN
#' @return logical
#' @seealso \code{municipios}
#' @export
es_municipio <- function(x, dbf = character()) {
    stopifnot("arg. x inadmisible" = filled_num(x) || filled_char(x),
              "arg. dbf inadmisible" = is.character(dbf))

    es_dm(x, municipios(dbf = dbf))
}

#' Departamento-válido
#' @description Comprueba si el código o nombre del departamento está
#'     en la base de datos de departamentos
#' @param x código (numeric) o nombre (character) del departamento
#' @param dbf character: ruta de la base de datos de los
#'     departamentos; por omisión, se toma de la variable-ambiente
#'     DBDEPMUN
#' @seealso \code{departamentos}
#' @export
#'
es_departamento <- function(x, dbf = character()) {
    stopifnot("arg. x inadmisible" = filled_num(x) || filled_char(x),
              "arg. dbf inadmisible" = is.character(dbf))

    es_dm(x, departamentos(dbf), "dpt", "departamento")
}

#' Municipio-código
#' @description Genera el código "extendido" de un municipio,
#'     concatenando los códigos de departamento y municipio
#' @details Se espera que los parámtros dp y mu tengan el mismo número
#'     de elementos; pero si difieren, el que tenga menos es
#'     "replicado" tantas veces como sea necesario.
#' @param dp integer: código del departamento
#' @param mu integer: código del municipio
#' @param validar logical: verificar resultado es código válido? FALSE
#'     por omisión
#' @param dbf character: ruta de la base de datos de municipios
#' @return integer
#' @seealso \code{municipios}
#' @examples
#' dptmun(5, 5) #-> 505
#' dptmun(5, c(5, 20)) #-> c(505, 520)
#' @export
#' @author eddy castellón
dptmun <- function(dp = integer(), mu = integer(), validar = FALSE,
                      dbf = character()) {
    stopifnot(exprs = {
        filled_num(dp)
        filled_num(mu)
        all(vapply(dp, num_entre, TRUE, 0, 100))
        all(vapply(mu, num_entre, TRUE, 0, 100))
        is.character(dbf) })

    nd <- length(dp)
    nm <- length(mu)
    if (nd > nm) {
        mu <- rep(mu, length.out = nd)
    } else {
        if (nd < nm) {
            dp <- rep(dp, length.out = nm)
        }
    }
    mm <- mapply(concatenar_int, dp, mu, USE.NAMES = FALSE)

    if (validar) {
        dm <- municipios(dbf = dbf)
        if (is.null(dm)) {
            warning("\n... imposible validar !!!", call. = FALSE)
        } else {
            ii <- !es_dm(mm, dm)

            if (any(ii)) {
                warning("\n...", sum(ii), " códigos inválidos", call. = FALSE)
                mm[ii] <- NA_integer_
            }
        }
    }
    mm
}
