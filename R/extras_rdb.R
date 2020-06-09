# ----------------------------------------------------------------------------------------
# Funções para tratamento de dados
# ----------------------------------------------------------------------------------------

#' Remove acentos de caracteres.
#'
#' @description Remove acentos agudos, circunflexos, tremas, crases e cedilhas de Caractéres e strings.
#' @param str vetor de characters.
#' @return Vetor de characters sem acentos e sem cedilhas.
#' @examples
#' \dontrun{
#' palavras <- c("\u00e1gua", "op\u00e7\u00e3o", "\u00ff\u00c0\u00ea")
#' rdb_rm_accent(palavras)
#' }
#' @importFrom stringi stri_unescape_unicode stri_escape_unicode
#' @export
rdb_rm_accent <- function(str) {
  if(!is.character(str)) {
    str <- as.character(str)
  }
  str <- stringi::stri_escape_unicode(str)
  symbols <- c(
    acute = stringi::stri_unescape_unicode('\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00fd\u00dd'),
    grave = stringi::stri_unescape_unicode('\u00e0\u00e8\u00ec\u00f2\u00f9\u00c0\u00c8\u00cc\u00d2\u00d9'),
    circunflex = stringi::stri_unescape_unicode('\u00e2\u00ea\u00ee\u00f4\u00fb\u00c2\u00ca\u00ce\u00d4\u00db'),
    tilde = stringi::stri_unescape_unicode('\u00e3\u00f5\u00c3\u00d5\u00f1\u00d1'),
    umlaut = stringi::stri_unescape_unicode('\u00e4\u00eb\u00ef\u00f6\u00fc\u00c4\u00cb\u00cf\u00d6\u00dc\u00ff'),
    cedil = stringi::stri_unescape_unicode('\u00e7\u00c7')
  )
  nudeSymbols <- c(acute = 'aeiouAEIOUyY', grave = 'aeiouAEIOU', circunflex = 'aeiouAEIOU', tilde = 'aoAOnN', umlaut = 'aeiouAEIOUy', cedil = 'cC')
  return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), stringi::stri_unescape_unicode(str)))
}

#' Substitui NA numerico ou char.
#'
#' @description Substitui NA por algum valor fornecido ou padroniza com NA.
#' @param x Vetor qualquer
#' @param from Qual o codigo que quer remover.
#' @param to Qual o valor a substituir
#' @details Quando from = NULL, padrão, busca em numérico os valores  'NA, NaN, NULL, -Inf, Inf' e
#' quando character busca por '','.','NA','na','N/A','n/a','NaN','nan' e troca pelo valor de
#' to. Quando to = NULL, padrão, substitui pelo padrão do R que é NA.
#' @return Vetor x com as devidas alteracoes
#' @export
rdb_rm_na <- function(x, from = NULL, to = NULL){
  i <- x
  codes <- if(is.numeric(i)){
    c(NA, NaN, NULL, -Inf, Inf)
  } else if(!is.numeric(i)) {
    if(is.logical(i)){
      return(i)
    } else {
      c('','.','NA','na','N/A','n/a','NaN','nan',NA)
    }
  }
  if(is.null(from)){
    if(is.null(to)){
      i[which(i %in% codes)] <- NA
    } else {
      i[which(i %in% codes)] <- to
    }
  } else {
    if(is.null(to)){
      i[which(i %in% from)] <- NA
    } else {
      i[which(i %in% from)] <- to
    }
  }
  return(i)
}


#' Limpa texto de e-mail baseado em regex
#'
#' @description Aplica-se uma regex bem formatada para checar se as strings contendo dados de e-mails
#' estão corretas. Se houver alguma alguma string que não bate com a regex então esta é removida
#' e tratada como não e-mail ou e-mail inválido.
#' @param email vetor de characters.
#' @param rm_accent lógico indicando se deve buscar e remover acentos em email
#' @param fix_na se tiver algum tipo de na no vetor "email" será removido
#' @return Vetor de characters detectados como e-mail
#' @importFrom stringr str_squish
#' @export
rdb_limpa_email <- function(email, rm_accent = TRUE, fix_na = TRUE){
  if(rm_accent) {
    x <- rdb_rm_accent(email)
  }
  if(fix_na) {
    x <- rdb_rm_na(x)
  }

  l <- grep("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE, value = TRUE)
  x[!x %in% l] <- NA

  return(stringr::str_squish(x))
}

#' Verifica se a string eh email
#'
#' @description Aplica-se uma regex bem formatada para checar se as strings contendo dados de e-mails
#' está correta. Se houver alguma alguma que não bate com a regex então esta é removida
#' e tratada como não e-mail ou e-mail inválido.
#' @param email vetor de characters.
#' @param rm_accent lógico indicando se deve buscar e remover acentos em email
#' @param fix_na se tiver algum tipo de na no vetor será removido
#' @return Vetor de characters detectados como e-mail
#' @importFrom stringr str_squish
#' @export
is_email <- function(email, rm_accent = TRUE, fix_na = TRUE) {
  x <- stringr::str_squish(email)
  if(rm_accent) {
    x <- rdb_rm_accent(x)
  }
  if(fix_na) {
    x <- rdb_rm_na(x)
  }
  return(grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE))
}


#' Testa se o valor eh int64 ou lógico
#'
#' @param i vetor qualqer
#' @return TRUE se o vetor i é de classe integer64
is_int64 <- function(i) {
  if( inherits(i, c("integer64","logical"))) TRUE else FALSE
}

#' Testa se o valor eh data
#'
#' @description Permite avaliar, através do tipo ou do conteúdo, se um vetor de dados recebido é ou não no tipo data
#' @param i vetor qualqer
#' @param check se TRUE (padão) avalia o conteúdo de i e infere se a variável total é de data
#' @param fix_na se tiver algum tipo de na no vetor será removido
#' @return TRUE se o vetor é das classes de data
#' @export
is_date <- function(i, check = TRUE, fix_na = TRUE) {

  if(fix_na){
    i <- rdb_rm_na(i)
  }

  check_structure <- function(i){
    if(sapply(i, function(x) !all(is.na(as.Date(as.character(x), format = c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%Y-%m-%d")))))) TRUE else FALSE
  }
  if(check) {
    if(all(sapply(i, function(x) check_structure(x)))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    if(inherits(i, c("POSIXlt","POSIXct","POSIXt","Date"))) {
      return(TRUE)
    } else {
      FALSE
    }
  }
}

#' Testa se o valor eh numerico
#'
#' @description Permite avaliar, através do tipo ou do conteúdo, se um vetor de dados recebido é ou não no tipo numérico
#' @param i vetor qualqer numérico
#' @param check se TRUE (padão) avalia o conteúdo de i e infere se a variável é de data
#' @param fix_na se tiver algum tipo de NA no vetor será removido
#' @return TRUE se o vetor i é das classes numéricas
#' @importFrom stringr str_squish str_replace_all
#' @export
is_num <- function(i, check = TRUE, fix_na = FALSE){

  if(fix_na){
    i <- rdb_rm_na(i)
  }

  i <- if(check){
    stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  } else {
    if(inherits(i, c("factor","character","integer64"))) {
      as.character(i)
    } else {
      i
    }
  }
  is.numeric(as.numeric(i))
}

#' Testa se o valor eh numerico. Se nao for, converte.
#'
#' @description Verifica se o conteúdo do vetor passado é de números. Quando check = TRUE, a estrutura do dado
#' é testada via regex e se o padrão for detectado, o vetor é definido como numérico.
#' @param i vetor qualquer
#' @param check TRUE checa a estrutura do dado
#' @param fix_na vetor de strings a serem removidas, caso existam.
#' @return vetor numérico tratado a partir de i
#' @importFrom stringr str_squish
#' @export
as_num <- function(i, check = TRUE, fix_na = TRUE){
  i <- if(inherits(i, c("factor","character","integer64"))) {
    as.character(i)
  } else {
    i
  }
  if(fix_na) {
    i <- rdb_rm_na(i)
  }
  if(check){
   l <- stringr::str_squish(grepl("^[\\-\\+]?[0-9]+[\\.]?[0-9]*$|^[\\-\\+]?[0-9]+[L]?$|^[\\-\\+]?[0-9]+[\\.]?[0-9]*[eE][0-9]+$",i,perl=TRUE))
   i[which(l %in% FALSE)] <- NA
  }
  as.numeric(i)
}

#' Testa se o valor eh CPF
#'
#' @description Com apoio do pacote "validaRA" testa se o vetor recebido é um cpf.
#' @param i vetor de CPF. Ver \code{\link[validaRA]{valida_doc}}
#' @param fix_na vetor de strings a serem removidas caso existam.
#' @import validaRA
#' @importFrom stringr str_squish str_replace_all
#' @export
is_cpf <- function(i, fix_na = TRUE){
  if(fix_na){
    i <- rdb_rm_na(i)
  }
  out <- stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  validaRA::valida_doc(as.numeric(out), type = "cpf")
}

#' Testa se o valor eh CNPJ
#'
#' @description Com apoio do pacote "validaRA" testa se o vetor recebido é um CNPJ.
#' @param i vetor de CNPJ. Ver \code{\link[validaRA]{valida_doc}}
#' @param fix_na vetor de strings a serem removidas caso existam.
#' @import validaRA
#' @importFrom stringr str_squish str_replace_all
#' @export
is_cnpj <- function(i, fix_na = TRUE){
  if(fix_na){
    i <- rdb_rm_na(i)
  }
  out <- stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  validaRA::valida_doc(as.numeric(out), type = "cnpj")
}

#' Testa se o valor eh PIS
#'
#' @description Com apoio do pacote "validaRA" testa se o vetor recebido é um PIS
#' @param i vetor de números PIS. Ver \code{\link[validaRA]{valida_doc}}
#' @param fix_na vetor de strings a serem removidas caso existam.
#' @import validaRA
#' @importFrom stringr str_squish str_replace_all
#' @export
is_pis <- function(i, fix_na = TRUE){
  if(fix_na){
    i <- rdb_rm_na(i)
  }
  out <- stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  validaRA::valida_doc(as.numeric(out), type = "pis")
}


#' Testa se o valor eh CEP
#'
#' @description Via regex e limpeza de dados, verifica se i é um cep.
#' @details Olha-se apenas a estrutura. Não validamos se o cep é valido nos correios.
#' @param i vetor de números CEP
#' @param fix_na vetor de strings a serem removidas caso existam.
#' @importFrom stringr str_extract str_replace_all str_squish
#' @export
is_cep <- function(i, fix_na = TRUE){
  if(fix_na){
    i <- rdb_rm_na(i)
  }
  out <- stringr::str_extract(string = i, pattern = "[:digit:]{5}-[:digit:]{3}|[:digit:]+") %>%
    stringr::str_replace_all("[^[:digit:]]", "") %>%
    stringr::str_squish() %>%
    stringr::str_pad(width = 8, pad = 0)

  return(nchar(out) <= 8)
}

#' Limpa CEP
#'
#' @description Via regex e limpeza de dados, verifica-se a estrutura do vetor informado, removendo tudo que não for
#' número e completando com zeros a esquerda casos com menos de 8 digitos. Se o CEP for da forma
#' 99999-999, o traço é removido e o número retorna limpo.
#' não validamos se o cep é valido nos correios.
#' @param cep vetor de números ou strings de CEP
#' @param fix_na padroes de NA removidos do número ou string i
#' @importFrom stringr str_extract str_replace_all str_squish str_pad
#' @export
rdb_limpa_cep <- function(cep, fix_na = TRUE){
  i <- if(fix_na){
    rdb_rm_na(cep)
  } else {
    cep
  }
  i %>%
    stringr::str_extract(pattern = "[:digit:]{5}-[:digit:]{3}|[:digit:]+") %>%
    stringr::str_replace_all("[^[:digit:]]", "") %>%
    stringr::str_squish() %>%
    as_num() %>%
    stringr::str_pad(width = 8, side = "left", pad = "0")
}

#' Limpa CPF/CNPJ
#'
#' @description Com apoio do pacote "validaRA", regexes e avaliação do comprimento das strings no campo de
#' CPF/CNPJ recebido, verifica a validade do campo segundo regras de construcao do CPF/CNPJ e acerta o tamanho.
#' Pontos, barras e tracos sao removidos e apenas um vetor númerico em forma de string é retornado.
#' Ver \code{\link[validaRA]{valida_doc}} para mais detalhes.
#' @param doc vetor de números ou strings de CPF/CNPJ.
#' @param fix_na padrões de NA removidos do número ou string i
#' @importFrom stringr str_replace_all str_squish str_pad str_length
#' @importFrom dplyr tibble mutate case_when select
#' @import validaRA
#' @return tibbla com três colunas, uma com o doc outras com lógico se CPF = TRUE ou CNPJ = TRUE
#' @export
rdb_limpa_cpf_cnpj <- function(doc, fix_na = TRUE) {
  i <- if(fix_na){
    rdb_rm_na(doc)
  } else {
    doc
  }
  out <- stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  out <- dplyr::tibble(doc_ = as_num(out)) %>%
    dplyr::mutate(doc = dplyr::case_when(stringr::str_length(doc_) <= 11 | is_cpf(doc_)  ~ stringr::str_pad(doc_, width = 11, side = "left", pad = "0"),
                                         stringr::str_length(doc_)  > 11 & is_cnpj(doc_) ~ stringr::str_pad(doc_, width = 14, side = "left", pad = "0"),
                                         TRUE ~ NA_character_),
                  cpf  = stringr::str_length(doc_) <= 11 | is_cpf(doc_),
                  cnpj = stringr::str_length(doc_)  > 11 & is_cnpj(doc_)
                  ) %>%
    dplyr::select(-doc_)
  return(out)
}

#' Corrige UTF-8 encoding
#'
#' @description Recebe um vetor string de tipos em formado UTF-8 e tentar corrigir para latin-1
#' desta forma, conservando letras como c cidilhado e acentos. Outros formatos de encoding de
#' saida são suportados.
#' @seealso \code{\link{Encoding}}
#' @param x vetor string
#' @param enc flag destino do encoding.
#' @export
rdb_try_fix_encoding <- function(x, enc = "latin1"){
  if(inherits(x, c("character","factor"))){
    x <- as.character(x)
    y <- iconv(x = enc2utf8(x), from = "UTF-8", to = enc)
    y <- iconv(x = y, from = "UTF-8", to = enc)
  } else {
    y <- x
  }
  return(y)
}

#' Leitura de dados com pacote rio
#'
#' @description Este é apenas um wraper que deixa a função \code{\link[rio]{import}}
#' melhor formatada para passar na função \code{\link{rdb_read}}
#' @param file Nome do arquivo a ser importado
#' @param ... Argumentos
#' @importFrom rio import
#' @export
rdb_read_bf_rio <- function(file, ...){
  #requireNamespace("rio")
  rio::import(file, ...)
}


#' Leitura de dados com pacote data.table
#'
#' @description Este é apenas um wraper que deixa a função \code{\link[data.table]{fread}}
#' melhor formatada para passar na função \code{\link{rdb_read}}
#' @param file Nome do arquivo a ser importado.
#' @param control Controle com todos argumentos necessários para importar dados para uso inteno com a rdb_read()
#' @importFrom data.table fread
#' @export
rdb_read_bf_dt <- function(file, control = rdb_read_control(type = "fread")){
  #requireNamespace("data.table")
  data.table::fread(input = file, sep = control$delim, dec = control$dec, nrows = control$nrows, quote = control$quote,
                    encoding = control$encoding, header = control$header, skip = control$skip, select = control$select,
                    colClasses = control$colClasses
  )
}

#' Leitura de dados com pacote readr
#'
#' @description Este é apenas um wraper que deixa as funções \code{\link[readr]{read_csv2}} e
#' \code{\link[readr]{read_delim}} conveniente para passar na função \code{\link{rdb_read}}
#' @param file Nome do arquivo a ser importado.
#' @param control Controle com todos argumentos necessários para importar dados para uso inteno com a rdb_read()
#' @importFrom readr read_csv2 read_delim
#' @export
rdb_read_bf_rd <- function(file, control = rdb_read_control(type = "readr")){
  #requireNamespace("readr")
  if(is.null(control$delim)){
    readr::read_csv2(file = file, col_names = control$col_names, col_types = control$col_types, locale = control$locale,
                     na = control$na, quote = control$quote, comment = control$comment, trim_ws = control$trim_ws,
                     skip = control$skip, n_max = control$n_max, guess_max = control$guess_max,
                     skip_empty_rows = control$skip_empty_rows)
  } else {
    readr::read_delim(file = file, delim = control$delim, quote = control$quote, escape_backslash = control$escape_backslash,
                      guess_max = control$guess_max, escape_double = control$escape_double, col_names = control$col_names,
                      col_types = control$col_types, locale = control$locale, na = control$na, trim_ws = control$trim_ws,
                      skip = control$skip, n_max = control$n_max
    )
  }
}

#' Leitura de dados com pacote vroom
#'
#' @description Este é apenas um wraper que deixa a função \code{\link[vroom]{vroom}}
#' melhor formatada para passar na função \code{\link{rdb_read}}
#' @param file Nome do arquivo a ser importado.
#' @param control Controle com todos argumentos necessários para importar dados para uso inteno com a rdb_read()
#' @importFrom vroom vroom
#' @export
rdb_read_bf_vr <- function(file, control = rdb_read_control(type = "vroom")){
  #requireNamespace("vroom")
  vroom::vroom(file = file, delim = control$delim, quote = control$quote, escape_backslash = control$escape_backslash,
               escape_double = control$escape_double, col_names = control$col_names, col_types = control$col_types,
               locale = control$locale, na = control$na, trim_ws = control$trim_ws, skip = control$skip,
               n_max = control$n_max, altrep = control$altrep, guess_max = control$guess_max
  )
}

#' @title Controle para leitura de arquivos grandes (Big Files)
#'
#' @description Esta função é uma chave para passar argumentos para a função rdb_read
#' @param type Nome do mecanisco de leitura. Podendo sem um em "fread","readr","vroom" ou "rio". Veja os detalhes abaixo para saber mais.
#' @param delim Delimitador de colunas, se não souber deixe NULL. Pode ser, ",", ";", "|" e outros conforme seu arquivo.
#' @param dec Separador decimal.
#' @param try_append Se TRUE (padrão) tenta apendar (empilhar) os dados. Se FALSE retorna lista de tabelas, uma para cada arquivo importado.
#' @param sep Agrupador de campos para formatação de números
#' @param quote Regex para quotation. Serve para não confundir com aspas que delimita strings.
#' @param escape_backslash Utilizar contrabarra para saltar Caractéres especiais
#' @param escape_double Se TRUE o valor '""' representara um unico \"
#' @param col_names TRUE, FALSE, um vetor de nomes ou números que serao tratados como índices para selecionar colunas
#' @param col_types Se NULL trabalha com os padroes do pacote readr. Se data.table, utiliza os padroes da funão \code{\link[data.table]{fread}}
#' @param col_select Vetor de nomes ou índices a selecionar.
#' @param encoding Encoding do arquivo. Como padrão pega as configurações do Sistema Operacional
#' @param na Tipo de valor para substituir NA
#' @param comment Caractére a ser tratado como comentario. padrão e hashtag
#' @param trim_ws Limpar espacos em branco a direita e esquerda das colunas
#' @param skip número de linhas a saltar na leitura dos dados
#' @param n_max número de linhas que quer extrair. Se Inf retorna o maximo de linhas
#' @param guess_max Total de linhas por partes lidas (chunksize)
#' @param altrep Controle dos tipos de dados na leitura por \code{\link[vroom]{vroom}}
#' @param progress Mostrar progresso da leitura dos dados
#' @param skip_empty_rows Ignorar linhas vazias
#' @export
rdb_read_control <- function(type = "fread", delim = NULL, dec = NULL, try_append = TRUE,
                             sep = NULL, quote = "\"", escape_backslash = FALSE,
                             escape_double = TRUE, col_names = TRUE, col_select = NULL,
                             col_types = NULL, encoding = NULL, na = c("", "NA"),
                             comment = "", trim_ws = FALSE, skip = 0, n_max = Inf,
                             guess_max = min(1000, n_max), altrep = TRUE,
                             progress = NULL, skip_empty_rows = TRUE){

  type <- match.arg(type, c("fread","readr","vroom","rio"))

  control <- switch(type,
                    fread = list(dec = if(is.null(dec)) "." else dec,
                                 delim = if(is.null(delim)) "auto" else delim,
                                 quote = quote,
                                 nrows = n_max,
                                 header = if(col_names) "auto" else col_names,
                                 skip = if(skip == 0) "__auto__" else skip,
                                 select = if(is.null(col_select)) NULL else col_select,
                                 colClasses = if(is.null(col_types)) NULL else col_types,
                                 encoding = if(is.null(encoding)) "unknown" else encoding
                    ),
                    readr = list(delim = delim,
                                 quote = quote,
                                 escape_backslash = escape_backslash,
                                 escape_double = escape_double,
                                 col_names = col_names,
                                 col_types = col_types,
                                 locale = encoding,
                                 na = na,
                                 comment = comment,
                                 trim_ws = trim_ws,
                                 skip = skip,
                                 n_max = n_max,
                                 guess_max = min(1000, n_max),
                                 progress = progress,
                                 skip_empty_rows = skip_empty_rows
                    ),
                    vroom = list(delim = delim,
                                 quote = quote,
                                 escape_backslash = escape_backslash,
                                 col_select = col_select,
                                 escape_double = escape_double,
                                 col_names = col_names,
                                 col_types = col_types,
                                 locale = encoding,
                                 na = na,
                                 comment = comment,
                                 trim_ws = trim_ws,
                                 skip = skip,
                                 n_max = n_max,
                                 guess_max = min(1000, n_max),
                                 altrep = altrep,
                                 progress = progress,
                                 skip_empty_rows = skip_empty_rows
                    ),
                    rio = list(format = delim,
                               dec = if(is.null(dec)) "." else dec,
                               header = col_names)
  )
}

#' @title Leitura de arquivos grandes (Big Files)
#' @description Esta função é um ensemble que junta muitas funções otimizadas de pacotes especiais do
#' R. Entre eles, readr, data.table, vroom e rio.
#' @param file Vetor ou lista de strings com nomes de arquivos a serem importados da maioria das extensoes. Arquivos copactados em .gz e .bgz
#' @param type Nome do mecanisco de leitura. Podendo sem um em "fread","readr","vroom" ou "rio". Veja os detalhes abaixo para mais detalhes.
#' @param delim Delimitador de colunas, se não souber deixe NULL. Pode ser, ",", ";", "|" e outros conforme seu arquivo.
#' @param dec Deparador decimal.
#' @param try_append Se TRUE (padrão) tenta apendar (empilhar) os dados. Se FALSE retorna lista de tabelas, uma para cada arquivo importado.
#' @param sep Agrupador de campos para formatação de números
#' @param quote Regex para quotation. Serve para não confundir com aspas que delimita strings.
#' @param escape_backslash Utilizar contrabarra para saltar Caractéres especiais
#' @param escape_double Se TRUE o valor '""' representará um unico \"
#' @param col_names TRUE, FALSE, um vetor de nomes ou números que serao tratados como índices para selecionar colunas
#' @param col_types Se NULL trabalha com os padroes do pacote readr. Se data.table, utiliza os padroes da funão \code{\link[data.table]{fread}}
#' @param col_select Vetor de nomes ou índices a selecionar.
#' @param encoding Encoding do arquivo. Como padrão pega as configurações do Sistema Operacional
#' @param na Tipo de valor para substituir NA
#' @param comment Caractére a ser tratado como comentario. padrão e hashtag
#' @param trim_ws Limpar espacos em branco a direita e esquerda das colunas
#' @param skip número de linhas a saltar na leitura dos dados
#' @param n_max número de linhas que quer extrair. Se Inf retorna o maximo de linhas
#' @param guess_max Total de linhas por partes lidas (chunksize)
#' @param altrep Controle dos tipos de dados na leitura por \code{\link[vroom]{vroom}}
#' @param progress Mostrar progresso da leitura dos dados
#' @param skip_empty_rows Ignorar linhas vazias
#' @param ... Outros argumentos de funções quando trabalhando com o pacote \code{\link[rio]{import}}
#' @details Para obter detalhes completos destas funções e seus argumentos
#' consulte a documentacao original dos seus desenvolvedores conforme lista abaixo:
#' \itemize{
#'     \item Para type = "fread" veja \code{\link[data.table]{data.table}} e \code{\link[data.table]{fread}}.
#'     \item Para type = "readr" veja \code{\link[readr]{readr}} e \code{\link[readr]{read_delim}}.
#'     \item Para type = "vroom" veja \code{\link[vroom]{vroom}}.
#'     \item Para type = "rio"   veja \code{\link[rio]{rio}} e \code{\link[rio]{import}}.
#' }
#' @importFrom data.table fread setDT is.data.table
#' @importFrom vroom vroom
#' @importFrom readr read_csv read_csv2
#' @importFrom rio import
#' @importFrom dplyr bind_rows
#' @importFrom purrr map map_df
#' @examples
#' \dontrun{
#' library("csmon")
#' f2 <- list.files(pattern = "1-c000.csv.gz$", full.names = TRUE)
#' a <- rdb_read(file = f2, type = "fread")
#' b <- rdb_read(file = f2, type = "readr", delim = ",", col_names = FALSE)
#' d <- rdb_read(file = f2, type = "vroom", delim = ",", col_names = FALSE)
#' e <- rdb_read(file = "/teste_02.xls", type = "rio", sheet = 1, range = "a3:j23")
#' }
#' @export
rdb_read <- function(file, type = "fread", delim = NULL, dec = NULL, try_append = TRUE,
                     sep = NULL, quote = "\"", escape_backslash = FALSE,
                     escape_double = TRUE, col_names = TRUE, col_select = NULL,
                     col_types = NULL, encoding = NULL, na = c("", "NA"),
                     comment = "", trim_ws = FALSE, skip = 0, n_max = Inf,
                     guess_max = min(1000, n_max), altrep = TRUE,
                     progress = NULL, skip_empty_rows = TRUE, ...){

  control <- rdb_read_control(type = type, delim = delim, dec = dec, try_append = try_append,
                              sep = sep, quote = quote, escape_backslash = escape_backslash,
                              escape_double = escape_double, col_names = col_names,
                              col_select = col_select, col_types = col_types, encoding = encoding,
                              comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max,
                              guess_max = guess_max, altrep = altrep, progress = progress,
                              skip_empty_rows = skip_empty_rows)

  try_read_fb <- switch(type,
                        fread = try(purrr::map(.x = as.list(file), .f = function(i){
                          rdb_read_bf_dt(i, control, ...)
                        })),
                        readr = try(purrr::map(.x = as.list(file), .f = function(i){
                          rdb_read_bf_rd(i, control, ...)
                        })),
                        vroom = try(purrr::map(.x = as.list(file), .f = function(i){
                          rdb_read_bf_vr(i, control, ...)
                        })),
                        rio = try(purrr::map(.x = as.list(file), .f = function(i){
                          rdb_read_bf_rio(i, ...)
                        }))
  )
  out <- if(try_append){
    data.table::setDT(dplyr::bind_rows(try_read_fb))
  } else {
      purrr::map(try_read_fb, function(i) data.table::setDT(i))
  }
  return(out)
}

#' Particao de dados
#'
#' @description Ajuda a quebrar um conjunto de dados com base no total de linhas desejado.
#' @param n Total de linhas da base total
#' @param nrows Total de linhas por bloco
#' @export
rdb_cut <- function(n, nrows = 1000){
  x <- seq_len(n)
  n <- length(x)
  p <- nrows / n

  x <- as.character(cut(x, breaks = seq(1, n, by = round(p*n)), dig.lab = 10))

  y <- gsub(pattern = "\\[|\\]|\\(|\\)", replacement = "", x = x)
  y <- gsub(pattern = ",", replacement = "-", x = y)
  u <- sum(is.na(y))
  if(u > 0){
    ultimo = max(as.numeric(unlist(strsplit(y, "-"))), na.rm = TRUE)
    y <- c(na.exclude(y), rep(paste0(ultimo, "-", n), u))
  }
  return(y)
}

#' Remove colchetes, parentesis e chaves
#' @param x string de dados
rdb_clean_brackets <- function(x){
  trimws(gsub(pattern = '\\[|\\]|\\(|\\)|\\{|\\}|\\\\|"\"', replacement = "", x))
}


#' Grava dados em um banco via ODBC
#'
#' @description Recebe parametros de uma conexao odbc ativa e exporta uma base de dados
#' como data.frame, tibbla, matriz ou data.table subindo por partes.
#'
#' @details A configuracao dos tipos de dados e feita através de métodos do pacote DBI,
#' dplyr e dbplyr. Existem duas formas de salvar dados no banco de dados. Se a escolha for
#' method = 'dbi' o R tentara salvar a base de dados em parte utilizando methodos do pacote DBI,
#' caso method = 'copy' ele tentara salvar os dados de uma unica vez sem quebra em partes.
#' Este metodo pode evitar problemas com tipos de dados, mas costuma ser custoso computacionalmente.
#' Para mais detalhes ver \code{\link[DBI]{dbWriteTable}}, \code{\link[dplyr]{copy_to}} e
#' \code{\link[dbplyr]{in_schema}}.
#' @param con Conexao ativa odbc. Veja \code{\link[DBI]{dbConnect}} para mais detalhes
#' @param data Objeto de dados. Pdoe ser data.frame ou qualquer objeto que se posssa converter em data.frame
#' @param name String de nome da tabela que vai ser grabada no banco com os dados do objeto 'data'
#' @param schema String de nome do schema do banco de dados que deseja salvar. padrão e 'dbo'
#' @param method String com um entre os valoes 'dbi' ou 'copy'. Vaje details para mais informacoes.
#' @param chunk_size Define o total de linhas que cada lote salvara no banco por vez.
#' @param verbose Se TRUE que mostra todo o log do processo ou apenas algumas partes.
#' @param append Se TRUE e a tabela ja existir no banco de dados ele tenta apendar (empilhar) os dados.
#' @importFrom DBI dbWriteTable dbIsValid dbExistsTable
#' @importFrom dplyr copy_to
#' @importFrom dbplyr in_schema
#' @importFrom data.table `:=` setDT data.table
#' @importFrom purrr map map_df
#' @importFrom tibble as_tibble
#' @export
rdb_db_write <- function(con, data, name,
                         schema = "dbo", method = "dbi",
                         chunk_size = 5999, verbose = TRUE, append = FALSE){

  method <- match.arg(method, c("dbi","copy"))
  query <- paste0(schema, ".", name)
  nm_base <- colnames(data)
  ini0 <- Sys.time()

  if(!inherits(data, "data.table")){
    data <- data.table::setDT(data)
  }

  aux_method_dbi_write <- function(con, da, append, ...){
    query <- paste0(schema, ".", name)
    DBI::dbWriteTable(conn = con, name = DBI::SQL(query), value = da,
                      #field.types = as.character(tipos),
                      overwrite = FALSE, append = append,
                      encoding = "latin1", row.names = FALSE, ...)
  }

  aux_method_dbi_append <- function(con, da, append, ...){
    query = paste0(schema, ".", name)
    DBI::dbWriteTable(conn = con, name = DBI::SQL(query), value = da,
                      #field.types = as.character(tipos),
                      overwrite = FALSE, append = append,
                      encoding = "latin1", row.names = FALSE, ...)
  }

  aux_method_copy_write <- function(con, da, ...){
    dplyr::copy_to(dest = con, df = da,
                   name = dbplyr::in_schema(schema, name),
                   temporary = FALSE, overwrite = FALSE, ...)
  }

  aux_split_data <- function(da, chunk_size){
    nm_base <- colnames(da)
    da[, `:=` (temp_quebra__ = rdb_cut(.N, nrows = chunk_size))]
    splt <- purrr::map(split(da, da$temp_quebra__), function(i){
      tibble::as_tibble(i[, ..nm_base])
    })
    rm(da)
    return(splt)
  }

  if(!DBI::dbIsValid(con)){
    stop(cat("log: Conexao perdida, favor reconectar!\n"))
  }

  if(DBI::dbExistsTable(conn = con, name = DBI::SQL(query)) & append == FALSE){
    stop(cat("log: A tabela (", query, ") ja existe no banco.",
             con@info$dbname, "Se nao for possivel dropar defina outro nome.\n"))
  }

  if(chunk_size >= nrow(data) | method == "copy"){
    chunk_size <- nrow(data) -1
    split_data <- FALSE
  } else {
    chunk_size <- chunk_size
    split_data <- TRUE
  }

  if(missing(schema) || is.null(schema)){
    schema <- "dbo"
  }

  if(!split_data | method == "copy"){
    ini <- Sys.time()
    cat(timestamp(prefix =  "log: ", suffix = " Processando dados e checando comunicacao com o banco ...\n", quiet = TRUE))

    out <- if(method == "copy"){
      cat(timestamp(prefix =  "log: ", suffix = " Sem quebras definidas. Subindo tabela completa ...\n", quiet = TRUE))
      try(aux_method_copy_write(con, data))
    } else {
      try(aux_method_dbi_write(con, data, append))
    }
    if(inherits(out, "try-error")){
      stop(FALSE)
    } else {
      if(verbose){
        verb <- paste0(" Duracao: ", round(as.numeric(Sys.time() -ini, units = "secs"), 4),
                       " secs; linhas: ", nrow(data),  ";",
                       " upload total: ", format(object.size(data), units = "Mb", digits = 4), " ...\n")
        cat(timestamp(prefix = "log: ",  suffix = verb, quiet = TRUE))
      }
      return(invisible(out))
    }
  } else {
    cat(timestamp(prefix =  "log: ", suffix = " Processando quebras e preparando para subida ...\n", quiet = TRUE))
    out <- c()
    splt <- aux_split_data(data, chunk_size)
    nm <- names(sort(sapply(names(splt),
                            function(i){max(as.numeric(unlist(strsplit(i, "-"))))})))
    P1 <- splt[[nm[1]]]
    PN <- splt[ nm[-1]]
    rm(data)
    # Gravar parte 01
    {
      query <- paste0(schema, ".", name)
      cat(timestamp(prefix = "log: ",  suffix = paste0(" Iniciando upload de (", length(nm), ") partes ...\n"), quiet = TRUE))
      ini <- Sys.time()

      try(aux_method_dbi_write(con, P1, append))

      if(verbose){
        obsize <- object.size(P1)
        fim <- Sys.time()
        verb <- paste0(" Duracao: ", round(as.numeric(fim -ini, units = "secs"), 4),
                       " secs; linhas: ", nm[[1]],  ";",
                       " upload progressivo: ", format(obsize, units = "Mb", digits = 4), "...\n")
        cat(timestamp(prefix = "log: ",  suffix = verb, quiet = TRUE))
      }
    }
    # Gravar restante das bases em loop por insert.
    {
      for(i in names(PN)){
        ini <- Sys.time()
        try(aux_method_dbi_append(con, PN[[i]], append = TRUE))

        if(verbose) {
          obsize <- obsize + object.size(PN[[i]])
          fim <- Sys.time()
          verb <- paste0(" Duracao: ", round(as.numeric(fim-ini, units = "secs"), 4),
                         " secs; linhas: ", i, ";",
                         " upload progressivo: ", format(obsize, units = "Mb", digits = 4), "...\n")
          out <- rbind(out, verb)
          cat(timestamp(prefix = "log: ",  suffix = verb, quiet = TRUE))
        }
      }
    }
    verb <- paste0(" Tempo da carga: ", round(as.numeric(Sys.time()-ini0, units = "mins"), 4), " minutos;",
                   " upload total: ", format(obsize, units = "Mb", digits = 4))

    if(verbose) cat(timestamp(prefix =  "log: ", suffix = verb, quiet = TRUE))

    return(invisible(out))
  }
}
