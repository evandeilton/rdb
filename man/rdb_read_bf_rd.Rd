% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extras_rdb.R
\name{rdb_read_bf_rd}
\alias{rdb_read_bf_rd}
\title{Leitura de dados com pacote readr}
\usage{
rdb_read_bf_rd(file, control = rdb_read_control(type = "readr"))
}
\arguments{
\item{file}{Nome do arquivo a ser importado.}

\item{control}{Controle com todos argumentos necessários para importar dados para uso inteno com a rdb_read()}
}
\description{
Este é apenas um wraper que deixa as funções \code{\link[readr]{read_csv2}} e
\code{\link[readr]{read_delim}} conveniente para passar na função \code{\link{rdb_read}}
}
