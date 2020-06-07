
<!-- README.md is generated from README.Rmd. Please edit that file -->

# O rdb

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rdb)](https://CRAN.R-project.org/package=rdb)
<!-- badges: end -->

O objetivo do pacote rdb é dispor uma série de funções especiais para
obter dados, mipular e limpar. São utulizados métodos de pacotes como
[data.table](https://cran.r-project.org/web/packages/data.table/index.html),
[\!vroom](https://cran.r-project.org/web/packages/vroom/index.html),
algumas dependências do [\!tidyverse](https://www.tidyverse.org/). Para
exportar dados via ODBC, trabalhamos com apoio da biblioteca
[\!DBI](https://cran.r-project.org/web/packages/DBI/index.html). O
pacote é experimental e ainda está em desenvolvimento, então não está
livre de bugs.

## Instalação

O rdb não está no cran, mas pode ser instalado direto do github.

``` r
devtools::install_github("evandeilton/rdb")
```

Se não econtrar a biblioteca [\!validaRA](), ela poderá ser instaladas
antes da mesma forma.

``` r
devtools::install_github("ipea/validaRA")
```

## Exemplos

Exemplo da lida de dados de um site web com apoio da função `rdb`.

``` r
library(rdb)
library(tidyverse)

## Lendo dados da página Web do covid-19
da <- rdb_read(file = "https://brasil.io/dataset/covid19/caso_full/?format=csv", type = "rio")

da %>%
    dplyr::summarise_if(.predicate = "is_num", .funs = function(i){
        mean(as_num(i), na.rm = TRUE)
    }) %>%
  knitr::kable()
```

| epidemiological\_week | date | order\_for\_place | state | city | city\_ibge\_code | place\_type | last\_available\_confirmed | last\_available\_confirmed\_per\_100k\_inhabitants | new\_confirmed | last\_available\_deaths | new\_deaths | last\_available\_death\_rate | estimated\_population\_2019 | is\_last | is\_repeated |
| --------------------: | ---: | ----------------: | ----: | ---: | ---------------: | ----------: | -------------------------: | -------------------------------------------------: | -------------: | ----------------------: | ----------: | ---------------------------: | --------------------------: | -------: | -----------: |
|              19.93447 |  NaN |          25.34681 |   NaN |  NaN |          3095939 |         NaN |                   147.5864 |                                            85.3245 |       7.754466 |                9.032975 |   0.4145429 |                    0.0650882 |                    189827.7 |      NaN |          NaN |
