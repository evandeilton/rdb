
<!-- README.md is generated from README.Rmd. Please edit that file -->

# O rdb

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rdb)](https://CRAN.R-project.org/package=rdb)
[![Travis build
status](https://travis-ci.com/evandeilton/rdb.svg?branch=master)](https://travis-ci.com/evandeilton/rdb)
<!-- badges: end -->

O objetivo do pacote rdb é dispor uma série de funções especiais para
obter dados, mipular e limpar. São utulizados métodos de pacotes como
[data.table](https://cran.r-project.org/web/packages/data.table/index.html),
[vroom](https://cran.r-project.org/web/packages/vroom/index.html),
algumas dependências do [tidyverse](https://www.tidyverse.org/). Para
exportar dados via ODBC, trabalhamos com apoio da biblioteca
[DBI](https://cran.r-project.org/web/packages/DBI/index.html). O pacote
é experimental e ainda está em desenvolvimento, então não está livre de
bugs.

## Instalação

O rdb não está no cran, mas pode ser instalado direto do github.

``` r
devtools::install_github("evandeilton/rdb")
```

Se não econtrar a biblioteca
[validaRA](https://github.com/cran/validara), ela poderá ser instaladas
antes da mesma forma.

``` r
devtools::install_github("evandeilton/validaRA")
```

## Exemplos

Exemplo da lida de dados de um site web com apoio da função `rdb_read`,
teste lógico com `is_num` e coarse clase com `as_num`.

``` r
library(rdb)
library(tidyverse)

## Lendo dados da página Web do covid-19
da <- rdb_read(file = "https://brasil.io/dataset/covid19/caso_full/?format=csv", type = "rio")

da %>% 
  dplyr::select(state, dplyr::contains("last_available_")) %>% 
  dplyr::group_by(state) %>%
  dplyr::summarise_if(.predicate = "is_num",
                      .funs = function(i){
                        mean(as_num(i), na.rm = TRUE)
                        }
                      ) %>% 
  knitr::kable(digits = 4, col.names = gsub("last_available_", "", names(.)))
```

| state | confirmed | confirmed\_per\_100k\_inhabitants |  deaths | death\_rate |
| :---- | --------: | --------------------------------: | ------: | ----------: |
| AC    |  249.4474 |                          195.1299 |  6.5630 |      0.0136 |
| AL    |  124.6396 |                          107.1824 |  5.8449 |      0.1087 |
| AM    |  642.7486 |                          487.2342 | 38.0019 |      0.0528 |
| AP    |  497.6652 |                          547.8953 | 12.5000 |      0.0251 |
| BA    |   81.7376 |                           33.0292 |  2.7723 |      0.0574 |
| CE    |  272.7256 |                          133.7924 | 17.7841 |      0.0809 |
| DF    | 2002.3808 |                           88.4900 | 32.3846 |      0.0115 |
| ES    |  172.8632 |                          104.7464 |  7.1697 |      0.0345 |
| GO    |   36.4144 |                           28.3616 |  1.3397 |      0.0604 |
| MA    |  196.8105 |                          199.4844 |  6.6601 |      0.0312 |
| MG    |   32.5507 |                           33.0460 |  0.9845 |      0.0623 |
| MS    |   37.3003 |                           62.1289 |  0.6770 |      0.0258 |
| MT    |   35.8015 |                           38.7885 |  0.9904 |      0.0536 |
| PA    |  259.0146 |                          156.5613 | 20.4240 |      0.0702 |
| PB    |   92.6787 |                          137.4448 |  3.1469 |      0.0547 |
| PE    |  217.3123 |                           84.4551 | 17.7846 |      0.1404 |
| PI    |   46.5801 |                           63.0785 |  1.5848 |      0.0953 |
| PR    |   27.4540 |                           40.5724 |  1.2937 |      0.0665 |
| RJ    |  490.0938 |                           80.0658 | 49.8245 |      0.0715 |
| RN    |   64.6588 |                           67.1758 |  2.7531 |      0.0817 |
| RO    |  125.9306 |                           67.6180 |  4.1533 |      0.0583 |
| RR    |  216.9326 |                          198.5702 |  5.9623 |      0.0383 |
| RS    |   38.2836 |                           74.0989 |  1.1019 |      0.0340 |
| SC    |   50.0533 |                           82.9830 |  0.8962 |      0.0348 |
| SE    |  113.1286 |                           73.8721 |  2.4007 |      0.0540 |
| SP    |  262.7960 |                           50.3733 | 19.2857 |      0.0814 |
| TO    |   48.2126 |                          152.7174 |  0.9351 |      0.0316 |
