
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
devtools::install_github("ipea/validaRA")
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
| AC    |  240.2124 |                          185.3706 |  6.3201 |      0.0134 |
| AL    |  119.7374 |                          101.8259 |  5.6861 |      0.1105 |
| AM    |  624.4032 |                          469.0966 | 37.3339 |      0.0534 |
| AP    |  478.5446 |                          514.4364 | 12.1440 |      0.0252 |
| BA    |   79.4150 |                           31.6578 |  2.7071 |      0.0581 |
| CE    |  264.2859 |                          127.2672 | 17.2762 |      0.0814 |
| DF    | 1899.4008 |                           83.8728 | 31.1206 |      0.0115 |
| ES    |  166.8621 |                          100.5543 |  6.9121 |      0.0343 |
| GO    |   35.3578 |                           27.4525 |  1.3163 |      0.0607 |
| MA    |  190.2440 |                          189.9891 |  6.5433 |      0.0314 |
| MG    |   31.6518 |                           31.6374 |  0.9696 |      0.0631 |
| MS    |   35.9166 |                           59.5592 |  0.6757 |      0.0262 |
| MT    |   34.4849 |                           37.2271 |  0.9531 |      0.0538 |
| PA    |  247.1304 |                          146.4978 | 19.6953 |      0.0707 |
| PB    |   89.1463 |                          131.2594 |  3.0950 |      0.0560 |
| PE    |  212.5728 |                           81.7904 | 17.3895 |      0.1414 |
| PI    |   45.1067 |                           60.5433 |  1.5358 |      0.0959 |
| PR    |   26.9258 |                           39.7477 |  1.2829 |      0.0668 |
| RJ    |  473.8491 |                           76.3832 | 48.2388 |      0.0718 |
| RN    |   63.0996 |                           65.1395 |  2.6879 |      0.0823 |
| RO    |  120.6037 |                           63.5477 |  4.0033 |      0.0594 |
| RR    |  208.6818 |                          190.6194 |  5.7529 |      0.0387 |
| RS    |   37.3283 |                           71.4646 |  1.0853 |      0.0341 |
| SC    |   48.9882 |                           80.7487 |  0.8842 |      0.0350 |
| SE    |  109.6125 |                           71.0004 |  2.3121 |      0.0539 |
| SP    |  257.2953 |                           48.9327 | 18.9859 |      0.0819 |
| TO    |   46.8646 |                          148.0605 |  0.9150 |      0.0319 |
