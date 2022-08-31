# Modelo da Previdência
## Dados Populacionais
População: WPP (World Population Prospects) 2022 da ONU, convertido para o formado de coorte.
- - - -
## Mercado de Trabalho (Dados da PNAD)
Todos os dados vêm da PNAD contínua do 4º trimestre de 2018 a 2021.
### Segurados
Imediatamente disponível o dado para contribuintes (clientela_sexo_idade). A variáveis de interesse são:

``` r
  'V1022' # Situação do Domicílio: 1- Urbano 2-Rural
  'V2007' # Sexo
  'V2009' #Idade
  'VD4012' #Contribuição para Previdencia 1-Contribuinte/2-Não Contribuinte
```

Dados agrupados no formato de coorte.

#### Pontos de Melhoria
*  Falta informações sobre segurais especiais .
* Clientela definida pelo local de moradia

### População Economicamente Ativa (PEA)

Variáveis de interesse:

``` r
  'V1022' # Situação do Domicílio: 1- Urbano 2-Rural
  'V2007' # Sexo
  'V2009' #Idade
  'VD4002' # Condição de ocupação 1. Ocupadas/2. Desocupadas NA
```
* PEA são as pessoas que se declaram ocupadas ou desocupadas, maiores de 16 anos.
* Tabelas geradas por idade