library(stringr)
library(magrittr)

# Traduzir
rmd2qmd <- function(arquivo) {
  
  if (endsWith(arquivo, '.Rmd')) {
    saida <- str_replace(arquivo, '\\.Rmd$', '.qmd')
  } else {
    saida <- paste0(arquivo, '.qmd')
  }

  message('Lendo ', arquivo, '...')
    
  arquivo %>% 
    ler() %>% 
    deletar_vazias_inicio() %>% 
    deletar_yaml() %>% 
    incluir_math() %>% 
    traduzir_callouts() %>% 
    traduzir_respostas() %>% 
    gravar(saida)

  message('Gravando ', saida, '...')
  message('Fim.')
    
}

# Read entire file into vector
ler <- function(arquivo) {
  
  readLines(arquivo, ok = TRUE)
  
}

# Delete leading empty lines
deletar_vazias_inicio <- function(rmd) {
  
  i <-  1
  linha <- rmd[i]
  
  while (str_detect(linha, '^[[:blank:]]*$')) {
    i <- i + 1
    if (i > length(rmd)) {
      stop('No non-empty lines in rmd file.')
    }
    linha <- rmd[i]
  }
  
  rmd[i:length(rmd)]

}

# Delete yaml block beginning at current line
deletar_yaml <- function(rmd) {
  
  if (str_detect(rmd[1], '^\\s*---\\s*$')) {
    
    fim_bloco <- match(TRUE, str_detect(rmd[-1], '^\\s*---\\s*$')) + 1
    return(rmd[(fim_bloco + 1):length(rmd)]) 
    
  } else {
    return(rmd)
  }
  
}

# Include math
incluir_math <- function(rmd) {
  
  incluir <- c('{{< include _math.qmd >}}', '')
  c(incluir, rmd)
  
}

# Translate callouts, respecting indent
traduzir_callouts <- function(rmd) {
  
  str_replace(
    rmd, 
    '^(\\s*):::\\s*\\{.rmd([^ ]+)\\s+(latex\\s*=\\s*1)?\\}', 
    '\\1::: {.callout-\\2}'
  )

}

# Translate respostas, respecting indent
traduzir_respostas <- function(rmd) {

  rmd <- str_replace(
    rmd, 
    '^(\\s*)`r inicio_resposta\\(\\)`', 
    '\\1::: {.callout-tip title="Resposta" collapse="true"}'
  )

  rmd <- str_replace(
    rmd, 
    '^(\\s*)`r fim_resposta\\(\\)`', 
    '\\1:::'
  )
  
  rmd
      
}

# Gravar
gravar <- function(rmd, saida) {
  
  writeLines(rmd, saida)
  
}
