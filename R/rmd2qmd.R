library(stringr)
library(magrittr)
library(zeallot)

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
    incluir_math_e_setup() %>% 
    traduzir_callouts() %>% 
    traduzir_respostas() %>% 
    indentar_chunks() %>% 
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
incluir_math_e_setup <- function(rmd) {
  
  incluir <- c(
    '{{< include _math.qmd >}}', 
    '',
    '```{r echo=FALSE, include=FALSE}',
    'source(\'_setup.R\')',
    '```',
    ''
  )
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

# Indentar chunks
indentar_chunks <- function(rmd) {
  
  n_linhas <- length(rmd)
  indent_atual <- 0
  
  i <-  1
  while (i <= n_linhas) {
    
    linha <- rmd[i]
    
    # Início de chunk: processar
    if (inicia_chunk(linha)) {
      # indentar_chunk() retorna número da última linha do chunk (novo_i)
      c(rmd, novo_i) %<-% indentar_chunk(rmd, i, indent_atual)
      i <- novo_i
    } else {
      # Alterar indent atual, se necessário
      indent_atual <- calcular_indent(linha, indent_atual)
    }
    
    i <- i + 1
    
  }
  
  rmd
  
}

item_lista_ordenada <- function(linha) {
  # Um dígito ou uma letra minúscula antes do ponto
  str_detect(linha, '^\\s*[0-9a-z]\\. ')
}
    
item_lista_nao_ordenada <- function(linha) {
  # Provavelmente pode dar falso positivo com fórmula LaTeX
  str_detect(linha, '^\\s*\\* ')
}

# Só retorna valor correto com espaços, não tabs
# Garantia: linha não é parte de chunk
calcular_indent <- function(linha, indent_atual) {
  
  # Linha vazia
  if (str_detect(linha, '^[[:blank:]]*$')) {
    return(indent_atual)
  }
  
  # Quantos espaços antes do primeiro char?
  espacos <- str_extract(linha, '^(\\s*)[^ ]', group = 1)
  
  if (item_lista_ordenada(linha)) {
    return(nchar(espacos) + 3)
  }

  if (item_lista_nao_ordenada(linha)) {
    return(nchar(espacos) + 2)
  }
  
  return(nchar(espacos))
  
}

inicia_chunk <- function(linha) {
  str_detect(linha, '^\\s*```\\{.*\\}\\s*$')
}

termina_chunk <- function(linha) {
  str_detect(linha, '^\\s*```\\s*$')
}

indentar_chunk <- function(rmd, i, indent_atual) {
  
  # browser()
  
  # Indent da primeira linha do chunk:
  indent_original <- calcular_indent(rmd[i])
  
  n_linhas <- length(rmd)
  while (i <= n_linhas) {
    
    linha <- rmd[i]
    nova_linha <- paste0(
      paste0(rep(' ', indent_atual), collapse = ''),
      str_sub(linha, indent_original + 1)  # Remover indent original
    )
    rmd[i] <- nova_linha
    i <- i + 1
    
    if (termina_chunk(linha)) {
      break
    }
    
  }
  
  list(rmd, i)
  
}

# Gravar
gravar <- function(rmd, saida) {
  
  writeLines(rmd, saida)
  
}
