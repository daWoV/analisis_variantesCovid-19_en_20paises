library(seqinr)
library(Biostrings)
library(adegenet)
library(ape)
library(ggtree)
library(DECIPHER)
library(viridis)
library(ggplot2)
library(phytools)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

options(max.cat = 20000)

country_names <- c("sars_usa", "sars_india", "sars_france", "sars_germany", "sars_brazil", "sars_japan", "sars_southkorea", "sars_italy", "sars_uk", "sars_russia", "sars_turkey", "sars_spain", "sars_vietnam", "sars_australia", "sars_taiwan", "sars_argentina", "sars_netherlands", "sars_iran", "sars_mexico", "sars_indonesia")
country_seq <- c()

for(country in country_names){
  cat("\n")
  cat(country)
  file_path <- paste("C:/Users/daeld/OneDrive/Documentos/Programas/", country, ".fasta", sep="")
  current <- readDNAStringSet(file_path)
  
  cat("\n")
  cat(paste(width(current), " pares de bases"))
  country_seq <- append(country_seq, current)
}

# juntar secuencias de variantes en un solo set
merged <- DNAStringSet(country_seq)

seqs <- OrientNucleotides(merged)

aligned <- AlignSeqs(seqs)

# ver alineacion de secuencias en navegador
BrowseSeqs(aligned, highlight=0)

writeXStringSet(aligned,file="variantes_alineadas.fasta")

dna_aligned <- read.alignment("variantes_alineadas.fasta", format = "fasta")

# agrupar secuencias por similaridad
D <- dist.alignment(dna_aligned, matrix = "similarity")

arbol_filogenetico <- nj(D)
plot(arbol_filogenetico, cex = 0.6)
title("Similaridad entre genomas de variantes de SARS-CoV-2 de 20 países")

data <- data.frame(variante = country_names, secuencia = country_seq)

# Separar cada letra de la secuencia en una columna
data_sep <- data %>% separate(col = secuencia, into = c("A", "C", "G", "T"), sep = "(?<=.)(?=.)", fill = "right")

# Convertir a formato largo
data_long <- data_sep %>% pivot_longer(cols = c("A", "C", "G", "T"), names_to = "tipo_base", values_to = "num_bases")

# Gráfico de barras
ggplot(data_long, aes(x = variante, y = num_bases, fill = tipo_base)) + 
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Número de bases por tipo de base", x = "Variante", y = "Número de bases", fill = "Tipo de base")