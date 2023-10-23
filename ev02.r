library(Biostrings)
library(seqinr)
library(stringr)
library(ggplot2)
library(adegenet)
library(ape)
library(DECIPHER)
getwd()

setwd("C:/Users/daeld/OneDrive/Documentos/Programas")

argentina <- read.fasta("sars_argentina.fasta")
australia <- read.fasta("sars_australia.fasta")
brazil <- read.fasta("sars_brazil.fasta")
france <- read.fasta("sars_france.fasta")
germany <- read.fasta("sars_germany.fasta")
india <- read.fasta("sars_india.fasta")
indonesia <- read.fasta("sars_indonesia.fasta")
iran <- read.fasta("sars_iran.fasta")
italy <- read.fasta("sars_italy.fasta")
japan <- read.fasta("sars_japan.fasta")
mexico <- read.fasta("sars_mexico.fasta")
netherlands <- read.fasta("sars_netherlands.fasta")
russia <- read.fasta("sars_russia.fasta")
southkorea <- read.fasta("sars_southkorea.fasta")
spain <- read.fasta("sars_spain.fasta")
taiwan <- read.fasta("sars_taiwan.fasta")
turkey <- read.fasta("sars_turkey.fasta")
uk<- read.fasta("sars_uk.fasta")
usa <- read.fasta("sars_usa.fasta")
vietnam <- read.fasta("sars_vietnam.fasta")

argentinalen <- length(argentina[[1]])
australialen <- length(australia[[1]])
brazillen <- length(brazil[[1]])
francelen <- length(france[[1]])
germanylen <- length(germany[[1]])
indialen <- length(india[[1]])
indonesialen <- length(indonesia[[1]])
iranlen <- length(iran[[1]])
italylen <- length(italy[[1]])
japanlen <- length(japan[[1]])
mexicolen <- length(mexico[[1]])
netherlandslen <- length(netherlands[[1]])
russialen <- length(russia[[1]])
southkorealen <- length(southkorea[[1]])
spainlen <- length(spain[[1]])
taiwanlen <- length(taiwan[[1]])
turkeylen <- length(turkey[[1]])
uklen <- length(uk[[1]])
usalen <- length(usa[[1]])
vietnamlen <- length(vietnam[[1]])

print(paste0("SARS Argentina: ",argentinalen))
print(paste0("SARS Australia: ",australialen))
print(paste0("SARS Brazil: ",brazillen))
print(paste0("SARS France: ",francelen))
print(paste0("SARS Germany: ",germanylen))
print(paste0("SARS India: ",indialen))
print(paste0("SARS Indonesia: ",indonesialen))
print(paste0("SARS Iran: ",iranlen))
print(paste0("SARS Italy: ",italylen))
print(paste0("SARS Japan: ",japanlen))
print(paste0("SARS Mexico: ",mexicolen))
print(paste0("SARS Netherlands: ",netherlandslen))
print(paste0("SARS Russia: ",russialen))
print(paste0("SARS South Korea: ",southkorealen))
print(paste0("SARS Spain: ",spainlen))
print(paste0("SARS Taiwan: ",taiwanlen))
print(paste0("SARS Turkey: ",turkeylen))
print(paste0("SARS UK: ",uklen))
print(paste0("SARS USA: ",usalen))
print(paste0("SARS Vietnam: " ,vietnamlen))

argentinadf <- as.data.frame(count(argentina[[1]],1))
colnames(argentinadf) <- c("Nucleotides", "Argentina")
australiadf <- as.data.frame(count(australia[[1]],1))
colnames(australiadf) <- c("Nucleotides", "Australia")
brazildf <- as.data.frame(count(brazil[[1]],1))
colnames(brazildf) <- c("Nucleotides", "Brazil")
francedf <- as.data.frame(count(france[[1]],1))
colnames(francedf) <- c("Nucleotides", "France")
germanydf <- as.data.frame(count(germany[[1]],1))
colnames(germanydf) <- c("Nucleotides", "Germany")
indiadf <- as.data.frame(count(india[[1]],1))
colnames(indiadf) <- c("Nucleotides", "India")
indonesiadf <- as.data.frame(count(indonesia[[1]],1))
colnames(indonesiadf) <- c("Nucleotides", "Indonesia")
italydf <- as.data.frame(count(italy[[1]],1))
colnames(italydf) <- c("Nucleotides", "Italy")
irandf <- as.data.frame(count(iran[[1]],1))
colnames(irandf) <- c("Nucleotides", "Iran")
japandf <- as.data.frame(count(japan[[1]],1))
colnames(japandf) <- c("Nucleotides", "Japan")
mexicodf <- as.data.frame(count(mexico[[1]],1))
colnames(mexicodf) <- c("Nucleotides", "Mexico")
netherlandsdf <- as.data.frame(count(netherlands[[1]],1))
colnames(netherlandsdf) <- c("Nucleotides", "Netherlands")
russiadf <- as.data.frame(count(russia[[1]],1))
colnames(russiadf) <- c("Nucleotides", "Russia")
southkoreadf <- as.data.frame(count(southkorea[[1]],1))
colnames(southkoreadf) <- c("Nucleotides", "Southkorea")
spaindf <- as.data.frame(count(spain[[1]],1))
colnames(spaindf) <- c("Nucleotides", "Spain")
taiwandf <- as.data.frame(count(taiwan[[1]],1))
colnames(taiwandf) <- c("Nucleotides", "Taiwan")
turkeydf <- as.data.frame(count(turkey[[1]],1))
colnames(turkeydf) <- c("Nucleotides", "Turkey")
ukdf <- as.data.frame(count(uk[[1]],1))
colnames(ukdf) <- c("Nucleotides", "UK")
usadf <- as.data.frame(count(usa[[1]],1))
colnames(usadf) <- c("Nucleotides", "USA")
vietnamdf <- as.data.frame(count(vietnam[[1]],1))
colnames(vietnamdf) <- c("Nucleotides", "Vietnam")

df1 <- merge(argentinadf,australiadf)
df2 <- merge(df1,brazildf)
df3 <- merge(df2,francedf)
df4 <- merge(df3,germanydf)
df5 <- merge(df4,indiadf)
df6 <- merge(df5,indonesiadf)
df7 <- merge(df6,italydf)
df8 <- merge(df7,irandf)
df9 <- merge(df8,japandf)
df10 <- merge(df9,mexicodf)
df11 <- merge(df10,netherlandsdf)
df12 <- merge(df11,russiadf)
df13 <- merge(df12,southkoreadf)
df14 <- merge(df13,spaindf)
df15 <- merge(df14,taiwandf)
df16 <- merge(df15,turkeydf)
df17 <- merge(df16,ukdf)
df18 <- merge(df17,usadf)
df19 <- merge(df18,vietnamdf)

cantidad1 <- argentinadf[,2]
cantidad2 <- australiadf[,2]
cantidad3 <- brazildf[,2]
cantidad4 <- francedf[,2]
cantidad5 <- germanydf[,2]
cantidad6 <- indiadf[,2]
cantidad7 <- indonesiadf[,2]
cantidad8 <- italydf[,2]
cantidad9 <- irandf[,2]
cantidad10 <- japandf[,2]
cantidad11 <- mexicodf[,2]
cantidad12 <- netherlandsdf[,2]
cantidad13 <- russiadf[,2]
cantidad14 <- southkoreadf[,2]
cantidad15 <- spaindf[,2]
cantidad16 <- taiwandf[,2]
cantidad17 <- turkeydf[,2]
cantidad18 <- ukdf[,2]
cantidad19 <- usadf[,2]
cantidad20 <- vietnamdf[,2]

grafica <- data.frame(
  "Nucleótidos" = c("A","C", "G", "T"),
  "Virus" = c(rep("Argentina", 4), rep("Australia", 4), rep("Brazil", 4), rep("France", 4), rep("Germany", 4), rep("India", 4), rep("Indonesia", 4), rep("Italy", 4), rep("Iran", 4), rep("Japan", 4), rep("Mexico", 4), rep("Netherlands", 4), rep("Russia", 4), rep("Southkorea", 4), rep("Spain", 4), rep("Taiwan", 4), rep("Turkey", 4), rep("UK", 4), rep("USA", 4), rep("Vietnam", 4)),
  "Cantidad" = c(cantidad1, cantidad2, cantidad3, cantidad4,cantidad5,cantidad6,cantidad7,cantidad8,cantidad9,cantidad10,cantidad11,cantidad12,cantidad13,cantidad14,cantidad15,cantidad16,cantidad17,cantidad18,cantidad19,cantidad20)
)

graficar <- print(ggplot(grafica, aes(x = Nucleótidos, y = Cantidad, fill=Virus)) + geom_bar(position="dodge", stat="identity"))
graficar

country_names <- c("sars_usa", "sars_india", "sars_france", "sars_germany", "sars_brazil", "sars_japan", "sars_southkorea", "sars_italy", "sars_uk", "sars_russia", "sars_turkey", "sars_spain", "sars_vietnam", "sars_australia", "sars_taiwan", "sars_argentina", "sars_netherlands", "sars_iran", "sars_mexico", "sars_indonesia")
country_seq <- c()

for(country in country_names){
  cat("\n")
  file_path <- paste("C:/Users/daeld/OneDrive/Documentos/Programas/", country, ".fasta", sep="")
  current <- readDNAStringSet(file_path)
  country_seq <- append(country_seq, current)
}

merged <- DNAStringSet(country_seq)

seqs <- OrientNucleotides(merged)

aligned <- AlignSeqs(seqs)

writeXStringSet(aligned,file="variantes_alineadas.fasta")

dna_aligned <- read.alignment("variantes_alineadas.fasta", format = "fasta")

D <- dist.alignment(dna_aligned, matrix = "similarity")

arbol_filogenetico <- nj(D)
plot(arbol_filogenetico, cex = 0.6)
title("Similaridad entre genomas de variantes de SARS-CoV-2 de 20 países")