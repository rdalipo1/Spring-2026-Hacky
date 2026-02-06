###1 Counting DNA Nucleotides ----------------

# A string is simply an ordered collection of symbols selected from some alphabet and formed into a word; the length of a string is the number of symbols that it contains.

# An example of a length 21 DNA string (whose alphabet contains the symbols 'A', 'C', 'G', and 'T') is "ATGCTTCAGAAAGGTCTTACG."

# Given: A DNA string of length at most 1000 nt.

# AGCTATACGTGGAAGATCGTTTCAGTGGCTTGCCTGGAAACGTGACACCAAGGGTGGTTATAGCTTCTACGCCACCTTCGAGGAAAAGTTGGGGATTAGTTACATAGGCTCCAACTCTTTCGCAAGGAGCTATTTACCTGCACGTCTAGTGATTCTACTCGAGAGCACCCACTTATGGAGCCCCCGCTAATACGTCTAAATCGGGGAGCCCGAACTATACAGAATGAACAGAGCTCTCGATGAAGGGCCATAGTTCAGGAGCAGGCGTACATCAACTCCTCCAACTCTAAGAGCTGCAGACTGCTTTCACTAATTAGCGCTGCGCCGCGCGTCACCTATTAATCGCAGTCTGAGCCAATCCGCCACACCGTTTAGGTGTCGCTGTATCGGGTTCCAGCGTTCTATGACCACTCCATCATATATCGAGTAGATTGCCGGTATTGACCAAAAAGGGTGGAGTGGAGCCGCACCATTGGAGAGCACTCAATTTGTAACACCGGTGTATACGAATGCTGGCGCCTATAATTACGACCTTACGAGCAAGAGCTTTGGTGATACAGGTGAAGACAACTGCCACGCATCCGCCAGTGACACTCTGGCCGGTAACCCCAGACTCCACAAGTCAAGTGGGGCTCAGTATTCTGCCCAGATATCGAATGAACAGGGTATACCCTGATGACGTAAACCTCGCACCTCAAGCGCTGATGATGGTCCGCGGAGCTGCTAGCTACTCCAATGATCCGTCCGTTCCTGATGCTGGGGGGGTATAAAAGATCTGCAAGGGCTTGATCTAACCCTCGAGCGTGCTAAACCCAATCCGGATGGGACTGGGCATACCAAAAGTGGCCGAGTTCAGGTGCTTGTCAGCCAAGTTGCTGTCGAGCCGGGCTTATAAGTGAGCATGCAAGTACGTT

# Return: Four integers (separated by spaces) counting the respective number of times that the symbols 'A', 'C', 'G', and 'T' occur in s

# 232 237 235 210


dna <- 'AGCTATACGTGGAAGATCGTTTCAGTGGCTTGCCTGGAAACGTGACACCAAGGGTGGTTATAGCTTCTACGCCACCTTCGAGGAAAAGTTGGGGATTAGTTACATAGGCTCCAACTCTTTCGCAAGGAGCTATTTACCTGCACGTCTAGTGATTCTACTCGAGAGCACCCACTTATGGAGCCCCCGCTAATACGTCTAAATCGGGGAGCCCGAACTATACAGAATGAACAGAGCTCTCGATGAAGGGCCATAGTTCAGGAGCAGGCGTACATCAACTCCTCCAACTCTAAGAGCTGCAGACTGCTTTCACTAATTAGCGCTGCGCCGCGCGTCACCTATTAATCGCAGTCTGAGCCAATCCGCCACACCGTTTAGGTGTCGCTGTATCGGGTTCCAGCGTTCTATGACCACTCCATCATATATCGAGTAGATTGCCGGTATTGACCAAAAAGGGTGGAGTGGAGCCGCACCATTGGAGAGCACTCAATTTGTAACACCGGTGTATACGAATGCTGGCGCCTATAATTACGACCTTACGAGCAAGAGCTTTGGTGATACAGGTGAAGACAACTGCCACGCATCCGCCAGTGACACTCTGGCCGGTAACCCCAGACTCCACAAGTCAAGTGGGGCTCAGTATTCTGCCCAGATATCGAATGAACAGGGTATACCCTGATGACGTAAACCTCGCACCTCAAGCGCTGATGATGGTCCGCGGAGCTGCTAGCTACTCCAATGATCCGTCCGTTCCTGATGCTGGGGGGGTATAAAAGATCTGCAAGGGCTTGATCTAACCCTCGAGCGTGCTAAACCCAATCCGGATGGGACTGGGCATACCAAAAGTGGCCGAGTTCAGGTGCTTGTCAGCCAAGTTGCTGTCGAGCCGGGCTTATAAGTGAGCATGCAAGTACGTT'



num_A <- lengths(regmatches(dna,gregexec('A', dna)))
num_T <- lengths(regmatches(dna,gregexec('T', dna)))
num_C <- lengths(regmatches(dna,gregexec('C', dna)))
num_G <- lengths(regmatches(dna,gregexec('G', dna)))

print(c(num_A, num_C, num_G, num_T))


###2 Transcribing DNA into RNA ----------------

# An RNA string is a string formed from the alphabet containing 'A', 'C', 'G', and 'U'.
# 
# Given a DNA string t corresponding to a coding strand, its transcribed RNA string u is formed by replacing all occurrences of 'T' in t with 'U' in u
# 
# 
# Given: A DNA string t having length at most 1000 nt.
#   
# Return: The transcribed RNA string of t


# Sample input:
# GATGGAACTTGACTACGTAAATT

# Sample output:
# GATGGAACTTGACTACGTAAATT


DNAtoRNA <- function(dnaseq){
  gsub("T", "U", dnaseq)
}

DNAtoRNA('CCTGGACTTGATAATTAAGGTACCGCGTCCCCGTTACTATCGCCCGGTTAAATGCCGAATAAGTCGCCTCCGTAAGGATGACCACGGCGATGAATAGAATACAATCTCG')



###3 Complementing a Strand of DNA ----------------

# 
# In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'.
# 
# The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, then taking the complement of each symbol (e.g., the reverse complement of "GTCA" is "TGAC")
# 
# Given: A DNA string s of length at most 1000 bp.
# 
# Return: The reverse complement sc of s

# Sample input: 
# AAAACCCGGT
# Sample output: 
# ACCGGGTTTT

dna <- 'GAAATGCCTTGCGATGGTCGAACGCCATCTGAATCTCCCCGGAGATTCTTCGGAAATACTGCTCATTTCAACGTGTCCAGTCTCTGGAGTCACCGAAGCCTACTTTACCATTTTACCC'

dna_list <- strsplit(dna,"")[[1]]

rev_dna <- rev(dna_list)

for(nc in 1:length(rev_dna)){
  if(rev_dna[nc] == 'A'){rev_dna[nc] <- 'T'}
  else if(rev_dna[nc] == 'T'){rev_dna[nc] <- 'A'}
  else if(rev_dna[nc] == 'G'){rev_dna[nc] <- 'C'}
  else if(rev_dna[nc] == 'C'){rev_dna[nc] <- 'G'}
}

print(paste(rev_dna, collapse = ""))

###4 Rabbits and Recurrence Relations ----------------


# A sequence is an ordered collection of objects (usually numbers), which are allowed to repeat. Sequences can be finite or infinite. Two examples are the finite sequence (π,−2–√,0,π) and the infinite sequence of odd numbers (1,3,5,7,9,…). We use the notation an to represent the n-th term of a sequence.

# A recurrence relation is a way of defining the terms of a sequence with respect to the values of previous terms. In the case of Fibonacci's rabbits from the introduction, any given month will contain the rabbits that were alive the previous month, plus any new offspring. A key observation is that the number of offspring in any month is equal to the number of rabbits that were alive two months prior. As a result, if Fn represents the number of rabbit pairs alive after the n-th month, then we obtain the Fibonacci sequence having terms Fn that are defined by the recurrence relation Fn=Fn−1+Fn−2(with F1=F2=1 to initiate the sequence). Although the sequence bears Fibonacci's name, it was known to Indian mathematicians over two millennia ago.

#   Given: Positive integers n≤40and k≤5.
# 
#   Return: The total number of rabbit pairs that will be present after n months, if we begin with 1 pair and in each generation, every pair of reproduction-age rabbits produces a litter of k rabbit pairs (instead of only 1 pair).

# Sample Input:
# 5 3

# Sample output:
# 19

recRabbits <- function(nk){
  initial = 1

  nk_split <- strsplit(nk, " ")[[1]]
  n = as.integer(nk_split[1])
  k = as.integer(nk_split[2])
  
  rabbits = integer()
  rabbits[1] <- initial
  rabbits[2] <- initial
  
  for (month in 3:n){
    rabbits[month] = rabbits[month-1] + k*rabbits[month-2]
  }  
  options(scipen =20)
  return (rabbits[n])
}


recRabbits('28 2')






recRabbits1 <- function(nk){
  
  nk_split <- strsplit(nk, " ")[[1]]
  n = as.integer(nk_split[1])
  k = as.integer(nk_split[2])
  
  F1 <-  1
  F2 <-  1
  Fn <-  0
  month <-  0
  while (month < (n-2)){
    Fn <-  F2 + F1* k
    F1 <-  F2
    F2 <-  Fn
    month <- month + 1
    }
    return (Fn)
}
recRabbits1('5 3')





rabiits <- function(n,k){
  a <- 1
  b <- 1
  for(i in 1:n){
    cat(a,"")
    next_num <- a + b *k
    b <- a
    a <- next_num
  }
}

rab <- 4
rabiits(5,3)







lapply()

for(i in c(3,5,6)){
  print(i)
  break
}

counter <- 0
while(counter < 5){
  print(counter)
  counter <- counter + 1
}




###5 Computing GC Content ----------------

# The GC-content of a DNA string is given by the percentage of symbols in the string that are 'C' or 'G'. For example, the GC-content of "AGCTATAG" is 37.5%. Note that the reverse complement of any DNA string has the same GC-content.

# DNA strings must be labeled when they are consolidated into a database. A commonly used method of string labeling is called FASTA format. In this format, the string is introduced by a line that begins with '>', followed by some labeling information. Subsequent lines contain the string itself; the first line to begin with '>' indicates the label of the next string.

# In Rosalind's implementation, a string in FASTA format will be labeled by the ID "Rosalind_xxxx", where "xxxx" denotes a four-digit code between 0000 and 9999.
 
# Given: At most 10 DNA strings in FASTA format (of length at most 1 kbp each).
 
# Return: The ID of the string having the highest GC-content, followed by the GC-content of that string. Rosalind allows for a default error of 0.001 in all decimal answers unless otherwise stated; please see the note on absolute error below.

# Sample input:
# >Rosalind_6404
# CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
# TCCCACTAATAATTCTGAGG
# >Rosalind_5959
# CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
# ATATCCATTTGTCAGCAGACACGC
# >Rosalind_0808
# CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
# TGGGAACCTGCGGGCAGTAGGTGGAAT

# Sample output:
# Rosalind_0808
# 60.919540

compareGC <- function(fasta){
  rs <- strsplit(fasta, '>')[[1]]
  rs1 <- rs[-1]
  rs2 <- gsub("\n", "", rs1)
  
  seq_name <- c()
  seq_gc <- c()
  for (i in 1:length(rs2)) {
    s1 <- strsplit(rs2[i], NULL)[[1]]
    seqn <- paste(s1[1:13], collapse="")
    seq <- s1[14:length(s1)]
    seqsum <- summary(as.factor(seq))
    gc <- unname(seqsum["G"]) + unname(seqsum["C"])
    seq_gc[i] <- gc/length(seq)*100
    seq_name[i] <- seqn
  }
  names(seq_gc) <- seq_name
  print(noquote(names(seq_gc[which(seq_gc == max(seq_gc))])))
  return(max(seq_gc))
  }
compareGC(">Rosalind_8915
ATGACAGTAACTAGCATTGAAAGTAGATGACCACTTGGCGTAATGTAAAAGACAACGAAC
GTTATTCGAGCACGAATATGTTGAGTCGTAACTATAGACCGTTTCTACGGTCCAGTTTGA
CATGACCTGGGGATCAAAAGAATGATGAGTCATGAGCACCCTTTCACGGGTTCAGGTGCG
CATCTAGCCACCAGTCACATTCCCGTGAGCAAAGTAAAGCTTACTTTTCGCGGGTGTCGC
TTCCGTTCTCCGCGTGCATACAGGGTACGTGAACGCTAGTGAGCGTTGTGGCCTATTGAT
ACTAGGACATTCGTTCCGAAAGCTAATGCCGCAATGCTGTGATGTAAATTGTGCTAAGAC
CTTGCCTAAAATTTATCACAACACGGTTTCTACTAGCGTGCCACGAGGGGGTGCACCGCT
TATCATTTGCATGAGGGTGAGTCATGAGAGGTGATTAGTCTGCTCCCTGGCACAAGTGAG
AAAGCCCAGGACACACGGTAGTCTGCCGGGTTCGCTTTATGCGTTACCGGTTCACCGGTA
AAATACCAAACCCCCGGTCATTATGGTATTCACCGGCGCCCCCTTGTTTGTGTATGCTTT
CCAAACTACGATTAACATAGTAGAAAGATGTATCCGGGTAACCTGCGACTTAGTCTTTGA
TTGGTGCCTTGCCCATAGCCCGTATGCGCTACCCAAGATACTCGAGTCGATTGATGGGGA
CACAGGTGTCTAATAGACGGCTAATACGTTGTGGCCTGGACTCTGTGGAAACTAATGTGT
AACTGTCTAGACTCCAACGTTCCCGCGGTCTAGCATGACAGCTAACTCAGCTAGTCTAGA
CGTAGAGTGCGCTGACCATCGTTGGGGAGCCGACCCGTGTGGCGCACCGGAC
")

library(seqinr)
seqs <- read.fasta("rosalind_gc.txt")
gc_content <- apply(matrix(names(seqs)), 1, function(x)GC(seqs[[x]]))
highest_gc <- which(gc_content == max(gc_content))
print(names(seqs)[highest_gc])
print(paste(signif(gc_content[highest_gc], 7) * 100, "%", sep=""))


###6 Counting Point Mutations ----------------

# Given two strings s and t of equal length, the Hamming distance between s and t, denoted dH(s,t), is the number of corresponding symbols that differ in s and t.

# Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
# 
# Return: The Hamming distance  dH(s,t)

# Sample input: 
# GAGCCTACTAACGGGAT
# CATCGTAATGACGGCCT

# Sample output: 
# 7

seqs <- 'CTGATATCAACAACTTGGGCCGCCTTGTTCGTTTTGATCCGGGCCCTTGCGAAACTATTGACGTTAGATGGTATTAGTGGACGCAGCGCAGTTGAATGAAGAATAAAGAATGGTTCCTATTACATCTGCGAAACTATGTTAAGTCATTTGCGACTGTCGTACGATGTCACAGACAGCTAATCACCGGCTGCTTAAATTCCCTATGGATCCTGCACGACAATGGCCGAAGTGAACGAGAGATTCATGCACGCAGAGGCACGAAATTCACTTACTTGATGTGTACAACCTGATACTCCCATAGAGATTTCTATGCTTGCACCATTAATATGACCGCAGTTCCTGTTCGTTGTTTTCATCTATCAGCGCGTAGCTATTGCTACCTTACTCTAAGGTTGTAATTTCATACAAAGCAAGAGAGTGGATACCTGTCCTGACTAACGCATCCATTACACCTATTTGACTAGCCCAAAACATGTAAGTGGAGTATTCACCAACCAATTTGTGCGCGCACGCGGGCATAGCCCGAAGCAGCAGCATAGCCGCAACTCTTGATTTCGACCAGGTGAGGTCATCTCCAAGTCGGACCGGTCTTGAGTCCGTACCCCTTAGCAGCGAAACAGTGTGGCTCCAGACCGTAGCCACTATATAAGGGTCATGTCCGATCGGTTTGGTAGAATACTGTATTGAGCGTGGCTGAGGTGTGGCCCAACTTCGAGACTCCAGTCACTTTTCTTGTTTTGCTAAAGCTGGGTGCGGAAAGTTTTGACAGAACATTTTTGGACGTGAAACGATGAGTATGCTGAAATTTAGACTTGTCCGGGTATCGGGCGTTTCTGGGGAACGTCTTCGGCGAGCATCGCGTCCGACTGTACTAACCATGCAGTTGAGTGAACACGGATCGTGTCCACCCATGACGGTGGTGTTGGGAAGATGTATGTACTGTACCGGCTGCATGACTAGGATCGCATCCGTCCGTTACGTTCACGAACA
AAGGCTTCACCCAGACGGGCCGAGCCTTTAATCGCTGGTGGCGCACGGACAAAAAAAAGTAGGAAGGATGGCTCGTGTTAATGGATCTCAGCTTCGGCATAGGTAAAACATCGTTTCCAGAATAGTTGGCAATCTGTCGAAGGCCAACAGCAAATAGGATGTAGTGGCCCGGACCGCCGTGGATACGGTTCTCACATGGGTTACGGATCTAGCACGAACGTCTAGGACCCGACCTTCAAGCTCACGTACGCGGGAGGGTGCCTATCACTTAATTGAGTTTTATAACATGATCGACTACTAGCGACTACCGTGCCCGTCCCGGTGATATGACCGCGGCTCAAAACCGTTGTGGGTATGTCTCGGCGCGTCCCAATTCCAAGGCTCAGGTAAAGCCGAATTTTCGAGGTCACTAACAGAGCGTATATCAGTACTCTCTGATAACGCCGTTATCATGGTATTACTAACGGCAGGTTGGTTCACGCCGTTTTTACGAAGCAAATTGTATGCTTTAGTCCGACAAGCGTCTCGCCGCAGGAGGGGGCCCCGAATTGTTGTAGTACTGGTGAGGTGTTTGCCTACCCCGATCGGCCCTCAATTCATGCCACTTGTCAGGGATCCCAGTAGTATCCGGATTTACCCATCTATTTACGGTTCAGTCTGCATAAATTTGGTGAATTACCATCATGCCAAGCGCCGTGTCTTGGGTCTATATCACTAGACTATTTAGTTGTCGGTTCTAGATACTGCCGGACCAAGTTAGGATGGCACCTAGATTAGAGTCCGTGTAGACATGATTTTGCAGACACTCGGAGACTTTGGGGTGAAGGGCCCTTGAAGGCCCAGACTAGCGGGTGGTCTGCGGTGACGTGGCCTAATGCTCGAAGGGCTGTTTAAACGTTATGGTCTAGATCGGCGCGTGGTATAGGTAAAATGTGTGGAGACCGGACTCAGTCTCACTTCTGTGGTCTCCTGCCTTTACTCCCAGAATGC'
seqs_list <- strsplit(seqs,"\n")[[1]]
seq1 <- strsplit(seqs_list[1],"")[[1]]
seq2 <- strsplit(seqs_list[2],"")[[1]]
length(which(seq1 != seq2))

count = 0
for(i in 1:length(seq1)){
  if (seq1[i] != seq2[i]) {
    count <- count + 1
  }
}
print(count)


###7 Mendel's First Law ----------------

# Given: Three positive integers k, m, and n, representing a population containing k+m+n organisms: k individuals are homozygous dominant for a factor, m are heterozygous, and n are homozygous recessive.

# Return: The probability that two randomly selected mating organisms will produce an individual possessing a dominant allele (and thus displaying the dominant phenotype). Assume that any two organisms can mate.


# Sample input:
# 2 2 2 

# Sample output:
# 0.78333

sims <- rep(NA,1000000)
pop <- c(rep('AA', 20), rep('Aa',26), rep('aa',22))
for(i in 1:length(sims)){
  pair <- sample(pop,2)
  sims[i] <- sample(strsplit(pair[1],NULL)[[1]],1) == 'A' | sample(strsplit(pair[2],NULL)[[1]],1) == 'A'}

sum(sims)/length(sims)

###8 Finding a Motif in DNA ----------------

# Given two strings s and t, t is a substring of s if t is contained as a contiguous collection of symbols in s (as a result, t must be no longer than s).

# The position of a symbol in a string is the total number of symbols found to its left, including itself (e.g., the positions of all occurrences of 'U' in "AUGCUUCAGAAAGGUCUUACG" are 2, 5, 6, 15, 17, and 18). The symbol at position i of s is denoted by s[i].

# A substring of s can be represented as s[j:k], where j and k represent the starting and ending positions of the substring in s; for example, if s = "AUGCUUCAGAAAGGUCUUACG", then s[2:5] = "UGCU".

# The location of a substring s[j:k] is its beginning position j; note that t will have multiple locations in s if it occurs more than once as a substring of s (see the Sample below).

# Given: Two DNA strings s and t (each of length at most 1 kbp).

# Return: All locations of t as a substring of s.

# Sample input:
# GATATATGCATATACTT
# ATAT

# Sample output: 
# 2 4 10


input <- "TAATGTCTGCATCCAGTCTGCAGTCTGCATTGTCTGCAGTCTGCAAGTCTGCAGTCTGCATTAGTCTGCACGTCTGCACGCCCGGGGGTCTGCAGTCTGCATTCGTCTGCACTTGTCTGCAACGTCTGCAACCGTCTGCATTATGTCTGCAATTTGTCTGCAGACTGTCTGCATATGGGTCTGCAAGTCTGCATGTCTGCACCCCTCGTCTGCATTGACGTCTGCACGTCTGCACTCGTCTGCAGGGGTGGTCTGCAGGTCTGCAGGTCTGCAGTCTGCAGTCTGCAGTCTGCAACGTCTGCAGGTGTCTGCATATGTCTGCACTGTCTGCAACGTCTGCAGCGTCTGCAAGGTCTGCATTGTCTGCAGTCTGCAGTCTGCACACATTATTCTGGTGTCTGCAGGTCTGCAAATCGTCTGCAGTCTGCATGACGGGTCTGCATGGTCTGCACAGTCTGCAGTCTGCATTAGGTCTGCATCTTTGTCTGCAGGTCTGCACTTGCGGACTGAGTTGGTCTGCACCAAACTAGTCTGCATGGTCTGCAGCTACCGTCTGCACCACCTCACGGTCTGCACAGTCTGCACGGCAAGCACCCCGAGTCTGCAGTCTGCAGTCTGCAGTCTGCAGTCTGCAGTCTGCAGTCTGCAGGGCGTCTGCAGGTCTGCAGGTCTGCAGTCTGCAATGTCTGCAAGTCTGTCTGCAGGTCTGCATTCCGTCTGCAGGGTCTGCAATGTACATGCGTGTCTGCAGTCTGCAACGCAGTTATCTCGTCTGCAAAGTCTGCACCGTCTGCATGTCTGCAGTCTGCA
GTCTGCAGT"

seqs_list <- strsplit(input,"\n")[[1]]
seq <- strsplit(seqs_list[1],NULL) [[1]]
target <- strsplit(seqs_list[2],NULL) [[1]]


match_start = c()
for (i in 1:length(seq)){
  end = length(target) - 1 + i
  if(paste(target, collapse = "") == paste(seq[i:end], collapse = "")){
    match_start <- c(match_start, i)
  }
}
paste(match_start,collapse = " ")


###9 Calculating Expected Offspring ----------------

# For a random variable X taking integer values between 1 and n, the expected value of X is E(X)=∑nk=1k×Pr(X=k). The expected value offers us a way of taking the long-term average of a random variable over a large number of trials.

# As a motivating example, let X be the number on a six-sided die. Over a large number of rolls, we should expect to obtain an average of 3.5 on the die (even though it's not possible to roll a 3.5). The formula for expected value confirms that E(X)=∑6k=1k×Pr(X=k)=3.5.

# More generally, a random variable for which every one of a number of equally spaced outcomes has the same probability is called a uniform random variable (in the die example, this "equal spacing" is equal to 1). We can generalize our die example to find that if X is a uniform random variable with minimum possible value a and maximum possible value b, then E(X)=a+b2. You may also wish to verify that for the dice example, if Y is the random variable associated with the outcome of a second die roll, then E(X+Y)=7.

# Given: Six nonnegative integers, each of which does not exceed 20,000. The integers correspond to the number of couples in a population possessing each genotype pairing for a given factor. In order, the six given integers represent the number of couples having the following genotypes:

# AA-AA
# AA-Aa
# AA-aa
# Aa-Aa
# Aa-aa
# aa-aa


# Return: The expected number of offspring displaying the dominant phenotype in the next generation, under the assumption that every couple has exactly two offspring.


num_couples <- '18178 18716 18749 16622 17829 16725'
num_couples <- strsplit(num_couples, " ")[[1]]

probablities_having_dominant <- c(1,1,1, 0.75, 0.5, 0)

total <- sum(2*(as.integer(num_couples)*probablities_having_dominant))
total

###10 Translating RNA into Protein  ----------------

# The 20 commonly occurring amino acids are abbreviated by using 20 letters from the English alphabet (all letters except for B, J, O, U, X, and Z). Protein strings are constructed from these 20 symbols. Henceforth, the term genetic string will incorporate protein strings along with DNA strings and RNA strings.

# The RNA codon table dictates the details regarding the encoding of specific codons into the amino acid alphabet.

# Given: An RNA string s corresponding to a strand of mRNA (of length at most 10 kbp).

# Return: The protein string encoded by s.

# Sample input: AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA

# Sample output: MAMAPRTEINSTRING
lines_string <- readLines("data/rosalind_prot.txt")
rna_example <- paste(lines_string,collapse="")

rna_to_protein <- function(rna_string){
  codon_table <- c(
    "UUU"="F", "UUC"="F", "UUA"="L", "UUG"="L", "UCU"="S", "UCC"="S", "UCA"="S", "UCG"="S",
    "UAU"="Y", "UAC"="Y", "UAA"="Stop", "UAG"="Stop", "UGU"="C", "UGC"="C", "UGA"="Stop", "UGG"="W",
    "CUU"="L", "CUC"="L", "CUA"="L", "CUG"="L", "CCU"="P", "CCC"="P", "CCA"="P", "CCG"="P",
    "CAU"="H", "CAC"="H", "CAA"="Q", "CAG"="Q", "CGU"="R", "CGC"="R", "CGA"="R", "CGG"="R",
    "AUU"="I", "AUC"="I", "AUA"="I", "AUG"="M", "ACU"="T", "ACC"="T", "ACA"="T", "ACG"="T",
    "AAU"="N", "AAC"="N", "AAA"="K", "AAG"="K", "AGU"="S", "AGC"="S", "AGA"="R", "AGG"="R",
    "GUU"="V", "GUC"="V", "GUA"="V", "GUG"="V", "GCU"="A", "GCC"="A", "GCA"="A", "GCG"="A",
    "GAU"="D", "GAC"="D", "GAA"="E", "GAG"="E", "GGU"="G", "GGC"="G", "GGA"="G", "GGG"="G"
  )
  
  codons <- substring(rna_string, seq(1,nchar(rna_string)-2,3), seq(3, nchar(rna_string),3))
  
  protein_vec <- codon_table[codons]
  
  stop_codon <- which(protein_vec == "Stop")
  
  protein_vec <- protein_vec[1:(stop_codon[1]-1)]
  
  return(paste(protein_vec, collapse = ""))
}

rna_to_protein(rna_example)

###11 Consensus and Profile ----------------

# Given: A collection of at most 10 DNA strings of equal length (at most 1 kbp) in FASTA format.

# Return: A consensus string and profile matrix for the collection. (If several possible consensus strings exist, then you may return any one of them.)

#Sample Dataset
#>Rosalind_1
#ATCCAGCT
#>Rosalind_2
#GGGCAACT
#>Rosalind_3
#ATGGATCT
#>Rosalind_4
#AAGCAACC
#>Rosalind_5
#TTGGAACT
#>Rosalind_6
#ATGCCATT
#>Rosalind_7
#ATGGCACT

#Sample Output:
#ATGCAACT
#A: 5 1 0 0 5 5 0 0
#C: 0 0 1 4 2 0 6 1
#G: 1 1 6 3 0 1 0 0
#T: 1 5 0 0 0 1 1 6


###12 Locating Restriction Sites ----------------

# Given: A DNA string of length at most 1 kbp in FASTA format.

# Return: The position and length of every reverse palindrome in the string having length between 4 and 12. You may return these pairs in any order.

# Sample input:

#>Rosalind_24
#TCAATGCATGCGGGTCTATATGCAT

# Sample Out:
# 4 6
# 5 4
# 6 6
# 7 4
# 17 4
# 18 4
# 20 6
# 21 4

###13 Enumerating Gene Orders ----------------

#A permutation of length n is an ordering of the positive integers {1,2,…,n}. For example, π=(5,3,2,1,4) is a permutation of length 5.

# Given: A positive integer n≤7

# Return: The total number of permutations of length n, followed by a list of all such permutations (in any order)

# Sample input: 3

# Sample output: 
# 6
# 1 2 3
# 1 3 2
# 2 1 3
# 2 3 1
# 3 1 2
# 3 2 1

#install.packages("combinat")
# The easy way
library(combinat)

get_perm <- function(n){
  n_seq <- 1:n
  
  perms <- permn(n_seq)
  
  cat(length(perms), '\n')
  
  for(p in perms) {
    cat(p, sep = " ")
    cat("\n")
  }
}

get_perm(6)

# The pro way

perm1 <- function(l, k) {
  unlist(lapply(l, function(x){ lapply(1:(length(x)+1), function(i) { 
    if (i == 0){c(k,x) } 
    else if(i==(length(x)+1)) { c(x,k) } 
    else { c(x[0:(i-1)],k,x[i:length(x)])}})}), 
    recursive=F)
}
permAll <- function(n) {  if (n == 1) {return (list(1))} else {return(perm1(permAll(n-1), n))} }
permAll(3)

###14 Finding a Shared Motif  ----------------

# A common substring of a collection of strings is a substring of every member of the collection. We say that a common substring is a longest common substring if there does not exist a longer common substring. For example, "CG" is a common substring of "ACGTACGT" and "AACCGTATA", but it is not as long as possible; in this case, "CGTA" is a longest common substring of "ACGTACGT" and "AACCGTATA".

# Note that the longest common substring is not necessarily unique; for a simple example, "AA" and "CC" are both longest common substrings of "AACC" and "CCAA".

# Given: A collection of k(k≤100) DNA strings of length at most 1 kbp each in FASTA format.

# Return: A longest common substring of the collection. (If multiple solutions exist, you may return any single solution.)


# Sample input: 
# >Rosalind_1
# GATTACA
# >Rosalind_2
# TAGACCA
# >Rosalind_3
# ATACA

# Sample output: 

# AC





###15 Overlap Graphs  ----------------

# A graph whose nodes have all been labeled can be represented by an adjacency list, in which each row of the list contains the two node labels corresponding to a unique edge.

# A directed graph (or digraph) is a graph containing directed edges, each of which has an orientation. That is, a directed edge is represented by an arrow instead of a line segment; the starting and ending nodes of an edge form its tail and head, respectively. The directed edge with tail v and head w is represented by (v,w) (but not by (w,v)). A directed loop is a directed edge of the form (v,v).

# For a collection of strings and a positive integer k, the overlap graph for the strings is a directed graph Ok in which each string is represented by a node, and string s is connected to string t with a directed edge when there is a length k suffix of s that matches a length k prefix of t, as long as s≠t; we demand s≠t to prevent directed loops in the overlap graph (although directed cycles may be present).


# Given: A collection of DNA strings in FASTA format having total length at most 10 kbp.

# Return: The adjacency list corresponding to O3. You may return edges in any order.

# Sample input: 
# >Rosalind_0498
# AAATAAA
# >Rosalind_2391
# AAATTTT
# >Rosalind_2323
# TTTTCCC
# >Rosalind_0442
# AAATCCC
# >Rosalind_5013
# GGGTGGG

# Sample output:
# Rosalind_0498 Rosalind_2391
# Rosalind_0498 Rosalind_0442
# Rosalind_2391 Rosalind_2323



###16 Independent Alleles   ----------------



# Two events A and B are independent if Pr(A and B) is equal to Pr(A)×Pr(B). In other words, the events do not influence each other, so that we may simply calculate each of the individual probabilities separately and then multiply.

# More generally, random variables X and Y are independent if whenever A and B are respective events for X and Y, A and B are independent (i.e., Pr(A and B)=Pr(A)×Pr(B)).

# As an example of how helpful independence can be for calculating probabilities, let X and Y represent the numbers showing on two six-sided dice. Intuitively, the number of pips showing on one die should not affect the number showing on the other die. If we want to find the probability that X+Y is odd, then we don't need to draw a tree diagram and consider all possibilities. We simply first note that for X+Y to be odd, either X is even and Y is odd or X is odd and Y is even. In terms of probability, Pr(X+Y is odd)=Pr(X is even and Y is odd)+Pr(X is odd and Y is even). Using independence, this becomes [Pr(X is even)×Pr(Y is odd)]+[Pr(X is odd)×Pr(Y is even)], or (12)2+(12)2=12. You can verify this result in Figure 2, which shows all 36 outcomes for rolling two dice.

# Given: Two positive integers k(k≤7) and N(N≤2k). In this problem, we begin with Tom, who in the 0th generation has genotype Aa Bb. Tom has two children in the 1st generation, each of whom has two children, and so on. Each organism always mates with an organism having genotype Aa Bb.

# Return: The probability that at least N Aa Bb organisms will belong to the k-th generation of Tom's family tree (don't count the Aa Bb mates at each level). Assume that Mendel's second law holds for the factors.


# Sample input: 
# 2 1

# Sample output: 
# 0.684

binomial_function <- function(k,N) {
  n <- 2^k
  p <- 0.25
  prob_N_in_k_gen <- 1-pbinom(N-1, size = n, prob = p)
  return(round(prob_N_in_k_gen,3))
}

binomial_function(6,16)

################ SPRING 2026 ################ 


###17 Enumerating k-mers Lexicographically   ----------------

#Assume that an alphabet A has a predetermined order; that is, we write the alphabet as a permutation A=(a1,a2,…,ak), where a1<a2<⋯<ak. For instance, the English alphabet is organized as (A,B,…,Z).

#Given two strings s and t having the same length n, we say that s precedes tin the lexicographic order (and write s<Lext) if the first symbol s[j] that doesn't match t[j] satisfies sj<tj in A.

#Given: A collection of at most 10 symbols defining an ordered alphabet, and a positive integer n(n≤10).

#Return: All strings of length n that can be formed from the alphabet, ordered lexicographically (use the standard order of symbols in the English alphabet).


enumerate_lex <- function(alphabet, n){

  alphabet_lists <- replicate(n, alphabet, simplify = F)

  lex_grid <- expand.grid(alphabet_lists)

  lex_grid <- lex_grid[,seq(n,1)]

  results <- apply(lex_grid, 1, paste, collapse="")
  
  return(results)
}
alphabet1 <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

solved_lex <- enumerate_lex(alphabet1, 3)
rosalind_format <- paste(solved_lex, collapse = "\n")
cat(rosalind_format)

# Cool solution that actually scans the rosalind output file

dat <- scan( "C:/rosalind/lexf/rosalind_lexf.txt", what=character(), sep="\n" )

seq <- unlist( strsplit( dat[1], " " ) )
num <- dat[2]

kRev <- function(x) { 
  sapply( strsplit( x, "" ), function(xx) {
    paste( rev.default( xx ), collapse = "" )
  })
}

cat( kRev( do.call( paste0, expand.grid( rep( list(seq), num ) ) ) ), 
     file="C:/rosalind/lexf/out.txt", sep="\n" )

###18 RNA Splicing   ----------------

#After identifying the exons and introns of an RNA string, we only need to delete the introns and concatenate the exons to form a new string ready for translation.

#Given: A DNA string k (of length at most 1 kbp) and a collection of substrings of k acting as introns. All strings are given in FASTA format.

#Return: A protein string resulting from transcribing and translating the exons of k. (Note: Only one solution will exist for the dataset provided.)

#Sample Input:
#>Rosalind_10
#ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
#>Rosalind_12
#ATCGGTCGAA
#>Rosalind_15
#ATCGGTCGAGCGTGT

#Sample Output

#MVYIADKQHVASREAYGHMFKVCA
 

library(Biostrings)

fasta <- readDNAStringSet("data/rosalind_splc.txt")

dna_full <- as.character(fasta[1])
introns  <- as.character(fasta[2:length(fasta)])

spliced_dna <- dna_full
for (i in introns){
  spliced_dna <- gsub(i, "", spliced_dna)
}

final_protein <- translate(DNAString(spliced_dna))

cat(as.character(final_protein),'\n')

###19 k-Mer Composition   ----------------

#For a fixed positive integer k, order all possible k-mers taken from an underlying alphabet lexicographically.

#Then the k-mer composition of a string s can be represented by an array A for which A[m] denotes the number of times that the m th k-mer (with respect to the lexicographic order) appears in s.

#Given: A DNA string s in FASTA format (having length at most 100 kbp).

#Return: The 4-mer composition of s.


#Sample input:
#>Rosalind_6431
#CTTCGAAAGTTTGGGCCGAGTCTTACAGTCGGTCTTGAAGCAAAGTAACGAACTCCACGG
#CCCTGACTACCGAACCAGTTGTGAGTACTCAACTGGGTGAGAGTGCAGTCCCTATTGAGT
#TTCCGAGACTCACCGGGATTTTCGATCCAGCCTCAGTCCAGTCTTGTGGCCAACTCACCA
#AATGACGTTGGAATATCCCTGTCTAGCTCACGCAGTACTTAGTAAGAGGTCGCTGCAGCG
#GGGCAAGGAGATCGGAAAATGTGCTCTATATGCGACTAAAGCTCCTAACTTACACGTAGA
#CTTGCCCGTGTTAAAAACTCGGCTCACATGCTGTCTGCGGCTGGCTGTATACAGTATCTA
#CCTAATACCCTTCAGTTCGCCGCACAAAAGCTGGGAGTTACCGCGGAAATCACAG

#Sample output:
#4 1 4 3 0 1 1 5 1 3 1 2 2 1 2 0 1 1 3 1 2 1 3 1 1 1 1 2 2 5 1 3 0 2 2 1 1 1 1 3 1 0 0 1 5 5 1 5 0 2 0 2 1 2 1 1 1 2 0 1 0 0 1 1 3 2 1 0 3 2 3 0 0 2 0 8 0 0 1 0 2 1 3 0 0 0 1 4 3 2 1 1 3 1 2 1 3 1 2 1 2 1 1 1 2 3 2 1 1 0 1 1 3 2 1 2 6 2 1 1 1 2 3 3 3 2 3 0 3 2 1 1 0 0 1 4 3 0 1 5 0 2 0 1 2 1 3 0 1 2 2 1 1 0 3 0 0 4 5 0 3 0 2 1 1 3 0 3 2 2 1 1 0 2 1 0 2 2 1 2 0 2 2 5 2 2 1 1 2 1 2 2 2 2 1 1 3 4 0 2 1 1 0 1 2 2 1 1 1 5 2 0 3 2 1 1 2 2 3 0 3 0 1 3 1 2 3 0 2 1 2 2 1 2 3 0 1 2 3 1 1 3 1 0 1 1 3 0 2 1 2 2 0 2 1 1


library(seqinr)
  
sequence <- read.fasta("data/rosalind_kmer.txt")
cat(count(sequence[[1]],4))

###20 Calculating Protein Mass  ----------------
#In a weighted alphabet, every symbol is assigned a positive real number called a weight. A string formed from a weighted alphabet is called a weighted string, and its weight is equal to the sum of the weights of its symbols.

#The standard weight assigned to each member of the 20-symbol amino acid alphabet is the monoisotopic mass of the corresponding amino acid.

#Given: A protein string P of length at most 1000 aa.

#Return: The total weight of P. Consult the monoisotopic mass table.

#Sample dataset:
#SKADYEK

#Sample output:
#821.392

monoiso_mass_table <- c("A" = 71.03711, "C" = 103.00919, "D" = 115.02694, "E" = 129.04259, "F" = 147.06841,"G" = 57.02146, "H" = 137.05891, "I" = 113.08406, "K" = 128.09496, "L" = 113.08406,"M" = 131.04049, "N" = 114.04293, "P" = 97.05276, "Q" = 128.05858, "R" = 156.10111,"S" = 87.03203, "T" = 101.04768, "V" = 99.06841, "W" = 186.07931, "Y" = 163.06333)



###21 Inferring mRNA from Protein  ----------------

#For positive integers a and n, a modulo n(written a mod n in shorthand) is the remainder when a is divided by n. For example, 29 mod 11=7 because 29=11×2+7.

#Modular arithmetic is the study of addition, subtraction, multiplication, and division with respect to the modulo operation. We say that a and b are congruent modulo n if a mod n=b mod n; in this case, we use the notation a≡b mod n.

#Two useful facts in modular arithmetic are that if a≡b mod n and c≡d mod n, then a + c ≡ b + d mod n and a × c ≡ b × d mod n. To check your understanding of these rules, you may wish to verify these relationships for a=29, b=73, c=10, d=32, and n=11.

#As you will see in this exercise, some Rosalind problems will ask for a (very large) integer solution modulo a smaller number to avoid the computational pitfalls that arise with storing such large numbers.

#Given: A protein string of length at most 1000 aa.

#Return: The total number of different RNA strings from which the protein could have been translated, modulo 1,000,000. (Don't neglect the importance of the stop codon in protein translation.)

#Sample dataset:
#MA

#Sample output:
#12

