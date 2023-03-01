library(readxl)
library(tidyverse)


db <- read_excel('dataset.xlsx')

summary(db)

db[,'id'] <- 1:192


# quante righe per ogni valore delle 4 variabili
table(db$V1)
table(db$V2)
table(db$V3)
table(db$V4)
# per avere A, B e C bilanciate nel complesso bisogna 
# avere 64 A, 64 B e 64 C (192 / 3 = 64)


# guardiamo le distribuzioni di V2 e V3 complessive
table(db$V2, db$V3)


# guardiamo le distribuzioni di V2 e V3 stratificate per V4
table(db$V2, db$V3, db$V4)
# sono già abbastanza bilanciate... ma non al massimo possibile



# un metodo è lavorare sulle 3 table:
# - table(db$V2, db$V3, db$V4)
# e far si che risultino distribuite il più possibile simili


# per lavorare su queste table ci si può servire
# della funzione expand.grid:
# mette prima tutta la  prima colonna della table, poi tutta la seconda colonna...
expand.grid(table(db$V2, db$V3)) 



# funzione che in input vuole  V2 e V3
# e in output torna, per ogni combinazione di V2 e V3,
# quante 'A' 'B' e 'C' mettere


# settando diversamente questo seme
# si possono ottenere combinazioni di 'A' 'B' e 'C' leggermente
# diverse (in termini delle table marginali di 
# V2 e V4 e di V3 e di V4)
seme <- 910


# table V2 x V3 (8x5)
tt <- table(db$V2, db$V3)

# creaimo un data.frame dalla table con expand.grid
eg <- expand.grid(tt)

# riferimento al numero di V2
eg[,'V2'] <- rep(1:8, 5)

# riferimento al numero di V3
eg[,'V3'] <- sort(rep(c(1,2,3,4,6),8))

# cambio il nome alla frequenza relativa
# alla combinazione di V2 x V3
names(eg)[1] <- 'freq'


# creo colonne che indicheranno, per ogni combo di V2 e V3,
# quanti valori di 'A' 'B' e 'C' ci sono
eg[,'num_A'] <- rep(0, 40)
eg[,'num_B'] <- rep(0, 40)
eg[,'num_C'] <- rep(0, 40)



# come primo passo dell'algoritmo guardiamo i numeri
# maggiori di 3... di questi valori prendiamo il valore
# multiplo di 3 più piccolo a cui associamo 1/3 di 'A'
# 1/3 di 'B' e 1/3 di 'C'... con questa mossa
# bilanciamo perfettamente 171 valori (57 A, 57 B e 57 C)

for(i in 1:NROW(eg)){
  
  div_3 <- floor(eg$freq[i] / 3)
  
  if(div_3 > 0){
    
    # assegno 1/3 del numero (divisibile per 3)
    # ad 'A', a 'B' ed a 'C'
    eg$num_A[i] <- div_3
    eg$num_B[i] <- div_3
    eg$num_C[i] <- div_3
    
    # aggiorno la frequenza, contando quante lettere ho assegnato
    eg$freq[i] <- eg$freq[i] - 3*div_3
    
  }
}

# ora sia V3 che  V2 sono
# perfettamente bilanciate... però rimangono ancora
# 21 'posti' da assegnare: 7 per A, 7 per B e 7 per C


# si potrebbe fare in maniera grezza e provare tutte le combinazioni
# rimaste... però siamo nell'ordine di 10^19 e quindi è infattibile

# data la struttura di eg si vede che 2 righe relative a V3 con valore 2 sommano
# a 6 (multiplo di 3) e così assegnando 2 A 2 B e 2 C si possono bilanciare perfettamente
# le V3 da 2... poi così facendo si sbilancerebbero le relative righe di V2...
# però, dopo aver bilanciato le V3 da 2, le righe di V2 da 2,3,7 e 8 non vengono
# modificate, e queste sono in tutto 6.... poi ancora si avrebbe uno sbilnaciamento 
# sulle colonne e così via però intanto 12 lettere su 21 bilancerebbero le V2
# e le V3 da 2...

# quindi ora prima assegniamo casualmente 'A' 'B' e 'C' alle V3 da 2, non mettendo
# nella stessa cella V2 x V3 la stessa lettera (quando ci sono 2 'posti')
# e poi facciamo lo stesso con le righe di V2 da 2,3,7 e 8 


# V3 da 2:

# si può cambiare l'ordine delle lettere
# cambiando il seme all'inizio (riga 52)
set.seed(seme)
ordine_lettere <- sample(rep(c('A','B','C'),2))

# controllo che non ci siano due lettere di fila uguali,
# sennò le cambio.. questo per le righe con freq = 2
cond_lett <- FALSE

while(!cond_lett){
  
  true_conseg <- rep(FALSE,6)
  
  for(i in 1:5){
    
    if(ordine_lettere[i] == ordine_lettere[i+1]){
      true_conseg[i] <- TRUE
      
    }
  }
  
  if(!any(true_conseg)){
    cond_lett <- TRUE
  }
  
  else{
    set.seed(seme*sample(1:1e4,1))
    ordine_lettere <- sample(rep(c('A','B','C'),2))
  }
}

# ora metto le lettere (le conto) nelle righe con V3 2 (rimaste con frequenza diversa da zero)

# prima riga (con V3 = 2 e con freq != 0)
eg$num_A[eg$V3 == 2 & eg$freq != 0][1] <- ifelse(ordine_lettere[1] == 'A',
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][1] + 1,
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][1])
eg$num_B[eg$V3 == 2 & eg$freq != 0][1] <- ifelse(ordine_lettere[1] == 'B',
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][1] + 1,
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][1])
eg$num_C[eg$V3 == 2 & eg$freq != 0][1] <- ifelse(ordine_lettere[1] == 'C',
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][1] + 1,
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][1])
eg$num_A[eg$V3 == 2 & eg$freq != 0][1] <- ifelse(ordine_lettere[2] == 'A',
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][1] + 1,
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][1])
eg$num_B[eg$V3 == 2 & eg$freq != 0][1] <- ifelse(ordine_lettere[2] == 'B',
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][1] + 1,
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][1])
eg$num_C[eg$V3 == 2 & eg$freq != 0][1] <- ifelse(ordine_lettere[2] == 'C',
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][1] + 1,
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][1])

# seconda riga
eg$num_A[eg$V3 == 2 & eg$freq != 0][2] <- ifelse(ordine_lettere[3] == 'A',
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][2] + 1,
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][2])
eg$num_B[eg$V3 == 2 & eg$freq != 0][2] <- ifelse(ordine_lettere[3] == 'B',
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][2] + 1,
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][2])
eg$num_C[eg$V3 == 2 & eg$freq != 0][2] <- ifelse(ordine_lettere[3] == 'C',
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][2] + 1,
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][2])

# terza riga
eg$num_A[eg$V3 == 2 & eg$freq != 0][3] <- ifelse(ordine_lettere[4] == 'A',
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][3] + 1,
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][3])
eg$num_B[eg$V3 == 2 & eg$freq != 0][3] <- ifelse(ordine_lettere[4] == 'B',
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][3] + 1,
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][3])
eg$num_C[eg$V3 == 2 & eg$freq != 0][3] <- ifelse(ordine_lettere[4] == 'C',
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][3] + 1,
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][3])

# quarta riga
eg$num_A[eg$V3 == 2 & eg$freq != 0][4] <- ifelse(ordine_lettere[5] == 'A',
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][4] + 1,
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][4])
eg$num_B[eg$V3 == 2 & eg$freq != 0][4] <- ifelse(ordine_lettere[5] == 'B',
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][4] + 1,
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][4])
eg$num_C[eg$V3 == 2 & eg$freq != 0][4] <- ifelse(ordine_lettere[5] == 'C',
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][4] + 1,
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][4])
eg$num_A[eg$V3 == 2 & eg$freq != 0][4] <- ifelse(ordine_lettere[6] == 'A',
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][4] + 1,
                                                    eg$num_A[eg$V3 == 2 & eg$freq != 0][4])
eg$num_B[eg$V3 == 2 & eg$freq != 0][4] <- ifelse(ordine_lettere[6] == 'B',
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][4] + 1,
                                                    eg$num_B[eg$V3 == 2 & eg$freq != 0][4])
eg$num_C[eg$V3 == 2 & eg$freq != 0][4] <- ifelse(ordine_lettere[6] == 'C',
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][4] + 1,
                                                    eg$num_C[eg$V3 == 2 & eg$freq != 0][4])


# e ora azzeriamo le frequenze
eg$freq[eg$V3 == 2 & eg$freq != 0] <- 0


# ora facciamo la stessa cosa per le righe relative
# a 2,3,7,8 V2

# (messo seme*3 a caso... per non avere lo stesso ordinamento di prima)
set.seed(seme*3)
ordine_lettere <- sample(rep(c('A','B','C'),2))

# controllo che non ci siano due lettere di fila uguali,
# sennò le cambio.. questo per le righe con freq = 2
cond_lett <- FALSE

while(!cond_lett){
  
  true_conseg <- rep(FALSE,6)
  
  for(i in 1:5){
    
    if(ordine_lettere[i] == ordine_lettere[i+1]){
      true_conseg[i] <- TRUE
      
    }
  }
  
  if(!any(true_conseg)){
    cond_lett <- TRUE
  }
  
  else{
    set.seed(seme*sample(1:1e4,1))
    ordine_lettere <- sample(rep(c('A','B','C'),2))
  }
}

# prima riga con freq != 0 e con V2 in 2,3,7,8
eg[2,'num_A'] <- ifelse(ordine_lettere[1] == 'A',
                        eg[2,'num_A'] +1,
                        eg[2,'num_A'])
eg[2,'num_B'] <- ifelse(ordine_lettere[1] == 'B',
                        eg[2,'num_B'] +1,
                        eg[2,'num_B'])
eg[2,'num_C'] <- ifelse(ordine_lettere[1] == 'C',
                        eg[2,'num_C'] +1,
                        eg[2,'num_C'])


# seconda riga
eg[3,'num_A'] <- ifelse(ordine_lettere[2] == 'A',
                        eg[3,'num_A'] +1,
                        eg[3,'num_A'])
eg[3,'num_B'] <- ifelse(ordine_lettere[2] == 'B',
                        eg[3,'num_B'] +1,
                        eg[3,'num_B'])
eg[3,'num_C'] <- ifelse(ordine_lettere[2] == 'C',
                        eg[3,'num_C'] +1,
                        eg[3,'num_C'])
eg[3,'num_A'] <- ifelse(ordine_lettere[3] == 'A',
                        eg[3,'num_A'] +1,
                        eg[3,'num_A'])
eg[3,'num_B'] <- ifelse(ordine_lettere[3] == 'B',
                        eg[3,'num_B'] +1,
                        eg[3,'num_B'])
eg[3,'num_C'] <- ifelse(ordine_lettere[3] == 'C',
                        eg[3,'num_C'] +1,
                        eg[3,'num_C'])

# terza riga
eg[18,'num_A'] <- ifelse(ordine_lettere[4] == 'A',
                         eg[18,'num_A'] +1,
                         eg[18,'num_A'])
eg[18,'num_B'] <- ifelse(ordine_lettere[4] == 'B',
                         eg[18,'num_B'] +1,
                         eg[18,'num_B'])
eg[18,'num_C'] <- ifelse(ordine_lettere[4] == 'C',
                         eg[18,'num_C'] +1,
                         eg[18,'num_C'])

# quarta riga
eg[23,'num_A'] <- ifelse(ordine_lettere[5] == 'A',
                         eg[23,'num_A'] +1,
                         eg[23,'num_A'])
eg[23,'num_B'] <- ifelse(ordine_lettere[5] == 'B',
                         eg[23,'num_B'] +1,
                         eg[23,'num_B'])
eg[23,'num_C'] <- ifelse(ordine_lettere[5] == 'C',
                         eg[23,'num_C'] +1,
                         eg[23,'num_C'])

# qiunta riga
eg[40,'num_A'] <- ifelse(ordine_lettere[6] == 'A',
                         eg[40,'num_A'] +1,
                         eg[40,'num_A'])
eg[40,'num_B'] <- ifelse(ordine_lettere[6] == 'B',
                         eg[40,'num_B'] +1,
                         eg[40,'num_B'])
eg[40,'num_C'] <- ifelse(ordine_lettere[6] == 'C',
                         eg[40,'num_C'] +1,
                         eg[40,'num_C'])

# azzero frequenze

eg[c(2,3,18,23,40), 'freq'] <- 0


# ora rimangono 9 posti da assegnare (3 A, 3 B e 3 C)

# ci sono 3 righe che hanno freq = 2.. assegniamo a ciascuna riga
# una A, una B e una C casualmente

# (anche qui seme*5 per averlo diverso da prima ma
# che comunque sia riproducibile)
set.seed(seme*5)
rand_lett <- sample(c('A','B','C'))

# prima riga
eg$num_A[1] <- ifelse(rand_lett[1] == 'A',
                      eg$num_A[1] + 1,
                      eg$num_A[1])
eg$num_B[1] <- ifelse(rand_lett[1] == 'B',
                      eg$num_B[1] + 1,
                      eg$num_B[1])
eg$num_C[1] <- ifelse(rand_lett[1] == 'C',
                      eg$num_C[1] + 1,
                      eg$num_C[1])

# seconda riga
eg$num_A[21] <- ifelse(rand_lett[2] == 'A',
                       eg$num_A[21] + 1,
                       eg$num_A[21])
eg$num_B[21] <- ifelse(rand_lett[2] == 'B',
                       eg$num_B[21] + 1,
                       eg$num_B[21])
eg$num_C[21] <- ifelse(rand_lett[2] == 'C',
                       eg$num_C[21] + 1,
                       eg$num_C[21])

# terza riga
eg$num_A[29] <- ifelse(rand_lett[3] == 'A',
                       eg$num_A[29] + 1,
                       eg$num_A[29])
eg$num_B[29] <- ifelse(rand_lett[3] == 'B',
                       eg$num_B[29] + 1,
                       eg$num_B[29])
eg$num_C[29] <- ifelse(rand_lett[3] == 'C',
                       eg$num_C[29] + 1,
                       eg$num_C[29])

# mandiamo ad 1 le frequenze
eg$freq[c(1,21,29)] <- 1


# ora si vede che, degli ultimi 6 spazi rimasti,
# 3 sono relativi alle righe da 4 V3 e pertanto
# assegniamo 'A' 'B' e 'C' a caso tra questi 3 spazi

set.seed(seme*7)
rand_lett <- sample(c('A','B','C'))



# prima riga
eg$num_A[28] <- ifelse(rand_lett[1] == 'A',
                       eg$num_A[28] + 1,
                       eg$num_A[28])
eg$num_B[28] <- ifelse(rand_lett[1] == 'B',
                       eg$num_B[28] + 1,
                       eg$num_B[28])
eg$num_C[28] <- ifelse(rand_lett[1] == 'C',
                       eg$num_C[28] + 1,
                       eg$num_C[28])

# seconda riga
eg$num_A[29] <- ifelse(rand_lett[2] == 'A',
                       eg$num_A[29] + 1,
                       eg$num_A[29])
eg$num_B[29] <- ifelse(rand_lett[2] == 'B',
                       eg$num_B[29] + 1,
                       eg$num_B[29])
eg$num_C[29] <- ifelse(rand_lett[2] == 'C',
                       eg$num_C[29] + 1,
                       eg$num_C[29])

# terza riga
eg$num_A[30] <- ifelse(rand_lett[3] == 'A',
                       eg$num_A[30] + 1,
                       eg$num_A[30])
eg$num_B[30] <- ifelse(rand_lett[3] == 'B',
                       eg$num_B[30] + 1,
                       eg$num_B[30])
eg$num_C[30] <- ifelse(rand_lett[3] == 'C',
                       eg$num_C[30] + 1,
                       eg$num_C[30])

# azzeriamo le frequenze

eg$freq[c(28,29,30)] <- 0


# ora ne rimangono solo 3: righe 1, 21, 22
# (chiaramente cambiando il seme iniziale quasi sicuramente verranno 3 righe diverse,
# ma il codice è automatizzato e capirà da solo quali 3 righe
# avranno una frequenza diversa da 0 e pari ad 1)
# e gli assegniamo la lettera che sono meno 
# presenti per quelle combinazioni V2-V3

righe_freq_1 <- which(eg$freq == 1)

tot_lett <- c('A','B','C')

min_1 <- names(which.min(eg[righe_freq_1[1],4:6]))

if(min_1 == 'num_A'){
  eg$num_A[righe_freq_1[1]] <- eg$num_A[righe_freq_1[1]] + 1
  tot_lett <- setdiff(tot_lett, 'A')
}

if(min_1 == 'num_B'){
  eg$num_B[righe_freq_1[1]] <- eg$num_B[righe_freq_1[1]] + 1
  tot_lett <- setdiff(tot_lett, 'B')
}

if(min_1 == 'num_C'){
  eg$num_C[righe_freq_1[1]] <- eg$num_C[righe_freq_1[1]] + 1
  tot_lett <- setdiff(tot_lett, 'C')
}


min_2 <- names(which.min(eg[righe_freq_1[2],4:6]))

if((min_2 == 'num_A') & ('A' %in% tot_lett)){
  eg$num_A[righe_freq_1[2]] <- eg$num_A[righe_freq_1[2]] + 1
  tot_lett <- setdiff(tot_lett, 'A')
}

if((min_2 == 'num_B') & ('B' %in% tot_lett)){
  eg$num_B[righe_freq_1[2]] <- eg$num_B[righe_freq_1[2]] + 1
  tot_lett <- setdiff(tot_lett, 'B')
}

if((min_2 == 'num_C') & ('C' %in% tot_lett)){
  eg$num_C[righe_freq_1[2]] <- eg$num_C[righe_freq_1[2]] + 1
  tot_lett <- setdiff(tot_lett, 'C')
}



if(tot_lett == 'A'){
  eg$num_A[righe_freq_1[3]] <- eg$num_A[righe_freq_1[3]] + 1
}

if(tot_lett == 'B'){
  eg$num_B[righe_freq_1[3]] <- eg$num_B[righe_freq_1[3]] + 1
}

if(tot_lett == 'C'){
  eg$num_C[righe_freq_1[3]] <- eg$num_C[righe_freq_1[3]] + 1
}


# azzeriamo le frequenze

eg$freq[righe_freq_1] <- 0



# abbiamo finito le lettere da assegnare

##############################################################################


# ora guardiamo alle varie table se è stato fatto tutto correttamente:

# intanto giustamente abbiamo 64 A, 64 B e 64 C
colSums(eg[,4:6])



# ricostruiamo un database facsimile di quello originale
# per vedere le varie tables

db_new <- data.frame(V3 = rep(1,sum(eg[1,4:6])),
                     V2 = rep(1,sum(eg[1,4:6])),
                     V4 = c(rep('A',eg[1,4]),rep('B',eg[1,5]),rep('C',eg[1,6])))
for(i in 2:40){
  db_new <- rbind(db_new,
                  data.frame(
                    V3 = rep(eg$V3[i],sum(eg[i,4:6])),
                    V2 = rep(eg$V2[i],sum(eg[i,4:6])),
                    V4 = c(rep('A',eg$num_A[i]),
                           rep('B',eg$num_B[i]),
                           rep('C',eg$num_C[i]))))
  
}

db_new


table(db_new$V3, db_new$V4) # una delle configurazioni a bilanciamento massimo raggiungibile

table(db_new$V2, db_new$V4) # una delle configurazioni a bilanciamento massimo raggiungibile

# cambiando il seme i risultati possono variare leggermente,
# ma mantenendo la somma di colonna e di riga di eg invariate


table(db_new$V2, db_new$V3, db_new$V4) # guardando le 3 table
# contemporaneamente sembrano molto bilanciate.. probabilmente
# è una delle configurazioni a maggior bilanciamento...



##############################################################################

# qualche  ulteriore controllo

tt <- as.data.frame(table(db_new$V2, db_new$V3, db_new$V4))

dif_AB <- tt$Freq[tt$Var3=='A'] - tt$Freq[tt$Var3=='B']
dif_AC <- tt$Freq[tt$Var3=='A'] - tt$Freq[tt$Var3=='C']
dif_BC <- tt$Freq[tt$Var3=='B'] - tt$Freq[tt$Var3=='C']


sum(dif_AB != 0) # 10
sum(dif_AC != 0) # 10
sum(dif_BC != 0) # 10


# ho provato con un po' di semi diversi e 
# sum(dif_AB != 0) + sum(dif_AC != 0) + sum(dif_BC != 0) = 30 sempre...
# quindi non si possono avere bilanciamenti migliori


##############################################################################

# ora, una volta scelto il seme che genera la specifica combinazione
# di 'A', 'B' e 'C', andiamo a introdurla nel database di partenza
# per 'allacciarli' a V1

dbb <- db
bdd <- db_new


for(i in 1:NROW(dbb)){
  j <- 1
  cond <- FALSE
  while(!cond){
    if((dbb[i,'V2'] == bdd[j,'V2']) & (dbb[i,'V3'] == bdd[j,'V3'])){
      dbb[i,'V4'] <- bdd[j,'V4']
      bdd <- bdd[-j,]
      cond <- TRUE
    }
    j <- j + 1
  }
}



# ora nel complesso 'A', 'B' e 'C' sono distrubuiti adeguatamente rispetto a V2 e a V3,
# però rispetto a V1 no....

# per mantenere la distribuzione complessiva com'è basta sostituire
# i V4 relativi alle righe che hanno stesse V2 e V3 ...


# ora quindi bisogna scambiare l'ordinamento dei V4 (muovendoli tra i
# vari livelli di V1), ma mantenendo le table complessive così come sono

# un modo è quello di provare un tot di combo (appunto sempre mantenendo le lettere 
# associate alle V3 e agli V2, così da non cambiare le table complessive) 
# e tenere le combo con maggior entropia all'interno degli istituti


# funzione per calcolare l'entropia di un vettore che contiene 'A', 'B' e 'C'...
# usando il log in base 3 si ha che la funzione va da 0 a 1...
# 0 indica minima entropia (i.e. ovunque stessa lettera)
# mentre 1 indica massima entropia (i.e. tutte lettere 
# diverse, i.e. lettere di V4 perfettamente bilanciate)
entropia_3 <- function(vect_combo){
  
  # num elementi in vect combo
  nn <- length(vect_combo)
  
  # frequenza di occorrenze di 'c in vect combo
  prob_c <- sum('A' == vect_combo) / nn
  # frequenza di occorrenze di 's in vect combo
  prob_s <- sum('B' == vect_combo) / nn
  # frequenza di occorrenze di 'v in vect combo
  prob_v <- sum('C' == vect_combo) / nn
  
  # definisco nuova funzione log3, che mi torna 
  # 0 quando faccio -Inf*0.. questo è motivato 
  # dal limite notevole x*logx --> x
  
  log3 <- function(x){
    res <- ifelse(x == 0, 0, logb(x, 3))
    res
  }
  
  # entropia
  entropia <- -(prob_c*log3(prob_c) + prob_s*log3(prob_s) + prob_v*log3(prob_v))
  
  return(entropia)
  
}




tapply(dbb$V4, dbb$V1, entropia_3) 
# avendo assegnato i V4 (seppur bilanciati nel complesso)
# a caso fra i livelli di V1, ci sono alcuni livelli di V1
# con 0 di entropia, stando a significare che hanno solo una 
# fra 'A', 'B' e 'C' tra le righe in cui compaiono...


# intanto creaimoci un database d'appoggio, con solo
# le info che ci servono (V1, V4 e la combo tra V2 e V3)

dati <- data.frame(id = dbb$id,
                   V1 = dbb$V1,
                   V4 = dbb$V4,
                   V2V3 = interaction(dbb$V2, dbb$V3))



entropia <- tapply(dbb$V4, dbb$V1, entropia_3) 


dati[,'entropia'] <- rep(0, 192)

# aggiungiamo al database l'entropia associata ad ogni V1
for(i in 1:length(unique(dbb$V1))){
  dati[dati$V1 == names(entropia)[i], 'entropia'] <- entropia[i]
}



# per tenere conto dell'entropia all'interno
# di ciascun livello di V1 si può semplicemente
# utilizzare l'entropia media di tutto il database,
# media calcolata come media ponderata tra le entropie
# dei vari livelli di V1



# funzione per calcolare l'entropia media del database,
# in input vuole V1 e V4
entromedia <- function(V1, V4){
  uni_V1 <- unique(V1)
  len_V1 <- rep(0, length(uni_V1)) -> entro_V1
  
  for(i in 1:length(len_V1)){
    len_V1[i] <- length(V1[V1 == uni_V1[i]])
    entro_V1[i] <- entropia_3(V4[V1 == uni_V1[i]])
  }
  
  medd <- sum(entro_V1 * len_V1) / sum(len_V1)
  
  return(medd)
  
}


dati[,'entromedia'] <- entromedia(dati[,2],dati[,3])

dati[,'entromedia'][1]


# intanto diamo una prima 'passata' grezza... ovvero scorriamo tutte le righe
# del database e scambiamo quelle con stessa V3+V2 se apporta una maggior entropia media

# questa serve per diminuire drasticamente i tempi di esecuzione della randomizzazione finale

# è grezza perché quasi sicuramente valuto più volte la stessa
# combinazione mentre alcune non le valuto proprio...
# ma comunque aumenta notevolmente l'entropia media
# senza spendere troppo tempo

db2 <- dati

for(i in 1:191){
  print(i)
  for(j in (i+1):192){
    if((db2[i,4] == db2[j,4]) & (db2[i,3] != db2[j,3])){
      # print(TRUE)
      db3 <- db2
      db3[i,3] <- db2[j,3]
      db3[j,3] <- db2[i,3]
      entr2 <- entromedia(db2[,2], db2[,3])
      # print(entr2)
      entr3 <- entromedia(db3[,2], db3[,3])
      # print(entr3)
      if(entr3 > entr2){
        print('switch')
        db2 <- db3
      }
      
    }
  }
  
}

db2


db3[,'entromedia'] <- entromedia(db3[,2], db3[,3])
db3[,'entromedia'][1]

# la media delle entropie entro gli istituti è circa raddoppiata !!
# passa da meno di 50 % a più del 90 %


# guardiamola per ogni singolo livello di V1
entropia <- tapply(db3$V4, db3$V1, entropia_3) 
db3[,'entropia'] <- rep(0, 192)

# aggiungiamo al database l'entropia associata ad ogni livello di V1
for(i in 1:length(unique(db3$V1))){
  db3[db3$V1 == names(entropia)[i], 'entropia'] <- entropia[i]
}

unique(db3[,'entropia'])

# anche all'interno dei singoli livelli di V1 si è alzata molto (chiaramente)



# ora, sempre per diminuire i tempi di esecuzione della randomizzazione finale,
# si può fare una pasata grezza simile alla precedente, ma partendo dall'ultima riga e andando
# 'in su', invece che partire dalla prima riga e andare in giù (infatti si vede come 
# i livelli di V1 relativi alle ultime righe abbiano entropia maggiore di quelli nelle prime)


db4 <- db3


for(i in 192:2){
  print(i)
  for(j in (i-1):1){
    if((db4[i,4] == db4[j,4]) & (db4[i,3] != db4[j,3])){
      #print(TRUE)
      db5 <- db4
      db5[i,3] <- db4[j,3]
      db5[j,3] <- db4[i,3]
      entr4 <- entromedia(db4[,2], db4[,3])
      # print(entr2)
      entr5 <- entromedia(db5[,2], db5[,3])
      # print(entr3)
      if(entr5 > entr4){
        print('switch')
        db4 <- db5
      }
      
    }
  }
  
}


db4


db5[,'entromedia'] <- entromedia(db5[,2], db5[,3])

db5[,'entromedia'][1]

# la media delle entropie entro i livelli di V1 è aumentata di circa 5 % !!


# guardiamola per ogni singolo livello di V1
entropia <- tapply(db5$V4, db5$V1, entropia_3) 
db5[,'entropia'] <- rep(0, 192)

# aggiungiamo al database l'entropia associata ad ogni V1
for(i in 1:length(unique(db5$V1))){
  db5[db5$V1 == names(entropia)[i], 'entropia'] <- entropia[i]
}

unique(db5[,'entropia'])
# anche all'interno di ogni livello di V1 si è alzata (chiaramente)


##############################################################################



# intanto creiamo un database aggregato per capire la situazione

db_aggr <- data.frame(V1 = names(table(db5$V1)),
                      righe_V1 = as.data.frame(table(db5$V1))[,2],
                      entropia = as.data.frame(entropia)[,1])

arrange(db_aggr, righe_V1)

# a parte i due livelli di V1 con 1 e 2 righe, gli altri hanno quasi tutti l'entropia
# molto alta... vediamo se si può alzare ancora, creando varie configurazioni
# per cui l'entropia almeno non scenda (o di poco),
# ma che comunque venga preservata la struttura globale delle table


##############################################################################

# ora quindi facciamo la randomizzazione finale:
# scegliamo il numero di ordinamenti di V4 che vogliamo,
# indicato con num_iter (qui ho messo 1000)

db6 <- db5 

db6[,'ind_V1'] <- as.numeric(db5$V1)

res <- list()

# cambiando il seme si ottengono ordinamenti diversi,
# anche se nel complesso risulterebbero simili
seme <- 910

k <- 0

num_iter <- 1e3

n <- 0

while(k < num_iter){
  
  # n mi serve per generare un seme diverso ad ogni passo
  # ma che comunque sia riproducibile completamente
  n <- n + 1
  
  
  if((n %% 1000) == 0) cat('n =', n, '\n')
  
  set.seed(n + seme)
  i <- sample(1:192, 1)
  set.seed(n - seme)
  j <- sample(1:192, 1)
  
  if(i != j){
    
    if((db6[i,4] == db6[j,4]) & (db6[i,3] != db6[j,3]) & (db6[i,7] != db6[j,7])){
      
      
      db7 <- db6
      
      # switch
      db7[i,3] <- db6[j, 3]
      db7[j,3] <- db6[i, 3]
      
      # entropia entro i due livelli di V1 dove si è fatto lo switch cambia
      db7[db7$ind_V1 == db7[i,7], 'entropia'] <- entropia_3(db7[db7$ind_V1 == db7[i,7], 'V4'])
      db7[db7$ind_V1 == db7[j,7], 'entropia'] <- entropia_3(db7[db7$ind_V1 == db7[j,7], 'V4'])
      
      #entr6 <- entromedia(db6[,2], db6[,3])
      entr7 <- entromedia(db7[,2], db7[,3])
      
      # il massimo dell'entropia media raggiungibile è 0.9802945
      # e ad ogni passo la si può diminuire di circa 0.001...
      # quindi mantenerla sopra lo 0.97 sembra essere un compromesso ragionevole
      # tra entropia elevata e varietà di combinazioni...
      # chiaramente si può cambiare questa soglia quando si vuole
      if(entr7 > .97){
        
        k <- k + 1
        
        res[[k]] <- db7
        
        db6 <- db7
        db6[,6] <- entr7
        
        
        
        if((k %% 100) == 0) cat('k =', k, '\n')
        
      }
      
    }
  }
  
  # arresto l'algoritmo (dopo molto) se non si raggiunge 
  # il numero predefinito (num_iter) di database da generare
  if(n > num_iter*1e3) break
  
}

# ci mette meno di minuto su un calcolatore standard e con le impostazioni predefinite




length(res) # 1000 chiaramente... che è il numero impostato (num_iter)



# guardiamo se gli ordinamenti sono effettivamente
# diversi fra loro, oppure se vengono trovate sempre le solite combinazioni
for(i in sample(1:length(res),10)){
  for(j in sample(1:length(res),10)){
    if(i != j){
      
      tot_dif <- sum(res[[i]][,3] != res[[j]][,3])
      cat('tra ', i, '-esimo res e ', j, '-esimo res il numero di posizioni', 
          'in cui le lettere sono diverse è di ', tot_dif, '\n')
      
    }
  }
}

# i database trovati sono molto diversi tra loro
# (in termini di ordinamento di V4)




for(i in sample(1:length(res),10)) print(table(res[[i]]$V2V3, res[[i]]$V4))
# ma comunque tutti hanno le table complessive che sono rimaste
# uguali a quelle dopo l'assegnamento iniziale
# delle lettere 'A', 'B' e 'C' (riga 520)



media_entro_tot <- unlist(lapply(res, function(x) x$entromedia[1]))
sort(media_entro_tot, decreasing = T)
# inoltre tutti i database hanno entropie elevate
# (> 0.97, come imposto nell'ultimo ciclo d'altronde)



# ora creaimo una lista con 1000 database...
# tutti uguali al database iniziale e uguali fra loro se non
# per la colonna V4


lista_db <- list()


for(i in 1:length(res)){
  
  lista_db[[i]] <- db
  lista_db[[i]][,'V4'] <- res[[i]][,'V4']
  
}



# diamo l'ultima controllata alle table complessive

for(i in sample(1:length(lista_db),10)) print(table(lista_db[[i]]$V3, lista_db[[i]]$V4))
# tutte uguali

for(i in sample(1:length(lista_db),10)) print(table(lista_db[[i]]$V2, lista_db[[i]]$V4))
# tutte uguali


for(i in sample(1:length(lista_db),10)){
  tot_col <- c() -> tot_r
  tot_col <- c(tot_col, colSums(table(lista_db[[i]]$V2, lista_db[[i]]$V3, lista_db[[i]]$V4)))
  tot_r <- c(tot_r, rowSums(table(lista_db[[i]]$V2, lista_db[[i]]$V3, lista_db[[i]]$V4)))
  print(tot_col)
  print(tot_r)
  }
# tutte uguali






