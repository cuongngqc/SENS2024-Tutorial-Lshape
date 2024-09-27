
# R script for a function that calculates salience index. The function also returns the probability of each index being generated at random, assigning a p-value.

# Use

# salience(ll, detail=FALSE)

# Arguments

# ll	
#	A data.frame with two categorical variables: the identification of the interviewees / informants and the cited items for which the index is to be calculated. 

# detail
#	Must be true if the user wants to see the full output of the analysis

# Details

# The formula used to calculate the salience index is:
# Smith (1997): (sum((Li - Rj + 1)/Li))/N

# Where, for all formulas,
# Li = Size of the list where the term is cited,
# Rj = Position of the item in a given list Li,
# N = Total number of lists (or interviewed),
# F = Frequency of an item.


salience<- function (ll, detail=FALSE){
  
colnames(ll)<-c("nomes", "item")		# I renamed the columns to facilitate
llord<-ll[order(ll$nomes),]			# Some functions I have used return the results in alphabetical order, 
# so I thought it would be better to start the same way
ninf<-length(levels(ll$nomes))		# Total number of lists (or interviewed)
nintens<-length(levels(ll$item))		#Total number of items
tamlista<-as.vector(table(ll$nomes))	#Creates a vector with the size of each list
freq<-as.data.frame(table(ll$item))   # Calculates the frequency of the items


#__________________________Smith97________________________________________________

resultLRL.1<-(matrix(data=NA, nrow=nintens, ncol=ninf)) # Creates a matrix to store the rankings and the partial calculation of the salience index
ranking<-(matrix(data=NA, nrow=nintens, ncol=ninf)) # Creates a matrix to store the ranks of the cited items 
for(i in 1:ninf){  ## Fill in the matrix with the ranking values and the partial calculation of the salience index
    resultLRL.1[,i]<-c((tamlista[i]-seq(1, tamlista[i])+1)/tamlista[i], rep(NA,times=(nintens-tamlista[i]))) ## Partial calculation Smith97
    ranking[,i]<-c(seq(1, tamlista[i]), rep(NA,times=(nintens-tamlista[i]))) 
    }
llord$LRL.1<-(as.numeric(na.omit(as.vector(resultLRL.1))))  # Transforms the matrix into vector (removing NAs)
llord$ranking<-(as.numeric(na.omit(as.vector(ranking))))   # Transforms the matrix into vector (removing NAs)
Smith1997<-round(as.numeric(tapply(llord$LRL.1, llord$item, sum)/ninf), digits = 4) # Finalizing the calculation of the salience index
Posic_Media<-round(as.numeric(tapply(llord$ranking, llord$item, sum))/freq$Freq, digits = 4) # Finalizing the calculation of the mean position
dfSmith1997<-data.frame(levels(llord$item), Smith1997, Posic_Media, freq$Freq)			#Creates the exit data frame 
Exit<-dfSmith1997[order(dfSmith1997$Smith1997, decreasing=T),] #Organize the data frame based on the calculated values
colnames(Exit)<-c("Itens", "Smith97", "Mean.pos", "Freq")
  
  #__________________Preparing for Monte Carlo____________ 
  
  listsimul<-as.character(seq(1, length(levels(ll$item))))	# Creates a list of items with the same number as the total items
  matrlista<-(matrix(data=NA, nrow=nintens, ncol=ninf))		# Creates a matrix to store the items drawn,
  matrank<-(matrix(data=NA, nrow=nintens, ncol=ninf))		# the ranking of the item to Smith index, 
  rankingMC<-(matrix(data=NA, nrow=nintens, ncol=ninf))		# the ranking of the item for the calculation of mean position, 
  Result.freqMC<-matrix(NA, nrow=nintens, ncol = 300)   # the frequency,
  resultMC<-(matrix(data=NA, nrow=nintens, ncol=300))		# the simulated overhang values.
  Posic_MediaMC<-(matrix(data=NA, nrow=nintens, ncol=300))
  
  
  
  #___________Monte Carlo Smith97____________________________
  
  
  for(j in 1:300){
    for(k in 1:ninf){
      num.item<-sample(tamlista, 1)	 # Randomly chooses how many distinct items will be listed
      listaparc<-sample(listsimul, size=num.item)  # Creates a simulated list with drawn size on the previous line 
      matrlista[,k]<-c(listaparc, rep(NA,times=(nintens-length(listaparc)))) 		# Fill in the matrix with the items drawn
      matrank[,k]<-c((length(listaparc)-seq(1, length(listaparc))+1)/length(listaparc), rep(NA,times=(nintens-length(listaparc)))) # Fill in the matrix with the rankings and partial values of the calculation
      rankingMC[,k]<-c(seq(1, length(listaparc)), rep(NA,times=(nintens-length(listaparc)))) 		# Fill in the matrix with the rankings for the calculation of mean position
    }
    freqMC<-as.data.frame(table(matrlista))
    Result.freqMC[,j]<-c(freqMC$Freq, rep(NA, times=(nintens-length(freqMC$Freq))))  #Pra calcular o p da frequencia
    resrankingMC<-as.numeric(na.omit(as.vector(rankingMC)))# Transforms the matrix into vector
    resA<-as.character(na.omit(as.vector(matrlista)))# Transforms the matrix into vector
    resB<- as.numeric(na.omit(as.vector(matrank)))# Transforms the matrix into vector
    resABrank<-data.frame(resA, resB, resrankingMC)# Create a data frame 
    resABC<-as.numeric(tapply(resABrank$resB, resABrank$resA, sum)/ninf)# Finish calculation 
    resultMC[,j]<-c(resABC, rep(NA,times=(nintens-length(resABC))))	# Fill in the matrix with the results
    res_Pos.Med<-as.numeric(tapply(resABrank$resrankingMC, resABrank$resA, sum))/freqMC$Freq# Finish calculation 
    Posic_MediaMC[,j]<-c(res_Pos.Med, rep(NA,times=(nintens-length(resABC))))# Fill in the matrix with the results
  }


#############___________________Calculating the p-value of salience________________________####

resultMC2<-(as.numeric(na.omit(as.vector(resultMC))))				# Transforms the matrix with the results of the vector simulations
Sal<-Exit[,2]										# Creates an object with the salience values for each item 
Exit$p.valor<-rep(NA, times=length(Sal))						# Creates a column in the output data frame the p-value
for (i in 1:length(Sal))							# Compares each salience value with the output data frame
{
  if(Sal[i]>mean(resultMC2))					# Verifies that the salience value is greater than the average 
  {
    caudadir<-sum(resultMC2>=Sal[i])		# If the value is greater than the average, we calculate the 
    p.D<-caudadir/length(resultMC2)			# probability of this value to be greater than the values of the null scenario
    Exit$p.valor[i]<-round(p.D, digits = 4)
  }
  else
  {
    caudaesq<-sum(resultMC2<=Sal[i])		# If the value is less than the average, we calculate the
    p.E<-caudaesq/length(resultMC2)		  	# probability of this value to be less than the values of the null scenario
    Exit$p.valor[i]<-round(p.E, digits = 4)
  }
}



#############___________Calculating the p-value Mean.pos________________________

Posic_MediaMC2<-(as.numeric(na.omit(as.vector(Posic_MediaMC))))				
MedPos<-Exit[,3]
Exit$MedPosp.valor<-rep(NA, times=length(MedPos))						
for (i in 1:length(MedPos))
{
  if(MedPos[i]>mean(Posic_MediaMC2))
  {
    caudadir<-sum(Posic_MediaMC2>=MedPos[i])
    p.D<-caudadir/length(Posic_MediaMC2)		
    Exit$MedPosp.valor[i]<-round(p.D, digits = 4)
  }
  else
  {
    caudaesq<-sum(Posic_MediaMC2<=MedPos[i])
    p.E<-caudaesq/length(Posic_MediaMC2)		
    Exit$MedPosp.valor[i]<-round(p.E, digits = 4)
  }
}


#############___________Calculating the p-value FREQ________________________

Result.freqMC2<-(as.numeric(na.omit(as.vector(Result.freqMC))))	
Freq.<-Exit[,4]		
Exit$Freq_p.valor<-rep(NA, times=length(Freq.))	
for (i in 1:length(Freq.))
{
  if(Freq.[i]>mean(Result.freqMC2))
  {
    caudadir<-sum(Result.freqMC2>=Freq.[i])
    p.D<-caudadir/length(Result.freqMC2)	
    Exit$Freq_p.valor[i]<-round(p.D, digits = 4)
  }
  else
  {
    caudaesq<-sum(Result.freqMC2<=Freq.[i])	
    p.E<-caudaesq/length(Result.freqMC2)		
    Exit$Freq_p.valor[i]<-round(p.E, digits = 4)
  }
}


##############_____Preparing the output _____________________

Exit2<-Exit[, c(1, 2, 5, 3, 6, 4, 7)]
colnames(Exit2)<-c("Item", "Smith97", "Sal_p.value", "Mean_pos", "Mean_p.value", "Freq", "Freq_p.value")
meanfreq<-mean(freq$Freq)
meansalience<-mean(Smith1997)

Complet.Exit<-list("Number of lists" = ninf,
                   "Total number of items" = nintens,
                   "Mean of salience index" = meansalience,
                   "Mean of frequency" = meanfreq, "Complete output" = Exit2)

sum(Exit2$Freq==1)

Exit3<-Exit[, c(1, 2, 5)]

if (detail==TRUE) {
  return(Complet.Exit)
}else{
  return(Exit3)
}
}

