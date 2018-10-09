

runSim <- function(inputProbs) {

	pardons <- sample(prob = inputProbs,x = c("A","B","C"),size = 10000,replace = TRUE)
	
	Results <- data.frame("1","2")
	colnames(Results) <- c("Pardon","WardenTells")


	
	for(i in 1:length(pardons)){

	pardon <- pardons[i]

	if(pardon == "A") {
	
		wardenTell <- sample( x = c("B","C"),size = 1)	
		
		hold <- data.frame(pardon,wardenTell)
		colnames(hold) <- c("Pardon","WardenTells")
			
		Results <- rbind(Results,hold)		

	}
	
	if(pardon == "B"){

		wardenTell <- "C"	
		
		hold <- data.frame(pardon,wardenTell)
		colnames(hold) <- c("Pardon","WardenTells")
		Results <- rbind(Results,hold)		

		

	
	}

	if(pardon == "C"){

		wardenTell <- "B"
		
		hold <- data.frame(pardon,wardenTell)
		colnames(hold) <- c("Pardon","WardenTells")
		Results <- rbind(Results,hold)
	}	


	}


	return(Results)
}

i_1 <- c(1/3,1/3,1/3)

i_2 <- c(1/2,1/4,1/4)

i_3 <- c(1/4,1/2,1/4)

i_4 <- c(1/4,1/4,1/2)

i_5 <- c(1/7,2/7,4/7)

results <- runSim(i_5)[-1,] 
results$Pardon <- as.character(results$Pardon)
results$WardenTells <- as.character(results$WardenTells)


write.csv(x = results, file= "results.csv")

tellsB <- table(results[results$WardenTells == "B",]$Pardon)
tellsC <- table(results[results$WardenTells == "C",]$Pardon)


print("Tells about B")
print(tellsB/sum(tellsB))
print("Tells about C")
print(tellsC/sum(tellsC))
