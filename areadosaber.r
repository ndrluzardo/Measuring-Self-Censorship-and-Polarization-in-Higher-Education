# Fisher's exact test for knowledge area and topics of expression

# loads the dataframe "dados_clean"
load("dados_NA.Rda") 

# creates a table to visualize the raw data
table(dados_clean$area)

# categorizes the demographic variable
lista1 <- list("Ciências Humanas", "Ciências Sociais Aplicadas", "Linguística, Letras e Artes")
lista2 <- list("Ciências da Saúde", "Ciências Exatas e da Terra", "Engenharias", "Ciências Agrárias")

dados_clean$area[(dados_clean$area  %in% lista1)] <- "Ciências Humanas e Sociais"
dados_clean$area[(dados_clean$area  %in% lista2)] <- "Ciências Naturais e Exatas"
dados_clean$area[dados_clean$area=="Prefiro não responder"] <- NA


# creates a barplot to visualize the counts in each category
barplot(table(dados_clean$area))

# creates a dataframe with the variables to test
cols <- data.frame(dados_clean[c('pol_comf','rac_comf','sex_comf','gen_comf','rel_comf','adm_comf','mat_comf')])

# Creates empty list to store the test results and a dataframe to store p values
fisher_results <- list()
p_values <- data.frame(matrix(nrow=length(cols), ncol=4))
colnames(p_values) <- c('Topic','p','p<0.001','p<0.01')


# loops through all variables and save test results
for(i in 1:7) {                                                        # Head of for-loop
  output <- fisher.test(table(dados_clean$area, cols[,i]))           # Output of iteration i
  fisher_results[[i]] <- output                                        # Store output in list
  p_values[i,1] <- colnames(cols)[i]
  p_values[i,2] <- output$p.value
  p_values[i,3] <- output$p.value < 0.001
  p_values[i,4] <- output$p.value < 0.01
}

# check the resulting p values
p_values


