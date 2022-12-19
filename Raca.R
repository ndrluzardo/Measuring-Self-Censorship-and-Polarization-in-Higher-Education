# Fisher's exact test for Race and topics of expression

# loads the dataframe "dados_clean"
load("dados_NA.Rda") 

# creates a table to visualize the counts in each category
table(dados_clean$raca)

# turns 'Prefiro nao responder' and "Não sei" to NA
dados_clean$raca[dados_clean$raca=="Prefiro não responder"]<-NA
dados_clean$raca[dados_clean$raca=="Não sei"]<-NA

# creates a dataframe with the variables to test
cols <- data.frame(dados_clean[c('pol_comf','rac_comf','sex_comf','gen_comf','rel_comf','adm_comf','mat_comf')])

# creates empty list to store the test results and a dataframe to store p values
fisher_results <- list()
p_values <- data.frame(matrix(nrow=length(cols), ncol=4))
colnames(p_values) <- c('Topic','p','p<0.001','p<0.01')

# loops through all variables and save test results
for(i in 1:7) {                                                       # Head of for-loop
  output <- fisher.test(table(dados_clean$raca, cols[,i]))     # Output of iteration i
  fisher_results[[i]] <- output                                       # Store output in list
  p_values[i,1] <- colnames(cols)[i]
  p_values[i,2] <- output$p.value
  p_values[i,3] <- output$p.value < 0.001
  p_values[i,4] <- output$p.value < 0.01
}

# inspect the resulting p values
p_values

# plots the significant findings
library(ggstatsplot)
ggbarstats(
  dados_clean, raca, adm_comf,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(fisher_results[[6]]$p.value < 0.001, "< 0.001", round(fisher_results[[6]]$p.value, 3))
  )
)
