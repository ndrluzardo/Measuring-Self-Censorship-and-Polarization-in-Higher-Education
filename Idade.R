# Fisher's exact test for Age and topics of expression

# loads the dataframe "dados_clean"
load("dados_NA.Rda") 

# creates a table to visualize the raw data
table(dados_clean$idade)

# categorizes the ages
dados_clean["idade_grupo"] = cut(dados_clean$idade, c(16, 18, 25, 30, 40, Inf), c("16-18", "19-25", "26-30","31-40", ">40"), include.lowest=TRUE)

# creates a barplot to visualize the counts in each category
barplot(table(dados_clean$idade_grupo))

# creates a dataframe with the variables to test
cols <- data.frame(dados_clean[c('pol_comf','rac_comf','sex_comf','gen_comf','rel_comf','adm_comf','mat_comf')])

# Creates empty list to store the test results and a dataframe to store p values
fisher_results <- list()
p_values <- data.frame(matrix(nrow=length(cols), ncol=4))
colnames(p_values) <- c('Topic','p','p<0.001','p<0.01')

# loops through all variables and save test results
for(i in 1:7) {                                                       # Head of for-loop
  output <- fisher.test(table(dados_clean$idade_grupo, cols[,i]))     # Output of iteration i
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
  dados_clean, idade_grupo, pol_comf,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(fisher_results[[1]]$p.value < 0.001, "< 0.001", round(fisher_results[[1]]$p.value, 3))
  )
)

ggbarstats(
  dados_clean, idade_grupo, rel_comf,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(fisher_results[[5]]$p.value < 0.001, "< 0.001", round(fisher_results[[5]]$p.value, 3))
  )
)


