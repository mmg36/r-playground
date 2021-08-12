library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
raw_loan_data = read.csv("loan_data.csv", header = TRUE, sep = ",")
head(raw_loan_data)
debt_loan_data = raw_loan_data
debt_loan_data = (
  subset(dplyr::filter(raw_loan_data, purpose == "debt_consolidation", 
                     ),
         select = -purpose))

data_subset = slice(debt_loan_data, 1:100)
colnames(data_subset)
data_subset= filter(data_subset, credit.policy == 1)
pairs(data_subset)

for (count in 10:13) {
  print(unique(raw_loan_data[,count]))
}


plot(debt_loan_data$fico, debt_loan_data$dti, pch = 19, 
     col=factor(debt_loan_data$credit.policy))
legend("bottomright", legend = c("Credit", "No credit"),
       pch = 19,
       col = factor(debt_loan_data$credit.policy),
       text.col = "blue")


debt_loan_data = dplyr::filter(raw_loan_data, purpose == "debt_consolidation")
plot1 = ggplot(debt_loan_data, 
       aes(x=fico, y=int.rate, color=factor(fico, credit.policy), 
           group = factor(fico, credit.policy))) +
  geom_point() +
  stat_boxplot(fill = NA) + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)
  
  
debt_loan_data = dplyr::filter(raw_loan_data, purpose == "credit_card")
  plot2 = ggplot(debt_loan_data, 
                 aes(x=fico, y=int.rate, color=factor(credit.policy), 
                     group = factor(credit.policy))) +
    geom_point() +
    stat_boxplot(fill = NA) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)
  
grid.arrange(plot1, plot2, ncol = 2)