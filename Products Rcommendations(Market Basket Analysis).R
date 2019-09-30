# install and call pacakges arules and arulesViz
install.packages("arules", dependencies = TRUE)
install.packages("arulesViz", dependencies = TRUE)
library(arules)
library(arulesViz)
set.seed(520)

# import and inspect transaction data set
transactions <- read.transactions(file = "ElectronidexTransactions2017.csv",
                  format = "basket",
                  sep = ",",
                  rm.duplicates = T)
inspect(transactions)
length(transactions)
size(transactions)
LIST(transactions)
itemLabels(transactions)

# item frequency 
itemFrequencyPlot(transactions, type = "absolute",topN =10, col = rainbow(4), main = "High Frequency") #better
itemFrequencyPlot(transactions,col = rainbow(4), support = 0.1 )
dev.off()

itemFrequency(transactions[1:10])
lowFreqency <- sort(table(unlist(LIST(transactions))), decreasing = FALSE)[1:10]
lowFreqency
par(mar=c(1,4,2,0.5))
barplot(sort(table(unlist(LIST(transactions))))[1:10],
        horiz=TRUE,
        las = 1,
        col=rainbow(4),
        main = "Low Frequency")

image(transactions[1:20,])   
image(sample(transactions, 20))

# applying and evaluate apriori rule
aprioriRule <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.55, minlen = 2, maxlen = 4))
inspect(aprioriRule)
summary(aprioriRule)
inspect(sort(aprioriRule, by = "lift")[1:10])
is.redundant(aprioriRule)
aRule <- sort(aprioriRule, by = "lift")

xRules <- subset(aprioriRule, items %in% "Kindle") 
inspect(xRules)
summary(xRules)

# lhs = iMac
iMacRules_lh <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.1),
                        appearance = list(default = "rhs", lhs = "iMac"))


# visualize the result
?plot
plot(aprioriRule, method = "scatterplot", measure = c("support","confidence"),
     shading = "lift")
plot(aRule, method = "graph", control = list(type = "items"), max = 20)













