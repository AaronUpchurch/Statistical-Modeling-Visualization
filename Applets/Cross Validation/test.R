titanic <- reactiveVal(read.csv("datasets/titanic.csv"))

titanic()$Name

data$LastName <- sapply(strsplit(data$Name, ","), `[`, 1)
data
