life_table_data <- read.csv("data/life_table2011.csv",
         stringsAsFactors = FALSE,
         skip = 3,
         header = F)
colnames(life_table_data) <- c("age",
                               "qx",
                               "lx",
                               "dx",
                               "L",
                               "Tx",
                               "ex")
# remove last row, the source of data
life_table_data <- life_table_data[1:(nrow(life_table_data) - 1), ]
save(life_table_data, file = "data/life_table_data.rda")
