# Load functions from "utility" script
source("utility.R")

# Load seeds data into dataframe
data <- read.table("seeds.data.txt", header = T, sep = "\t" )

# Force R to recognize data as numbers
data$length <- as.double(data$length)
data$seed_qty <- as.double(data$seed_qty)

# TEST @todelete
subsets <- split_by_content(df = data,
                 column_name = "length",
                 initial_step = 8.5,
                 step = 2)
print(subsets)