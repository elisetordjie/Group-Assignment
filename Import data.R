library(readxl)
A <- readxl::excel_sheets("data/Raw_data_.xlsx")
A
B <- readxl::read_excel("data/Raw_data_.xlsx",sheet = A[3])
write.csv(B, "data/Assay 3.csv")
