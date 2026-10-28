#-------------------------------------------------------------------------------
#  Step 1 - connect to mymimic database and save sepsisfinal as .rds          --
#-------------------------------------------------------------------------------

library(DBI)
library(RPostgres)


# Set connection credentials
username <- "emmad"
password <- Sys.getenv("postgres_pw")
db_name <- "mimiciv"
host_name <- "127.0.0.1" # or your server's IP address
port_num <- 5432

# Establish connection
con <- dbConnect(
  Postgres(),
  dbname = db_name,
  host = host_name,
  port = port_num,
  user = username,
  password = password
)

password = ""


mymimic_sepsisfinal <- dbReadTable(con, Id(schema = "mimic_my", table = "sepsis_final"))
head(mymimic_sepsisfinal)
colnames(mymimic_sepsisfinal)

dbDisconnect(con)


# Save final dataset
setwd(r"(C:\Users\emmad\Documents\Thesis)")
saveRDS(mymimic_sepsisfinal, file = "data/mymimic_sepsisfinal.rds")
fwrite(mymimic_sepsisfinal,"data/mymimic_sepsisfinal.csv")
