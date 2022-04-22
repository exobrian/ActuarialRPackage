#This code drops the zipcodeData table from DC1BISQLDEV01.dbo.ActuarialSandbox and grabs data
#from zipcodeR for icw main states specified in stateList array. Then uploads it to the db above.

library(zipcodeR)
library(tidyverse)

uploadDate = Sys.time()

#ICW Main States: Add more as needed
stateList <- c("CA", "FL", "GA", "IA", "IL", 
               "IN", "KY", "MD", "MI", "MN", 
               "MO", "NC", "NJ", "NV", "NY", 
               "OK", "PA", "SC", "TN", "TX", 
               "VA", "WI")

#Initiate an empty dataframe
df <- data.frame()
i = 1

#pull specific columns from zipcodeR for each zipcode in stateList
for (st in stateList){
  CityInfo <- data.frame(rep(uploadDate), search_state(stateList[i])[c("zipcode", "major_city", "county", "state","population")])
  df <- rbind(df, CityInfo) 
  i = i + 1
}

#Fips data from tidyverse package
fipsData <- tidycensus::fips_codes
df2 <- df %>% left_join(fipsData, by = c("county","state")) %>%
  mutate(FipsCode = paste(state_code, county_code, sep="")) %>%
  select("rep.uploadDate.",
         "zipcode",
         "major_city",
         "county",
         "state",
         "state_code",
         "county_code",
         "FipsCode",
         "population"
         )

#SQL Database Connection
myconn <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    database = "ActuarialSandbox",
                    server = "DC1BISQLDEV01")
TableName <- DBI::Id(table  = "ZipcodeData")

#Drop existing table. Appending table doesn't seem to work. Table is empty every time. I suspect
#it has to do with the identity constraint on id. We probably don't need versioning anyways, so
#creating table is good enough for now.
if (dbExistsTable(myconn, TableName)){
  dbRemoveTable(myconn ,TableName)
}

#Renaming to a more friendly name.
names(df2) <- c("UploadDate", "Zipcode", 
                "MajorCity", "County", 
                "State", "StateCode",
                "CountyCode", "FipsCode",
                "Population")

#create an index based on row number.
df2 <- tibble::rowid_to_column(df2, "id")
dbWriteTable(myconn, TableName, value = df2, row.names = FALSE, overwrite = FALSE, append = TRUE)
dbDisconnect(myconn)