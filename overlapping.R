
# Finding and eliminating overlapping records from the data.
# Permits can overlap, licences or licences and permits can not

library(tibble)
library(dplyr)

# sample data
data <- tibble(
  Document_ID = c(100:125),
  Personal_ID = c(1001, 1001, 1002, 1003, 1005, 1004, 1007, 1006, 1007, 1008, 1011, 1022, 1033,
                  1002, 1003, 1002, 1003, 1006, 1004, 1007, 1006, 1004, 1008, 1042, 1051, 1034),
  Document_Type = c("PERMIT", "PERMIT", "LICENSE", "PERMIT", "PERMIT", "LICENSE", "LICENSE", "PERMIT", "LICENSE", "PERMIT",
                    "LICENSE", "LICENSE", "LICENSE", "PERMIT", "LICENSE", "LICENSE", "LICENSE", "PERMIT", "PERMIT", "PERMIT",
                    "LICENSE", "PERMIT", "LICENSE", "PERMIT", "PERMIT", "LICENSE"),
  Start_Date = as.POSIXct(c("2023-01-01", "2022-02-03", "2023-03-07", "2023-04-01", "2023-05-02", "2023-06-01", "2023-07-01", "2023-08-12", "2023-10-01", "2023-10-08",
                            "2023-01-01", "2023-02-05", "2023-03-03", "2023-04-01", "2023-05-01", "2023-06-02", "2023-07-06", "2022-09-21", "2023-11-30", "2023-10-09",
                            "2022-08-21", "2023-09-30", "2023-01-01",  "2023-08-18", "2023-05-17", "2023-02-01"), tz = "UTC"),
  End_Date = as.POSIXct(c("2023-01-15", "2023-02-15", "2023-03-15", "2023-04-15", "2023-05-15", "2023-06-15", "2023-07-15", "2023-10-15", NA, "2023-10-15",
                          NA, NA, "2023-03-15", "2023-04-15", NA, "2023-06-15", "2023-07-15", "2023-11-15", NA, NA,
                          "2022-12-07", "2023-12-11", NA, "2023-10-15", NA, NA), tz = "UTC")
)


data <- as.data.frame(data)


# 1) finding Personal_IDs who have had more than one record in the data

data1 <- data %>% 
  select(Personal_ID, Document_Type) %>% 
  group_by(Personal_ID) %>% 
  tally() %>% 
  filter(n>1)

# 2) filtering those with multiple records from the main data

data2 <- data %>% 
  filter(Personal_ID %in% data1$Personal_ID) %>%
  select(Personal_ID, Document_ID, Document_Type, Start_Date, End_Date) %>% 
  mutate(Start_Date = as.Date(Start_Date),
         End_Date = as.Date(End_Date))


# 3) Replacing missing end dates with eternity to avoid an error

eternity <- ymd("3023-12-31") 

data2 <- data2 %>%
  mutate(End_Date = case_when(is.na(End_Date) ~ as.Date(eternity, "%Y-%m-%d"), 
                                TRUE ~ End_Date))


# 4) checking overlaps for those who have had more than one record

data3 <- data2 %>% 
  group_by(Personal_ID) %>% 
  mutate(Int = interval(Start_Date, End_Date), 
         overlaps = map(seq_along(Int), function(x){
           #Get all Int indexes other than the current one
           y = setdiff(seq_along(Int), x)
           #The interval overlaps with any other intervals
           return(any(int_overlaps(Int[x], Int[y])))
         })) %>% 
  ungroup()


# 5) finding overlapping records and keeping only the one that started earlier.
# from filtering leaving out those whose records are only for permits (these can overlap with each other)

faulty <- data3 %>% 
  filter(overlaps == TRUE) %>% # kattuvad kirjed
  group_by(Personal_ID) %>%
  filter(!all(Document_Type == "PERMIT")) %>%  # not including overlapping permits
  arrange(Personal_ID, Start_Date) %>% 
  distinct(Personal_ID, .keep_all = T) %>% # distincting the earlier record from the overlapping ones
  ungroup()


# 6) detecting only necessary records from the main data by Document_ID

data <- data %>% 
  filter(!Document_ID %in% faulty$Document_ID)


rm(data1, data2, data3, eternity, faulty)
