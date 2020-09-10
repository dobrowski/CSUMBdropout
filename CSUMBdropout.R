

library(tidyverse)
library(here)
library(MCOE)


MCOE::setup_dir()


schools <- c("Alisal",
"Gonzales",
"Greenfield",
"King City",
"North Monterey County High",
"Seaside",
"Soledad")

#### Dropout ----

# for 16-17


dropout_all <- read_tsv(here("data","filesdropouts.txt"))


dropout_select <- dropout_all 

### CGR -----

# 12month for 17-18


cgr <- read_tsv(here("data","cgr12mo18.txt"), col_types = "ccccccccccccnnnnnnnnnnnn")

cgr_select <- cgr %>%
    # mutate(Geo = if_else(AggregateLevel == "T", "California" , CountyName)) %>%
    # mutate(CGR12 = `College Going Rate - Total (12 Months)`, CGR12 = as.numeric(CGR12))%>%
    select(AggregateLevel,CountyCode:SchoolCode, SchoolName, CharterSchool:CompleterType, `College Going Rate - Total (12 Months)`) %>%
    filter(  AggregateLevel %in% c("S", "C"),
            CountyCode %in% c("27","00"), 
             ReportingCategory == "TA",
             CharterSchool =="All",
             AlternativeSchoolAccountabilityStatus == "All",
             CompleterType =="TA",
            str_detect(SchoolName,paste0(schools, collapse = "|"))
            ) %>%
    mutate(CDS_CODE = paste0(CountyCode,DistrictCode,SchoolCode)) %>%
    select(CDS_CODE, SchoolName, `College Going Rate - Total (12 Months)`)



dropout_select <- dropout_all %>%
    filter(CDS_CODE %in% cgr_select$CDS_CODE) %>%
    group_by(CDS_CODE) %>%
    mutate(Total_Enroll = sum(ETOT),
           Total_Drop = sum(DTOT)) %>%
    ungroup() %>%
    select(CDS_CODE, Total_Enroll, Total_Drop) %>%
    distinct()


joint <- cgr_select %>%
    left_join(dropout_select)

write_csv(joint, "Dropout 16-17 and College Going 17-18.csv")

