library(ecosscraper)
library(ggplot2)
library(ggthemes)
library(stringr)
library(viridis)

data(HCP_SHA)
data(CCAA)
data(CCA)

data(state)

dim(HCP_SHA)
names(HCP_SHA)
names(HCP_SHA)[12] <- "FR_Date"
names(HCP_SHA)[13] <- "Date_Agmt_Permit"
names(HCP_SHA)[15] <- "Date_Agmt_Permit_Expired"
names(HCP_SHA)[21] <- "Non_Listed_Species"
names(HCP_SHA)[24] <- "Area_Covered"
names(HCP_SHA)[25] <- "Area_Enrolled"
names(HCP_SHA)[30] <- "FR_Documents"

names(CCAA)
names(CCAA)[10] <- "FR_Date"
names(CCAA)[11] <- "Date_Agmt_Permit"
names(CCAA)[13] <- "Date_Agmt_Permit_Expired"
names(CCAA)[21] <- "Area_Covered"
names(CCAA)[22] <- "Area_Enrolled"
names(CCAA)[27] <- "FR_Documents"

names(CCA)
names(CCA)[9] <- "Date_Agmt_Permit"
names(CCA)[11] <- "Date_Agmt_Permit_Expired"
names(CCA)[16] <- "Area_Covered"
names(CCA)[17] <- "Area_Enrolled"
names(CCA)[22] <- "FR_Documents"

shared <- intersect(names(HCP_SHA), intersect(names(CCAA), names(CCA)))
shared

HCP_sub <- HCP_SHA[shared]
CCAA_sub <- CCAA[shared]
CCA_sub <- CCA[shared]

cons_agmt <- rbind(HCP_sub, CCAA_sub, CCA_sub)
dim(cons_agmt)
names(cons_agmt)
summary(as.Date(cons_agmt$Date_Agmt_Permit, format = "%m/%d/%Y"))
summary(as.Date(cons_agmt$Date_Agmt_Permit_Expired, format = "%m/%d/%Y"))
devtools::use_data(cons_agmt)

################################################
# Now to get to the overview

# how many docs are linked?
outs <- strsplit(x = cons_agmt$Outlinks, split = "|", fixed = TRUE)
n_outs <- lapply(outs, length)
hist(unlist(n_outs))
sum(unlist(n_outs > 0))
length(unlist(outs))

# Summary by agreement type
table(cons_agmt$Agreement_Type)

# Summary by FWS_region
table(cons_agmt$USFWS_Regions)
region <- strsplit(x = cons_agmt$USFWS_Regions, split = " , ")
cons_agmt$Region_ls <- region
table(unlist(region))
table(cons_agmt$USFWS_Regions, cons_agmt$Agreement_Type)
# I need to expand the agreement type and region list to a df to table...

# By ES/field office
tab_ESO <- table(cons_agmt$USFWS_Field_Offices)
head(sort(tab_ESO, decreasing = TRUE))

# HCPs and SHAs by NEPA process
table(HCP_SHA$NEPA_Process)

##################################
# now some date conversions and quick glances
tmp <- as.Date(HCP_SHA$FR_Date, format="%m/%d/%Y")
t2 <- as.numeric(tmp)
t3 <- data.frame(t2)
ggplot(t3, aes(x = t2)) + 
  geom_histogram() + 
  theme_bw()# Need to add date xlabs
HCP_SHA$FR_Date <- tmp

tmp <- as.Date(cons_agmt$Date_Agmt_Permit, format="%m/%d/%Y")
summary(tmp)
t2 <- as.numeric(tmp)
t3 <- data.frame(Numeric_Date = t2, Agreement_Type = cons_agmt$Agreement_Type)
# A quick plot
ggplot(t3, aes(x = Numeric_Date, fill = Agreement_Type)) + 
  geom_histogram() + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_viridis(discrete = TRUE)
  # Need to add date xlabs and clean up, e.g., legend
cons_agmt$Date_Agmt_Permit <- tmp
summary(cons_agmt$Date_Agmt_Permit)

tmp <- as.Date(HCP_SHA$Date_Application_Withdrawn, format="%m/%d/%Y")
unique(tmp)
HCP_SHA$Date_Application_Withdrawn <- tmp

tmp <- as.Date(cons_agmt$Date_Agmt_Permit_Expired, format="%m/%d/%Y")
summary(tmp)
cons_agmt$Date_Agmt_Permit_Expired <- tmp
t2 <- as.numeric(tmp)
hist(t2)

tmp <- as.Date(HCP_SHA$Date_Permit_Denied, format="%m/%d/%Y")
unique(tmp)
HCP_SHA$Date_Permit_Denied <- tmp

tmp <- as.Date(HCP_SHA$Date_Permit_Suspended, format="%m/%d/%Y")
unique(tmp)
HCP_SHA$Date_Permit_Suspended <- tmp

tmp <- as.Date(HCP_SHA$Date_Permit_Revoked, format="%m/%d/%Y")
unique(tmp)
HCP_SHA$Date_Permit_Revoked <- tmp
tmp <- HCP_SHA[!is.na(HCP_SHA$Date_Permit_Revoked), ]
glimpse(tmp) # The HCP that was tossed by a court last year

###########################################################################
# Analysis of species: HCP/SHA
tmp <- strsplit(HCP_SHA$Listed_Species, 
                split = "|", 
                fixed = TRUE)
t2 <- lapply(tmp, FUN = str_trim)
head(t2)
HCP_SHA$Listed_Species_ls <- t2
t3 <- lapply(HCP_SHA$Listed_Species_ls, FUN = length)
hist(unlist(t3), breaks = 20)

spp_covered <- unlist(HCP_SHA$Listed_Species_ls)
spp_cov_tab <- table(spp_covered)
head(sort(spp_cov_tab, decreasing = TRUE), 10)

tmp <- strsplit(HCP_SHA$Non_Listed_Species, 
                split = "|", 
                fixed = TRUE)
t2 <- lapply(tmp, FUN = str_trim)
head(t2)
HCP_SHA$Non_Listed_Species_ls <- t2

spp_covered <- unlist(HCP_SHA$Non_Listed_Species_ls)
spp_cov_tab <- table(spp_covered)
head(sort(spp_cov_tab, decreasing = TRUE), 10)


###########################################################################
# Habitat
length(unique(cons_agmt$Habitat))
sum(cons_agmt$Habitat != "", na.rm = TRUE)
hab_tab <- table(cons_agmt$Habitat)
head(sort(hab_tab, decreasing = TRUE), 8)
# There's something here I think, but probably need to do some NLP on these...

###########################################################################
# States
st_un <- unique(cons_agmt$States)

res <- list()
for(i in 1:length(cons_agmt$States)) {
  st <- cons_agmt$States[i]
  if(any(grepl(pattern = st, x = state.name))) {
    res[[i]] <- st
  } else {
    sts <- c()
    for(j in state.name) {
      if(grepl(x = st, pattern = j)) {
        sts <- c(sts, j)
      }
    }
    if(length(sts) > 0) {
      res[[i]] <- sts
    } else {
      res[[i]] <- st[i]
    }
  }
}

table(unlist(res))
cons_agmt$States_ls <- res

###########################################################################
# Location
locs <- cons_agmt$Location
loc_patt <- str_match_all(locs, pattern = "[A-Za-z\\ ]+\\ (Co\\.|County)")

res <- list()
for(i in 1:length(loc_patt)) {
  res[[i]] <- str_trim(loc_patt[[i]][,1])
}

res2 <- lapply(res, gsub, pattern = "Co\\.", replacement = "County")
head(res2)

res3 <- str_match_all(res2, pattern = "(\\w+ \\w+|\\w+) County")
head(res3)

res4 <- list()
for(i in 1:length(res3)) {
  res4[[i]] <- str_trim(res3[[i]][,1])
}
head(res4)

# res5 <- c()
# for(i in 1:length(res4)) {
#   res5 <- c(res5, str_match_all(res4[[i]], pattern = "^\\w+"))
# }
# head(res5)
# unique(unlist(res5))
# head(sort(table(unlist(res5)), decreasing = TRUE), 50)

clean_loc <- lapply(res4, gsub,
                    pattern = "^in |^Subdivision |^and |^of |^CA |^III |^northern |^southwestern |^City |^within |^adn |^central |^northeast |^northen |^s |^SC |^the |^through |^Eat",
                    replacement = "")
head(clean_loc, 50)

loc2 <- unlist(res2)
length(unique(loc2))
loc_tab <- table(loc2)
head(sort(loc_tab, decreasing = TRUE))

loc_tab2 <- table(unlist(clean_loc))
head(sort(loc_tab2, decreasing = TRUE))

cons_agmt$Location_ls <- clean_loc

###########################################################################
# Area covered
# unique(cons_agmt$Area_Covered)
ac <- cons_agmt$Area_Covered
head(ac)

ac_patt <- str_match_all(cons_agmt$Area_Covered, pattern = "[0-9\\.]+ acres")
head(ac_patt)

ac_pat2 <- lapply(ac_patt, gsub, pattern = "\\ acres", replacement = "")
head(ac_pat2)
ac_num <- lapply(ac_pat2, as.numeric)
ac_len <- lapply(ac_num, length)
cons_agmt$Acres_Covered_num <- ac_num

sum(unlist(ac_num), na.rm = TRUE)  #??? 1.2B acres! How to account for overlap?
hist(unlist(ac_num))
summary(unlist(ac_num))
sub_ac <- unlist(ac_num)
sub_ac <- sub_ac[sub_ac < 100000]
sub_ac <- sub_ac[sub_ac < 100]
hist(sub_ac, breaks = 50)

# huge <- cons_agmt[grep(x = cons_agmt$Area_Covered, pattern = "1611"), ]
# huge$Plan_Name

###########################################################################
# Area enrolled
length(unique(cons_agmt$Area_Enrolled))
enroll_un <- unique(cons_agmt$Area_Enrolled)
enroll_un[!grepl(pattern = "^Data not avail", x = enroll_un)]
# nothing to see

###########################################################################
# Land use
LU <- cons_agmt$Land_Use
unique(LU)
# probably amenable to a bag-of-words analysis, but unstructured here...

###########################################################################
# Durations of agreements
unique(cons_agmt$Duration)

yr_mo <- str_match_all(string = cons_agmt$Duration,
                       pattern = "[0-9]+\\ years,\\ [0-9]+ months")
yr_mo <- unlist(yr_mo)
head(yr_mo)
table(yr_mo)

dur_yr <- unlist(str_match_all(string = yr_mo, pattern = "^[0-9]+"))
head(dur_yr)
dur_yr <- as.numeric(dur_yr)
hist(dur_yr[dur_yr < 999], breaks = 50)
cons_agmt$Duration_Years <- dur_yr

dur_mo <- unlist(gsub(pattern = ", ", replacement = "",
                      x = str_match_all(string = yr_mo, pattern = ", [0-9]+")))
head(dur_mo)
dur_mo <- as.numeric(dur_mo)
cons_agmt$Duration_Months <- dur_mo

###########################################################################
# Re-write the data
head(data.frame(cons_agmt))
summary(cons_agmt$Date_Agmt_Permit)
devtools::use_data(cons_agmt, overwrite = TRUE)


