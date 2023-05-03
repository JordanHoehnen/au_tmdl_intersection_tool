
#This tool is designed to merge EPA 305b IDs with WDNR assesment numbers, WIBIC, and TMDLD IDs in addition to other pertinent info for TMDL development

#Follow the steps numbered ##1-4 to route to the correct data paths

arc.check_product()
library(arcgisbinding)
library(sf)
library(sp) 
library(tidyverse)
library(ROracle)
library(rgdal)
library(tidyr)

options(scipen = 9)

causes = c("Sediment/Total Suspended Solids",
           "Total Phosphorus")

listed_waters = c("303d Listed", "TMDL Development")

## 1
#Insert username and password for DNR_SECPRD.WORLD
usr = rstudioapi::askForPassword(prompt="Please enter username")
pwd = rstudioapi::askForPassword()

com_delim = function(df, col) {
  out = df %>%
    # st_drop_geometry() %>%
    .[[col]] %>%
    str_split(., ",") %>%
    unlist(.) %>%
    str_trim() %>%
    unique() %>%
    sort() %>%
    paste(., collapse=", ")
  return(out)
}


# Pulling Tables ----------------------------------------------------------

#"info" table -- waterbody name, waters ID, WBIC, County, Start & End mile, source category, impairment indicator, pollutant, TP Criterion, Basin, TMDL Subbasin
con = dbConnect(drv=Oracle(), dbname="dnr_secprd.world", username=usr, password=pwd)
info_tbl = dbGetQuery(con, "select OFFICIAL_NAME, WBIC, ASSESSMENT_UNIT_SEQ_NO, COUNTY_NAME, START_MILE_NO, END_MILE_NO, IMPAIRED_WATERS_CATEGORY,
                      IMPAIRMENT, POLLUTANT, WATERSHED_NAME, TMDL_ID, STATUS_CODE from W23321.WT_IMPAIREDWATERS_CNTY_LIST_MV") %>%
  left_join(
    dbGetQuery(con, "SELECT ASSESSMENT_UNIT_SEQ_NO, EPA_ID305B FROM W23321.WT_ASSESSMENT_UNIT"),
    by="ASSESSMENT_UNIT_SEQ_NO") %>%
  filter(POLLUTANT %in% causes)%>%
  filter(STATUS_CODE %in% listed_waters)
dbDisconnect(con)


# TP Criteria  ------------------------------------------------------------

#Lakes/Impoundments TP Criteria
con = dbConnect(drv=Oracle(), dbname="dnr_secprd.world", username=usr, password=pwd)
lakes_tp_criteria = dbGetQuery(con, "SELECT SNAPSHOT_ID, ASSESSMENT_UNIT_SEQ_NO, TP_THRESHHOLD FROM W23321.WT_LAKE_TP_STATION_SUMMARY_VX")
lakes_tp_criteria_drop <- lakes_tp_criteria[duplicated(lakes_tp_criteria$ASSESSMENT_UNIT_SEQ_NO), ]
length(unique(lakes_tp_criteria_drop$ASSESSMENT_UNIT_SEQ_NO))
length(unique(lakes_tp_criteria_drop$ASSESSMENT_UNIT_SEQ_NO)) == nrow(lakes_tp_criteria_drop)
dbDisconnect(con)


#Stream/River TP Criteria
con = dbConnect(drv=Oracle(), dbname="dnr_secprd.world", username=usr, password=pwd)
stream_tp_criteria = dbGetQuery(con, "SELECT ASSESSMENT_UNIT_SEQ_NO, TP_THRESHHOLD_100_FLAG FROM W23321.WT_ASSESSMENT_UNIT")
dbDisconnect(con)

stream_tp_criteria <- stream_tp_criteria %>%
  mutate(tp_criteria = case_when(TP_THRESHHOLD_100_FLAG == 'Y' ~ '100',
                                 TP_THRESHHOLD_100_FLAG == 'N' ~ '75'))
stream_tp_criteria = select(stream_tp_criteria, -2)


# Subbasins ---------------------------------------------------------------

## 2
#Insert corresponding Plan_Seq_No for the TMDL or area of interest and direct to the path on your computer to WT_PLAN_TMDL_SUBBASINS_AR shapefile
tmdl_subbasins = arc.open("C:/Users/hoehnj/Documents/ArcGIS/Projects/generic_work/HOEHNJ[DNRWQRP].sde/DNRWQRP.WT_PLAN_TMDL_SUBBASINS_AR")
tmdl_subbasins = arc.select(tmdl_subbasins, where_clause = "PLAN_SEQ_NO = 153881362")
tmdl_subbasins_sf = arc.data2sf(tmdl_subbasins)


# TMDL Lakes --------------------------------------------------------------

## 3
#Lakes variable: Route correct path to DNR_SDE.SDE and pull W23321.WT_ASSESSMENT_UNIT_AR_24K shapefile
au_lakes = arc.open("C:/Users/hoehnj/Documents/ArcGIS/Projects/generic_work/dnr_sde.world.sde/W23321.WT_ASSESSMENT_UNIT_DATA_24K/W23321.WT_ASSESSMENT_UNIT_AR_24K")
au_lakes = arc.select(au_lakes)
au_lakes_sf = arc.data2sf(au_lakes) %>%
  left_join(info_tbl, by=c("ASSESSMENT_UNIT_SEQ_NO"="ASSESSMENT_UNIT_SEQ_NO")) %>%
  left_join(lakes_tp_criteria, by=c("ASSESSMENT_UNIT_SEQ_NO"="ASSESSMENT_UNIT_SEQ_NO")) %>%
  filter(STATUS_CODE %in% listed_waters)

#Intersection between lakes and TMDL subbasins
lakes_intersect <- st_intersection(au_lakes_sf, tmdl_subbasins_sf) %>% 
  select(OFFICIAL_NAME, ASSESSMENT_UNIT_SEQ_NO, WBIC, COUNTY_NAME, START_MILE_NO, END_MILE_NO,
         IMPAIRED_WATERS_CATEGORY, IMPAIRMENT, POLLUTANT, WATERSHED_NAME, TMDL_ID, EPA_ID305B, TP_THRESHHOLD, SUBBASIN) %>%
  group_by(OFFICIAL_NAME, ASSESSMENT_UNIT_SEQ_NO, WBIC, COUNTY_NAME, START_MILE_NO, END_MILE_NO, WATERSHED_NAME, TP_THRESHHOLD, TMDL_ID, EPA_ID305B, IMPAIRED_WATERS_CATEGORY) %>%
  nest() %>%
  mutate(
    IMPAIRMENT=lapply(data, com_delim, col="IMPAIRMENT"),
    POLLUTANT=lapply(data, com_delim, col="POLLUTANT"),
    TMDL_Subbasin=lapply(data, com_delim, col="SUBBASIN"),
  ) %>%
  ungroup()

impaired_lakes <- filter(lakes_intersect[!duplicated(lakes_intersect$WBIC),])
impaired_lakes <- select(impaired_lakes, -12)
impaired_lakes <- apply(impaired_lakes,2,as.character)

write.csv(impaired_lakes, "_impaired_lakes.csv", row.names = FALSE)

st_write(lakes_intersect, dsn="~/Projects/AU_TMDL_Intersection/temp", layer="lakes_intersection.shp", driver ="ESRI Shapefile")


# TMDL Streams ------------------------------------------------------------

## 4
#Streams variable: Route correct path to DNR_SDE.SDE and pull W23321.WT_ASSESSMENT_UNIT_LN_24K shapefile 
au_streams = arc.open("C:/Users/hoehnj/Documents/ArcGIS/Projects/generic_work/dnr_sde.world.sde/W23321.WT_ASSESSMENT_UNIT_DATA_24K/W23321.WT_ASSESSMENT_UNIT_LN_24K")
au_streams = arc.select(au_streams)
au_streams_sf = arc.data2sf(au_streams) %>%
  left_join(info_tbl, by=c("ASSESSMENT_UNIT_SEQ_NO"="ASSESSMENT_UNIT_SEQ_NO")) %>%
  left_join(stream_tp_criteria, by=c("ASSESSMENT_UNIT_SEQ_NO"="ASSESSMENT_UNIT_SEQ_NO")) %>%
  filter(STATUS_CODE %in% listed_waters)

#Intersection between streams and TMDL subbasins
streams_intersect <- st_intersection(au_streams_sf, tmdl_subbasins_sf) %>% 
  select(OFFICIAL_NAME, ASSESSMENT_UNIT_SEQ_NO, WBIC, COUNTY_NAME, START_MILE_NO, END_MILE_NO, IMPAIRED_WATERS_CATEGORY, IMPAIRMENT,
         POLLUTANT, WATERSHED_NAME, TMDL_ID, EPA_ID305B, STATUS_CODE, SUBBASIN, tp_criteria) %>%
  mutate(LEN_miles=as.numeric(st_length(geom)*0.000621371)) %>%
  group_by(ASSESSMENT_UNIT_SEQ_NO) %>%
  mutate(AU_LEN_FRAC = LEN_miles / sum(LEN_miles)) %>%
  arrange(ASSESSMENT_UNIT_SEQ_NO, SUBBASIN)


impaired_streams <- streams_intersect[!duplicated(streams_intersect$geom),] %>%
  filter(AU_LEN_FRAC > 0.000000004) %>%
  group_by(OFFICIAL_NAME, ASSESSMENT_UNIT_SEQ_NO, WBIC, COUNTY_NAME, START_MILE_NO, END_MILE_NO,
           IMPAIRED_WATERS_CATEGORY,  WATERSHED_NAME, TMDL_ID, EPA_ID305B, tp_criteria) %>%
  nest() %>%
  mutate(
    IMPAIRMENT=lapply(data, com_delim, col="IMPAIRMENT"),
    POLLUTANT=lapply(data, com_delim, col="POLLUTANT"),
    TMDL_Subbasin=lapply(data, com_delim, col="SUBBASIN"),) %>%
  ungroup() 
impaired_streams = select(impaired_streams, -12)

impaired_streams <- apply(impaired_streams,2,as.character)

write.csv(impaired_streams, "_impaired_streams.csv", row.names = FALSE)

st_write(streams_intersect, dsn="~/Projects/AU_TMDL_Intersection/temp", layer="streams_intersection.shp", driver ="ESRI Shapefile")