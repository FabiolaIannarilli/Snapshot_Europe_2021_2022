# function to clean dataset Snapshot Europe 2021


clean_2021 <- function(year = 2021){
  
  # load data
  cam <- se21_cam
  depl <- se21_depl
  seq <- se21_seq
  proj <- se21_proj
  # load and rbind images files
  images <- rbind(se21_images0, se21_images1)
  
  # remove seq before August 15th and after November 15th
  seq <- seq %>% 
    filter(ymd_hms(start_time) >= ymd_hms("2021-08-15 00:00:00") &
             ymd_hms(end_time) <= ymd_hms("2021-11-15 23:59:59")) 
  
  # adjust start and end dates of sampling at the different deployments
  depl <- depl %>% 
    mutate(start_date = ifelse(ymd_hms(start_date) < ymd_hms("2021-08-15 00:00:00"), "2021-08-15 00:00:00", start_date),
           end_date = ifelse(ymd_hms(end_date) > ymd_hms("2021-11-15 23:59:59"), "2021-11-15 23:59:59", end_date))
  
  # remove locations 
  depl <- depl %>% 
    filter(!(placename %in% c("GER_Forest_Bavarian Forest National Park_21_loc_20", 
                              "ES_Wetland_Aiguamolls_Empordà_21_loc_1", 
                              "PL_commercial_forest_stand_Celestynow_Forest_D_loc40",
                              "PL_commercial_forest_stand_Celestynow_Forest_D_loc27",
                              "RO_PNPV_38",
                              "RO_PNPV_20",
                              "MK_Forest_Mavrovo National Park_Brzovec",
                              "SW_Forest_Sodermanland_21_Loc_04",
                              "SW_Forest_Sodermanland_21_Loc_08",
                              "SW_Forest_Sodermanland_21_Loc_02", 
                              "SW_Forest_Sodermanland_21_Loc_10")))
  
  # remove deployments 
  depl <- depl %>% 
    filter(!(deployment_id %in% c("ES_Wetland_Aiguamolls_Empordà_21_loc_8.2 09/06/2021")))
  
  # remove duplicates
  depl <- depl %>% distinct()
  
  # keep only records associated to remaining deployments
  seq <- seq %>% filter(deployment_id %in% depl$deployment_id)
  images <- images %>% filter(deployment_id %in% depl$deployment_id)
  
  # return list
  cleaned_ls <- list(cam, depl, seq, proj, images)
  names(cleaned_ls) <- c("cam", "depl", "seq", "proj", "images")
  
  return(cleaned_ls)
}