# function to clean dataset Snapshot Europe 2022


clean_2022 <- function(year = 2022){
  
  # load data
  cam <- se22_cam
  depl <- se22_depl
  seq <- se22_seq
  proj <- se22_proj
  # load and rbind images files
  images <- rbind(se22_images0, se22_images1, se22_images2)
  
  # remove seq before August 15th and after November 15th
  seq <- seq %>% 
    filter(ymd_hms(start_time) >= ymd_hms("2022-08-15 00:00:00") &
             ymd_hms(end_time) <= ymd_hms("2022-11-15 23:59:59")) %>% 
    filter(sequence_id != "5260496")
  
  # adjust start and end dates of sampling at the different deployments
  depl <- depl %>% 
    mutate(start_date = ifelse(ymd_hms(start_date) < ymd_hms("2022-08-15 00:00:00"), "2022-08-15 00:00:00", start_date),
           end_date = ifelse(ymd_hms(end_date) > ymd_hms("2022-11-15 23:59:59"), "2022-11-15 23:59:59", end_date))
  
  # remove subprojects
  depl <- depl %>% 
    filter(!(subproject_name %in% c("ES_Forest_Cantabrian Mountains_22", 
                                    "RU_Forest_Kerzhensky Nature Reserve_22")))

  # remove locations 
  depl <- depl %>% 
    filter(!(placename %in% c("TR_Forest_Sarikamis_22_111_F", 
                              "TR_Forest_Sarikamis_22_39_9", 
                              "TR_Forest_Sarikamis_22_47_F",
                              "Biebrza National Park")))
  
  # remove deployments 
  depl <- depl %>% 
    filter(!(deployment_id %in% c("IT_ Forest_Salbertrand_ 22_Loc07_first", 
                                  "Rizana13-BR205",
                                  "Rizana16-BR318",
                                  "Rizana19-BR170",
                                  "Rizana31-BR253",
                                  "Rizana01-BR219",
                                  "Rizana07-BR228"
    )))
  
  # remove duplicates
  depl <- depl %>% distinct()
  
  # keep only records associated to remaining deployments
  seq <- seq %>% filter(deployment_id %in% depl$deployment_id)
  images <- images %>% filter(deployment_id %in% depl$deployment_id)
  
  # fix taxonomy: all Bovidae as Cetartiodactyla
  seq <- seq %>% 
    mutate(order = ifelse(family == "Bovidae", "Cetartiodactyla", order))
  
  # return list
  cleaned_ls <- list(cam, depl, seq, proj, images)
  names(cleaned_ls) <- c("cam", "depl", "seq", "proj", "images")
  
  return(cleaned_ls)
}