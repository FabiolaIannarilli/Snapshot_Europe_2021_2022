# function to clean dataset Snapshot Europe 2021

clean_2021 <- function(year = 2021){
  
  # load data
  cam <- se21_cam
  depl <- se21_depl
  seq <- se21_seq
  proj <- se21_proj
  
  # rbind images files
  #images <- rbind(se21_images0, se21_images1)
  
  # remove seq before August 15th and after November 15th
  seq <- seq %>% 
    filter(ymd_hms(start_time) >= ymd_hms("2021-08-15 00:00:00") &
             ymd_hms(end_time) <= ymd_hms("2021-11-15 23:59:59")) 
  
  # adjust start and end dates of sampling at the different deployments
  depl <- depl %>% 
    mutate(start_date = ifelse(ymd_hms(start_date) < ymd_hms("2021-08-15 00:00:00"), "2021-08-15 00:00:00", start_date),
           end_date = ifelse(ymd_hms(end_date) > ymd_hms("2021-11-15 23:59:59"), "2021-11-15 23:59:59", start_date))
    
  
  # return list
  #cleaned_ls <- list(cam, depl, seq, proj, images)
  #names(cleaned_ls) <- c("cam", "depl", "seq", "proj", "images")
  cleaned_ls <- list(cam, depl, seq, proj)
  names(cleaned_ls) <- c("cam", "depl", "seq", "proj")
  
  return(cleaned_ls)
}