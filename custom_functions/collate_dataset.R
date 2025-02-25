# function to collate the two Snapshot Europe dataset by object content

collate_dataset <- function(dat1 = cleaned_2021, dat2 = cleaned_2022){
  
  # info about camera model and make
  cam <- rbind(data.frame(year = 2021, dat1$cam),
               data.frame(year = 2022, dat2$cam))
  
  # info on deployments
  depl <- rbind(data.frame(year = 2021, dat1$depl),
               data.frame(year = 2022, dat2$depl)) %>% 
    mutate(subproject_name_no_year = substr(subproject_name, 1, nchar(subproject_name)-3))
  
  # info on sequences (observations)
  seq <- rbind(data.frame(year = 2021, dat1$seq),
                data.frame(year = 2022, dat2$seq)) 
  
  # info on projects
  proj <- rbind(data.frame(year = 2021, dat1$proj),
               data.frame(year = 2022, dat2$proj))
  
  # info on images
  images <- rbind(data.frame(year = 2021, dat1$images),
                data.frame(year = 2022, dat2$images))

  # return list
  cleaned_ls <- list(cam, depl, seq, proj, images)
  names(cleaned_ls) <- c("cam", "depl", "seq", "proj", "images")
  
  return(cleaned_ls)
  

}