#function to uppend zero's
#one zero to the existing single digit number in the tens place(left)
#two zeros if no number



uppzero <- function(Req_hour)
{
  j <- 1
  
  for (k in Req_hour[])
  {
    if (is.na(Req_hour[j])!=1)
    {
      if (nchar(Req_hour[j]) == 1)
      {
        Req_hour[j] <- paste0("0",Req_hour[j])
      }
      else if (nchar(Req_hour[j]) == 0)
      {
        Req_hour[j] <- paste0("00",Req_hour[j])
      }
      
    }
    else
    {
      Req_hour[j] <- "00"
    }
    
    j <- j+1
    
  }
  
  return(Req_hour)
}
