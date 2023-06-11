get_fundingM <- function(){
  years <- c(2018, 2019, 2020, 2021, 2022)
  funds <- c(24281.98, 23821.88, 43519.16, 61256.52, 59935.84)
  total <- sum(funds)
  total <- formatC(total, digits = 2, big.mark=",", format = "f")
  total
  return(total)
}