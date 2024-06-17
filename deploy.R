library(rsconnect)
library(httr)

rsconnect::setAccountInfo(name='afdalmh952', 
                          token='2426F2A484798892B7DD3C1B4FF4ACE5', 
                          secret='gvdgsaVqh1wYb2QQl6msO7TpK0hX9Gb/D2EC5Mji')

rsconnect::setAccountInfo(name='afdalmh952', 
                          token='6482819ADFF0D1757DE7BE2028BA4052', 
                          secret='ZdZ+ub8tfiPXSGSVVNfBTBuLqoHq8FCxC62m0EWL')

rsconnect::setAccountInfo(name='afdalmh952', 
                          token='3CA2C9412C9991CC93F54C9285BD202B', 
                          secret='rnqRsMXm6T8irjemf8l5dC3mlVv+VZX4fr9VHyhr')

rsconnect::setAccountInfo(name='afdalmh952', 
                          token='1D70802D34D6307D9062A9ED8351480F', 
                          secret='XAMSoKUrZLs6gVPu2lUktDKAwMcv7TKfQpWplBMy')

rsconnect::setAccountInfo(name='afdalmh952', 
                          token='1A11691C2A49C0EE58363D67CC99931E', 
                          secret='ZPk0vXTk+YKeOsi+/ozQSbNdq+/LM4I63uoNQh45')

# Set max bundle size option
options(rsconnect.max.bundle.size = 1e20)

# Set HTTP config to increase timeout
httr::set_config(httr::config(ssl_verifypeer = 1L, timeout = 100000))
options(rsconnect.http.timeout = 100000)

# Increase the HTTP connection timeout
#Sys.setenv(RSCONNECT_TARBALL_COMPRESSION_LEVEL = 2)

rsconnect::deployApp(
  appDir = "C:/Users/mhafd/OneDrive/Documents/GitHub/EtherealSEC",
  account = "mhafdal95"
)

rsconnect::deployApp()

# Show logs
#rsconnect::showLogs(appName = "etherealsec", account = "afdalmh952")

library(rsconnect)

# Fungsi untuk mengecek apakah deployment berhasil
deployAppWithRetry <- function(appDir, account, max_attempts = 10) {
  attempt <- 1
  success <- FALSE
  
  while (!success && attempt <= max_attempts) {
    cat("Attempt", attempt, "to deploy the app...\n")
    try({
      rsconnect::deployApp(appDir = appDir, account = account)
      success <- TRUE
      cat("Deployment successful on attempt", attempt, "!\n")
    }, silent = TRUE)
    
    if (!success) {
      cat("Deployment failed on attempt", attempt, ". Retrying...\n")
      attempt <- attempt + 1
      Sys.sleep(10) # Wait for 10 seconds before retrying
    }
  }
  
  if (!success) {
    cat("Failed to deploy the app after", max_attempts, "attempts.\n")
  }
}

# Panggil fungsi dengan direktori aplikasi dan akun Anda
deployAppWithRetry(
  appDir = "C:/Users/mhafd/OneDrive/Documents/GitHub/EtherealSEC",
  account = "mhafdal95"
)

