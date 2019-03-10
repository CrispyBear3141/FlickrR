library(RCurl) 
library(XML)   # To read XML format
library(httr)  # For API calls


#***************************************************
#Login to API***************************************
#***************************************************
api_key<-"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" #API key and secret must be obtained from https://www.flickr.com/services/api/misc.api_keys.html
secret<- "xxxxxxxxxxxxxxxx"
myapp<-oauth_app("AppName",key= api_key,secret= secret) #creates the app passing the key and secret
ep<-oauth_endpoint(request="https://www.flickr.com/services/oauth/request_token"    #get authentication credentials from the API
                   ,authorize="https://www.flickr.com/services/oauth/authorize",
                   access="https://www.flickr.com/services/oauth/access_token")
sig<-oauth1.0_token(ep,myapp,cache=F)                                             #creates variable with authentication credentials
fl_sig <- sign_oauth1.0(myapp,sig)                                                #authenticate


#***************************************************
#Set up Variables***********************************
#***************************************************
keyword <- "ethiopia" #Replace this with the keyword you want to lookup, this will be fed to the "text" variable in the URL
extras<-"date_taken,geo,tags,owner_name,license,url_o,o_dims" #extra data points. https://www.flickr.com/services/api/flickr.photos.search.html for more info
workdir <- "C:/Users/andrew.yang/Desktop/Desktop/Passion Projects/Flickr Download/" #Sets base working directory for writing files
baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,sep="")   #set base URL
userURL <- paste("https://api.flickr.com/services/rest/?method=flickr.people.getInfo&api_key=",api_key,sep="")   #set user base URL
pics_tmp<-NULL  #Initializes pics_tmp as NULL
license <-"4%2C5%2C9%2C10" #Using license 4,5,9 and 10
perpage <-10 #Pulling X photos per page
pagenum <-3  #Manually setting the maximum number of pages
imgdir <- paste(workdir, keyword, sep="") #Sets the path to save images, uses they keyword as a folder name
dir.create(imgdir) #Creates the folder in the working directory
filename <- paste("photo_details_",keyword,".csv", sep="") #Sets the name for the csv to be written


#***************************************************
#Extract Photo Info*********************************
#***************************************************
for (i in 1:pagenum){
  getPhotos <- paste(baseURL                                           #request URL
                     ,"&text=",keyword,"&per_page=",perpage,"&format=rest","&extras=",extras,"&page="
                       ,i,"&license=",license,"&dimension_search_mode=min&height=1024&width=1024","&sort=relevance",sep="")
  

  getPhotos_data <- xmlRoot(xmlTreeParse(getURL
                                         (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                                         ,useInternalNodes = TRUE ))
  
  id<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"id")                 #extract photo id
  idowner<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"owner")         #extract user id
  datetaken<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"datetaken")   #extract date picture was taken
  tags<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"tags")            #extract tags
  latitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"latitude")    #extract latitude
  longitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"longitude")  #extract longitude
  farm<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"farm")             #extract farm
  server<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"server")         #extract server
  secret<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"secret")         #extract secret
  ownernm<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"ownername")     #extract ownername
  url_o<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"url_o")           #extract url_o
  height_o<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"height_o")     #extract height_o
  width_o<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"width_o")       #extract width_o
  tmp_df<-data.frame(cbind(id,idowner,datetaken,tags,latitude,longitude,farm,server,secret,ownernm,
                           url_o,height_o,width_o),stringsAsFactors=FALSE)  #Writes all the datapoints to data frame tmp_df
  tmp_df$page <- i #adds a cloumn for the page number to tmp_df
  tmp_df$keyword <- keyword #adds a column for the keyword to tmp_df
  pics_tmp<-rbind(pics_tmp,tmp_df) #appends tmp_df to the full data frame pics_tmp
}

#***************************************************
#Extract User Info*********************************
#***************************************************
for (i in 1:nrow(pics_tmp)){
  ownerid <- pics_tmp$idowner[i]
  getUser <- paste(userURL,"&user_id=",ownerid,sep="")
  getUser_data <- xmlRoot(xmlTreeParse(getURL
                                         (getUser,ssl.verifypeer=FALSE, useragent = "flickr")
                                         ,useInternalNodes = TRUE ))

  pics_tmp$ownerpage[i] <- ownerpage<-xpathSApply(getUser_data,"//photosurl",xmlValue) 
}

write.table(pics_tmp, filename, quote=F, sep=",", row.names=F, col.names=T)
  
#***************************************************
#Download Photos************************************
#***************************************************

# for (u in 1:nrow(pics_tmp)) {
#   id <- pics_tmp$id[u]
#   #farm <- pics_tmp$farm[u]
#   #server <- pics_tmp$server[u]
#   #secret <- pics_tmp$secret[u]
#   #url <- paste("http://farm", farm, ".staticflickr.com/", server, "/", id, "_", secret, "_o_d.jpg", sep="")
#   url_o <- pics_tmp$url_o[u]
#   temp <- paste(imgdir, "/", id, ".jpg", sep="")
#   download.file(url_o, temp, mode="wb")
# }
  
  
#***************************************************
#Download Photos Small******************************
#***************************************************  
for (u in 1:nrow(pics_tmp)) {
  id <- pics_tmp$id[u]
  farm <- pics_tmp$farm[u]
  server <- pics_tmp$server[u]
  secret <- pics_tmp$secret[u]
  url <- paste("http://farm", farm, ".staticflickr.com/", server, "/", id, "_", secret, ".jpg", sep="")
  temp <- paste(imgdir, "/", id, "_s.jpg", sep="")
  download.file(url, temp, mode="wb")
}
  

#***************************************************

# <licenses>
#   <license id="0" name="All Rights Reserved" url="" />
#   <license id="1" name="Attribution-NonCommercial-ShareAlike License" url="https://creativecommons.org/licenses/by-nc-sa/2.0/" />
#   <license id="2" name="Attribution-NonCommercial License" url="https://creativecommons.org/licenses/by-nc/2.0/" />
#   <license id="3" name="Attribution-NonCommercial-NoDerivs License" url="https://creativecommons.org/licenses/by-nc-nd/2.0/" />
#   <license id="4" name="Attribution License" url="https://creativecommons.org/licenses/by/2.0/" />
#   <license id="5" name="Attribution-ShareAlike License" url="https://creativecommons.org/licenses/by-sa/2.0/" />
#   <license id="6" name="Attribution-NoDerivs License" url="https://creativecommons.org/licenses/by-nd/2.0/" />
#   <license id="7" name="No known copyright restrictions" url="https://www.flickr.com/commons/usage/" />
#   <license id="8" name="United States Government Work" url="http://www.usa.gov/copyright.shtml" />
#   <license id="9" name="Public Domain Dedication (CC0)" url="https://creativecommons.org/publicdomain/zero/1.0/" />
#   <license id="10" name="Public Domain Mark" url="https://creativecommons.org/publicdomain/mark/1.0/" />
# </licenses>
#   