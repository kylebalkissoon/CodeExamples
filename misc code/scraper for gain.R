library(rvest)
library(XML)



###Change to your own folder!!
target_folder = "c:/Users/Kyle/Documents/fxdata/"
stub = "http://ratedata.gaincapital.com/"
for(yearz in 2010:2015){
  
  temp_url = paste0(stub,yearz)
  
 s1= getHTMLLinks(temp_url)
 ##Remove each months data blah blah blah
 s1 = s1[!grepl("Each",s1)]
 ##Make new URL use substr to get rid of the old \ as it escapes!!!!
 
 monthz=paste0(temp_url,"/",substr(s1,3,nchar(s1)))
 monthz=monthz[!grepl(".7z",monthz)]
 monthz = monthz[!grepl("This is the home",monthz)]
 ###Loop through months and download
 for( mos in monthz ){
 files_to_dl = getHTMLLinks(mos)
 files_to_dl = files_to_dl [!grepl("Each",files_to_dl)] 
 flist = substr(files_to_dl,3,nchar(files_to_dl))
 files_to_dl = paste0(mos,"/",substr(files_to_dl,3,nchar(files_to_dl)))
 
 for(i in 1:length(files_to_dl)){
 download.file(files_to_dl[i],paste0(target_folder,yearz,"_",substr(mos,38,39),"_",flist[i]))
}}}


