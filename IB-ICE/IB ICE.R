require(foreign)
setwd("~/GitHub/Projects/IB-ICE")
require(foreign)
IB_Data = read.spss(file = 'IB ICE.sav',to.data.frame = T)
require(mlr)
summarizeColumns(IB_Data)
require(dplyr)
IB_Data_New = IB_Data %>%
                select(Slno,
                       Light1,Light2,Light3,
                       Emp1,Emp2,Emp3,
                       assort1,assort2,assort3,assort4,
                       Urge1,Urge2,Urge3_Changed,
                       IB1_changed,IBT2,IBT3,IBT4,IBT5,
                       Layout1,Layout2,Layout3,
                       Timetaken,Moneyavailable,Moneyspent,Age,Gender,
                       IBProp,IBProp) %>%
                mutate(Light = mean(c(Light1,Light2,Light3),na.rm=T),
                       Emp = mean(c(Emp1,Emp2,Emp3,na.rm=T)),
                       Assort = mean(c(assort1,assort2,assort3,assort4),na.rm=T),
                       Urge = mean(c(Urge1,Urge2,Urge3_Changed),na.rm=T),
                       IBT = mean(c(IB1_changed,IBT2,IBT3,IBT4,IBT5),na.rm=T),
                       Layout = mean(c(Layout1,Layout2,Layout3,na.rm=T)))

install.packages("pdftools")
library(pdftools)
download.file("http://eci.nic.in/eci_main/archiveofge2009/Stats/VOLII/VolII_ConstituencyDataSummary.pdf", "test.pdf", mode = "wb")
txt <- pdf_text("test.pdf")

library(RCurl)
library(XML)
theurl <- "http://myneta.info/ls2014/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary"
webpage <- getURL(theurl)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){})
tablehead <- xpathSApply(pagetree, "//*/table[@class='divTableWithFloatingHeader']/tr/th", xmlValue)
table <- xpathApply(xmlRoot(pagetree), "//table", xmlValue)

body <- pagetree$children$html$children$body 
divbodyContent <- body$children$div$children[[1]]$children$div$children[[4]]
tables <- divbodyContent$children[names(divbodyContent)=="table"]
divbodyContent <- pagetree$children[[1]][2]$body[8]
tables <- divbodyContent$children[names(divbodyContent)=="table"]



library(rvest)
webpages <- read_html(theurl)
rank_data_html <- html_nodes(webpage,'.divTableWithFloatingHeader')


library(rvest)
webpages <- read_html('https://m.imdb.com/chart/top')
rank_data_html <- html_nodes(webpage,'.unbold')
