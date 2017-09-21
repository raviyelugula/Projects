### Missing States handling with the help of location
Dataset$StateCorrected = Dataset$State
Dataset[which(Dataset$StateCorrected == 'Chattisgarh'),'StateCorrected'] = 'Chhattisgarh'
Dataset[which(Dataset$StateCorrected == 'Orissa'),'StateCorrected'] = 'Odisha'
Dataset[which(Dataset$StateCorrected == 'W Bengal'),'StateCorrected'] = 'West Bengal'
Dataset[which(Dataset$StateCorrected == 'TN'),'StateCorrected'] = 'Tamil Nadu'

Dataset$StateCorrected = tolower(trimws(Dataset$StateCorrected))
Dataset$`City-final` = tolower(trimws(Dataset$`City-final`))
Dataset$City = tolower(trimws(Dataset$City))
Dataset$Location = tolower(trimws(Dataset$Location))

States_Cities = read_excel(path = 'Cities_States.xlsx')
States_Cities$`Name of City` = trimws(tolower(States_Cities$`Name of City`),which = 'both')
States_Cities$State = trimws(tolower(States_Cities$State),which = 'both')

Dataset$StateCorrected = trimws(tolower(Dataset$StateCorrected),which = 'both')
Dataset$Location = trimws(tolower(Dataset$Location),which = 'both')

# Mapping based on loaction
require(readxl)
Locations_Assigned = read_excel(path = 'data/Cities_States.xlsx')
Locations_Missed = read_excel(path = 'data/Workingdata.xlsx',sheet = 'State_City_Missing')

Locations_Assigned$Location = tolower(trimws(Locations_Assigned$Location))
Locations_Missed$Location = tolower(trimws(Locations_Missed$Location))

require(stringdist)
DistanceNameMatrix<-matrix(NA, ncol = length(Locations_Missed$Location),
                           nrow = length(Locations_Assigned$Location))
for(i in 1:length(Locations_Missed$Location)) {
  for(j in 1:length(Locations_Assigned$Location)) { 
    DistanceNameMatrix[j,i]<-stringdist(tolower(Locations_Missed[i,]$Location),
                                        tolower(Locations_Assigned[j,]$Location),
                                        method ='jw')      
  }
}

Match_Location_DF<-NULL
MinName<-apply(DistanceNameMatrix, 1, base::min)
for(i in 1:nrow(DistanceNameMatrix)){
  S2<-match(MinName[i],DistanceNameMatrix[i,])
  S1<-i
  Match_Location_DF<-rbind(data.frame(Missed_Id=S2,Assigned_Id=S1,
                                      Missed=Locations_Missed[S2,]$Location, 
                                      Assigned=Locations_Assigned[S1,]$Location, 
                                      adist=MinName[i],
                                      method='jm'),
                           Match_Location_DF)
}

Match_Location_DF = Match_Location_DF[Match_Location_DF$adist<=0.05,]   
Match_Location_DF = Match_Location_DF[order(Match_Location_DF$Assigned_Id),]
Match_Location_DF = Match_Location_DF[!duplicated(Match_Location_DF[,3:4]),] 

for(i in 1:nrow(Match_Location_DF)){
  temp_Assigned_Id = Match_Location_DF$Assigned_Id[i]
  temp_state = Locations_Assigned$State[temp_Assigned_Id]
  temp_location = as.character(Match_Location_DF$Missed[i])
  Dataset = within(Dataset,StateCorrected[Location == temp_location ] <- temp_state) 
}  ### 4546 state are mapped !!!

# Mapping based on City
excel_sheets(path = 'data/Workingdata.xlsx')
Cities_Missed = read_excel(path = 'data/Workingdata.xlsx',sheet = 'State_Missing_CityDetails')

Locations_Assigned$Location = tolower(trimws(Locations_Assigned$Location))
Cities_Missed$City = tolower(trimws(Cities_Missed$City))

DistanceNameMatrix<-matrix(NA, ncol = length(Cities_Missed$City),
                           nrow = length(Locations_Assigned$Location))
for(i in 1:length(Cities_Missed$City)) {
  for(j in 1:length(Locations_Assigned$Location)) { 
    DistanceNameMatrix[j,i]<-stringdist(tolower(Cities_Missed[i,]$City),
                                        tolower(Locations_Assigned[j,]$Location),
                                        method ='jw')      
  }
}

Match_Location_DF<-NULL
MinName<-apply(DistanceNameMatrix, 1, base::min)
for(i in 1:nrow(DistanceNameMatrix)){
  S2<-match(MinName[i],DistanceNameMatrix[i,])
  S1<-i
  Match_Location_DF<-rbind(data.frame(Missed_Id=S2,Assigned_Id=S1,
                                      Missed=Cities_Missed[S2,]$City, 
                                      Assigned=Locations_Assigned[S1,]$Location, 
                                      adist=MinName[i],
                                      method='jm'),
                           Match_Location_DF)
}

Match_Location_DF = Match_Location_DF[Match_Location_DF$adist==0,]   
Match_Location_DF = Match_Location_DF[order(Match_Location_DF$Assigned_Id),]
Match_Location_DF = Match_Location_DF[!duplicated(Match_Location_DF[,3:4]),] 
for(i in 1:nrow(Match_Location_DF)){
  temp_Assigned_Id = Match_Location_DF$Assigned_Id[i]
  temp_state = Locations_Assigned$State[temp_Assigned_Id]
  temp_location = as.character(Match_Location_DF$Missed[i])
  Dataset = within(Dataset,StateCorrected[Location == temp_location ] <- temp_state) 
}  ### 3257 state are mapped !!!

# Corrected State wise Analysis
OrderState = read_excel('StateOrder.xlsx')
Dataset$StateCorrected = tolower(trimws(Dataset$StateCorrected))
Dataset = Dataset %>%
  left_join(OrderState,by='StateCorrected')

GGBarPlot_Facet <- function(dataset,title){
  colnames(dataset) = c('X','Y')
  dataset %>%
    group_by(X)%>%
    ggplot(aes(Y))+
    facet_wrap(~X, scales = "free_x")+
    geom_bar(fill=barColour)+
    ggtitle(paste('Question',title,'Response across states'))+xlab('')+ylab('')
}

for(i in 30:39){
  print(GGBarPlot_Facet(Dataset[,c(40,i)],i-29))
}
Dataset %>%
  ggplot(aes(StateCorrected))+
  geom_bar()+
  ggtitle('State wise records count')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Year wise analysis
GGplotYoYLinePlot <- function(dataset,i){
  colnames(dataset) = c('Y','D')
  dataset %>%
    group_by(Y,D)%>%
    summarise(records = length(D))%>%
    ggplot(aes(Y,records,colour = D))+
    guides(colour=guide_legend(title='Rating Index'))+
    geom_line()+ggtitle(paste('Question',i,'Rating over the years'))+
    xlab('Years')+ylab('No of Records')
}
for(i in 30:39){
  print(GGplotYoYLinePlot(Dataset[,c(4,i)],i-29))
}

qplot(Year,..count..,data = Dataset,
      geom = 'bar',main='Year wise records count')
 
CustomerBasedDataset = Dataset[,c(4,5,8,9,10,12,15,13,11,14,28,29,40)][!apply(Dataset[,14], 1, function(x) any(x=="" | is.na(x))),] 
CustomerBasedDataset = CustomerBasedDataset[order(CustomerBasedDataset$`Customer name`,CustomerBasedDataset$`Month&Year`),]
repeatedCustRecIds = which(duplicated(CustomerBasedDataset$`Customer name`))
repeatedCustRecIds = sort(unique(c(repeatedCustRecIds,repeatedCustRecIds-1)))
CustomerBasedDataset_Repeated = CustomerBasedDataset[repeatedCustRecIds,]
NotifNoBasedDataset = Dataset[,c(4,5,8,9,10,12,13,15,11,14,28,29,40)][!apply(Dataset[,9], 1, function(x) any(x=="" | is.na(x))),] 
NotifNoBasedDataset = NotifNoBasedDataset[order(NotifNoBasedDataset$`Notification no`,NotifNoBasedDataset$`Month&Year`),]
repeatedNotifNoIds = which(duplicated(NotifNoBasedDataset$`Notification no`))
repeatedNotifNoIds = sort(unique(c(repeatedNotifNoIds,repeatedNotifNoIds-1)))
NotifNoBasedDataset_Repeated = NotifNoBasedDataset[repeatedNotifNoIds,]
StateWiseDF_List = split(Dataset,f = Dataset$StateCorrected)


