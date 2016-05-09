run_analysis <- function()
{
        if (!file.exists('./Assignment4')) { dir.create('./Assignment4') }
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl, destfile='./Assignment4/data.zip', method="curl")
        path='/Users/aycaaltay/Coursera/Assignment4'
        setwd(path)
        unzip('data.zip')
        
        setwd(paste0(path,'/UCI HAR Dataset'))
        features=read.table('features.txt')
        activities=read.table('activity_labels.txt')
        
        
        setwd(paste0(path,'/UCI HAR Dataset/train'))
        x_train=read.table('x_train.txt')
        y_train=read.table('y_train.txt')
        subject_train=read.table('subject_train.txt')
        
        setwd(paste0(path,'/UCI HAR Dataset/test'))
        x_test=read.table('x_test.txt')
        y_test=read.table('y_test.txt')
        subject_test=read.table('subject_test.txt')
        
        #Merging here
        x_all=rbind(x_train, x_test)
        y_all=rbind(y_train,y_test)
        subjects=rbind(subject_train,subject_test)
        
        #I have to reach for the feature list to see if I have any mean and std related features
        indicesOfAtt=grep('mean|std',features[,2])
        reducedX=x_all[,indicesOfAtt]
        #The inputs are reduced to mean and standard deviation.
        
        #Replace 1-6 with activity names.
        for (i in 1:nrow(y_all))
        {
                value=y_all[i,1]
                y_all$yNames[i]=as.character(activities[value,2])
        }
        
        #Labeling part
        reducedXLabeled=cbind(y_all,reducedX)
        allData=cbind(subjects,reducedXLabeled)
        
        #Find averages
        datax=allData[,1:2]
        datac=unique(datax)
        averages=matrix(,nrow=nrow(datac), ncol=(ncol(allData)-1))
        for (i in 1:nrow(datac))
        {
                sset=subset(allData,allData[,1]==datac[i,1] & allData[,2]==datac[i,2] )
                sset=sset[,4:ncol(sset)]
                averages[i,1]=datac[i,1]
                averages[i,2]=datac[i,2]
                averages[i,3:ncol(averages)]=colMeans(sset,na.rm=TRUE)
        }
        
        return(averages)
        
        
        
}