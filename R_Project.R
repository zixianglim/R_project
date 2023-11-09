cwd <- '/Users/zixiang/Documents'
setwd(cwd)

# install.packages("gbm", dependencies=TRUE, repos="https://cran.rstudio.com/")
# install.packages("lsr", dependencies=TRUE, repos="https://cran.rstudio.com/")
# install.packages("tigerstats", dependencies=TRUE, repos="https://cran.rstudio.com/")
# install.packages("gridExtra", dependencies=TRUE, repos="https://cran.rstudio.com/")

library(tigerstats)
library(data.table)
library(rpart.plot)
library(gridExtra)
library(ggplot2)
library(e1071)
library(rpart)
library(nnet)
library(VIM)
library(gbm)
library(lsr)
library(car)

dt <- fread('2015.csv')

# ++======================++
# ||   1. Data Cleaning   ||
# ++======================++

    # +---------------------------+
    # | 1.1 Fix structural errors |
    # +---------------------------+

        names(dt) <- gsub('_','',names(dt)) # remove underscores from headers
        
        dt[MENTHLTH==88               , MENTHLTH:=0 ]
        dt[MENTHLTH>=77               , MENTHLTH:=NA]
        dt[HLTHPLN1==7  | HLTHPLN1==9 , HLTHPLN1:=NA]
        dt[MEDCOST ==7  | MEDCOST ==9 , MEDCOST :=NA]
        dt[CHECKUP1==7  | CHECKUP1==9 , CHECKUP1:=NA]
        dt[CHECKUP1==8                , CHECKUP1:=5 ]
        dt[CVDCRHD4==7  | CVDCRHD4==9 , CVDCRHD4:=NA] 
        dt[ADDEPEV2==7  | ADDEPEV2==9 , ADDEPEV2:=NA]
        dt[MARITAL ==9                , MARITAL :=NA]
        dt[EDUCA   ==9                , EDUCA   :=NA]
        dt[EMPLOY1 ==9                , EMPLOY1 :=NA]
        dt[CHILDREN==99               , CHILDREN:=NA]
        dt[CHILDREN==88               , CHILDREN:=0 ]
        dt[INCOME2==77  | INCOME2==99 , INCOME2 :=NA]
        dt[INTERNET==7  | INTERNET==9 , INTERNET:=NA]
        dt[SMOKER3 ==9                , SMOKER3 :=NA]
        dt[WTKG3   ==99999            , WTKG3   :=NA]
        dt[AGEG5YR ==14               , AGEG5YR :=NA]
        dt[SEATBELT>=7                , SEATBELT:=NA]
        dt[SCNTWRK1==98               , SCNTWRK1:=0 ]
        dt[EMPLOY1 >= 3               , SCNTWRK1:=0 ]
        dt[SCNTWRK1==98               , SCNTWRK1:=0 ]
        dt[SCNTWRK1>=97               , SCNTWRK1:=NA]
        #dt[SCNTPAID>=7                , SCNTPAID:=NA]
        dt[AVEDRNK2>=77               , AVEDRNK2:=NA]
        dt[ALCDAY5==888               , AVEDRNK2:=0 ]
      
    # +-------------------------------------------------+
    # | 1.2 Remove Duplicate or Irrelevant Observations |
    # +-------------------------------------------------+
      
        # check for duplicated rows
        sum(duplicated(dt)) > 0 
        
        # manually filter out important columns 
        dt <- subset(dt, select = c('HLTHPLN1',
                                    'MEDCOST',
                                    'CHECKUP1',
                                    'CVDCRHD4', # Y variable
                                    'ADDEPEV2',
                                    'SEX',
                                    'MARITAL',
                                    'EDUCA',
                                    'EMPLOY1',
                                    'CHILDREN',
                                    'INCOME2',
                                    'INTERNET',
                                    'AGEG5YR',
                                    'HTM4',
                                    'WTKG3',
                                    'SMOKER3',
                                    'FRUTDA1',
                                    'BEANDAY',
                                    'GRENDAY',
                                    'PA1VIGM',
                                    'MICHD',    # Y variable
                                    'PA1MIN',
                                    'SEATBELT',
                                    'SCNTWRK1',
                                    'MENTHLTH',
                                    'AVEDRNK2'
        ))
        
        # we want to use CVDCRHD4 & MICHD as our Y variable (incidence of heart disease),
        # but first, check if either variable is a subset of the other
        # if true, drop the subset column, else do an OR operation to form our Y
        table(dt$CVDCRHD4)
        table(dt$MICHD)
        unique((2-as.numeric(dt$CVDCRHD4)) - (2-as.numeric(dt$MICHD)))
        unique((2-as.numeric(dt$MICHD)) - (2-as.numeric(dt$CVDCRHD4)))
        
        # Conclusion: CVDCRHD4 is subset of MICHD, so we drop CVDCRHD4
        dt <- subset(dt, select = -c(CVDCRHD4))
        setnames(dt, 'MICHD', 'Y') # change 'MICHD' column name to 'Y'
        dt <- dt[, Y:=2-Y] # set MICHD==1 and MICHD==2 to become Y=1 and 0 respectively
        
        # fix data types
        dt[, HTM4    := HTM4/100 ]
        dt[, WTKG3    := WTKG3/100 ]
        dt[, PA1MIN   := as.integer(PA1MIN) ]
        dt[, PA1VIGM  := as.integer(PA1VIGM)]
        dt[, AVEDRNK2 := as.integer(AVEDRNK2)]
        dt[, GRENDAY  := as.integer(GRENDAY)/100]
        dt[, BEANDAY  := as.integer(BEANDAY)/100]
        dt[, FRUTDA1  := as.integer(FRUTDA1)/100]
        dt[, SCNTWRK1 := as.integer(SCNTWRK1)]
        dt[, CHILDREN := as.integer(CHILDREN)]
        
        dt[, Y := as.factor(Y)]
        dt[, SEX      := as.factor(SEX)     ]
        dt[, HLTHPLN1 := as.factor(HLTHPLN1)]
        dt[, MEDCOST  := as.factor(MEDCOST) ]
        dt[, ADDEPEV2 := as.factor(ADDEPEV2)]
        dt[, MARITAL  := as.factor(MARITAL) ]
        dt[, EMPLOY1  := as.factor(EMPLOY1) ]
        dt[, INCOME2  := as.factor(INCOME2) ]
        dt[, INTERNET := as.factor(INTERNET)]
        
        dt[, SMOKER3  := factor(SMOKER3,  levels=c(1,2,3,4))]
        dt[, CHECKUP1 := factor(CHECKUP1, levels=c(1,2,3,4,5))]
        dt[, SEATBELT := factor(SEATBELT, levels=c(1,2,3,4,5))]
        dt[, EDUCA    := factor(EDUCA,    levels=c(1,2,3,4,5,6))]
        dt[, AGEG5YR  := factor(AGEG5YR , levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13))]
        
        str(dt)
      
    # +-----------------------------------+
    # | 1.3 Remove Invalid Rows & Columns |
    # +-----------------------------------+
      
        # remove rows with missing Y
        dt <- dt[!is.na(Y),]
        
        # drop rows where there are missing values in columns with >35% missing 
        colSums(is.na(dt))
        names(dt)[colSums(is.na(dt))/nrow(dt) > 0.2]
        dt <- dt[complete.cases(dt[ , c('SCNTWRK1','PA1VIGM',"PA1MIN")]), ]
        
        nrow(dt)
        colSums(is.na(dt))
        
        dt <- dt[which(rowMeans(!is.na(dt)) > 0.8), ] # drop rows with more than 80% missing fields
        
        # Under sampling of data to balance labels in 1:1 ratio 
        #set.seed(190602)
        
        set.seed(190602)
        table(dt$Y)
        dt.yes <- dt[which(dt$Y==1)]
        dt.sample <- rbind(dt[sample(which(dt$Y!=1), nrow(dt.yes)), ], dt.yes)
        table(dt.sample$Y)
      
    # +---------------------+
    # | 1.4 Remove Outliers |
    # +---------------------+
      
        boxplot(dt.sample$PA1MIN, axes = TRUE, staplewex = 1, boxwex = 0.4, main = "PA1MIN",  col = "orange")
        
        boxplot(dt.sample$GRENDAY, axes = TRUE, staplewex = 1, boxwex = 0.4, main = "GRENDAY",  col = "orange")
        
        boxplot(dt.sample$BEANDAY, axes = TRUE, staplewex = 1, boxwex = 0.4, main = "BEANDAY",  col = "orange")
        
        boxplot(dt.sample$FRUTDA1, axes = TRUE, staplewex = 1, boxwex = 0.4, main = "FRUTDA1",  col = "orange")
        
        boxplot(dt.sample$AVEDRNK2, axes = TRUE, staplewex = 1, boxwex = 0.4, main = "AVEDRNK2",  col = "orange")
        
        remove_outlier <- function(col) {
          quantiles <- quantile(col, na.rm=TRUE)
          iqr <- as.numeric(quantiles[4] - quantiles[2])
          upp <- as.numeric(quantiles[4] + 1.5*iqr)
          low <- as.numeric(quantiles[2] - 1.5*iqr)
          sapply(col, function(x) ifelse(!is.na(x) && (x>upp | x<low), FALSE, TRUE))
        }
        
        dt.sample <- dt.sample[remove_outlier(dt.sample$GRENDAY)]
        dt.sample <- dt.sample[remove_outlier(dt.sample$BEANDAY)]
        dt.sample <- dt.sample[remove_outlier(dt.sample$FRUTDA1)]
        dt.sample <- dt.sample[remove_outlier(dt.sample$AVEDRNK2)]
        dt.sample <- dt.sample[remove_outlier(dt.sample$PA1MIN)]
      
    # +------------------------------------+
    # | 1.5 Remove Insignificant Variables |
    # +------------------------------------+
      
        # we can identify the important variables by the ones used in the optimal CART
        # CV error cap
        cart1 <- rpart(Y ~ ., data = dt.sample, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
        plot(cart1$variable.importance, type='b', xlab='', xaxt='n')
        axis(1, at=1:length(names(cart1$variable.importance)), labels=names(cart1$variable.importance), las=2, cex.axis=0.5)
        
        # further filter columns based on the variable importance
        dt.sample <- subset(dt.sample, select = c('WTKG3', 'HTM4', 'AGEG5YR', 'EMPLOY1', 'GRENDAY', 'FRUTDA1', 'MARITAL', 'BEANDAY', 'EDUCA', 'INCOME2', 'PA1MIN', 'PA1VIGM', 'CHECKUP1', 'MENTHLTH', 'SCNTWRK1', 'CHILDREN', 'SMOKER3', 'Y'))
        
        nrow(dt.sample)
        table(dt.sample$Y)
          
    # +---------------------+
    # | 1.6 Data Imputation |
    # +---------------------+
       
        # Data imputation using K-Nearest Neighbors
        dt <- kNN(data=dt.sample, 
                  k=9,
                  trace=TRUE,
                  imp_var=FALSE)
        
        
        # save cleaned data set without index column
        write.csv(dt, 'saved.csv', row.names=FALSE) 

# ++==================================++
# ||   2. Exploratory Data Analysis   ||
# ++==================================++

    dt <- fread('saved.csv')
    
    # fix data types
    dt[, PA1MIN   := as.integer(PA1MIN) ]
    dt[, PA1VIGM  := as.integer(PA1VIGM)]
    dt[, SCNTWRK1 := as.integer(SCNTWRK1)]
    dt[, CHILDREN := as.integer(CHILDREN)]
    
    dt[, Y := as.factor(Y)]
    dt[, MARITAL  := as.factor(MARITAL) ]
    dt[, EMPLOY1  := as.factor(EMPLOY1) ]
    dt[, INCOME2  := as.factor(INCOME2) ]
    dt[, SMOKER3  := factor(SMOKER3,  levels=c(1,2,3,4))]
    dt[, CHECKUP1 := factor(CHECKUP1, levels=c(1,2,3,4,5))]
    dt[, EDUCA    := factor(EDUCA,    levels=c(1,2,3,4,5,6))]
    dt[, AGEG5YR  := factor(AGEG5YR , levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13))]
    
    str(dt)
      
    # +--------------------+
    # | Correlation Matrix |
    # +--------------------+
      
        # Correlation matrix to identify linear correlation between different columns
        dummy.dt <- cbind(dt, model.matrix(~MARITAL+EDUCA+CHECKUP1+SMOKER3+INCOME2+MENTHLTH+AGEG5YR+EMPLOY1+Y-1, dt)) # One-hot encode categorical variables
        dummy.dt <- subset(dummy.dt, select = -c(MARITAL,EDUCA,CHECKUP1,SMOKER3,INCOME2,MENTHLTH,AGEG5YR,EMPLOY1,Y)) # drop original categorical columns
        
        corr <- cor(dummy.dt) # calculate correlation coefficients
        corr[upper.tri(corr)] <- NA # set upper triangle of the matrix to be NA since we do the matrix is symmetric about the diagonal
        
        ggplot(data=melt(corr), aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile() + 
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle=90, size=8), plot.title=element_text(hjust=0.5)) +
        ggtitle('Correlation Matrix')
      
    # +-------------------------------+
    # | Beans Intake vs Heart Disease |
    # +-------------------------------+
      
        ggplot(dt, aes(x=BEANDAY, color=Y, fill=Y)) + 
        geom_histogram(aes(y=..density..), alpha=0.5, position="identity", col='black')+
        geom_density(alpha=.2, show.legend=FALSE, size=1.5) +
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'lightcoral')) +
        scale_colour_manual(values=c('lightseagreen', 'lightcoral')) +
        theme(legend.position=c(.9,.75))
      
    # +------------------------------+
    # | Vege Intake vs Heart Disease |
    # +------------------------------+
      
        ggplot(dt, aes(x=GRENDAY, color=Y, fill=Y)) + 
        geom_histogram(aes(y=..density..), alpha=0.5, position="identity", col='black')+
        geom_density(alpha=.2, show.legend=FALSE) +
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'lightcoral')) +
        scale_colour_manual(values=c('lightseagreen', 'lightcoral')) +
        theme(legend.position=c(.9,.75))
      
    # +-------------------------------+
    # | Fruit Intake vs Heart Disease |
    # +-------------------------------+
      
        ggplot(dt, aes(x=FRUTDA1, color=Y, fill=Y)) + 
        geom_histogram(aes(y=..density..), alpha=0.5, position="identity", col='black')+
        geom_density(alpha=.2, show.legend=FALSE) +
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'lightcoral')) +
        scale_colour_manual(values=c('lightseagreen', 'lightcoral')) +
        theme(legend.position=c(.9,.75))
      
    # +-------------------------+
    # | Income vs Mental Health |
    # +-------------------------+ 
      
        dt.group1 <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/7), INCOME2)]
        dt.group1 <- dt.group1[,percent:=N/sum(N), by=.(MENTHLTH)] 
        ggplot(data=dt.group1, aes(y=percent, x=factor(MENTHLTH), fill=INCOME2)) +
        geom_bar(position='dodge', stat='identity') +
        scale_fill_manual(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000'), 
                          values=c('antiquewhite2', 'antiquewhite3', 'antiquewhite4', 'darkcyan', 'cyan3', 'darkslategray3', 'darkslategray4', 'darkslategray')) +
        theme(plot.title=element_text(hjust=0.5)) +
        scale_x_discrete(labels = c('0-6 days', '7-13 days', '14-20 days', '21-27 days', '>=28 days')) +
        labs(x="Poor Mental Health Days")+
        ggtitle('Proportion of Income With no. of days mental health not good')

    # +-----------------------------+
    # | Checkup Frequency vs Income |
    # +-----------------------------+
      
        dt.group <- dt[, .N, by=.(CHECKUP1, INCOME2)]
        dt.group <- dt.group[, percent:=N/sum(N), by=.(CHECKUP1)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=CHECKUP1, fill=INCOME2)) +
        geom_bar(position='dodge', stat='identity') +
        scale_fill_manual(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000'), 
                          values=c('antiquewhite2', 'antiquewhite3', 'antiquewhite4', 'darkcyan', 'cyan3', 'darkslategray3', 'darkslategray4', 'darkslategray')) +
        scale_x_discrete(labels =c('< 1 yr', '< 2 yrs', '< 5 yrs', '>=5 yrs', 'Never')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ggtitle('Proportion of Income Within Each CheckUp Frequency')

    # +------------------------------+
    # | Education vs Mental Health   |
    # +------------------------------+
      
        dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/7), EDUCA)] # binning of MENTHLTH into 5 groups
        dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=factor(MENTHLTH), fill=EDUCA)) +
        geom_bar(stat='identity') +
        theme(plot.title=element_text(hjust=0.5)) +
        scale_fill_manual(labels = c('Never / Only kindergarten', 'Grades 1-8', 'Grades 9-11', 'Grade 12 or GED', 'College 1-3yrs', 'College >= 4yrs'), 
                          values=c("azure2", "cadetblue1", "cyan3", "cyan4", "darkcyan", 'darkslategray')) +
        scale_x_discrete(labels = c('0-6 days', '7-13 days', '14-20 days', '21-27 days', '>=28 days')) +
        labs(x="Poor Mental Health Days")+
        ggtitle('Education by Number of Days in Past 30 Days Where Mental Health Was Not Good')

    # +----------------------+
    # | Heart Disease by Age |
    # +----------------------+
      
        dt.group <- dt[, .N, by=.(AGEG5YR, Y)]
        dt.group[, percent:=N/sum(N), by=.(AGEG5YR)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=AGEG5YR, fill=Y)) +
        scale_y_continuous(labels = scales::percent)+
        geom_bar(stat = 'identity') +
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ggtitle('Proportion of Heart Disease by Age Group')
      
    # +------------------------------------+
    # | Physical Activity vs Heart Disease |
    # +------------------------------------+
      
        bins <- 60
        hist.Y <- hist(log(1+dt[Y==1]$PA1MIN, base=exp(1)), plot=FALSE, breaks=bins)
        hist.Y$density <- hist.Y$counts / sum(hist.Y$counts) # calculate density for each bin
        plot(hist.Y, freq=FALSE, col=rgb(255,0,0,max=255,alpha=50), xlab='log(PA1MIN)', ylim=c(0, 0.06), main='Distribution of Physical Activity by Heart Disease')
        
        hist.N <- hist(log(1+dt[Y==0]$PA1MIN, base=exp(1)), plot=FALSE, breaks=bins)
        hist.N$density <- hist.N$counts / sum(hist.N$counts) # calculate density for each bin
        plot(hist.N, freq=FALSE, col=rgb(0,255,0,max=255,alpha=50), add=TRUE)
        legend(x='topleft', legend=c('Heart Disease', 'No Heart Disease'), col=c('red', 'green'), lwd=10)
        
        # Young + low vegetable intake + low physical activity = higher chance of heart disease
        # compound effect? 
          
    # +-------------------------+
    # | PA1VIGM by Heart Disease |
    # +-------------------------+    
          
        ggplot(dt, aes(x=Y, y=PA1VIGM, fill=Y))+
        geom_violin(trim=FALSE)+
        geom_boxplot(width = 0.6, alpha = 0.5, fill = "lightgrey")+
        labs(y="PA1VIGM")+
        scale_x_discrete(labels=c("0" = "No Heart Disease", "1" = "Heart Disease"))+
        scale_fill_brewer(palette="Paired")+
        theme(axis.title.x = element_text(hjust=0.5,color="Dark Red", size=12, face="bold"),
              axis.title.y = element_text(hjust=0.5,color="black", size=12, face="bold"))

    # +-------------------------+
    # | Weight by Heart Disease |
    # +-------------------------+
      
        bins <- 50
        hist.Y <- hist(dt[Y==1]$WTKG3, plot=FALSE, breaks=bins)
        hist.Y$density <- hist.Y$counts / sum(hist.Y$counts) # calculate density for each bin
        plot(hist.Y, freq=FALSE, col=rgb(255,0,0,max=255,alpha=50), xlab='Weight', ylim=c(0, 0.12), main='Distribution of Weight by Heart Disease')
        
        hist.N <- hist(dt[Y!=1]$WTKG3, plot=FALSE, breaks=bins)
        hist.N$density <- hist.N$counts / sum(hist.N$counts) # calculate density for each bin
        plot(hist.N, freq=FALSE, col=rgb(0,255,0,max=255,alpha=50), add=TRUE)
        legend(x='topright', legend=c('Heart Disease', 'No Heart Disease'), col=c('red', 'green'), lwd=10)

    # +--------------------------+
    # | Smoking vs Heart Disease |
    # +--------------------------+
      
        dt.group <- dt[, .N, by=.(SMOKER3, Y)]
        dt.group[, percent:=N/sum(N), by=.(SMOKER3)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=SMOKER3, fill=Y)) +
        scale_y_continuous(labels = scales::percent)+
        geom_bar(stat = 'identity') +
        scale_fill_manual(labels=c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
        scale_x_discrete(labels=c('Smokes Everyday', 'Smokes Some Days', 'Former Smoker', 'Never Smoked')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ggtitle('Heart Disease by Smoking Habits')
      
    # +----------------------------+
    # | Education vs Heart Disease |
    # +----------------------------+
      
        dt.group <- dt[, .N, by=.(EDUCA, Y)]
        dt.group <- dt.group[, percent:=N/sum(N), by=.(EDUCA)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=EDUCA, fill=Y)) +
        geom_bar(stat = 'identity') +
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual(labels=c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
        scale_x_discrete(labels=c('Never/Kindergarten', 'Grades 1-8', 'Grades 9-11', 'Grade 12 or GED', 'College 1-3yrs', 'College >= 4yrs')) +
        theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=90, hjust=0)) +
        ggtitle('Heart Disease by Education')
      
    # +---------------------------+
    # | Marital vs Heart Disease |
    # +---------------------------+
      
        dt.group <- dt[, .N, by=.(MARITAL, Y)]
        dt.group <- dt.group[, percent:=N/sum(N), by=.(MARITAL)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=MARITAL, fill=Y)) +
        geom_bar(stat = 'identity') +
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
        scale_x_discrete(labels =c('Married', 'Divorced', 'Widowed', 'Separated', 'Never Married', 'Unmarried Couple')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ggtitle('Heart Disease by Marital Status')
      
    # +---------------------------+
    # | Children vs Heart Disease |
    # +---------------------------+
      
        dt.group <- dt[, .N, by=.(CHILDREN=ifelse(CHILDREN<7, CHILDREN, '7+'), Y)]
        dt.group <- dt.group[, percent:=N/sum(N), by=.(CHILDREN)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=CHILDREN, fill=Y)) +
        geom_bar(stat = 'identity') +
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
        #scale_x_discrete(labels =c('Married', 'Divorced', 'Widowed', 'Separated', 'Never Married', 'Unmarried Couple')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ggtitle('Heart Disease by Number of Children')
      
    # +----------------------------------+
    # | Age vs Children vs Heart Disease |
    # +----------------------------------+
      
        dt.group <- dt[,.N, by=.(CHILDREN=ifelse(CHILDREN<5, CHILDREN, '5+'), AGEG5YR, Y)] 
        dt.group <- dt.group[,percent:=N/sum(N), by=.(CHILDREN)][order(AGEG5YR)] # convert abs count to %
        ggplot(data=dt.group[Y==1], aes(y=percent, x=CHILDREN, fill=AGEG5YR)) +
        geom_bar(stat='identity') +
        theme(plot.title=element_text(hjust=0.5)) +
        scale_fill_manual(labels = c('18-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54','55-59', '60-64', '65-69', '70-74', '75-79', '>=80'), 
                          values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond', 'azure', 'cadetblue1', 'deepskyblue', 'deepskyblue3', 'dodgerblue4', 'blue4', 'black')) +
        scale_x_discrete(labels=c('0', '1', '2', '3', '4', '5+')) +
        xlab(label='CHILDREN') +
        ggtitle('Heart Disease by Age and Children')
      
    # +--------------------+
    # | Children vs Income |
    # +--------------------+
      
        dt.group <- dt[, .N, by=.(CHILDREN=ifelse(CHILDREN<7, CHILDREN, '7+'), INCOME2)]
        dt.group <- dt.group[,percent:=N/sum(N), by=.(CHILDREN)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=CHILDREN, fill=INCOME2)) +
        geom_bar(stat='identity') +
        scale_fill_manual(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000'),
                          values=c('antiquewhite2', 'antiquewhite3', 'antiquewhite4', 'darkcyan', 'cyan3', 'darkslategray3', 'darkslategray4', 'darkslategray')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ggtitle('Income by Number of Children')
        
    # +-------------------------+
    # | Heart Disease vs Income |
    # +-------------------------+  
        
        dt.group <- dt[, .N, by=.(INCOME2, Y)]
        dt.group <- dt.group[, percent:=N/sum(N), by=.(INCOME2)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=INCOME2, fill=Y)) +
        geom_bar(stat = 'identity') +
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
        scale_x_discrete(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ggtitle('Heart Disease by Income')
      
    # +--------------------------------+
    # | Mental Health vs Heart Disease |
    # +--------------------------------+
      
        dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/3), Y)] # binning of MENTHLTH into 5 groups
        dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
        ggplot(data=dt.group[complete.cases(dt.group)], aes(y=percent, x=factor(MENTHLTH), fill=Y)) +
        geom_bar(stat='identity') +
        theme(plot.title=element_text(hjust=0.5)) +
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
        scale_x_discrete(labels = c('0-2 days', '3-5 days', '6-8 days', '9-11 days', '12-14 days', '15-17 days', '18-20 days', '21-23 days', '24-26 days', '27-29 days', '30 days')) +
        ggtitle('Heart Disease by Number of Days in Past 30 Days Where Mental Health Was Not Good') +
        labs(x='Poor Mental Health Days')
          
    # +--------------------------------+
    # |     Employ vs Heart Disease    |
    # +--------------------------------+
          
        dt.group <- dt[, .N, by=.(EMPLOY1, Y)]
        dt.group <- dt.group[, percent:=N/sum(N), by=.(EMPLOY1)] # convert abs count to %
        ggplot(data=dt.group, aes(y=percent, x=EMPLOY1, fill=Y)) +
        geom_bar(stat = 'identity') +
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
        scale_x_discrete(labels=c('Worker', 'Self-Employed', 'Jobless for >1y', 'Jobless for <1y', 'housewife', 'student', 'retired', 'unable2work')) +
        theme(plot.title=element_text(hjust=0.5)) +
        ggtitle('Heart Disease by Employment Status')
      
    # +------------------------------------------------+
    # | Hours of Work Per Week vs Age vs Heart Disease |
    # +------------------------------------------------+
      
        dt.group <- dt[,.N, by=.(SCNTWRK1=as.integer(SCNTWRK1/9), AGEG5YR, Y)] # binning of SCNTWRK1 into 11 groups
        dt.group <- dt.group[,percent:=N/sum(N), by=.(SCNTWRK1)][order(AGEG5YR)] # convert abs count to %
        ggplot(data=dt.group[Y==1], aes(y=percent, x=factor(SCNTWRK1), fill=AGEG5YR)) +
        geom_bar(stat='identity') +
        theme(plot.title=element_text(hjust=0.5)) +
        scale_fill_manual(labels = c('18-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54','55-59', '60-64', '65-69', '70-74', '75-79', '>=80'), 
                          values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond', 'azure', 'cadetblue1', 'deepskyblue', 'deepskyblue3', 'dodgerblue4', 'blue4', 'black')) +
        scale_x_discrete(labels=c('0-8', '9-17', '18-26', '27-35', '36-44', '45-53', '54-62', '63-71', '72-80', '81-89')) +
        xlab(label='Hours of Work Per Week') +
        ggtitle('Heart Disease by No. Hrs of Work Per Week')
          
    # +------------------------------------------------+
    # |          EMPLOY vs Age vs Heart Disease        |
    # +------------------------------------------------+
          
        dt.group <- dt[,.N, by=.(EMPLOY1, AGEG5YR, Y)] # binning of SCNTWRK1 into 11 groups
        dt.group <- dt.group[,percent:=N/sum(N), by=.(EMPLOY1)][order(AGEG5YR)] # convert abs count to %
        ggplot(data=dt.group[Y==1], aes(y=percent, x=factor(EMPLOY1), fill=AGEG5YR)) +
        geom_bar(stat='identity') +
        theme(plot.title=element_text(hjust=0.5)) +
        scale_fill_manual(labels = c('18-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54','55-59', '60-64', '65-69', '70-74', '75-79', '>=80'), 
                          values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond', 'azure', 'cadetblue1', 'deepskyblue', 'deepskyblue3', 'dodgerblue4', 'blue4', 'black')) +
        scale_x_discrete(labels=c('Worker', 'Self-Employed', 'Jobless for >1y', 'Jobless for <1y', 'housewife', 'student', 'retired', 'unable2work')) +
        xlab(label='Employment Status') +
        ggtitle('Have Heart Disease by Type of employement')    
      
    # +----------------------+
    # | BMI by Heart Disease |
    # +----------------------+
      
        temp.dt <- dt
        temp.dt$BMI=temp.dt$WTKG3/(temp.dt$HTM4)^2
        
        bins <- 90
        hist.Y <- hist(temp.dt[Y==1]$BMI, plot=FALSE, breaks=bins)
        hist.Y$density <- hist.Y$counts / sum(hist.Y$counts) # calculate density for each bin
        plot(hist.Y, freq=FALSE, col=rgb(255,0,0,max=255,alpha=50), xlab='BMI', ylim=c(0, 0.09), main='Distribution of BMI by Heart Disease')
        
        hist.N <- hist(temp.dt[Y!=1]$BMI, plot=FALSE, breaks=bins)
        hist.N$density <- hist.N$counts / sum(hist.N$counts) # calculate density for each bin
        plot(hist.N, freq=FALSE, col=rgb(0,255,0,max=255,alpha=50), add=TRUE)
        legend(x='topright', legend=c('Heart Disease', 'No Heart Disease'), col=c('red', 'green'), lwd=8,cex=0.4)

    # +-----------+
    # | Nutrition |
    # +-----------+
      
        temp.dt$NUTRITION=temp.dt$GRENDAY+temp.dt$FRUTDA1+temp.dt$BEANDAY
        
        bins <- 30
        hist.Y <- hist(temp.dt[Y==1]$NUTRITION, plot=FALSE, breaks=bins)
        hist.Y$density <- hist.Y$counts / sum(hist.Y$counts) # calculate density for each bin
        plot(hist.Y, freq=FALSE, col=rgb(255,0,0,max=255,alpha=50), xlab='NUTRITION', ylim=c(0, 0.08), main='Distribution of NUTRITION by Heart Disease')
        
        hist.N <- hist(temp.dt[Y!=1]$NUTRITION, plot=FALSE, breaks=bins)
        hist.N$density <- hist.N$counts / sum(hist.N$counts) # calculate density for each bin
        plot(hist.N, freq=FALSE, col=rgb(0,255,0,max=255,alpha=50), add=TRUE)
        legend(x='topright', legend=c('Heart Disease', 'No Heart Disease'), col=c('red', 'green'), lwd=8,cex=0.4)

# ++==================++
# ||   3. Modelling   ||
# ++==================++
        
    # Model Evaluation
    # to store accuracy, recall, precision, f1 score
    seed <- 190602
    results <- rep(0, 20)
    
    # train test split
    set.seed(seed)
    train_indexes <- sample(seq_len(nrow(dt)), nrow(dt)*0.7) 
    train <- dt[train_indexes,]
    test <- dt[-train_indexes,]
        
    # +------+
    # | CART |
    # +------+
        
        set.seed(seed)
        
        # CV error cap
        cart1 <- rpart(Y ~ ., data = train, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
        CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,'xerror']), 'xerror'] + cart1$cptable[which.min(cart1$cptable[,'xerror']), 'xstd']
        
        # Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
        i <- 1; j<- 4
        while (cart1$cptable[i,j] > CVerror.cap) {
          i <- i + 1
        }
        
        # Geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
        cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
        
        # Prune the max tree using optimal CP value
        cart2 <- prune(cart1, cp = cp.opt)
        fit <- rpart.plot(cart2, type=1)
        #text(fit, cex=1)
        fit
        
        print(cart2)
        
        predictions = predict(cart2, newdata=test)
        
        # finding optimal threshold for threshold moving
        n <- 60
        recalls = rep(0, n)
        accuracies = rep(0, n)
        threshold <- 0.1
        
        for (i in 1:n) {
          threshold <- threshold + 0.01
          y.hat.cart <- ifelse(predictions[,2]<threshold, 0, 1)
          confusion_matrix <- table(test$Y, y.hat.cart, deparse.level=2) # confusion matrix: test data
          accuracies[i] <- sum(test$Y==y.hat.cart) / nrow(test)
          recalls[i] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        }
        
        total <- recalls+accuracies
        plot(accuracies, ylim=c(0, 1.7), col='blue')
        lines(accuracies)
        points(recalls, col='red')
        lines(recalls)
        points(total, col='black')
        lines(total)
        legend(x='topright', legend=c('Recall', 'Accuracy', 'Recall+Accuracy'), col=c('red', 'blue', 'black'), lwd=10)
        
        idx <- which.max(total)
        lines(c(21,21), c(0, 1.7))
        
        threshold <- 0.31
        y.hat.cart <- ifelse(predictions[,2]<threshold, 0, 1)
        
        confusion_matrix <- table(test$Y, y.hat.cart, deparse.level=2) # confusion matrix: test data
        confusion_matrix
        
        results[1] <- sum(test$Y==y.hat.cart) / nrow(test)
        results[6] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        results[11] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3]) # precision = TP/(TP+FP) 
        results[16] <- 2 * results[11] * results[6] / (results[11] + results[6]) # f1 score = 2 * precision * recall / (precision + recall)
        
    # ----------------------------------------------------------------------------------
        
        # for the following models, we use feature scaling before training to boost accuracy
        min_max_scaler <- function(x) {
          mn = min(x)
          mx = max(x)
          (x - mn) / (mx-mn) 
        }
        
        dt.scaled <- dt
        dt.scaled$WTKG3 <- min_max_scaler(dt.scaled$WTKG3)
        dt.scaled$HTM4 <- min_max_scaler(dt.scaled$HTM4)
        dt.scaled$CHILDREN <- min_max_scaler(dt.scaled$CHILDREN)
        dt.scaled$GRENDAY <- min_max_scaler(dt.scaled$GRENDAY)
        dt.scaled$FRUTDA1 <- min_max_scaler(dt.scaled$FRUTDA1)
        dt.scaled$SCNTWRK1 <- min_max_scaler(dt.scaled$SCNTWRK1)
        dt.scaled$BEANDAY <- min_max_scaler(dt.scaled$BEANDAY)
        dt.scaled$PA1MIN <- min_max_scaler(dt.scaled$PA1MIN)
        dt.scaled$PA1VIGM <- min_max_scaler(dt.scaled$PA1VIGM)
        dt.scaled$MENTHLTH <- min_max_scaler(dt.scaled$MENTHLTH)
        
        train.scaled <- dt.scaled[train_indexes]
        test.scaled <- dt.scaled[-train_indexes]
        
        
    # +---------------------------+
    # | Gradient Boosting Machine |
    # +---------------------------+
        
        set.seed(seed)
        dt.gbm  <- dt.scaled
        dt.gbm$Y = as.numeric(dt.gbm$Y)-1
        
        # need to convert to numeric if wan to run bernoulli
        m.gbm=gbm(Y ~ . ,distribution ="bernoulli", data=dt.gbm[train_indexes], cv.folds=5)
        m.gbm
        
        summary.gbm <- summary(m.gbm)  
        ggplot(data=summary.gbm, aes(y=reorder(var, rel.inf), x=rel.inf)) + 
          geom_bar(stat='identity') +
          labs(y='Variable', x='Relative Variable Importance')
        
        
        plot(m.gbm,i="AGEG5YR") 
        
        predictions = predict(m.gbm, newdata=dt.gbm[-train_indexes], type="response")
        
        # finding optimal threshold for threshold moving
        n <- 60
        recalls = rep(0, n)
        accuracies = rep(0, n)
        threshold <- 0.1
        
        for (i in 1:n) {
          threshold <- threshold + 0.01
          y.hat.gbm <- ifelse(predictions<threshold, 0, 1)
          confusion_matrix <- table(dt.gbm[-train_indexes]$Y, y.hat.gbm, deparse.level=2) # confusion matrix: test data
          accuracies[i] <- sum(dt.gbm[-train_indexes]$Y==y.hat.gbm) / nrow(test)
          recalls[i] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        }
        
        total <- recalls+accuracies
        plot(accuracies, ylim=c(0, 1.7), col='blue')
        lines(accuracies)
        points(recalls, col='red')
        lines(recalls)
        points(total, col='black')
        lines(total)
        legend(x='topright', legend=c('Recall', 'Accuracy', 'Recall+Accuracy'), col=c('red', 'blue', 'black'), lwd=10)
        
        lines(c(25,25), c(0, 1.7))
        
        threshold <- 0.35
        y.hat.gbm= ifelse(predictions<threshold, 0, 1)
        
        confusion_matrix <- table(dt.gbm[-train_indexes]$Y, y.hat.gbm, deparse.level=2) # confusion matrix: test data
        confusion_matrix
        
        results[2] <- sum(dt.gbm[-train_indexes]$Y==y.hat.gbm) / nrow(test)
        results[7] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        results[12] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3]) # precision = TP/(TP+FP) 
        results[17] <- 2 * results[12] * results[7] / (results[12] + results[7]) # f1 score = 2 * precision * recall / (precision + recall)
        
    # +---------------------+
    # | Logistic Regression |
    # +---------------------+
        
        set.seed(seed)
        logreg <- step(glm(Y~.,data=train.scaled, family='binomial'), direction="backward")
        
        summary(logreg)
        
        predictions <- predict(logreg, newdata=test.scaled, type='response')
        
        # finding optimal threshold for threshold moving
        n <- 60
        recalls = rep(0, n)
        accuracies = rep(0, n)
        threshold <- 0.1
        
        for (i in 1:n) {
          threshold <- threshold + 0.01
          y.hat.lr <- ifelse(predictions<threshold, 0, 1)
          confusion_matrix <- table(test.scaled$Y, y.hat.lr, deparse.level=2) # confusion matrix: test data
          accuracies[i] <- sum(test.scaled$Y==y.hat.lr) / nrow(test.scaled)
          recalls[i] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        }
        
        total <- recalls+accuracies
        plot(accuracies, ylim=c(0, 1.7), col='blue')
        lines(accuracies)
        points(recalls, col='red')
        lines(recalls)
        points(total, col='black')
        lines(total)
        legend(x='topright', legend=c('Recall', 'Accuracy', 'Recall+Accuracy'), col=c('red', 'blue', 'black'), lwd=10)
        
        idx <- which.max(total)
        lines(c(idx, idx), c(0, 1.7))
        
        threshold <- 0.1 + idx*0.01
        y.hat.lr <- ifelse(predictions<threshold, 0, 1)
        
        confusion_matrix <- table(test.scaled$Y, y.hat.lr, deparse.level=2) # confusion matrix: test data
        confusion_matrix
        
        results[3] <- sum(test.scaled$Y==y.hat.lr) / nrow(test.scaled)
        results[8] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        results[13] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3]) # precision = TP/(TP+FP) 
        results[18] <- 2 * results[13] * results[8] / (results[13] + results[8]) # f1 score = 2 * precision * recall / (precision + recall)
        
    # +----------------+
    # | Neural Network |
    # +----------------+
        
        # grid search for hyper parameter tuning
        # initialise empty dataframe to store fine tuning results
        gridsearch.df <- data.frame(size=integer(), 
                                    decay=double(),
                                    accuracy=double(),
                                    recall=double(),
                                    total=double())
        
        # grid search values
        gridsearch.sizes <- c(5,7,9,11,13,15,17)
        gridsearch.decay <- c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5)
        accuracies <- rep(0, length(gridsearch.sizes) * length(gridsearch.decay))
        recalls <- rep(0, length(accuracies))
        threshold <- 0.5
        idx <- 1
        
        # max iterations of 200 to be used consistently throughout grid search & actual modelling
        maxit <- 200
        
        # grid search every combination of size & decay
        for (size in gridsearch.sizes) {
          for (decay in gridsearch.decay) {
            set.seed(seed)
            nn <- nnet(Y~., data=dt.scaled, subset=train_indexes, size=size, decay=decay, maxit=maxit)
            predictions <- predict(nn, test.scaled)
            y.hat.nn <- ifelse(predictions<threshold, 0, 1)
            confusion_matrix <- table(test.scaled$Y, y.hat.nn, deparse.level=2) # confusion matrix: test data
            accuracy <- sum(test.scaled$Y==y.hat.nn) / nrow(test.scaled)
            recall <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
            gridsearch.df[idx, ] = c(size, decay, accuracy, recall, accuracy+recall)
            idx <- idx+1
          }  
        }
        
        # visualize grid search results
        wireframe(recall~size+decay, data=gridsearch.df, drape=TRUE, scales=list(arrows=FALSE))
        wireframe(accuracy~size+decay, data=gridsearch.df, drape=TRUE, scales=list(arrows=FALSE))
        wireframe(total~size+decay, data=gridsearch.df, drape=TRUE, scales=list(arrows=FALSE))
        opt.param <- gridsearch.df[which.max(gridsearch.df$total),]
        
        # actual model based on optimimal hyper parameters
        set.seed(seed)
        nn <- nnet(Y~., data=dt.scaled, subset=train_indexes, size=opt.param$size, decay=opt.param$decay, maxit=maxit) # to skip grid search, use size=7, decay=0.01
        predictions <- predict(nn, test.scaled)
        
        # finding optimal threshold for threshold moving
        n <- 60
        recalls = rep(0, n)
        accuracies = rep(0, n)
        threshold <- 0.1
        
        for (i in 1:n) {
          threshold <- threshold + 0.01
          y.hat.nn <- ifelse(predictions<threshold, 0, 1)
          confusion_matrix <- table(test.scaled$Y, y.hat.nn, deparse.level=2) # confusion matrix: test data
          accuracies[i] <- sum(test.scaled$Y==y.hat.nn) / nrow(test.scaled)
          recalls[i] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        }
        
        total <- recalls+accuracies
        plot(accuracies, ylim=c(0, 1.7), col='blue')
        lines(accuracies)
        points(recalls, col='red')
        lines(recalls)
        points(total, col='black')
        lines(total)
        legend(x='topright', legend=c('Recall', 'Accuracy', 'Recall+Accuracy'), col=c('red', 'blue', 'black'), lwd=10)
        
        lines(c(18, 18), c(0, 1.7))
        
        threshold <- 0.28
        y.hat.lr <- ifelse(predictions<threshold, 0, 1)
        y.hat.nn <- ifelse(predictions<threshold, 0, 1)
        
        confusion_matrix <- table(test.scaled$Y, y.hat.nn, deparse.level=2) # confusion matrix: test data
        confusion_matrix
        
        results[4] <- sum(test.scaled$Y==y.hat.nn) / nrow(test.scaled)
        results[9] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        results[14] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3]) # precision = TP/(TP+FP) 
        results[19] <- 2 * results[14] * results[9] / (results[14] + results[9]) # f1 score = 2 * precision * recall / (precision + recall)
        
    # +------------------------+
    # | Support Vector Machine |
    # +------------------------+
        
        # based on earlier models & EDA, we see that AGEG5YR & WTKG3 are 2 of the more important variables
        ggplot(data=dt, aes(x=as.numeric(AGEG5YR), y=WTKG3)) +
          geom_jitter(aes(color=Y)) +
          scale_color_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
          scale_x_discrete(labels = c('18-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54','55-59', '60-64', '65-69', '70-74', '75-79', '>=80')) +
          labs(x='AGEG5YR')
        
        # we will therefore use sigmoid kernel for transformation of inputs to increase separability of classes
        set.seed(seed)
        classifier = svm(formula = Y ~ .,
                         data = train.scaled,
                         type = 'C-classification',
                         kernel = 'sigmoid')
        
        summary(classifier)
        
        predictions = predict(classifier, newdata = test.scaled)
        confusion_matrix = table(test.scaled[, Y], predictions)
        
        results[5] <- sum(test.scaled$Y==predictions) / nrow(test.scaled)
        results[10] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
        results[15] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3]) # precision = TP/(TP+FP) 
        results[20] <- 2 * results[15] * results[10] / (results[15] + results[10]) # f1 score = 2 * precision * recall / (precision + recall)
        
        tab <- as.table(matrix(results, ncol=5, byrow=TRUE))
        colnames(tab) <- c('CART', 'GBM', 'Logistic Regression', 'Neural Network', 'SVM')
        rownames(tab) <- c('Validation Accuracy', 'Recall', 'Precision', 'F1 Score')
        tab

# +-------------------------+
# | Post Modelling Analysis |
# +-------------------------+

    #log sig variables
    
    summary(logreg)
    
    vif(logreg) #no multicollinearity between variables
    
    #WTKG3
    #HTM4
    #AGE
    #EMPLOY1
    #GRENDAY
    #MARITAL
    #INCOME
    #PA1VIGM
    #CHECKUP
    #MENTHLTH
    #SMOKER
    #decided to dive into the non-common factors that is not in research
    #marital, income

    # +-------------------------+
    # |        MARITAL          |
    # +-------------------------+
          
        #CART to find out what is related to it
        
        cart_m <- rpart(MARITAL ~ ., data = dt, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
        CVerror.cap <- cart_m$cptable[which.min(cart_m$cptable[,'xerror']), 'xerror'] + cart_m$cptable[which.min(cart_m$cptable[,'xerror']), 'xstd']
        # Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
        i <- 1; j<- 4
        while (cart_m$cptable[i,j] > CVerror.cap) {
          i <- i + 1
        }
        # Geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
        cp.opt = ifelse(i > 1, sqrt(cart_m$cptable[i,1] * cart_m$cptable[i-1,1]), 1)
        # Prune the max tree using optimal CP value
        cart_m <- prune(cart_m, cp = cp.opt)
        rpart.plot(cart_m, nn= T, main = "Maximal Tree for Marital")
        print(cart_m)
        #age!
          
            #Married Age
            dt.group <- dt[, .N, by=.(MARITAL, AGEG5YR)]
            dt.group <- dt.group[, percent:=N/sum(N), by=.(MARITAL)] # convert abs count to %
            ggplot(data=dt.group, aes(y=percent, x=MARITAL, fill=AGEG5YR)) +
            geom_bar(stat = 'identity') +
            scale_y_continuous(labels = scales::percent)+
            scale_fill_manual(labels = c('18-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54','55-59', '60-64', '65-69', '70-74', '75-79', '>=80'), 
                               values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond', 'azure', 'cadetblue1', 'deepskyblue', 'deepskyblue3', 'dodgerblue4', 'blue4', 'black')) +
            scale_x_discrete(labels=c('Married', 'Divorce','Widowed','Seperated', 'Never Married', 'unmarried couple')) +
            theme(plot.title=element_text(hjust=0.5)) +
            ggtitle('Age by Marital')
            
            #Married by age by heart disease
            dt.group <- dt[,.N, by=.(MARITAL, AGEG5YR, Y)] # binning of SCNTWRK1 into 11 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MARITAL)][order(AGEG5YR)] # convert abs count to %
            ggplot(data=dt.group[Y==1], aes(y=percent, x=factor(MARITAL), fill=AGEG5YR)) +
            geom_bar(stat='identity') +
            theme(plot.title=element_text(hjust=0.5)) +
            scale_fill_manual(labels = c('18-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54','55-59', '60-64', '65-69', '70-74', '75-79', '>=80'), 
                              values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond', 'azure', 'cadetblue1', 'deepskyblue', 'deepskyblue3', 'dodgerblue4', 'blue4', 'black')) +
            scale_x_discrete(labels=c('Married', 'Divorce', 'Widowed','Seperated', 'Never Married', 'unmarried couple')) +
            xlab(label='Marital Status') +
            ggtitle('Have Heart Disease by Marital Status')   
            
            dt.group <- dt[,.N, by=.(MARITAL, AGEG5YR, Y)] # binning of SCNTWRK1 into 11 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MARITAL)][order(AGEG5YR)] # convert abs count to %
            ggplot(data=dt.group[Y==0], aes(y=percent, x=factor(MARITAL), fill=AGEG5YR)) +
            geom_bar(stat='identity') +
            theme(plot.title=element_text(hjust=0.5)) +
            scale_fill_manual(labels = c('18-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54','55-59', '60-64', '65-69', '70-74', '75-79', '>=80'), 
                              values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond', 'azure', 'cadetblue1', 'deepskyblue', 'deepskyblue3', 'dodgerblue4', 'blue4', 'black')) +
            scale_x_discrete(labels=c('Married', 'Divorce', 'Widowed','Seperated', 'Never Married', 'unmarried couple')) +
            xlab(label='Marital Status') +
            ggtitle('No Heart Disease by Marital Status')  
            
            #compare all 3 graphs tgt realise that its still large related to age
            #check whether multicollinearity
            vif(logreg)
            #donthave
            
        #hypothesis testing
            
            #associationTest
            associationTest(formula=~Y+MARITAL, data=dt)
            #pvalue<0.001 = significant
          
        #check another variable from cart
            #marital ~ height
            
            ggplot(dt, aes(x=MARITAL, y=HTM4, fill=MARITAL))+
            geom_violin(trim=FALSE)+
            geom_boxplot(width = 0.6, fill = "lightgrey")+
            labs(y="Weight")+
            scale_x_discrete(labels=c("1" = "Married", "2" = "Divorced","3" = "Widowed","4"="Separated","5"="Never Married",
                                      "6" = "Attached"))+
            scale_fill_brewer(palette="Dark2")+
            theme(axis.title.x = element_text(hjust=0.5,color="Dark Red", size=12, face="bold"),
                  axis.title.y = element_text(hjust=0.5,color="black", size=12, face="bold"))
            #all q equal so no much diff dont need explore further
          
        #conclude that sig factor of marital is age which could be the indirect factor
        #that affects one getting heart disease
        #support by qualitative also, those heartbreak maybe
          
        #https://www.heart.org/en/health-topics/cardiomyopathy/what-is-cardiomyopathy-in-adults/is-broken-heart-syndrome-real
          
        #affect mental health
        #check relationship between marital and mental health
          
            dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/7), MARITAL)] # binning of MENTHLTH into 5 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
            ggplot(data=dt.group, aes(y=percent, x=factor(MENTHLTH), fill=MARITAL)) +
            geom_bar(stat='identity') +
            theme(plot.title=element_text(hjust=0.5)) +
            scale_fill_manual(labels=c("1" = "Married", "2" = "Divorced","3" = "Widowed","4"="Separated","5"="Never Married",
                                       "6" = "Attached"), 
                              values=c('antiquewhite2', 'antiquewhite3', 'antiquewhite4', 'darkcyan', 'cyan3', 'darkslategray3')) +
            scale_x_discrete(labels = c('0-6 days', '7-13 days', '14-20 days', '21-27 days', '>=28 days')) +
            labs(x="Poor Mental Health Days")+
            ggtitle('Marital Status Where Mental Health Was Not Good')
            
            dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/7), MARITAL, Y)] # binning of MENTHLTH into 5 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
            ggplot(data=dt.group[Y==0], aes(y=percent, x=factor(MENTHLTH), fill=MARITAL)) +
            geom_bar(stat='identity') +
            theme(plot.title=element_text(hjust=0.5)) +
            scale_fill_manual(labels=c("1" = "Married", "2" = "Divorced","3" = "Widowed","4"="Separated","5"="Never Married",
                                       "6" = "Attached"), 
                              values=c("cornsilk2", "cadetblue1", "cyan3", "cyan4", 'darkslategray', "cornflowerblue")) +
            scale_x_discrete(labels = c('0-6 days', '7-13 days', '14-20 days', '21-27 days', '>=28 days')) +
            labs(x="Poor Mental Health Days")+
            ggtitle('Marital Status with no Heart Disease, per Mental Health Status')
            
            dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/7), MARITAL, Y)] # binning of MENTHLTH into 5 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
            ggplot(data=dt.group[Y==1], aes(y=percent, x=factor(MENTHLTH), fill=MARITAL)) +
            geom_bar(stat='identity') +
            theme(plot.title=element_text(hjust=0.5)) +
            scale_fill_manual(labels=c("1" = "Married", "2" = "Divorced","3" = "Widowed","4"="Separated","5"="Never Married",
                                       "6" = "Attached"), 
                              values=c("cornsilk2", "cadetblue1", "cyan3", "cyan4", 'darkslategray', "cornflowerblue")) +
            scale_x_discrete(labels = c('0-6 days', '7-13 days', '14-20 days', '21-27 days', '>=28 days')) +
            labs(x="Poor Mental Health Days")+
            ggtitle('Marital Status with Heart Disease, per Mental Health Status')

        #Divorced and Widowed, unmarried have deeper mental health issues, thus why contribute to heart disease
      
    # +-------------------------+
    # |        INCOME           |
    # +-------------------------+ 
  
        #How does income affect Heart disease
        
        #associationTest
        associationTest(formula=~Y+INCOME2, data=dt)
        #pvalue<0.001 = significant
          
        #CART
        
            cart_i <- rpart(INCOME2 ~ ., data = dt, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
            CVerror.cap <- cart_i$cptable[which.min(cart_i$cptable[,'xerror']), 'xerror'] + cart_i$cptable[which.min(cart_i$cptable[,'xerror']), 'xstd']
            # Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
            i <- 1; j<- 4
            while (cart_i$cptable[i,j] > CVerror.cap) {
              i <- i + 1
            }
            # Geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
            cp.opt = ifelse(i > 1, sqrt(cart_i$cptable[i,1] * cart_i$cptable[i-1,1]), 1)
            # Prune the max tree using optimal CP value
            cart_i <- prune(cart_i, cp = cp.opt)
            rpart.plot(cart_i, nn= T, main = "Maximal Tree for Income")
            print(cart_i)
            
            #EDUCA impt
            
                dt.group <- dt[, .N, by=.(EDUCA, INCOME2)]
                dt.group <- dt.group[, percent:=N/sum(N), by=.(INCOME2)] # convert abs count to %
                ggplot(data=dt.group, aes(y=percent, x=INCOME2, fill=EDUCA)) +
                geom_bar(stat = 'identity') +
                scale_y_continuous(labels = scales::percent)+
                scale_fill_manual(labels=c('Never/Kindergarten', 'Grades 1-8', 'Grades 9-11', 'Grade 12 or GED', 'College 1-3yrs', 'College >= 4yrs'), 
                                  values=c('antiquewhite4', 'cyan', 'cyan3', 'darkslategray3', 'darkslategray4', 'darkslategray')) +
                scale_x_discrete(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000')) +
                theme(plot.title=element_text(hjust=0.5)) +
                ggtitle('Income by Education')
                
                dt.group <- dt[, .N, by=.(EDUCA, INCOME2,Y)]
                dt.group <- dt.group[, percent:=N/sum(N), by=.(INCOME2)] # convert abs count to %
                ggplot(data=dt.group[Y==0], aes(y=percent, x=INCOME2, fill=EDUCA)) +
                geom_bar(stat = 'identity') +
                scale_y_continuous(labels = scales::percent)+
                scale_fill_manual(labels=c('Never/Kindergarten', 'Grades 1-8', 'Grades 9-11', 'Grade 12 or GED', 'College 1-3yrs', 'College >= 4yrs'), 
                                  values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond')) +
                scale_x_discrete(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000')) +
                theme(plot.title=element_text(hjust=0.5)) +
                ggtitle('Income by Education with no Heart Disease')
                
                dt.group <- dt[, .N, by=.(EDUCA, INCOME2,Y)]
                dt.group <- dt.group[, percent:=N/sum(N), by=.(INCOME2)] # convert abs count to %
                ggplot(data=dt.group[Y==1], aes(y=percent, x=INCOME2, fill=EDUCA)) +
                geom_bar(stat = 'identity') +
                scale_y_continuous(labels = scales::percent)+
                scale_fill_manual(labels=c('Never/Kindergarten', 'Grades 1-8', 'Grades 9-11', 'Grade 12 or GED', 'College 1-3yrs', 'College >= 4yrs'), 
                                  values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond')) +
                scale_x_discrete(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000')) +
                theme(plot.title=element_text(hjust=0.5)) +
                ggtitle('Income by Education with Heart Disease')
            
        #Low income and work stress contribute to link between education, heart disease and stroke
        #https://www.escardio.org/The-ESC/Press-Office/Press-releases/Low-income-and-work-stress-contribute-to-link-between-education-heart-disease-and-stroke

            dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/7), INCOME2)] # binning of MENTHLTH into 5 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
            ggplot(data=dt.group, aes(y=percent, x=factor(MENTHLTH), fill=INCOME2)) +
            geom_bar(stat='identity') +
            theme(plot.title=element_text(hjust=0.5)) +
            scale_fill_manual(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000'), 
                              values=c('antiquewhite2', 'antiquewhite3', 'antiquewhite4', 'darkcyan', 'cyan3', 'darkslategray3', 'darkslategray4', 'darkslategray')) +
            scale_x_discrete(labels = c('0-6 days', '7-13 days', '14-20 days', '21-27 days', '>=28 days')) +
            labs(x="Poor Mental Health Days")+
            ggtitle('Income by no. of poor mental health days')
            
            dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/7), INCOME2,Y)] # binning of MENTHLTH into 5 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
            ggplot(data=dt.group[Y==0], aes(y=percent, x=factor(MENTHLTH), fill=INCOME2)) +
              geom_bar(stat='identity') +
              theme(plot.title=element_text(hjust=0.5)) +
              scale_fill_manual(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000'), 
                                values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond', 'azure', 'cadetblue1')) +
              scale_x_discrete(labels = c('0-6 days', '7-13 days', '14-20 days', '21-27 days', '>=28 days')) +
              labs(x="Poor Mental Health Days")+
              ggtitle('Income by no. of poor mental health days but no Heart Disease')
            
            dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/7), INCOME2,Y)] # binning of MENTHLTH into 5 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
            ggplot(data=dt.group[Y==1], aes(y=percent, x=factor(MENTHLTH), fill=INCOME2)) +
              geom_bar(stat='identity') +
              theme(plot.title=element_text(hjust=0.5)) +
              scale_fill_manual(labels = c('<10,000', '10,000-14,999', '15,000-19,999', '20,000-24,999', '25,000-34,999', '35,000-49,999', '50,000-74,999','>=75,000'), 
                                values=c('brown4', 'coral4', 'coral2', 'coral', 'burlywood1', 'blanchedalmond', 'azure', 'cadetblue1')) +
              scale_x_discrete(labels = c('0-6 days', '7-13 days', '14-20 days', '21-27 days', '>=28 days')) +
              labs(x="Poor Mental Health Days")+
              ggtitle('Income by no. of poor mental health days & with Heart Disease')
            

        #For both income and marital, they have links to mental health thru quantitative
        #research, and mentalhealth is linked to health disease. could be a reason for income and
        #marital. but ofc multicollinearity is low for all variables so income and marital by itself
        #is a sig variable for heart disease, a new finding in this report compared to prev research

            #mentalhealth to Heart
                
            dt.group <- dt[, .N, by=.(MENTHLTH=as.integer(MENTHLTH/3), Y)] # binning of MENTHLTH into 5 groups
            dt.group <- dt.group[,percent:=N/sum(N), by=.(MENTHLTH)] # convert abs count to %
            ggplot(data=dt.group[complete.cases(dt.group)], aes(y=percent, x=factor(MENTHLTH), fill=Y)) +
            geom_bar(stat='identity') +
            theme(plot.title=element_text(hjust=0.5)) +
            scale_fill_manual(labels = c('No Heart Disease', 'Heart Disease'), values=c('lightseagreen', 'dark red')) +
            scale_x_discrete(labels = c('0-2 days', '3-5 days', '6-8 days', '9-11 days', '12-14 days', '15-17 days', '18-20 days', '21-23 days', '24-26 days', '27-29 days', '30 days')) +
            ggtitle('Heart Disease by Number of Days in Past 30 Days Where Mental Health Was Not Good') +
            labs(x='Poor Mental Health Days')
      

