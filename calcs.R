
library(dplyr)
library(ggplot2)

unzip("activity.zip")
act <- read.csv("activity.csv")
act <- mutate(act,mon=ifelse(as.Date(date)<as.Date("2012-11-01"),10,11))
steps.all <- summarize(group_by(act,date),
                       mean=mean(steps,na.rm=TRUE),
                       min=min(steps,na.rm=TRUE),
                       max=max(steps,na.rm=TRUE),
                       total=sum(steps,na.rm=TRUE),
                       count=sum(!is.na(steps))
)
steps <- filter(steps.all, count>0)
all.missing <- sum(steps.all$count==0)
all.days <- dim(steps.all)[1]
steps.by.interval <- summarize(group_by(act,interval),
                               mean=mean(steps,na.rm=TRUE),
                               min=min(steps,na.rm=TRUE),
                               max=max(steps,na.rm=TRUE),
                               total=sum(steps,na.rm=TRUE),
                               count=sum(!is.na(steps))
)
steps.by.interval <- filter(steps.by.interval, count>0)

hist(steps$total,xlab="Total steps in a day\n(missing values excluded)",main="")

summary(steps$total)

print(qplot(x=steps.by.interval$interval,y=steps.by.interval$mean,geom="line",ylab="mean steps across all days",xlab="interval","Daily activity pattern"))
peak <- steps.by.interval[which(steps.by.interval$total==max(steps.by.interval$total)),]

# use the overall mean to impute steps
impute <- function(i) {
    mean(act[act$interval==i,]$steps,na.rm=TRUE)
}

# compute these once instead of many times
imputations <- sapply(1:288,function(i){ impute(act$interval[i]) })

act.imputed <- cbind(
    act,
    rep(imputations,61)
)

colnames(act.imputed) <- c(colnames(act.imputed)[1:4],c("imputed"))

act.imputed <- mutate(
    act.imputed,
    imputedsteps = ifelse(is.na(steps), imputed, steps)
)

steps.imputed <- summarize(group_by(act.imputed,date),
                       total=sum(imputedsteps)
)

hist(
    steps.imputed$total,
    xlab="Total steps in a day\n(missing values imputed using interval mean)",
    main=""
)

summary(steps.imputed$total)

act.imputed <- cbind(
    act.imputed,
    ifelse(weekdays(as.Date(act.imputed$date)) %in% c("Saturday","Sunday"),"weekend", "weekday")
)

colnames(act.imputed)[length(colnames(act.imputed))] <- "weekpart"

steps.imp.int <- summarize(
    group_by(act.imputed,interval,weekpart),
    mean=mean(imputedsteps)
)
    
p <- ggplot(steps.imp.int,aes(interval,mean)) + geom_line() + facet_grid(weekpart~.) +
    ylab("mean steps across all days") + ggtitle("Daily activity pattern")
print(p)

