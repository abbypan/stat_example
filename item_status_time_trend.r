library(plyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(grid)


cut_minute <- function(time, step_minute) {
    xx <- strptime(time,  format="%Y-%m-%d %H:%M:%S")
    step_t <- dminutes(step_minute)

    ss <-  floor_date(xx, 'day')
    while(1){
        e <- ss + step_t;
        if(xx < e) break;
        ss <- e;
    }
    format(ss, "%Y-%m-%d %H:%M:%S")
}

#每个object有一个覆盖的cnt
object_info <- read.table('prov_rank_stat.csv', header=TRUE,sep=',')
object_cnt <- object_info[,c(1,5)]
object_all_cnt <- sum(object_cnt$cnt)

#在不同node，指定item(>>> item 唯一 <<<)，监测object返回的status
#每个object只分配一个node进行监测
item_d <- read.table('item_status_on_object.csv', header=TRUE,sep=',')
d <- merge(item_d, object_cnt, by="object")

#按cut_time统计不同status所占的cnt总和
d$cut_time <- sapply(d$time, cut_minute, 10)
s <- ddply(d, .(cut_time, status) , summarise, stat_cnt = sum(cnt))
v <- dcast(s, cut_time ~ status , value.var = "stat_cnt")
v[is.na(v)] <- 0
vv <- ddply(v, .(cut_time), transform, unknown = object_all_cnt - good - normal - bad)

write.table(vv, file='item_status_stat.csv', sep=',',row.names=FALSE, quote=FALSE)

#m <- melt(vv)

#p <- ggplot(m, aes(x=cut_time,y=value,group=variable,fill=variable)) + 
#geom_area(position="fill") +
#ggtitle("mytest") +
#scale_x_discrete(expand = c(0, 0)) +  
#coord_cartesian(ylim=c(0,1)) +
#theme(axis.text.x  = element_text(angle=60, vjust=1, hjust=1),
      #plot.margin = unit(c(1,1,1,1), "cm")
      #)

#ggsave(p, filename="item_status_on_object.png", width=8) 
