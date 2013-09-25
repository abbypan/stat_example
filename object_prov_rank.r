#object 全局唯一，每个object都有一个cnt
#统计 每个object 的 cnt，在object所在省内排名、省内占比、国内排名、国内占比 

library('plyr') 

#取降序排列结果 
decr_rank <- function (d) match(d, sort(d,decreasing=TRUE))

#原始数据
d <- read.table('data_object_info.csv', header=TRUE, sep=',')

#省内排名，省内占比
p <- ddply(d, .(state,prov), transform,
        prov_rank = decr_rank(cnt), prov_rate = cnt/sum(cnt))

#国内排名，国内占比
s <- ddply(p, .(state), transform,
        state_rank = decr_rank(cnt), state_rate = cnt/sum(cnt))

#按用户数降序排列
r <- s[order(s$cnt,decreasing=TRUE), ]
names(r) <- c('object','state','prov','area','cnt',
                        'prov_rank','prov_rate','state_rank','state_rate');                                                                  
#最终数据
write.table(r, file='object_prov_rank.csv', sep=',',row.names=FALSE, quote=FALSE)
