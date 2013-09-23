library('plyr') 

#取降序排列结果 
decr_rank <- function (d) match(d, sort(d,decreasing=TRUE))

#原始数据
d <- read.table('prov_rank.csv', header=TRUE, sep=',')

#省内排名，省内占比
p <- ddply(d, .(state,prov), transform,
        prov_rank = decr_rank(value), prov_rate = value/sum(value))

#国内排名，国内占比
s <- ddply(p, .(state), transform,
        state_rank = decr_rank(value), state_rate = value/sum(value))

#按用户数降序排列
r <- s[order(s$value,decreasing=TRUE), ]
names(r) <- c('object','state','prov','area','value',
                        'prov_rank','prov_rate','state_rank','state_rate');                                                                  
#最终数据
write.table(r, file='prov_rank_stat.csv', quote=FALSE, sep=',')