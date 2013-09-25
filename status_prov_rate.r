# 按省份统计不同status的object的cnt总和，占该省份object的cnt总和的比例

library('plyr')
library('gdata')


d <- read.table('object_prov_rank.csv', header=TRUE, sep=',')
s <- read.table('data_object_status.csv', header=TRUE, sep=',')


ds <- merge(d, s,all.x=TRUE, by="object")
ds[,'status'] = NAToUnknown(ds[,'status'], unknown = 'unknown')


ds_prov_status <- ddply(ds, .(state, prov, status), summarise, s_cnt = sum(cnt))
ds_prov <- ddply(ds, .(state, prov), summarise, m_cnt = sum(cnt))
ds_main <- merge(ds_prov_status, ds_prov, all.x=TRUE, by=c("state", "prov"))
ds_main <- transform(ds_main, s_rate = s_cnt/m_cnt)

write.table(ds_main, file='status_prov_rate.csv', quote=FALSE, row.names=FALSE, sep=',')
