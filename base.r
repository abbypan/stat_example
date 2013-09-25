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
