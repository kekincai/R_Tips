logging <- function(FUN, ...) {
    # 保存函数log到文件
    # @param FUN 函数名
    # @param ... FUN函数的参数

    if(is.character(FUN)) {
        if(FUN=="")
            FUN <- NULL
        else 
            FUN <- get(FUN)
    }
    # 生成log文件
    zz <- file(paste0(Sys.Date(), "_log.txt"), open="wt")
    sink(zz)
    sink(zz, type="message")
    sink(zz, type="output")
    # 执行函数
    FUN(...)
    sink(type="message")
    sink(type="output")
    # 关闭文件
    close.connection(zz, type="wt")
}