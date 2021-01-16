
extract.comments <- function(fn) {
    fn.chk <- tail(strsplit(fn, ".", fixed=TRUE)[[1]], 1) %in% c("r", "R")
    if(!fn.chk)
        stop("输入文件类型错误")
    tmp_env <- new.env(parent=sys.frame())
    source(fn, tmp_env, echo=FALSE, print.eval=FALSE, verbose=FALSE, keep.source=TRUE)
    funs <- Filter(is.function, sapply(ls(tmp_env), get, tmp_env))

    comments <- lapply(names(funs), function(nm) {
        # get function source
        f <- funs[[nm]]
        src <- capture.output(f)
        doc <- list()
        # get function name
        doc$fun.name <- nm
        # get function parameter
        doc$param <- grep("^[[:blank:]]*#[[:blank:]]*@param", src, value=TRUE)
        doc$param <- gsub("^[[:blank:]]*#[[:blank:]]*", "", doc$param)
        # get function return
        doc$return <- grep("^[[:blank:]]*#[[:blank:]]*@return", src, value=TRUE)
        doc$return <- gsub("^[[:blank:]]*#[[:blank:]]*", "", doc$return)
        param <- NULL
        for(i in seq(along.with=doc$param)) {
            param <- paste(param, doc$param[i], "\n")
        }
        
        paste0("函数名: ", "\n", doc$fun.name, "\n",
                        "参数列表：", "\n", param,
                        "返回值：", "\n", doc$return, "\n")
    })
    for(i in seq(along.with=comments)) {
        cat(comments[[i]], "\n")
    }    
}
if(TRUE) {
    extract.comments("test.c")
}