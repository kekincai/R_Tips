plot.ellipse <- function(x0=0, y0=0, a=2, eps=0.5, phi=pi/4) {
    # 画椭圆
    # @param x0 中心点x轴坐标
    # @param y0 中心点y轴坐标
    # @param a 长轴长度
    # @param eps 离心率
    # @param phi 旋转角度
    
    b <- a * sqrt(1 - eps^2)
    t <- seq(-pi, pi, length.out=500)
    x <- a * cos(t)
    y <- b * sin(t)
    # browser()
    d <- data.frame(x, y)
    # trans.m <- matrix(c(cos(phi), -sin(phi),
    #                  sin(phi), cos(phi)),
    #                  nrow=2, byrow=TRUE)
    # par(pty="m")
    # d <- trans.m * d
    x <- x + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
    y <- y + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
    plot(x, y, pch=19, col='red')
    
}

if (FALSE) {
    source("./src/plot.ellipse.r")
    plot.ellipse(eps=0.8, phi=pi/4)
    
}

