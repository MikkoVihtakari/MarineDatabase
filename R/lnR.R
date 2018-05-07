#' @title Calculate logarithmic response ratio from a long table
#' @description Calculate logarithmic response ratio from a long table where each row is represents one measurement
#' @param X data frame
#' @param response character vector specifying the names of the columns for which the response ratio should be calculated.
#' @param levels Name of the column that contains factor which should be used to separate response ratios.  
#' @param groups character vector specifying the names of the columns, which should used as grouping factors. 
#' @param control the name of the control/base factor level in \code{levels} column. 
#' @param ci.type indicates the distribution to be used for confidence intervals. \code{'z'} refers to normal distribution and \code{'t'} to t distribution. The default (\code{NULL}) is to decide the distribution based on the lowest sqrt(n)*mean(response)/sd(response) (from Hedges et al.). If over 10\% of values are less than 3, t-distribution is used. Otherwise normal.
#' @param ci.conf the confidence level for the confidence interval. Defaults to 0.95 (95\%).
#' @param unlog logical indicating whether the output should be unlogged. Defaults to \code{FALSE}. Read the page 1152 under eq. 9 and what follows on the next page from Hedges et al. before you switch this to \code{TRUE} in publications. 
#' @param sqrt_transform Logical indicating whether values should be square root transformed prior calculation of means. This option makes the distributions more normally distributed, but might change the outcome. Highly experimental. DO NOT USE in publications.  
#' @param paired_tests Logical indicating whether \link[base]{wilcox.test} should be used to "confirm" the results indicated by the confidence intervals for the response ratios.
#' @param all.data logical indicating whether all data used in calculations should be returned instead of a concise table of relevant results. Defaults to \code{FALSE}.
#' @param signif number of significant digits in output. Defaults to 2. If \code{'all'} output will not be rounded.
#' @return Returns a list where \code{$data} element contains the calculated response ratios for each \code{response} in a separate list named by the response's column name. \code{$info} contains information how the response ratios were calculated and, if \code{paired_tests = TRUE}, the \code{$tests} element gives \link[base]{wilcox.test} results to "confirm" significance of the confidence intervals for the response ratios. Nonconforming tests are listed under \code{$nonconforming}.
#' @details The calculations are based on Hedges et al. (1999), with the exception that t-distribution is used to acquire confidence intervals instead of normal distribution. The difference is minimal for sample sizes > 20, but the confidence intervals will be a lot more conservative for small sample sizes leading to fewer false positives. Use \code{ci.type = "z"} to use normal distribution for CI estimation as described in the original source.
#' 
#' The square root transformation routine is experimental and little tested, but seems to produce slightly less nonconforming test results against \link[base]{wilcox.test} for non-normal data. 
#' 
#' It is recommended to plot your raw values to confirm any results given by this function.    
#' @references Hedges, L. V, Gurevitch, J., & Curtis, P.S. (1999) The meta-analysis of response ratios in experimental ecology. Ecology, 80, 1150–1156. 
#' @import reshape2 broom
#' @author Mikko Vihtakari
#' @example # make some...
#' @export

## Test parameters
# unlog = FALSE; ci.type = "t"; ci.conf = 0.95; all.data = FALSE; signif = 2; sqrt_transform = FALSE; paired_tests = TRUE
# X = x; response = "value"; levels = "ytemp"; groups = c("variable", "region"); control = "cold"
# X = env; response = c("temp", "sal", "ice"); levels = "ytemp"; groups = c("region"); control = "cold"


lnR <- function(X, response, levels, groups, control, ci.type = NULL, ci.conf = 0.95, paired_tests = FALSE, unlog = FALSE, all.data = FALSE, signif = 2, sqrt_transform = FALSE) {


if(!exists("control", mode = "character")) stop("control must be defined")
if(!exists("response", mode = "character")) stop("response must be defined")
if(!exists("levels", mode = "character")) stop("levels must be defined")
if(!exists("groups", mode = "character")) stop("groups must be defined")

if(!all(apply(X[response], 2, is.numeric))) stop("all response variables are not numeric")

if(length(levels) > 1) stop("length of levels must be 1")
if(!class(X[[levels]]) %in% c("factor", "ordered")) X[[levels]] <- factor(X[[levels]])
  
treat.levs <- levels(X[,levels])[!levels(X[,levels]) %in% control]
ids <- c(groups, levels)
gids <- c(groups)

tmp <- X[c(ids, response)]

if(sqrt_transform) {
tmp[response] <- lapply(response, function(j) sqrt(tmp[[j]]))
}

tmp <- lapply(seq_along(response), function(i) {
  tmp[c(ids, response[i])]
})

names(tmp) <- response

tmp <- lapply(tmp, function(x) reshape2::melt(x, id = ids, variable.name = "VAR"))

if(paired_tests) {
  tests <- lapply(tmp, function(z) {
    
    tp <- split(z, lapply(groups, function(j) z[[j]]), drop = TRUE)
    
    # k <- tp[[1]]
    tests <- lapply(tp, function(k) {
      mod <- suppressWarnings(wilcox.test(formula(paste0('value~',levels)), data = k))
      mod <- broom::tidy(mod)
      mod$p.value <- round(mod$p.value, 4)
        
      om <- reshape2::dcast(k, ...~ VAR, mean, na.rm = TRUE)
      names(om)[names(om) == "value"] <- "avg"
      osd <- reshape2::dcast(k, ...~ VAR, sd, na.rm = TRUE)
      names(osd)[names(osd) == "value"] <- "sd"
      on <- reshape2::dcast(k, ...~ VAR, value.var = "value", fun.aggregate = function(x) sum(!is.na(x)))
      names(on)[names(on) == "value"] <- "n"
      
      OUT <- merge(om, osd, sort = FALSE, all = TRUE)
      OUT <- merge(OUT, on, sort = FALSE, all = TRUE)
      OUT$se <- OUT$sd/sqrt(OUT$n)
    
      OUT <- rapply(OUT, function(x) round(x, signif), classes = "numeric", how = "replace")
      
      OUT <- merge(OUT, cbind(unique(k[groups]), mod), all = TRUE)
    list(mean_pars = OUT, test = cbind(unique(k[groups]), mod))
    })
    
    t1 <- do.call(rbind, lapply(tests, function(k) k$mean_pars)) 
    rownames(t1) <- 1:nrow(t1)
    
    t2 <- do.call(rbind, lapply(tests, function(k) k$test))
    rownames(t2) <- 1:nrow(t2)
    
    list(mean_pars = t1, test = t2)
    
  })
}

# z <- tmp[[1]]
tmp <- lapply(tmp, function(z) {tmp.mean <- reshape2::dcast(z, ...~ VAR, mean, na.rm = TRUE) 
  tmp.sd <- dcast(z, ... ~ VAR, sd, na.rm = TRUE)
  tmp.n <- dcast(z, ... ~ VAR, value.var = "value", fun.aggregate = function(x) sum(!is.na(x)))
  if(!identical(tmp.mean[ids], tmp.sd[ids])) stop("mean and standard deviation data frames are not identical")
  if(!identical(tmp.mean[ids], tmp.n[ids])) stop("mean and number of observations data frames are not identical")
  if(!identical(tmp.sd[ids], tmp.n[ids])) stop("standard deviation and number of observations data frames are not identical")
  list(mean = tmp.mean, sd = tmp.sd, n = tmp.n)  })

b <- lapply(tmp, function(z) {
  lapply(z, function(i) { g <- reshape(i, direction = "wide", idvar = gids, timevar = levels)
    nam <- strsplit(colnames(g), "\\.")
    colnames(g) <- lapply(nam, function(x) ifelse(length(x) == 1, x, ifelse(length(x) == 2, x[2], "error")))
    g})
  })

NORM_STAT <- lapply(b, function(g) {
  tp <- lapply(treat.levs, function(z) cbind(g$mean[gids], treatment = z, norm_stat_treat = sqrt(g$n[,z])*g$mean[,z]/g$sd[,z], norm_stat_cont = sqrt(g$n[,control])*g$mean[,control]/g$sd[,control]))
  do.call(rbind, tp)
})

CI_TYPE_SWITCH <-lapply(NORM_STAT, function(x) {
  daa <- apply(x[c("norm_stat_treat", "norm_stat_cont")], 1, function(g) min(g) < 3)
  100*sum(daa)/length(daa) > 10
  })
  
if(is.null(ci.type)) {
  ci.type <- ifelse(any(unlist(CI_TYPE_SWITCH)), "t", "z")
}

LNRR <- lapply(names(b), function(g) {
  tp <- lapply(treat.levs, function(z) cbind(b[[g]]$mean[gids], treatment = z, response = g, lnrr = log(b[[g]]$mean[,z]) - log(b[[g]]$mean[,control])))
  do.call(rbind, tp)})

names(LNRR) <- names(b)

VAR <- lapply(b, function(g) {
  tp <- lapply(treat.levs, function(z) {
    cbind(g$mean[gids], treatment = z, var = (g$sd[,z]^2)/(g$n[,z]*g$mean[,z]^2) + (g$sd[,control]^2)/(g$n[,control]*g$mean[,control]^2))})
  do.call(rbind, tp)})

if(ci.type == "t"){
CRIT <- lapply(b, function(g) {
  tp <- lapply(treat.levs, function(z) {cbind(g$mean[gids], treatment = z,
    crit = qt(ci.conf + (1-ci.conf)/2, g$n[,control] + g$n[,z] - 1))})
    do.call(rbind, tp)})
  } else {
if(ci.type == "z") {
CRIT <- lapply(b, function(g) {
  tp <- lapply(treat.levs, function(z) {cbind(g$mean[gids], treatment = z,
    crit = qnorm(ci.conf + (1-ci.conf)/2))})
    do.call(rbind, tp)})} else {
      stop("Unexpected ci.type")
    }} 

N <- lapply(b, function(g) {
  tp <- lapply(treat.levs, function(z) cbind(g$mean[gids], treatment = z, n_cont = g$n[,control], n_treat = g$n[,z]))
  do.call(rbind, tp)})

out <- list()

out$data <- lapply(response, function(g) {
  tp <- merge(LNRR[[g]], VAR[[g]], by = c(gids, "treatment"), all = T, sort = F)
  tp <- merge(tp, CRIT[[g]], by = c(gids, "treatment"), all = T, sort = F)
  tp <- merge(tp, N[[g]], by = c(gids, "treatment"), all = T, sort = F)
  merge(tp, NORM_STAT[[g]], by = c(gids, "treatment"), all = T, sort = F)})

names(out$data) <- response

## Calculate CIs

out$data <- lapply(out$data, function(g) {
  tp <- data.frame(g)
  tp$ci.min <- tp$lnrr - tp$crit*sqrt(tp$var)
  tp$ci.max <- tp$lnrr + tp$crit*sqrt(tp$var)
  tp})

## Conditions

if(unlog) {
  out$data <- lapply(out$data, function(g) {
    tp <- data.frame(g)
    tp[c("lnrr", "ci.min", "ci.max")] <- 100*exp(tp[c("lnrr", "ci.min", "ci.max")])
    tp
  })
}

if(!signif == "all") {
  out$data <- lapply(out$data, function(g) {
    tp <- data.frame(g)
    tp[sapply(tp, is.numeric)] <- round(tp[sapply(tp, is.numeric)], signif)
    tp
  })
}

if(paired_tests) {
  out$data <- lapply(seq_along(out$data), function(i) {
    tp <- data.frame(out$data[[i]])
    tp <-merge(tp, tests[[i]]$test, all = TRUE, sort = FALSE)
    
    tmp <- ifelse(tp$lnrr < 0 & tp$ci.max < 0, TRUE, ifelse(tp$lnrr > 0 & tp$ci.min > 0, TRUE, FALSE))
    tmp2 <- tp$p.value <= 1 - ci.conf
    tp$conform <- tmp == tmp2
    
    tp[c(groups, "treatment", "response", "var", "crit", "lnrr", "ci.min", "ci.max", "n_cont", "n_treat", "norm_stat_cont", "norm_stat_treat", "statistic", "p.value", "conform", "method", "alternative")]
  })
  
  names(out$data) <- response
  
  out$tests <- lapply(tests, function(k) k$mean_pars)
  
  out$nonconforming <- lapply(seq_along(out$data), function(i) {
    tp <- out$data[[i]]
    tp <- tp[!tp$conform,]
    tp <- split(tp, lapply(groups, function(j) tp[[j]]), drop = TRUE)
    
    ts <- out$tests[[i]]
    ts <- split(ts, lapply(groups, function(j) ts[[j]]), drop = TRUE)
    
    # k <- tp[[1]]
    nc <- lapply(tp, function(k) {
      # j <- ts[[19]]
      tg <- ts[unname(sapply(ts, function(j) all(unique(j[groups]) == k[groups])))][[1]]
      
      tt <- merge(tg, k)
      
      tt[c(groups, "treatment", levels, "response", "var", "crit", "lnrr", "ci.min", "ci.max", "avg", "sd", "n", "n_cont", "n_treat", "se", "norm_stat_cont", "norm_stat_treat", "statistic", "p.value", "method", "alternative")]
    })
    
    nc <- do.call(rbind, nc)
    rownames(nc) <- 1:nrow(nc)
    
    nc
  })
  
  names(out$nonconforming) <- response
}


if(!all.data) {
  out$data <- lapply(out$data, function(g) {
    tp <- data.frame(g)
    tp[!colnames(tp) %in% c("var", "crit", "n_cont", "n_treat", "norm_stat_treat", "norm_stat_cont", "method", "alternative")]
  })
}


if(length(response) > 1) {
  what <- paste(response[-length(response)], collapse = ", ")
  what <- paste0(what, ", and ", response[length(response)])
} else {
    what <- response
}

info <- paste0(if(unlog) "Unlogged" else "Logarithmic", " response ratios of ", what, " with ", 100*ci.conf, "%", " confidence intervals assuming a ", ifelse(ci.type == "t", "t", ifelse(ci.type == "z", "normal", "error")), "-distribution.") 

info2 <- ifelse(any(unlist(CI_TYPE_SWITCH)), "sqrt(n)*mean(response)/sd(response) is < 3 for many samples. Using t-distributions is recommended.", "sqrt(n)*mean(response)/sd(response) > 3 for most samples. Distributions propably converge normal distribution. Normal distribution recommended (ci.type = 'z'")


  
out$info <- paste(info, info2)

class(out) <- "lnRtest"

return(out)}
