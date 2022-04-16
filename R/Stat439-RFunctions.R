##
#####	Collection of functions written by D. Gillen that are used
#####	throughout Stat 111/202
#####
#####	Author: D. Gillen
#####	Course: Stat 111/202, Winter 2012
##
##
#####  Function to estimate and test measures of association in 2 x 2 tables
##
prop.comp <- function( x, estimate="all", conf.level=.95, transpose=FALSE ){
	if( transpose ) x <- t(x)
	rslt <- vector( "list", length=3 )
	names( rslt ) <- c( "riskdiff", "riskratio", "oddsratio" )
	diff.rslt <- suppressWarnings(prop.test( x, conf.level=conf.level ))
	rslt[[1]] <- rslt[[2]] <- rslt[[3]] <- epitab( x, method="riskratio", pvalue="chi2", conf.level=conf.level )$tab
	colnames( rslt[[1]] )[5] <- "riskdiff"
	rslt[[1]][,5] <- c(0,diff(rev(diff.rslt$estimate)))
	rslt[[1]][2,6:7] <- diff.rslt$conf.int
	colnames( rslt[[3]] )[5] <- "oddsratio"
	rslt[[3]][,5:8] <- suppressWarnings(epitab( x, method="oddsratio", pvalue="chi2", conf.level=conf.level )$tab[,5:8])
	if(is.null(names(dimnames(x)))){
		for(i in 1:3){
			colnames(rslt[[i]])[c(1,3)] <- c("Outcome=0", "Outcome=1")
			rownames(rslt[[i]]) <- c("Group=1", "Group=2")
			}
	}
	if( is.element( estimate, c("all", "oddsratio") ) ){ 
		if(is.null(names(dimnames(x)))){
			warning( "Estimated probabilities represent Pr[ Outcome | Group ]. For estimates of 
			Pr[ Group | Outcome ], change the value of 'transpose'.")
		}
		else
			warning( paste("Estimated probabilities represent Pr[", names(dimnames(x))[2], 
			"|",names(dimnames(x))[1], "]. For estimates of 
			Pr[", names(dimnames(x))[1], "|",names(dimnames(x))[2], "], change the value of 'transpose'.") )
		}
	if( estimate == "riskdiff" ) return(rslt[[1]])
	else if( estimate == "riskratio" ) return(rslt[[2]])
	else if( estimate == "oddsratio" ) return(rslt[[3]])
	else return(rslt)
}


##
#####  Function to exponentiate coefficients and produces CIs for LMs
##
lmCI <- function( model, expcoef=FALSE, robust=FALSE ){
	coef <- summary( model )$coef[,1]
	se <- ifelse1( robust, robust.se.lm(model)[,2], summary( model )$coef[,2] )
	tvalue <- coef / se
	pvalue <- 2*(1-pt(abs(tvalue), model$df.residual))
	if( expcoef ){
		ci95.lo <- exp( coef - qt(.975, model$df.residual) * se )
		ci95.hi <- exp( coef + qt(.975, model$df.residual) * se )
		est <- exp( coef )
	}
	else{
		ci95.lo <- coef - qt(.975, model$df.residual) * se
		ci95.hi <- coef + qt(.975, model$df.residual) * se
		est <- coef
	}
	rslt <- round( cbind( est, ci95.lo, ci95.hi, tvalue, pvalue ), 4 )
	colnames( rslt ) <- ifelse1( 	robust, 	
					c("Est", "robust ci95.lo", "robust ci95.hi", "robust t value", "robust Pr(>|t|)"),
					c("Est", "ci95.lo", "ci95.hi", "t value", "Pr(>|t|)") )			
	colnames( rslt )[1] <- ifelse( expcoef, "exp( Est )", "Est" )
	rslt
	}

##
#####	Function to plot dfBetas resulting from a lm() object
##
plot.dfbeta <- function( dfbeta.fit, labels ){
	oldmar <- par()$mar
	par( mar=c(5, 4, 3+.75*dim(dfbeta.fit)[2], 2) + 0.1 )
	plot( c(1,dim(dfbeta.fit)[1]*1.1), range(dfbeta.fit)*1.1, xlab="Obersvation", ylab="dfBeta", type="n" )
	for( i in 2:dim(dfbeta.fit)[2] ){
			points( 1:dim(dfbeta.fit)[1], dfbeta.fit[,i], col=i )
			text( 1:dim(dfbeta.fit)[1]+1, dfbeta.fit[,i]+.1, labels=labels, col=i )
			mtext( colnames( dfbeta.fit )[i], col=i, line=-1+i )
	}  
	abline( h=c(-1,1)*(2/sqrt(dim(dfbeta.fit)[1])), col="red", lwd=2 )
	par( mar=oldmar )
	}
	
##
#####
#####	robust.se() is a function to compute the Huber-White sandwich variance estimator
#####	for the linear regression model
#####	
##
robust.se.lm <- function( model) { 
  s <- summary( model) 
  X <- model.matrix( model )
  eps <- model$residuals
  sandwich.cov <- solve( t(X)%*%X ) %*%( t(X) %*% diag(eps^2) %*% X ) %*% solve( t(X)%*%X )
  sand.se <- sqrt( diag( sandwich.cov )) 
  t <- model$coefficients/sand.se
  p <- 2*pt( -abs( t ), dim(X)[1]-dim(X)[2] ) 
  ci95.lo <- model$coefficients - qt( .975, dim(X)[1]-dim(X)[2] ) * sand.se
  ci95.hi <- model$coefficients + qt( .975, dim(X)[1]-dim(X)[2] ) * sand.se
  rslt <- cbind( model$coefficients, sand.se, ci95.lo, ci95.hi, t, p ) 
  dimnames(rslt)[[2]] <- c( dimnames( s$coefficients )[[2]][1], "Robust SE", "ci95.lo", "ci95.hi", dimnames( s$coefficients )[[2]][3:4] ) 
  rslt 
} 


##
#####  Function to exponentiate coefficients and produces CIs for GLMs
##
glmCI <- function( model, transform=TRUE, robust=FALSE ){
	link <- model$family$link
	coef <- summary( model )$coef[,1]
	se <- ifelse1( robust, robust.se.glm(model)[,2], summary( model )$coef[,2] )
	zvalue <- coef / se
	pvalue <- 2*(1-pnorm(abs(zvalue)))

	if( transform & is.element(link, c("logit","log")) ){
		ci95.lo <- exp( coef - qnorm(.975) * se )
		ci95.hi <- exp( coef + qnorm(.975) * se )
		est <- exp( coef )
	}
	else{
		ci95.lo <- coef - qnorm(.975) * se
		ci95.hi <- coef + qnorm(.975) * se
		est <- coef
	}
	rslt <- round( cbind( est, ci95.lo, ci95.hi, zvalue, pvalue ), 4 )
	colnames( rslt ) <- ifelse1( 	robust, 	
					c("Est", "robust ci95.lo", "robust ci95.hi", "robust z value", "robust Pr(>|z|)"),
					c("Est", "ci95.lo", "ci95.hi", "z value", "Pr(>|z|)") )			
	colnames( rslt )[1] <- ifelse( transform & is.element(link, c("logit","log")), "exp( Est )", "Est" )
	rslt
	}

##
#####	Function to estimate linear contrasts of coefficients from a GLM fit
##
linContr.glm <- function( contr.names, contr.coef=rep(1,length(contr.names)), model, transform=TRUE ){
	beta.hat <- model$coef 
	cov.beta <- vcov( model )

	contr.index <- match( contr.names, dimnames( cov.beta )[[1]] )	
	beta.hat <- beta.hat[ contr.index ]
	cov.beta <- cov.beta[ contr.index,contr.index ]
	est <- contr.coef %*% beta.hat
	se.est <- sqrt( contr.coef %*% cov.beta %*% contr.coef )
	zStat <- est / se.est
	pVal <- 2*pnorm( abs(zStat), lower.tail=FALSE )
	ci95.lo <- est - qnorm(.975)*se.est
	ci95.hi <- est + qnorm(.975)*se.est
	
	link <- model$family$link
	if( transform & is.element(link, c("logit","log")) ){
		ci95.lo <- exp( ci95.lo )
		ci95.hi <- exp( ci95.hi )
		est <- exp( est )
		cat( "\nTest of H_0: exp( " )
		for( i in 1:(length( contr.names )-1) ){
			cat( contr.coef[i], "*", contr.names[i], " + ", sep="" )
			}
		cat( contr.coef[i+1], "*", contr.names[i+1], " ) = 1 :\n\n", sep="" )		
		}
	else{
		cat( "\nTest of H_0: " )
		for( i in 1:(length( contr.names )-1) ){
			cat( contr.coef[i], "*", contr.names[i], " + ", sep="" )
			}
		cat( contr.coef[i+1], "*", contr.names[i+1], " = 0 :\n\n", sep="" )
		}
	rslt <- data.frame( est, se.est, zStat, pVal, ci95.lo, ci95.hi )
	colnames( rslt )[1] <- ifelse( transform && is.element(link, c("logit","log")), "exp( Est )", "Est" )
	round( rslt, 3 )
}
	
##
#####  Function to collapse grouped binary data to binomial
##
collapse <- function( data, outcome ){
	index <- (1:length(names(data)))[ names(data)==outcome ]
	y <- data[,index]
	covnames <- names( data )[-index]
	data <- data[,-index]
	if( is.null( dim( data ) ) ){
		rslt <- aggregate( y, list(data), FUN=length)
		rslt <- as.data.frame( cbind( rslt, aggregate( y, list(data), FUN=sum)[dim(rslt)[2]] ) )
	}
	else{
		rslt <- aggregate( y, data, FUN=length)
		rslt <- as.data.frame( cbind( rslt, aggregate( y, data, FUN=sum)[dim(rslt)[2]] ) )
	}
	names( rslt ) <- c( covnames, "n", paste("n.", outcome, sep="") )
	rslt
}
##
#####	Function to compute deviance (LR) test p-Value
##
lrtest <- function( fit1, fit2 ){
	cat( "\nAssumption: Model 1 nested within Model 2\n\n" )
	rslt <- anova( fit1, fit2 )
	rslt <- cbind( rslt, c("", round( pchisq( rslt[2,4], rslt[2,3], lower.tail=FALSE ), 4 ) ) )
	rslt[,2] <- round( rslt[,2], 3 )
	rslt[,4] <- round( rslt[,4], 3 )
	rslt[1,3:4] <- c( "", "" )
	names( rslt )[5] <- "pValue"
	rslt
}
##
#####	H-L goodness of fit test
##
binary.gof <- function( fit, ngrp=10, print.table=TRUE ){
	y <- fit$y
	phat <- fitted( fit )
	fittedgrps <- cut( phat, quantile( phat, seq(0,1,by=1/ngrp) ), include.lowest=TRUE )
	n <- aggregate( y, list( fittedgrps ), FUN=length )[,2]
	Obs <- aggregate( y, list( fittedgrps ), FUN=sum )[,2]
	Exp <- aggregate( phat, list( fittedgrps ), FUN=sum )[,2]
	if( print.table==TRUE ){
		cat( "\nFitted Probability Table:\n\n" )
		rslt <- as.data.frame( cbind( 1:ngrp, n, Obs, Exp ) )
		names( rslt )[1] <- "group"
		print( rslt )
	}
	chisqstat <- sum( (Obs - Exp)^2 / ( Exp*(1-Exp/n) ) )
	df <- ngrp-2
	pVal <- pchisq( chisqstat, df, lower.tail=FALSE )
	cat( "\n Hosmer-Lemeshow GOF Test:\n\n" )
	cbind( chisqstat, df, pVal )
}

##
#####	Function to compute robust se for glms
##
robust.se.glm<-function(glm.obj){
	## 	Compute robust (sandwich) variance estimate
	if (is.matrix(glm.obj$x)) 
		xmat<-glm.obj$x
	else {
		mf<-model.frame(glm.obj)
		xmat<-model.matrix(terms(glm.obj),mf)		
	}
	umat <- residuals(glm.obj,"working")*glm.obj$weights*xmat
	modelv<-summary(glm.obj)$cov.unscaled
	robust.cov <- modelv%*%(t(umat)%*%umat)%*%modelv
	
	##	Format the model output with p-values and CIs
	s <- summary( glm.obj) 
	robust.se <- sqrt( diag( robust.cov )) 
	z <- glm.obj$coefficients/robust.se
	p <- 2*pnorm( -abs( z ) ) 
	ci95.lo <- glm.obj$coefficients - qnorm( .975 ) * robust.se
	ci95.hi <- glm.obj$coefficients + qnorm( .975 ) * robust.se
	rslt <- cbind( glm.obj$coefficients, robust.se, ci95.lo, ci95.hi, z, p ) 
	dimnames(rslt)[[2]] <- c( dimnames( s$coefficients )[[2]][1], "Robust SE", "ci95.lo", "ci95.hi", dimnames( s$coefficients )[[2]][3:4] ) 
	rslt 
	}

##
#####	Function to summarize the multinomial fit
##
summ.mfit <- function( model ){
	s <- summary( model )
	for( i in 1:(length(model$lev)-1) ){
		cat( "\nLevel ", model$lev[i+1],  "vs. Level ", model$lev[1], "\n" )
		coef <- s$coefficients[i,]
		rrr <- exp( coef )
		se <- s$standard.errors[i,]
		zStat <- coef / se
		pVal <- 2*pnorm( abs(zStat), lower.tail=FALSE )
		ci95.lo <- exp( coef - qnorm(.975)*se )
		ci95.hi <- exp( coef + qnorm(.975)*se )
		rslt <- cbind( rrr, se, zStat, pVal, ci95.lo, ci95.hi )
		print( round( rslt, 3 ) )
	}
}

##
#####	Function to test linear contrasts
##
LinContr.mfit <- function( contr.names, contr.coef, model ){ 
	beta.hat <- as.vector( t( summary( model )$coefficients ) )
	se <- as.vector( t( summary( model )$standard.errors ) )
	cov.beta <- vcov( model )

	contr.index <- is.element( dimnames( cov.beta )[[1]], contr.names )
	beta.hat <- beta.hat[ contr.index ]
	cov.beta <- cov.beta[ contr.index,contr.index ]
	est <- contr.coef %*% beta.hat
	rrr.est <- exp( est )
	se.est <- sqrt( contr.coef %*% cov.beta %*% contr.coef )
	zStat <- est / se.est
	pVal <- 2*pnorm( abs(zStat), lower.tail=FALSE )
	ci95.lo <- exp( est - qnorm(.975)*se.est )
	ci95.hi <- exp( est + qnorm(.975)*se.est )
	cat( "\nTest of H_0: " )
	for( i in 1:(length( contr.names )-1) ){
		cat( contr.coef[i], "*", contr.names[i], "+ " )
	}
	cat( contr.coef[i+1], "*", contr.names[i+1], "= 0 :\n\n" )
	rslt <- data.frame( rrr.est, se.est, zStat, pVal, ci95.lo, ci95.hi )
	round( rslt, 3 )
}

##
#####	Function to summarize the proportional odds fit
##
exp.olr <- function( model ){
		coef <- summary( model )$coef[ 1:length(model$coef), ]
		zStat <- coef[,3]
		pVal <- 2*pnorm( abs(zStat),lower.tail=FALSE )
		ci95.lo <- exp( coef[,1] - qnorm(.975) * coef[,2] )
		ci95.hi <- exp( coef[,1] + qnorm(.975) * coef[,2] )
		rslt <- round( cbind( exp( coef[,1] ), zStat, pVal, ci95.lo, ci95.hi), 4 )
		colnames( rslt )[1] <- "exp( Est )"
		rslt
		}
		

##
#####	Helper function to return vector for binary test
##
ifelse1 <- function (test, yes, no){
    if (test) yes
    else no
}
