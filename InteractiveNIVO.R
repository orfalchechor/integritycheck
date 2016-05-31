groupdiff <- function(x) {
   x
}

loaddat <- function(file="acc-data.csv") {
   dat <<- read.csv( file )
}

loadvlt <- function(file="acc.csv") {
   vlt <<- read.csv( file )
}

getgroups <- function( x ) {
   if ( 'nivoType' %in% colnames(x) ) {
      x[grep("groupOfNames",x$objectClass),]
   }
}

getmembers <- function( x, from='all' ) {
   if ( 'member' %in% colnames(x) ) {
      if ( from == 'all' ) {
         t <- as.data.frame( unlist(strsplit( as.character( x$member ), ';' ) ) )
         colnames(t) <- 'member'
         t
      } else {
         t <- as.data.frame( unlist(strsplit( as.character( x[x$nivoKey == from,]$member ), ';' )))
         colnames(t)
         t
      }
   }
}

getkey <- function( x, y ) {
   if ( 'dn' %in% colnames(y) ) {
      x[,1] <- factor(x[,1], levels=levels(y$dn))
      sapply( x[,1], function(z) as.character(y[y$dn == z,]['nivoKey'] ))
  }
}

getgroupkey <- function( x, y=dat, by='name' ) {
   if ( by == 'name' ) {
      y[y$nivoCourseName == x | y$nivoClassName == x | y$nivoDescription == x,]['nivoKey']
   }
}

getpmembers <- function( from, x ) {
   t <- as.matrix(x[x$nivoKey == from,][c('nivoType','dn')])
   with( x, x[eval(as.symbol(t[1])) == t[2], ]$nivoKey)
}