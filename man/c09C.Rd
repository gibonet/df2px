\name{c09C}
\alias{c09C}
\docType{data}
\title{
Salario mensile lordo standardizzato nel settore privato (in franchi), secondo il sesso, la posizione nella professione e lo statuto, in Ticino, dal 2002 al 2014
}
\description{
Statistiche sulla distribuzione dei salari nel settore privato in Ticino, secondo il sesso, la posizione nella professione e lo statuto, dal 2002 al 2014. Contiene tutti gli incroci possibili delle variabili e, per ognuno di questi, alcune statistiche sulla distribuzione dei salari del sottogruppo in questione.
}
\usage{data("c09C")}
\format{
  A data frame with 735 observations on the following 11 variables.
  \describe{
    \item{\code{anno}}{a factor with levels \code{2002} \code{2004} \code{2006} \code{2008} \code{2010} \code{2012} \code{2014}}
    \item{\code{posizione}}{a factor with levels \code{Totale} \code{Quadri superiori e medi} \code{Quadri inferiori} \code{Responsabile esecuzione lavori} \code{Senza funzione di quadro}}
    \item{\code{sesso}}{a factor with levels \code{Totale} \code{Uomini} \code{Donne}}
    \item{\code{statuto}}{a factor with levels \code{Totale} \code{Svizzeri} \code{Domiciliati (Cat. C)} \code{Dimoranti (Cat. B)} \code{Frontalieri (Cat. G)} \code{Dimoranti temporanei (Cat. L)} \code{Altri}}
    \item{\code{N_etp}}{a numeric vector}
    \item{\code{p10}}{a numeric vector}
    \item{\code{p25}}{a numeric vector}
    \item{\code{p50}}{a numeric vector}
    \item{\code{p75}}{a numeric vector}
    \item{\code{p90}}{a numeric vector}
    \item{\code{info}}{a character vector}
  }
}

\examples{
data(c09C)
str(c09C)
head(c09C)
}
\keyword{datasets}
