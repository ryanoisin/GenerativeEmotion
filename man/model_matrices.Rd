\name{model_matrices}
\alias{model_matrices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to create model matrices for the generative model for emotion dynamics
}
\description{The core function which creates the components of the generative model for emotin dynamics as described in "Towards a Generative Model for Emotion Dynamics" (link). This function takes as input various characteristics of the generative model, such as the number of situations, emotions, arguments which define the transition matrix, and the mapping from situations to emotions. It outputs a number of core model matrices which can be used to generate data from the model, using the `datagen` function. It also outputs a number of additional matrices which may be helpful for sensitivity analysis, see Details.
}
\usage{
model_matrices(s = 5, p = 6, stay_prob = 0.6, switch_prob = 0.4, 
means = rbind(rep(0, p), c(1, 0.5, 1, 0, 0, 0), 
c(0.5, 1, 0.5, 0, 0, 0), c(0, 0, 0, 1, 0.5, 1), 
c(0, 0, 0, 0.5, 1, 0.5)), 
theta = sqrt(0.25), tmat = NULL)
}
\arguments{
  \item{s}{number of "situations". Defaults to 5
}
  \item{p}{number of "emotions". Defaults to 6
}
  \item{stay_prob}{staying probability, uniform for all situations
}
  \item{switch_prob}{switching probabilitity, equal for all transitions
}
  \item{means}{s x p matrix of means for each emotion (columns) in each situation (rows)
}
  \item{theta}{ variance of emotions in each situation
}
  \item{tmat}{Optional; custom transition matrix. If specified, stay_prob and switch_prob ignored
}
}
\details{
%%  
}
\value{
Returns a list of matrices and arguments which define the generative model. The output can then be passed to `datagen` to simulate data from the model. 
The core model lists are 
\item{s}{ as in input}
\item{p}{ as in input}
\item{zstate}{ which defines the neutral state}, 
\item{Lambda}{ the mapping from situations to emotions in means (i.e. means of each emotion in each state}
\item{Theta}{the variance-covariance matrix of emotions (uniform across all states)}
\item{tmat}{the markov transition matrix between situations}

All other matrices are additional output not necessary for data generation, but which provide additional info which can be used, e.g., for sensitivty analysis and comparison to empirical phenomena. 
\item{Pi}{the stationary probabilities for each situation}
\item{Sigma}{the model-implied lag-0 covariance matrix of the emotions}
\item{Gamma}{the model-implied lag-1 autocovariance matrix of the emotions}
\item{Phi}{the model-implied matrix of lag-1 regression coefficients between emotions (as produced by a VAR(1) model)}
\item{Omega}{the model-implied lag-1 residual covariance matrix}
\item{Omega_pc}{the model-implied lag-1 residual partial correlation matrix}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Oisin Ryan <o.ryan@uu.nl>; Jonas Haslbeck <jonashaslbeck@gmail.com>; Fabian Dablander <dablander.fabian@gmail.com> 
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{datagen}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

\dontrun{
# basic model specification 
s <- 5
p <- 6

# define means in each state
 a <- 1
 b <- .5

#  mean matrix
 means <- rbind(rep(0,p),
                c(a, b, a,  0  ,  0  ,  0  ),
                c(a, b, b,  0  ,  0  ,  0  ),
                c( 0  ,  0  ,  0  , a, b, a),
                c( 0  ,  0  ,  0  , a, b, b))

# define transition matrix probabilities
stay_prob <- .6
switch_prob <- (1-stay_prob)/(s-1)

# define residual variance of all variables in all states
theta <- sqrt(.25)


# get model matrices
mm <- model_matrices(s = s,
                     p = p,
                     stay_prob = stay_prob,
                     switch_prob = switch_prob,
                     means = means,
                     theta = theta #, tmat = tmat
)

simdata <- datagen(mm = mm, n = 100, sout = FALSE)
plot(simdata[,1], type = "b", xlab = "Time", ylab = "Emotion")

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
