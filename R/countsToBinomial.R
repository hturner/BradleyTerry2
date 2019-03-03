#' Convert Contingency Table of Wins to Binomial Counts
#' 
#' Convert a contingency table of wins to a four-column data frame containing
#' the number of wins and losses for each pair of players.
#' 
#' 
#' @param xtab a contingency table of wins cross-classified by \dQuote{winner}
#' and \dQuote{loser}
#' @return A data frame with four columns \item{player1 }{ the first player in
#' the contest. } \item{player2 }{ the second player in the contest. }
#' \item{win1 }{ the number of times `player1` won. } \item{win2 }{ the
#' number of times `player2` won. }
#' @author Heather Turner
#' @seealso [BTm()]
#' @keywords models
#' @examples
#' 
#' ########################################################
#' ##  Statistics journal citation data from Stigler (1994)
#' ##  -- see also Agresti (2002, p448)
#' ########################################################
#' citations
#' 
#' ## Convert frequencies to success/failure data
#' citations.sf <- countsToBinomial(citations)
#' names(citations.sf)[1:2] <- c("journal1", "journal2")
#' citations.sf
#' 
#' @importFrom gtools combinations
#' @export
countsToBinomial <- function(xtab) {
    ## make square if necessary
    if (nrow(xtab) != ncol(xtab) || !all(rownames(xtab) == colnames(xtab))) {
        dat <- as.data.frame(xtab)
        lev <- union(rownames(xtab), colnames(xtab))
        dat[,1] <- factor(dat[,1], levels = lev)
        dat[,2] <- factor(dat[,2], levels = lev)
        xtab <- tapply(dat[,3], dat[1:2], sum)
        xtab[is.na(xtab)] <- 0
    }
    ##assumes square
    players <- rownames(xtab)
    comb <- combinations(nrow(xtab), 2)
    won <- xtab[comb]
    lost <- t(xtab)[comb]
    res <- !(won == 0 & lost == 0)
    player1 <- factor(players[comb[,1]], levels = players)[res]
    player2 <- factor(players[comb[,2]], levels = players)[res]
    data.frame(player1, player2, win1 = won[res], win2 = lost[res])
}

