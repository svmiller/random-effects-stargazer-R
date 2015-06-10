library(lme4)
library(stargazer)
data(cake)

insertrow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

summary(M1 <- lmer(angle ~ temp + (1 | replicate) + (1|recipe:replicate), cake, REML= FALSE))
summary(M2 <- lmer(angle ~ factor(temperature) + (1 | replicate) + (1|recipe:replicate), cake, REML= FALSE))



Tables <- stargazer(M1, M2, style="ajps", title="An Illustrative Model Using Cake Data",  dep.var.labels.include = FALSE, 
          covariate.labels=c( "Temperature (Continuous)",  "Temperature (Factor $<$ 185)", "Temperature (Factor $<$ 195)", "Temperature (Factor $<$ 205)", "Temperature (Factor $<$ 215)", "Temperature (Factor $<$ 225)")
)

Tables <- as.data.frame(Tables)
Tables$Tables <- as.character(Tables$Tables)
Tables

# Find where you want to put in the random effect. In our case, this is right after the last fixed effect. Line: 25.

r <- 25

# Create some standard rows to add.
randomeffect <- "{\\bf Random Effect} & & \\\\"
hline <- "\\hline"
newline <- "\\\\"

Tables <- insertrow(Tables, hline, r)
Tables <- insertrow(Tables,randomeffect,r+1)
Tables <- insertrow(Tables,hline,r+2)
# Type Tables into the terminal to see where it put it.

# Know your random effect groupings. In this case, they're "replicate" and "recipe:replicate". 
# Know their order too. Recipe:replicate comes first. Let's get the number of groupings in each random effect.

num.recipe.replicate <- sapply(ranef(M1),nrow)[1]
num.replicate <- sapply(ranef(M1),nrow)[2]

# Note: if this varies with the different models you're running, grab them (i.e. sub out M1 with M2, M3, or whatever)

# Let's get the standard deviation of the random effect now.
stddev.M1.recipe.replicate <- attributes(VarCorr(M1)$"recipe:replicate")$stddev
stddev.M1.replicate <- attributes(VarCorr(M1)$replicate)$stddev

stddev.M2.recipe.replicate <- attributes(VarCorr(M2)$"recipe:replicate")$stddev
stddev.M2.replicate <- attributes(VarCorr(M2)$replicate)$stddev

# Start creating some rows to add soon.

number.of.recipe.replicate <- paste("\\# of Recipe:Replicate & ", num.recipe.replicate, "&", num.recipe.replicate, "\\\\")
stddev.recipe.replicate <- paste("Recipe:Replicate Standard Deviation & ", round(stddev.M1.recipe.replicate, 3), "&", round(stddev.M2.recipe.replicate, 3), "\\\\")

number.of.replicate <-paste("\\# of Replicate & ", num.replicate, "&", num.replicate, "\\\\")
stddev.replicate <- paste("Replicate Standard Deviation & ", round(stddev.M1.replicate, 3), "&", round(stddev.M2.replicate, 3), "\\\\")

# Let's add them now.

Tables <- insertrow(Tables,number.of.recipe.replicate,r+3)
Tables <- insertrow(Tables,stddev.recipe.replicate,r+4)
Tables <- insertrow(Tables,newline,r+5)
Tables <- insertrow(Tables,number.of.replicate,r+6)
Tables <- insertrow(Tables,stddev.replicate,r+7)

write.table(Tables,file="tables.tex",sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)
