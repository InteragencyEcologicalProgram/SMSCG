#now let's really iron out those statistics

source("data manip FMWT.R")

dsznb1 = zeroinfl(catch~ Station + Operating2+julian + 
                     ECscaled + index, 
                  dist = "negbin", data = FMWT_DSmg4a, na.action = "na.fail")

dreznb = dredge(dsznb1)
dsznb1best = zeroinfl(catch~ Operating2+julian + 
                        ECscaled + index, 
                      dist = "negbin", data = FMWT_DSmg4a, na.action = "na.fail")
summary(dsznb1best)
visreg(dsznb1best)

#try it with the scaled julian day and index too
dsznb2 = zeroinfl(catch~ Station + Operating2+julianscaled + 
                    ECscaled + Indexscaled, 
                  dist = "negbin", data = FMWT_DSmg4a, na.action = "na.fail")

dreznb2 = dredge(dsznb2)
dsznb2best = zeroinfl(catch~ Operating2+julianscaled + 
                        ECscaled + Indexscaled, 
                      dist = "negbin", data = FMWT_DSmg4a, na.action = "na.fail")
summary(dsznb2best)
visreg(dsznb1best)
