calculateMode <- function(VAR){
 	   value <- table(VAR) == max(table(VAR))
  	   return (names(table(VAR))[value])
 	}

phoneRecords$PHONE_PLAN[phoneRecords$PHONE_PLAN == ""] <- calculateMode(phoneRecords$PHONE_PLAN)
phoneRecords$EDUCATION[phoneRecords$ EDUCATION == ""] <- calculateMode(phoneRecords$ EDUCATION)