USING PERIODIC COMMIT 100
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line
WITH DISTINCT line,
	// Clean up dates.  The default date is going to be [0,0,0].
	// There's a set of potential values, NAs, 'None' and `null` values.
	
	CASE
    WHEN line.AwardEffectiveDate = 'None' 
         OR line.AwardEffectiveDate = null 
         OR line.AwardEffectiveDate = 'NA' THEN [0,0,0]
    ELSE SPLIT(line.AwardEffectiveDate, '/')  END as effectiveDate,
    
    CASE
    WHEN line.AwardExpirationDate = 'None' 
         OR line.AwardExpirationDate = null
         OR line.AwardExpirationDate = 'NA' THEN [0,0,0]
    ELSE SPLIT(line.AwardExpirationDate, '/')  END as expirationDate,
    
    CASE
    WHEN line.MinAmdLetterDate = 'None' 
         OR line.MinAmdLetterDate = null 
         OR line.MinAmdLetterDate = 'NA' THEN [0,0,0]
    ELSE SPLIT(line.MinAmdLetterDate, '/')  END as minAmdLetterDate,
    
    CASE
    WHEN line.MaxAmdLetterDate = 'None' 
         OR line.MaxAmdLetterDate = null
         OR line.MaxAmdLetterDate = 'NA' THEN [0,0,0]
    ELSE SPLIT(line.MaxAmdLetterDate, '/')  END as maxAmdLetterDate

MATCH (award:Award {AwardID: line.AwardID})

// Dates for awards - 
// Here we do two things, we first make the matches, and then we add the
// award number to the path's relationship element.
// 
// Start date:
// As with others, this has to be done year, then month because otherwise the unique
// constraint of the award number screws things up.
MERGE (year:Year {Year: TOINT(effectiveDate[2]) } )
MERGE (month:Month {Month: TOINT(effectiveDate[0]) } )

MERGE (award)-[yst:Year_Started]-(year)
	ON CREATE SET yst.AwardID = [] + line.AwardID
	ON MATCH SET yst.AwardID = CASE WHEN NOT line.AwardID IN yst.AwardID THEN yst.AwardID + line.AwardID
	                                 ELSE yst.AwardID END

MERGE (year)-[mst:Month_Started]-(month)
	ON CREATE SET mst.AwardID = [] + line.AwardID
	ON MATCH SET  mst.AwardID = CASE WHEN NOT line.AwardID IN mst.AwardID THEN mst.AwardID + line.AwardID
	                                 ELSE mst.AwardID END

// Dates for awards - End date:
MERGE (year_end:Year {Year: TOINT(expirationDate[2]) } )
MERGE (month_end:Month {Month: TOINT(expirationDate[0]) } )

MERGE (award)-[yen:Year_Expired]-(year_end)
	ON CREATE SET yen.AwardID = [] + line.AwardID
	ON MATCH SET yen.AwardID = CASE WHEN NOT line.AwardID IN yen.AwardID THEN yen.AwardID + line.AwardID
	                                 ELSE yen.AwardID END

MERGE (year_end)-[men:Month_Expired]-(month_end)
	ON CREATE SET men.AwardID = [] + line.AwardID
	ON MATCH SET men.AwardID = CASE WHEN NOT line.AwardID IN men.AwardID THEN men.AwardID + line.AwardID
	                                 ELSE men.AwardID END

// Dates for awards - Amendment Letter min date:
MERGE (year_amdS:Year {Year: TOINT(minAmdLetterDate[2]) } )
MERGE (month_amdS:Month {Month: TOINT(minAmdLetterDate[0]) } )

MERGE (award)-[yam:Year_Amended_Min]-(year_amdS)
	ON CREATE SET yam.AwardID = [] + line.AwardID
	ON MATCH SET yam.AwardID = CASE WHEN NOT line.AwardID IN yam.AwardID THEN yam.AwardID + line.AwardID
	                                 ELSE yam.AwardID END

MERGE (year_amdS)-[mam:Month_Amended_Min]-(month_amdS)
	ON CREATE SET mam.AwardID = [] + line.AwardID
	ON MATCH SET mam.AwardID = CASE WHEN NOT line.AwardID IN mam.AwardID THEN mam.AwardID + line.AwardID
	                                 ELSE mam.AwardID END

// These are ammendment dates.
MERGE (year_amdE:Year {Year: TOINT(maxAmdLetterDate[2])})
MERGE (month_amdE:Month {Month: TOINT(maxAmdLetterDate[0])})

MERGE (award)-[ymx:Year_Amended_Max]-(year_amdE)
	ON CREATE SET ymx.AwardID = [] + line.AwardID
	ON MATCH SET ymx.AwardID = CASE WHEN NOT line.AwardID IN ymx.AwardID THEN ymx.AwardID + line.AwardID
	                                 ELSE ymx.AwardID END

MERGE (year_amdE)-[mmx:Month_Amended_Max]-(month_amdE)
	ON CREATE SET mmx.AwardID = [] + line.AwardID
	ON MATCH SET mmx.AwardID = CASE WHEN NOT line.AwardID IN mmx.AwardID THEN mmx.AwardID + line.AwardID
	                                 ELSE mmx.AwardID END;
