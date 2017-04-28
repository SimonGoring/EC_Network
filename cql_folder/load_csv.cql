// assert correct line count
// I had to move the file into /var/lib/neo4j/import/
//LOAD CSV FROM "file:///test_load.csv" AS line
//RETURN count(*);

// check first few raw lines
// LOAD CSV FROM "file:///test_load.csv" AS line WITH line
// RETURN line
// LIMIT 5;

// check first 5 line-sample with header-mapping
// LOAD CSV WITH HEADERS FROM "file:///test_load.csv" AS line WITH line
// RETURN line
// LIMIT 5;

// export FILEPATH="file:///test_load.csv"

// Uniqueness constraints.
CREATE CONSTRAINT ON (i:Institution) ASSERT i.Name IS UNIQUE;
CREATE CONSTRAINT ON (s:State) ASSERT s.Name IS UNIQUE;
CREATE CONSTRAINT ON (c:Country) ASSERT c.Name IS UNIQUE;
CREATE CONSTRAINT ON (y:Year) ASSERT y.Year IS UNIQUE;
CREATE CONSTRAINT ON (m:Month) ASSERT m.Month IS UNIQUE;
CREATE CONSTRAINT ON (a:Award) ASSERT a.AwardID IS UNIQUE;
CREATE CONSTRAINT ON (p:ProgramOfficer) ASSERT p.SignBlockName IS UNIQUE;

// Load the table.  It needs to be in /var/lib/neo4j/import/
USING PERIODIC COMMIT 500
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line
WITH DISTINCT line,
	// Clean up dates.  The default date is going to be [0,0,0].
	CASE
    WHEN line.AwardEffectiveDate = 'None' THEN [0,0,0]
    ELSE SPLIT(line.AwardEffectiveDate, '/')  END as effectiveDate,
    CASE
    WHEN line.AwardExpirationDate = 'None' THEN [0,0,0]
    ELSE SPLIT(line.AwardExpirationDate, '/')  END as expirationDate,
    CASE
    WHEN line.MinAmdLetterDate = 'None' THEN [0,0,0]
    ELSE SPLIT(line.MinAmdLetterDate, '/')  END as minAmdLetterDate,
    CASE
    WHEN line.MaxAmdLetterDate = 'None' THEN [0,0,0]
    ELSE SPLIT(line.MaxAmdLetterDate, '/')  END as maxAmdLetterDate,
    CASE
    WHEN line.`Investigator.StartDate` = 'None' THEN [0,0,0]
    ELSE SPLIT(line.`Investigator.StartDate`, '/')  END as invStartDate,
    CASE
    WHEN line.`Investigator.EndDate` = 'None' THEN [0,0,0]
    ELSE SPLIT(line.`Investigator.EndDate`, '/')  END as invEndDate,
    // We've got a bit of a problem with names.  I was using the email address as a key, but there's
    // a lot of None email addresses.  We're just going to lump everyone with the same name together
    // and then figure out a way to split them up.
    CASE
    WHEN line.`Investigator.EmailAddress` = 'None' THEN line.`Investigator.FirstName` + line.`Investigator.LastName`
    ELSE line.`Investigator.EmailAddress` END as emailAddress

// Since we add the AwardID along the relationship paths, we need to do the awards first.
// We know that all awards have unique AwardIDs:
MERGE (award:Award {AwardID: line.AwardID})
ON CREATE SET award.AwardTitle  = UPPER(line.AwardTitle),
    award.AwardAmount           = TOINT(line.AwardAmount),
    award.ARRAAmount            = TOINT(line.ARRAAmount),
    award.AwardInstrument       = line.`AwardInstrument.Value`,
    award.AbstractNarration     = line.AbstractNarration

// We ignore ON MERGE.  All rows in the table will have the same values.

// Now all the geographical things:
// 
MERGE ( state:State { Name: UPPER(line.`Institution.StateName`) })
	ON CREATE SET state.Code = UPPER(line.`Institution.StateCode`)

MERGE (country:Country { Name: UPPER(line.`Institution.CountryName`) } )

MERGE (institution:Institution { Name: UPPER(line.`Institution.Name`) } )
ON CREATE SET institution.PhoneNumber   = line.`Institution.PhoneNumber`,
			  institution.CityName      = UPPER(line.`Institution.CityName`),
			  institution.StreetAddress = UPPER(line.`Institution.StreetAddress`),
			  institution.ZipCode       = line.`Institution.ZipCode`

// The `person` has a fairly limited and there's a problem here that
// common names are going to get duplicated.
MERGE (person:Person {EmailAddress: emailAddress})
ON CREATE SET person.FirstName = line.`Investigator.FirstName`,
              person.LastName  = line.`Investigator.LastName`

// Investigators sometimes have end dates that predate the end of the award.
// For now I'm going to stick this in the relationship.  I'm not really sure
// this is the best way to do it:
MERGE (person)<-[awd:awarded_to]-(award)
ON CREATE SET awd.RoleCode = line.`Investigator.RoleCode`,
              awd.StartDate = invStartDate,
              awd.EndDate   = invEndDate

MERGE (po:ProgramOfficer {Name: line.`ProgramOfficer.SignBlockName`})

// Building Relationships:
// This whole set of relationships gets a basic pass, where we go through 
MERGE (po)-[mgd:Handled]->(award)
	ON CREATE SET mgd.AwardID = [] + line.AwardID
	ON MATCH SET  mgd.AwardID = mgd.AwardID + line.AwardID

MERGE (person)-[emp:Employed_by]->(institution)
	ON CREATE SET emp.AwardID = [] + line.AwardID
	ON MATCH SET  emp.AwardID = emp.AwardID + line.AwardID

MERGE (institution)-[is:In_State]->(state)
	ON CREATE SET is.AwardID = [] + line.AwardID
	ON MATCH SET is.AwardID = is.AwardID + line.AwardID

MERGE (state)-[ic:In_Country]->(country)
	ON CREATE SET ic.AwardID = [] + line.AwardID
	ON MATCH SET ic.AwardID = ic.AwardID + line.AwardID

MERGE (award)-[ai:Administered_by]->(institution)
	ON CREATE SET ai.AwardID = [] + line.AwardID
	ON MATCH SET ai.AwardID = ai.AwardID + line.AwardID

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

// Dates for awards - Amendment letter max date:
MERGE (year_amdE:Year {Year: TOINT(maxAmdLetterDate[2])})
MERGE (month_amdE:Month {Month: TOINT(maxAmdLetterDate[0])})

MERGE (award)-[ymx:Year_Amended_Max]-(year_amdE)
	ON CREATE SET ymx.AwardID = [] + line.AwardID
	ON MATCH SET ymx.AwardID = CASE WHEN NOT line.AwardID IN ymx.AwardID THEN ymx.AwardID + line.AwardID
	                                 ELSE ymx.AwardID END

MERGE (year_amdE)-[mmx:Month_Amended_Max]-(month_amdE)
	ON CREATE SET mmx.AwardID = [] + line.AwardID
	ON MATCH SET mmx.AwardID = CASE WHEN NOT line.AwardID IN mmx.AwardID THEN mmx.AwardID + line.AwardID
	                                 ELSE mmx.AwardID END

// Add the directorates and programs:
MERGE (direct:Directorate {Code: line.`Organization.Code`})
	ON CREATE SET direct.`Directorate` = line.`Organization.Directorate`

MERGE (division:Division {Division: line.`Organization.Division`})

MERGE (direct)-[dwn:Within]-(division)
	ON CREATE SET dwn.AwardID = [] + line.AwardID
		ON MATCH SET dwn.AwardID = CASE WHEN NOT line.AwardID IN dwn.AwardID THEN dwn.AwardID + line.AwardID
		                                 ELSE dwn.AwardID END

MERGE (award)-[fby:Funded_by]-(direct)
	ON CREATE SET fby.AwardID = [] + line.AwardID
	ON MATCH SET fby.AwardID = CASE WHEN NOT line.AwardID IN fby.AwardID THEN fby.AwardID + line.AwardID
	                                 ELSE fby.AwardID END

MERGE (pgrele:Program  {Code:line.`ProgramElement.Code`, Text:line.`ProgramElement.Text`})
MERGE (pgrref:Program {Code:line.`ProgramReference.Code`, Text:line.`ProgramReference.Text`})

MERGE (award)-[fpg:Funded_by]-(pgrele)
	ON CREATE SET fpg.AwardID = [] + line.AwardID
		ON MATCH SET fpg.AwardID = CASE WHEN NOT line.AwardID IN fpg.AwardID THEN fpg.AwardID + line.AwardID
		                                 ELSE fpg.AwardID END

MERGE (award)-[rpg:Referenced_by]-(pgrref)
	ON CREATE SET rpg.AwardID = [] + line.AwardID
		ON MATCH SET rpg.AwardID = CASE WHEN NOT line.AwardID IN rpg.AwardID THEN rpg.AwardID + line.AwardID
		                                 ELSE rpg.AwardID END

MERGE (pgrref)-[pgmof:Program_of]-(division)
	ON CREATE SET pgmof.AwardID = [] + line.AwardID
		ON MATCH SET pgmof.AwardID = CASE WHEN NOT line.AwardID IN pgmof.AwardID THEN pgmof.AwardID + line.AwardID
		                                 ELSE pgmof.AwardID END

MERGE (pgrele)-[pgmofb:Program_of]-(division)
	ON CREATE SET pgmofb.AwardID = [] + line.AwardID
		ON MATCH SET pgmofb.AwardID = CASE WHEN NOT line.AwardID IN pgmofb.AwardID THEN pgmofb.AwardID + line.AwardID
		                                 ELSE pgmofb.AwardID END;
