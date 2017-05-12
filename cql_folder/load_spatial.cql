USING PERIODIC COMMIT 100
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line
WITH DISTINCT line

// Since we add the AwardID along the relationship paths, we need to do the awards first.
// We know that all awards have unique AwardIDs:
MATCH (award:Award {AwardID: line.AwardID})

MERGE ( state:State { Name: UPPER(line.`Institution.StateName`) })
    ON CREATE SET state.Code = UPPER(line.`Institution.StateCode`)

MERGE (country:Country { Name: UPPER(line.`Institution.CountryName`) } )

MERGE (institution:Institution { Name: UPPER(line.`Institution.Name`) } )
ON CREATE SET institution.PhoneNumber   = line.`Institution.PhoneNumber`,
              institution.CityName      = UPPER(line.`Institution.CityName`),
              institution.StreetAddress = UPPER(line.`Institution.StreetAddress`),
              institution.ZipCode       = line.`Institution.ZipCode`

MERGE (institution)-[is:In_State]->(state)
    ON CREATE SET is.AwardID = [] + line.AwardID
    ON MATCH SET is.AwardID = is.AwardID + line.AwardID

MERGE (state)-[ic:In_Country]->(country)
    ON CREATE SET ic.AwardID = [] + line.AwardID
    ON MATCH SET ic.AwardID = ic.AwardID + line.AwardID

MERGE (award)-[ai:Administered_by]->(institution)
    ON CREATE SET ai.AwardID = [] + line.AwardID
    ON MATCH SET ai.AwardID = ai.AwardID + line.AwardID;