// Load the table.  It needs to be in /var/lib/neo4j/import/
USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line

WITH DISTINCT line.`Investigator.EmailAddress` AS Email,
    line.`Investigator.FirstName` AS FirstName,
    line.`Investigator.LastName`  AS LastName,
    line.`Investigator.StartDate` AS StartDate,
    line.`Investigator.EndDate`   AS EndDate

    // We've got a bit of a problem with names.  I was using the email address as a key, but there's
    // a lot of None email addresses.  We're just going to lump everyone with the same name together
    // and then figure out a way to split them up.
    CASE
    WHEN Email = 'None' THEN UPPER(FirstName + '_' + LastName)
    ELSE UPPER(Email) END as emailAddress,
    
    // The `person` has a fairly limited and there's a problem here that
    // common names are going to get duplicated.
    MERGE (person:Person {EmailAddress: emailAddress})
    ON CREATE SET person.FirstName = line.`Investigator.FirstName`,
                  person.LastName  = line.`Investigator.LastName`;

// Since we add the AwardID along the relationship paths, we need to do the awards first.
// We know that all awards have unique AwardIDs:
USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line

    CASE
    WHEN line.`Investigator.EmailAddress` = 'None' THEN 
         UPPER(line.`Investigator.FirstName` + '_' + line.`Investigator.LastName`)
    ELSE UPPER(line.`Investigator.EmailAddress`) END as emailAddress,
    CASE
    WHEN line.`Investigator.StartDate` = 'None' 
         OR line.`Investigator.StartDate` = null 
         OR line.`Investigator.StartDate` = 'NA' THEN [0,0,0]
    ELSE SPLIT(line.`Investigator.StartDate`, '/')  END as invStartDate,
     
    CASE
    WHEN line.`Investigator.EndDate` = 'None' 
         OR line.`Investigator.EndDate` = null 
         OR line.`Investigator.EndDate` = 'NA' THEN [0,0,0]
    ELSE SPLIT(line.`Investigator.EndDate`, '/')  END as invEndDate

MATCH (award:Award) WHERE award.AwardID = line.AwardID
MATCH (institution:Institution) WHERE institution.Name = UPPER(line.`Institution.Name`)

// Investigators sometimes have end dates that predate the end of the award.
// For now I'm going to stick this in the relationship.  I'm not really sure
// this is the best way to do it:
MERGE (person)<-[awd:Awarded_to]-(award)
ON CREATE SET awd.RoleCode = line.`Investigator.RoleCode`,
              awd.StartDate = invStartDate,
              awd.EndDate   = invEndDate

MERGE (person)-[emp:Employed_by]->(institution)
  ON CREATE SET emp.AwardID = [] + line.AwardID
  ON MATCH SET  emp.AwardID = emp.AwardID + line.AwardID

MERGE (po:ProgramOfficer {Name: line.`ProgramOfficer.SignBlockName`})

// Building Relationships:
// This whole set of relationships gets a basic pass, where we go through 
MERGE (po)-[mgd:Handled]->(award);