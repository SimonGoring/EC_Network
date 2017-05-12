USING PERIODIC COMMIT 100
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line
WITH DISTINCT line

// Since we add the AwardID along the relationship paths, we need to do the awards first.
// We know that all awards have unique AwardIDs:
MATCH (award:Award {AwardID: line.AwardID})

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

MERGE (pgrele:Program  {Code:line.`ProgramElement.Code`})
	ON CREATE SET pgrele.Text = line.`ProgramElement.Text`
MERGE (pgrref:Program {Code:line.`ProgramReference.Code`})
	ON CREATE SET pgrref.Text = line.`ProgramReference.Text`

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
