USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line

WITH DISTINCT line.`Organization.Division` AS Division, 
     	      line.`Organization.Code` AS Code, 
              line.`Organization.Directorate` AS Directorate

MERGE (direct:Directorate {Code: Code})
	ON CREATE SET direct.Directorate = Directorate

MERGE (division:Division {Division: Division});

// Load the Program Elements

USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line

WITH DISTINCT line.`ProgramElement.Code` as EleCode, line.`ProgramElement.Text` AS EleText

MERGE (pgrele:Program  {Code:EleCode})
	ON CREATE SET pgrele.Text = EleText;

// Load the Program Reference

USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line

WITH DISTINCT line.`ProgramReference.Code` as RefCode, line.`ProgramReference.Text` AS RefText

MERGE (pgrref:Program {Code:RefCode})
	ON CREATE SET pgrref.Text = RefText;

USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line

WITH line.AwardID                    AS AwardID,
	 line.`Organization.Division`    AS Division, 
     line.`Organization.Code`        AS Code,
     line.`Organization.Directorate` AS Directorate,
     line.`ProgramElement.Code`      AS EleCode, 
     line.`ProgramElement.Text`      AS EleText,
     line.`ProgramReference.Code`    AS RefCode, 
     line.`ProgramReference.Text`    AS RefText

// Since we add the AwardID along the relationship paths, we need to do the awards first.
// We know that all awards have unique AwardIDs:
MATCH (award:Award        {AwardID:  AwardID})
MATCH (division:Division  {Division: Division})
MATCH (direct:Directorate {Code: Code, Directorate: Directorate})
MATCH (pgrref:Program     {Code: RefCode, Text: RefText})
MATCH (pgrele:Program     {Code: EleCode, Text: EleText})

// Add the directorates and programs:

MERGE (direct)-[dwn:Within]->(division)
	ON CREATE SET dwn.AwardID = [] + AwardID
	ON MATCH SET dwn.AwardID = CASE WHEN NOT AwardID IN dwn.AwardID THEN dwn.AwardID + AwardID
		                       ELSE AwardID END

MERGE (award)-[fby:Funded_by]->(direct)
	ON CREATE SET fby.AwardID = [] + AwardID
	ON MATCH SET fby.AwardID = CASE WHEN NOT AwardID IN fby.AwardID THEN fby.AwardID + AwardID
	                           ELSE fby.AwardID END

MERGE (award)-[fpg:Funded_by]->(pgrele)
	ON CREATE SET fpg.AwardID = [] + AwardID
	ON MATCH SET fpg.AwardID = CASE WHEN NOT AwardID IN fpg.AwardID THEN fpg.AwardID + AwardID
		                                 ELSE fpg.AwardID END

MERGE (award)-[rpg:Referenced_by]->(pgrref)
	ON CREATE SET rpg.AwardID = [] + AwardID
	ON MATCH SET rpg.AwardID = CASE WHEN NOT AwardID IN rpg.AwardID THEN rpg.AwardID + AwardID
		                                 ELSE rpg.AwardID END

MERGE (pgrref)-[pgmof:Program_of]->(division)
	ON CREATE SET pgmof.AwardID = [] + AwardID
	ON MATCH SET pgmof.AwardID = CASE WHEN NOT AwardID IN pgmof.AwardID THEN pgmof.AwardID + AwardID
		                                 ELSE pgmof.AwardID END

MERGE (pgrele)-[pgmofb:Program_of]->(division)
	ON CREATE SET pgmofb.AwardID = [] + AwardID
	ON MATCH SET pgmofb.AwardID = CASE WHEN NOT AwardID IN pgmofb.AwardID THEN pgmofb.AwardID + AwardID
		                                 ELSE pgmofb.AwardID END;
