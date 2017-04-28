USING PERIODIC COMMIT 100
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line
WITH DISTINCT line

// Since we add the AwardID along the relationship paths, we need to do the awards first.
// We know that all awards have unique AwardIDs:
MERGE (award:Award {AwardID: line.AwardID})
ON CREATE SET award.AwardTitle  = UPPER(line.AwardTitle),
    award.AwardAmount           = TOINT(line.AwardAmount),
    award.ARRAAmount            = TOINT(line.ARRAAmount),
    award.AwardInstrument       = line.`AwardInstrument.Value`,
    award.AbstractNarration     = line.AbstractNarration;
