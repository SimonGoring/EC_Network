USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS
FROM "file:///award_file.csv" AS line

// Cleaning up duplicates first saves time.
WITH DISTINCT line.AwardID AS AwardID, UPPER(line.AwardTitle) AS AwardTitle, 
              TOINT(line.AwardAmount) AS AwardAmount, TOINT(line.ARRAAmount) AS ARRAAmount,
              line.`AwardInstrument.Value` AS AwardInstrument, line.AbstractNarration AS AbstractNarration

MERGE (award:Award {AwardID: AwardID})
ON CREATE SET award.AwardTitle  = AwardTitle,
    award.AwardAmount           = AwardAmount,
    award.ARRAAmount            = ARRAAmount,
    award.AwardInstrument       = AwardInstrument,
    award.AbstractNarration     = AbstractNarration;