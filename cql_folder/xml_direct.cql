// A loader, by individual file, for XML data to
// a neo4j database.  The `filename` is passed in as
// an environment variable from the bash script `build_db_xml.sh`.

CALL apoc.load.xmlSimple(url) YIELD value

// Now set all the variables we'll use in the script:

WITH value._Award AS award

UNWIND (CASE award._ProgramReference IS NULL 
	    WHEN true then {_Code:{_text:'None'}, _Text:{_text:'None'}} 
	    else award._ProgramReference end) AS ProgRef

UNWIND (CASE award._Investigator IS NULL 
	    WHEN true 
	    then {_FirstName: {_text:"None"}, 
	                    _LastName:{_text:"None"},
	                    _RoleCode:{_text:"None"},
	                    _EmailAddress:{_text:"NONE"}}
	    else award._Investigator end) AS Investigator

UNWIND (CASE 
	    award._ProgramElement  IS NULL 
	    WHEN true then {_Code:{_text:'None'}, _Text:{_text:'None'}}
	    else award._ProgramElement end) AS ProgEle


WITH award._AwardID._text                                     AS AwardID,
     award._AwardTitle._text                                  AS AwardTitle,
     TOINT(award._AwardAmount._text)                          AS AwardAmount, 
     TOINT(award._ARRAAmount._text)                           AS ARRAAmount,
     award._AwardInstrument._Value._text                      AS AwardInstrument,
     award._AbstractNarration._text                           AS AbstractNarration,
     UPPER(award._Institution._StateCode._text)               AS StateCode,
     award._Institution._PhoneNumber._text                    AS PhoneNumber,
     UPPER(award._Institution._CityName._text)                AS CityName,
     UPPER(award._Institution._StreetAddress._text)           AS StreetAddress,
     UPPER(award._Institution._ZipCode._text)                 AS ZipCode,
     UPPER(Investigator._RoleCode._text)                      AS RoleCode,
     UPPER(Investigator._EmailAddress._text)				  AS EmailAddress,
     UPPER(award._Organization._Division._LongName._text)     AS Division,
     UPPER(award._Organization._Code._text)                   AS Code,
     UPPER(award._Organization._Directorate._LongName._text)  AS Directorate,
     ProgEle._Code._text                                      AS EleCode,
     UPPER(ProgEle._Text._text)                               AS EleText,
     ProgRef._Code._text                                      AS RefCode,
     UPPER(ProgRef._Text._text)                               AS RefText,
     award._ProgramOfficer._SignBlockName._text               AS ProgOff,

	CASE
	WHEN award._Institution._CountryName._text IS NULL THEN 'NONE'
	ELSE UPPER(award._Institution._CountryName._text) END AS CountryName,

	CASE
	WHEN award._Institution._StateName._text IS NULL THEN 'NONE'
	ELSE UPPER(award._Institution._StateName._text) END AS StateName,

	CASE
	WHEN award._Institution._Name._text IS NULL THEN 'NONE'
	ELSE UPPER(award._Institution._Name._text) END AS Name,

	CASE 
    WHEN Investigator._FirstName._text IS NULL THEN 'NONE'
    ELSE UPPER(Investigator._FirstName._text) END AS FirstName,

    CASE
    WHEN Investigator._LastName._text IS NULL THEN 'NONE'
    ELSE UPPER(Investigator._LastName._text) END AS LastName,

	CASE
    WHEN    award._AwardEffectiveDate._text  = 'None' 
         OR award._AwardEffectiveDate._text  IS NULL 
         OR award._AwardEffectiveDate._text  = 'NA' THEN [0,0,0]
    ELSE SPLIT(award._AwardEffectiveDate._text, '/')  END as effectiveDate,
    
    CASE
    WHEN    award._AwardExpirationDate._text = 'None' 
         OR award._AwardExpirationDate._text IS NULL
         OR award._AwardExpirationDate._text = 'NA' THEN [0,0,0]
    ELSE SPLIT(award._AwardExpirationDate._text, '/')  END as expirationDate,
    
    CASE
    WHEN award._MinAmdLetterDate._text    = 'None' 
         OR award._MinAmdLetterDate._text  IS NULL
         OR award._MinAmdLetterDate._text = 'NA' THEN [0,0,0]
    ELSE SPLIT(award._MinAmdLetterDate._text, '/')  END as minAmdLetterDate,
    
    CASE
    WHEN    award._MaxAmdLetterDate._text = 'None' 
         OR award._MaxAmdLetterDate._text  IS NULL
         OR award._MaxAmdLetterDate._text = 'NA' THEN [0,0,0]
    ELSE SPLIT(award._MaxAmdLetterDate._text, '/')  END as maxAmdLetterDate,

    CASE
    WHEN Investigator._StartDate._text = 'None' 
         OR Investigator._StartDate._text IS NULL
         OR Investigator._StartDate._text = 'NA' THEN [0,0,0]
    ELSE SPLIT(Investigator._StartDate._text, '/')  END as invStartDate,
     
    CASE
    WHEN Investigator._EndDate._text = 'None' 
         OR Investigator._EndDate._text IS NULL
         OR Investigator._EndDate._text = 'NA' THEN [0,0,0]
    ELSE SPLIT(Investigator._EndDate._text, '/')  END as invEndDate

MERGE (awd:Award {AwardID: AwardID})
ON CREATE SET awd.AwardTitle  = AwardTitle,
    awd.AwardAmount           = AwardAmount,
    awd.ARRAAmount            = ARRAAmount,
    awd.AwardInstrument       = AwardInstrument,
    awd.AbstractNarration     = AbstractNarration

//
// Now match the awards to the institutions:
//

MERGE ( state:State { Name: StateName })
    ON CREATE SET state.Code = StateCode

MERGE (country:Country { Name: CountryName } )

MERGE (institution:Institution { Name: Name } )
ON CREATE SET institution.PhoneNumber   = PhoneNumber,
              institution.CityName      = CityName,
              institution.StreetAddress = StreetAddress,
              institution.ZipCode       = ZipCode

MERGE (institution)-[is:In_State]->(state)
    ON CREATE SET is.AwardID = [] + AwardID
    ON MATCH SET is.AwardID = is.AwardID + AwardID

// This shouldn't be necessary in practice, but there may be a reason for keeping it.
MERGE (state)-[ic:In_Country]->(country)
    ON CREATE SET ic.AwardID = [] + AwardID
    ON MATCH SET ic.AwardID = ic.AwardID + AwardID

MERGE (awd)-[ai:Administered_by]->(institution)

//
// Dates for awards - 
// Here we do two things, we first make the matches, and then we add the
// award number to the path's relationship element.
// 
// Start date:
// As with others, this has to be done year, then month because otherwise the unique
// constraint of the award number screws things up.
// 

MERGE (year:Year {Year: TOINT(effectiveDate[2]) } )
MERGE (month:Month {Month: TOINT(effectiveDate[0]) } )

MERGE (awd)-[yst:Year_Started]->(year)

MERGE (year)-[mst:Month_Started]->(month)
	ON CREATE SET mst.AwardID = [] + AwardID
	ON MATCH SET  mst.AwardID = CASE WHEN NOT AwardID IN mst.AwardID THEN mst.AwardID + AwardID
	                                 ELSE mst.AwardID END

// Dates for awards - End date:
MERGE (year_end:Year {Year: TOINT(expirationDate[2]) } )
MERGE (month_end:Month {Month: TOINT(expirationDate[0]) } )

MERGE (awd)-[yen:Year_Expired]->(year_end)

MERGE (year_end)-[men:Month_Expired]->(month_end)
	ON CREATE SET men.AwardID = [] + AwardID
	ON MATCH SET men.AwardID = CASE WHEN NOT AwardID IN men.AwardID THEN men.AwardID + AwardID
	                                 ELSE men.AwardID END

// Dates for awards - Amendment Letter min date:
MERGE (year_amdS:Year {Year: TOINT(minAmdLetterDate[2]) } )
MERGE (month_amdS:Month {Month: TOINT(minAmdLetterDate[0]) } )

MERGE (awd)-[yam:Year_Amended_Min]->(year_amdS)

MERGE (year_amdS)-[mam:Month_Amended_Min]->(month_amdS)
	ON CREATE SET mam.AwardID = [] + AwardID
	ON MATCH SET mam.AwardID = CASE WHEN NOT AwardID IN mam.AwardID THEN mam.AwardID + AwardID
	                                 ELSE mam.AwardID END

// These are amendment dates.
MERGE (year_amdE:Year {Year: TOINT(maxAmdLetterDate[2])})
MERGE (month_amdE:Month {Month: TOINT(maxAmdLetterDate[0])})

MERGE (awd)-[ymx:Year_Amended_Max]->(year_amdE)

MERGE (year_amdE)-[mmx:Month_Amended_Max]->(month_amdE)
	ON CREATE SET mmx.AwardID = [] + AwardID
	ON MATCH SET mmx.AwardID = CASE WHEN NOT AwardID IN mmx.AwardID THEN mmx.AwardID + AwardID
	                                 ELSE mmx.AwardID END

//
// Adding the People to the list:
//

MERGE (person:Person {EmailAddress: 
	CASE WHEN EmailAddress = 'NONE' 
	OR EmailAddress IS NULL
    THEN (UPPER(FirstName) + '_' + UPPER(LastName))
    ELSE UPPER(EmailAddress) END })
ON CREATE SET person.FirstName = FirstName,
              person.LastName  = LastName

MERGE (person)<-[awd_to:Awarded_to]-(awd)
ON CREATE SET awd_to.RoleCode  = RoleCode,
              awd_to.StartDate = invStartDate,
              awd_to.EndDate   = invEndDate

MERGE (person)-[emp:Employed_by]->(institution)
  ON CREATE SET emp.AwardID = [] + AwardID
  ON MATCH SET  emp.AwardID = emp.AwardID + AwardID

MERGE (po:ProgramOfficer {Name: ProgOff})

// Building Relationships:
// This whole set of relationships gets a basic pass, where we go through 
MERGE (po)-[mgd:Handled]->(awd)

//
// Divisions, Directorates and Programs
//

MERGE (direct:Directorate {Code: Code})
	ON CREATE SET direct.Directorate = Directorate

MERGE (division:Division {Division:CASE WHEN Division IS NULL THEN 'None' ELSE Division END})

MERGE (pgrele:Program  {Code:EleCode})
	ON CREATE SET pgrele.Text = EleText

MERGE (pgrref:Program {Code:RefCode})
	ON CREATE SET pgrref.Text = RefText

MERGE (awd)-[fby:Funded_by]->(direct)
MERGE (awd)-[fpg:Funded_by]->(pgrele)
MERGE (awd)-[rpg:Referenced_by]->(pgrref)

MERGE (direct)-[dwn:Within]->(division)
	ON CREATE SET dwn.AwardID = [] + AwardID
	ON MATCH SET dwn.AwardID = CASE WHEN NOT AwardID IN dwn.AwardID THEN dwn.AwardID + AwardID
		                       ELSE AwardID END

MERGE (pgrref)-[pgmof:Program_of]->(division)
	ON CREATE SET pgmof.AwardID = [] + AwardID
	ON MATCH SET pgmof.AwardID = CASE WHEN NOT AwardID IN pgmof.AwardID THEN pgmof.AwardID + AwardID
		                                 ELSE pgmof.AwardID END

MERGE (pgrele)-[pgmofb:Program_of]->(division)
	ON CREATE SET pgmofb.AwardID = [] + AwardID
	ON MATCH SET pgmofb.AwardID = CASE WHEN NOT AwardID IN pgmofb.AwardID THEN pgmofb.AwardID + AwardID
		                                 ELSE pgmofb.AwardID END;