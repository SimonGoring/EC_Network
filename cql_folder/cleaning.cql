// Data cleaning
// We will be using the regexpr to hit EarthCube proposals several times
// To speed this up, we'll add an element to EarthCube related proposals

MATCH (n:Award) WHERE n.AbstractNarration =~ ".*EarthCube.*"
SET n.EarthCube = 1;

MATCH (n:Directorate) WHERE LOWER(n.Directorate) = "direct for biological sciences"
SET n.Directorate = "Directorate for Biological Sciences";

MATCH (n:Directorate) WHERE LOWER(n.Directorate) = "direct for computer & info scie & enginr"
SET n.Directorate = "Directorate for Computer & Information Science & Engineering";

MATCH (n:Directorate) WHERE LOWER(n.Directorate) = "direct for mathematical & physical scien"
SET n.Directorate = "Directorate for Mathematical & Physical Sciences";

MATCH (n:Directorate) WHERE UPPER(n.Directorate) = "OFFICE OF THE DIRECTOR"
SET n.Directorate = "Office of the Director";

MATCH (n:Directorate) WHERE n.Directorate = "Direct For Education and Human Resources"
SET n.Directorate = "Directorate For Education and Human Resources";

MATCH (n:Directorate) WHERE n.Directorate = "Direct For Social, Behav & Economic Scie"
SET n.Directorate = "Directorate For Social, Behavioral & Economic Sciences";

MATCH (n:Directorate) WHERE n.Directorate = "Office Of Information & Resource Mgmt"
SET n.Directorate = "Office Of Information & Resource Management";

// *************************************************************************
// Clean up the divisions

MATCH (n:Division) WHERE n.Division = "Division Of Polar Progrms"
SET n.Division = "Division of Polar Programs";

MATCH (n:Division) WHERE n.Division = "Div Of Electrical, Commun & Cyber Sys"
SET n.Division = "Division of Electrical, Communnications & Cyber Systems";

MATCH (n:Division) WHERE n.Division = "Div Of Civil, Mechanical, & Manufact Inn"
SET n.Division = "Division of Civil, Mechanical, and Manufacturing Innovation";

MATCH (n:Division) WHERE n.Division = "Office of Polar Programs (OPP)"
SET n.Division = "Office of Polar Programs";

MATCH (n:Division) WHERE n.Division = "Division of Polar Progrms"
SET n.Division = "Division of Polar Programs";

MATCH (n:Division) WHERE n.Division = "Div Atmospheric & Geospace Sciences"
SET n.Division = "Division of Atmospheric and Geospace Sciences";

MATCH (n:Division) WHERE n.Division = "Office Of Internatl Science &Engineering"
SET n.Division = "Office of International Science & Engineering"

MATCH (n:Division) WHERE n.Division = "Division of Integrative Organismal Sys"
SET n.Division = "Division of Integrative Organismal Systems";

MATCH (n:Division) WHERE n.Division = "DIVISION OF MATERIALS DEVELOPMENT,"
SET n.Division = "Division of Materials Development";

MATCH (n:Division) WHERE n.Division = "SBE Off of Multidisciplinary Activities"
SET n.Division = "SBE Office of Multidisciplinary Activities";

MATCH (n:Division) WHERE n.Division = "DIVISION OF EXPERIMENTAL & INTEG ACTIVIT"
SET n.Division = "Division of Experimental & Integrative Activities";

MATCH (n:Division) WHERE n.Division = "Division of Industrial Innovation & Partnersh"
SET n.Division = "Division of Industrial Innovation and Partnerships";

MATCH (n:Division) WHERE n.Division = "National Center For S&E Statistics"
SET n.Division = "National Center for Science & Engineering Statistics";

MATCH (n:Division) WHERE n.Division = "National Center for Science and Engineering Statistics."
SET n.Division = "National Center for Science & Engineering Statistics";

MATCH (n:Division) WHERE n.Division = "SBE Off of Multidisciplinary Activities"
SET n.Division = "SBE Office of Multidisciplinary Activities";

MATCH (n:Division) WHERE n.Division = "Division of Advanced CyberInfrastructure"
SET n.Division = "Division of Advanced Cyberinfrastructure";

MATCH (n:Division) WHERE n.Division = "Division of Behavioral and Cognitive Sci"
SET n.Division = "Division of Behavioral and Cognitive Sciences";

MATCH (n:Division) WHERE n.Division = "Division of Chem, Bioeng, Env, & Transp Sys"
SET n.Division = "Division of Chemical, Bioengineering, Environmental, and Transport Systems";

MATCH (n:Division) WHERE n.Division = "Division of Computer and Communication Foundations"
SET n.Division = "Division of Computing and Communication Foundations";	

MATCH (n:Division) WHERE n.Division = "DIVISION OF EXPERIMENTAL & INTEG ACTIVIT"
SET n.Division = "Division of Experimental & Integrative Activities";

//
//
//
// General Fixes:
//

MATCH (n:Division) 
SET n.Division = REPLACE(n.Division, " Of ", " of ");

MATCH (n:Division) 
SET n.Division = REPLACE(n.Division, " For ", " for ");

MATCH (n:Division) 
SET n.Division = REPLACE(n.Division, " and ", " & ")
SET n.Division = REPLACE(n.Division, " And ", " & ");

MATCH (n:Division) WHERE n.Division =~ "^Div "
SET n.Division = REPLACE(n.Division, "Div ", "Division ");

MATCH (n:Division) WHERE n.Division =~ "^Divn "
SET n.Division = REPLACE(n.Division, "Divn ", "Division ");

// Strange Divisions:

MATCH (n:Division) WHERE n.Division = "Direct For Biological Sciences"
SET n.Division = "Directorate for Biological Sciences";

// Funded & referenced
// There's no reason to have a program both fund and reference the same award in the
// same year.  It should be one or the other.
// This clears approximately 200k relationships.

MATCH (a:Award)-[:Funded_by]-(pr:Program)
MATCH (a)-[ref:Referenced_by]-(pr)
DELETE ref;