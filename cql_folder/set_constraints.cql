// Uniqueness constraints.
// Constraints build indexes as well.
// 
CREATE CONSTRAINT ON (i:Institution) ASSERT i.Name IS UNIQUE
CREATE CONSTRAINT ON (s:State) ASSERT s.Name IS UNIQUE
CREATE CONSTRAINT ON (c:Country) ASSERT c.Name IS UNIQUE
CREATE CONSTRAINT ON (y:Year) ASSERT y.Year IS UNIQUE
CREATE CONSTRAINT ON (m:Month) ASSERT m.Month IS UNIQUE
CREATE CONSTRAINT ON (a:Award) ASSERT a.AwardID IS UNIQUE
CREATE CONSTRAINT ON (p:ProgramOfficer) ASSERT p.SignBlockName IS UNIQUE
CREATE CONSTRAINT ON (pgrele:Program) ASSERT  pgrele.Code IS UNIQUE
CREATE CONSTRAINT ON (person:Person) ASSERT person.EmailAddress IS UNIQUE

// There are several other things we search for, for those, we want to create indices:

CREATE INDEX ON :Program(Text)
CREATE INDEX ON :Directorate(Directorate)