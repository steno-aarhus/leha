#PRS
Hvad er polygenic risk scores?
    Det lader til at være en vurdering af, om man er genetisk prædisponeret for nogle bestemte sygdomme.
Hvordan kan man bruge det? Kan det eksempelvis være en covariabel man justerer for?


#Inklusion af variable (ud fra "project-variables.csv" linjenumre)
Hvordan kobler jeg de her variable?
p41270,Diagnoses - ICD10,440017,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270
p41280,Date of first in-patient diagnosis - ICD10,440014,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280
#Fra web: The corresponding ICD-10 diagnosis codes can be found in data-field Field 41270 and the two fields can be linked using the array structure.

p41271,Diagnoses - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41271
p41281,Date of first in-patient diagnosis - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41281


p41272,Operative procedures - OPCS4,440159,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41272
p41282,Date of first operative procedure - OPCS4,440153,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41282


B1#51-56 skal jeg have ICD-9 og -10 koder med som history of gallbladder disease i min DAG, eller skal jeg bruge det til at ekskludere deltagere?
