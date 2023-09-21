Inklusion af variable (ud fra "project-variables.csv" linjenumre)

Hvordan kobler jeg de her variable ("diagnose" og "date of diagnosis")

p41270,Diagnoses -
ICD10,440017,<https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270>

p41280,Date of first in-patient diagnosis -
ICD10,440014,<https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280>

Fra web: The corresponding ICD-10 diagnosis codes can be found in
data-field Field 41270 and the two fields can be linked using the array
structure.

p41271,Diagnoses -
ICD9,20299,<https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41271>

p41281,Date of first in-patient diagnosis -
ICD9,20299,<https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41281>

p41272,Operative procedures -
OPCS4,440159,<https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41272>

p41282,Date of first operative procedure -
OPCS4,440153,<https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41282>

B1#51-56 skal jeg have ICD-9 og -10 koder med som history of gallbladder
disease i min DAG, eller skal jeg bruge det til at ekskludere deltagere?
måske justere i sensitivitetsanalyse

Data management line 12-16 2887_i0 contains answer option "don't know"
And "less than 1 daily" which doesn't converge as they are characters
and the pivot_longer format will not work. Should be changed to a
number, but which? After deciding on the fate of p2887, mutate the
answer options to a relevant number and continue.

