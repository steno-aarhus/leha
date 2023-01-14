#1. Sort variables

# 1.1. Exposures
#Grouping foods
#by weight
data_weight<-data %>%
    mutate(Wlegumes=(beans+),
           Wleg_pea= (Wlegumes + peas)
           Wmeats=(beef+lamb+...),
           Wpoultry=(),
           Wfish=(),
           Wmixdish=(),
           Wruits= (),
           Wveggies=()
           Wfats=(),
           Wdrinks= (),
           Wcereal = (),
           Wdairy= (),
           Wsauces=(),
           Wsweets=(),
           Walcohol=(),

           )

           )


#by energy
#Energy provided per food <-- I will need to mutate as above with a multiplication for energy contributions from each foods, e.g. weight_beef/(100*E*0.239kcal)
data_kcal<-data %>%
    mutate(energy_foods=(p26002)*0.293,
           Elegumes=(),
           Emeats=((beef*E)+(lamb*E)+...)*0.239kcal,
    )


#1.2. outcomes
gallstone
cholecystect
cholecystitis
end of follow-up
death
loss to follow-up

#1.3. Covariables


#For liver/biliary/pancreas problems:
p20002,"Non-cancer illness code, self-reported",387470,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20002
#p20002: liver/biliary/pancreas problem: cholangitis, cholelithiasis, cholecystitis; mostly relevent for gallbladder disease
p41271,Diagnoses - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41271
p41281,Date of first in-patient diagnosis - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41281
#ICD-9 codes:574(cholelithiasis); 575.0-2 (Cholecystitis+obstruction of gallbladder)
p41270,Diagnoses - ICD10,440017,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270
p41280,Date of first in-patient diagnosis - ICD10,440014,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280
#ICD-10 code before entry?


