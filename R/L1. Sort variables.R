#1. Sort variables



# 1.1. Exposures
#Data management
Jeg har indtag i g af ærter+majs, og har antal portioner af ærter for hver deltager. 1 portion = 80 g, så jeg kan estimere deltagernes indtag af ærter og derefter trække det fra totalindtag af ærter+majs, så jeg har majs og ærter for sig.
#Grouping foods
#by weight
data_weight<-data %>%
    mutate(weight_legumes=(beans+),
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
Kommer de variable som energibidrag?

#1.2. outcomes
nafld
nash
end of follow-up
death
loss to follow-up

#1.3. Covariables

#For metabolic disease variable: hypertension, high cholesterol, diabetes
p20002,"Non-cancer illness code, self-reported",387470,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20002
#p20002: hypertension, high cholesterol, type 2 diabetes
    Egen sygdom – fra ID:
    20002 (hypertension, diabetes)
2443 (diabetes)
120007 (diabetes)
6177(medication for cholesterol, blood pressure, diabetes)
41270 (ICD10 – før start på opfølgning)
41271 (ICD9 –før start på opfølgning)

Families sygdom - fra ID:
20107
20110
20111



