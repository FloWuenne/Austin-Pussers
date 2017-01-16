
source("../uifunctions.R")
initialize('ath',TRUE)

# drugs<-c('ETOHResults','Methanol','Isopropanol','Acetone','THC','Carboxy_THC','Hydroxy_THC','Cocaine','Benzoylecgonine','Cocaethylene','Nicotine','Cotinine','Codeine','Hydrocodone','Hydromorphone','Morphine','Oxycodone','Oxymorphone','Acetaminophen','Amphetamines','Barbiturates','Benzodiazepines','Methadone','PCP','Propoxyphene','Salicylates','Anticholinergics_Antiparkinsonians','Anticonvulsants','Antidepressants','Antihistamines','AntiInflammatories','Antipsychotics','Hallucinogens','OtherDrugs')
# names(drugs)<-drugs



choices_for_covariate<-c('cuspidity','dilation','Preoperative_Data__NYHA_class','Preoperative_Data__Aortic_Stenosis','Preoperative_Data__Ascending_Aortic_Aneurysm','Preoperative_Data__Marfans_syndrome','Current_Medication__Medication._type','Current_Medication__Other thrombocyte inhibitor','Current_Medication__ASA','Current_Medication__Insulin','Current_Medication__Loop diuretics','Current_Medication__Calcium antagonist','Current_Medication__Beta-blocker','Current_Medication__Other','Current_Medication__Lipid-lowering agent','Current_Medication__Anticoagulant','Current_Medication__Angiotensin-II-blocker','Current_Medication__ACE inhibitor','Current_Medication__Preparation_and_dose','Clinical_Data__Blood_pressure._systolic_1','Clinical_Data__Blood_pressure._diastolic_1','Clinical_Data__Blood_pressure._systolic_2','Clinical_Data__Blood_pressure._diastolic_2','Clinical_Data__Blood_for_DNA_preparation','Clinical_Data__Serum','Clinical_Data__EDTA_plasma','Clinical_Data__Citrate_plasma','Clinical_Data__Fasting_time','Clinical_Data__Glucose','Clinical_Data__Creatinine','Clinical_Data__ALAT','Clinical_Data__GT','Clinical_Data__Leukocytes','Clinical_Data__Erythrocytes','Clinical_Data__Hemoglobin','Clinical_Data__Erc.B..MVC','Clinical_Data__Erc.B..MCH','Clinical_Data__Erc.B..MCHC','Clinical_Data__Thrombocytes','Clinical_Data__hsCRP','Clinical_Data__TSH','Clinical_Data__HbA1c','Clinical_Data__HbA1c_.IFCC.','Clinical_Data__EVF','Clinical_Data__HDL.cholesterol','Clinical_Data__Cholesterol','Clinical_Data__LDL.cholesterol','Clinical_Data__TG','Clinical_Data__Calculated_GFR','Computed_Tomography__Ascending_aorta._maximal_diameter','Perioperative_Data__Coronary_Bypass_.CBP.','Perioperative_Data__AX','Perioperative_Data__Circulatory_Arrest','Perioperative_Data__Randomized','Perioperative_Data__Number_of_commissures','Perioperative_Data__Commissures_fused','Perioperative_Data__Aortic_valve_replacement','Perioperative_Data__Tubular_graft','Perioperative_Data__Tubular_graft_size','Perioperative_Data__Composite_graft','Perioperative_Data__Composite_graft_size','Perioperative_Data__Biological_composite_graft','Perioperative_Data__Biological_composite_graft_size','Perioperative_Data__Other_surgery','Perioperative_Data__Number_of_repaired_cusps','Length_of_Stay__Length_of_stay._total','Length_of_Stay__Length_of_stay._Thoracic_ICU','Biochemical_Analyses__Fib','Biochemical_Analyses__Insulin','Biochemical_Analyses__Proinsulin','Biochemical_Analyses__ApoB','Biochemical_Analyses__ApoA1','Biochemical_Analyses__Lp.a.','Biochemical_Analyses__FibC','Biochemical_Analyses__FVIIc','Biochemical_Analyses__PAI.1ag','Biochemical_Analyses__PCSK9','Personal_Data__Project','Personal_Data__Date_of_birth','Personal_Data__Age','Personal_Data__Gender','Personal_Data__Place_of_birth','Personal_Data__Ethnicity','Preoperative_Data__Aortic_Regurgitation','Preoperative_Data__Annuloaortic_Ectasia','Clinical_Data__Date','Clinical_Data__Height','Clinical_Data__Weight','Clinical_Data__BMI','Clinical_Data__Waist_circumference','Clinical_Data__Hip_circumference','Perioperative_Data__Date','Perioperative_Data__Annuloaortic_Ectasia','Perioperative_Data__Ascending_Aortic_Aneurysm','Perioperative_Data__Number_of_cusps','QC_data__genotyping_batch')




shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("ASAP Gene-expression"),
  beginPage(),
  beginPanel('1/3'),
  
  textInput(inputId="email",label="Email", value=""),
  textInput(inputId="gene1", label = "HGNC-Genesymbol", value = ""),
  selectInput("expressionsetName", "Data set:", choices = c(
    "Aorta intima-media"="ASAP_AMed",
    "Aorta adventitita"="ASAP_AAdv",
    "Mammary artery intima-media"="ASAP_MMed",
    "Liver"="ASAP_L",
    "Heart"="ASAP_H"
  )),
  
  selectInput("focus", "Focus:", choices = c(
    "Dilation and cuspidity"="dil_and_cusp",
    "Specific covariate"="covar",
    "Other gene"="gene"
  )),
  
  conditionalPanel(
    condition = "input.focus == 'gene'",
    textInput("gene2", "Other gene",value="")
  ),
  conditionalPanel(
    condition = "input.focus == 'covar'",
    selectInput("covariate", "Co-variate:", choices = choices_for_covariate)
  ),
  
  checkboxInput("advanced", "Advanced options", value = FALSE),
  conditionalPanel(
    condition = "input.advanced & (input.focus == 'covar' | input.focus == 'dil_and_cusp')",
    selectInput("colourBy", "Colour by:", choices = c(
      "None"="none",
      "Cuspidity"="cuspidity",
      "Dilation"="dilation",
      "Age"="Personal_Data__Age",
      "Gender"="Personal_Data__Gender"),
      selected ="none"),
    selectInput("pchBy", "Dot-type by:", choices = c(
      "None"="none",
      "Cuspidity"="cuspidity",
      "Dilation"="dilation",
      "Gender"="Personal_Data__Gender"),
      selected ="none")
  ),
  actionButton("goButton","Investigate"),
  downloadButton("downloadData", label = "Download"),
  
  
  
  endPanel(),
  beginPanel('2/3'),
  plotOutput("plotGenes"),
  
  endPanel(),
  
  
  endPage(),
  footer()
  
)	
)







