library(sjPlot)

q <- read.csv(file = "dropnzvdata.csv")
num <- read.csv(file = "numeric_variables.csv")

cat_vars <- data.frame(
q$culture_growth_condition.culture_medium.medium_name,
q$culture_growth_condition.culture_medium.medium_name1,
q$culture_growth_condition.culture_medium.medium_name2,
q$culture_growth_condition.culture_temp.temperature_range,
q$environment_sampling_isolation_source.origin.continent,
q$environment_sampling_isolation_source.origin.country,
q$environment_sampling_isolation_source.origin.geo_loc_name,
q$environment_sampling_isolation_source.origin.sample_type,
q$molecular_biology.GC_content.GC_content,
q$molecular_biology.sequence.DB_sequence,
q$molecular_biology.sequence.DB_sequence1,
q$molecular_biology.sequence.DB_sequence2,
q$molecular_biology.sequence.seq_acc_num,
q$molecular_biology.sequence.seq_acc_num1,
q$molecular_biology.sequence.seq_acc_num2,
q$references.reference1.NA,
q$references.reference2.NA,
q$references.reference3.NA,
q$references.reference4.NA,
q$strain_availability.strain_history.history,
q$strain_availability.strains.strain_number,
q$taxonomy_name.strains.class,
q$taxonomy_name.strains.designation,
q$taxonomy_name.strains.family,
q$taxonomy_name.strains.full_scientific_name,
q$taxonomy_name.strains.genus,
q$taxonomy_name.strains.is_type_strain,
q$taxonomy_name.strains.ordo,
q$taxonomy_name.strains.phylum,
q$taxonomy_name.strains.species,
q$taxonomy_name.strains.species_epithet,
q$taxonomy_name.strains_synonyms_PNU.pnu_synonym,
q$taxonomy_name.strains_tax_PNU.class,
q$taxonomy_name.strains_tax_PNU.family,
q$taxonomy_name.strains_tax_PNU.full_scientific_name,
q$taxonomy_name.strains_tax_PNU.genus,
q$taxonomy_name.strains_tax_PNU.is_type_strain,
q$taxonomy_name.strains_tax_PNU.phylum,
q$taxonomy_name.strains_tax_PNU.species,
q$taxonomy_name.strains_tax_PNU.species_epithet,
q$taxonomy_name.strains_tax_PNU.status_gen,
q$taxonomy_name.strains_tax_PNU.status_spec)

df[] <- lapply(cat_vars,as.integer)
sjp.corr(df)
sjt.corr(df)

cmatrix <- matrix()
i = 1
j = 2

q <- cat_vars
while (i <= length(q) - 1) {
  while (j <= length(q)) {
    row1 <- colnames(q)[i]
    row2 <- colnames(q)[j]
    df <- q[!is.na(q[row1,]) & !is.na(q[row2],),]
    t <- table(df[row1,], df[row2],)
    cmatrix[i][j] <- chisq.test(t, correct = F)$p.value
    j = j+1
    print(i,j)
  }
  i = i+1
}

q <- read.csv(file = "dropnzvdata.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

num_vars <- data.frame(
q$application_interaction.risk_assessment.ID_reference,
q$culture_growth_condition.culture_medium.ID_reference,
q$culture_growth_condition.culture_medium.ID_reference1,
q$culture_growth_condition.culture_medium.ID_reference2,
q$culture_growth_condition.culture_temp.ID_reference,
q$culture_growth_condition.culture_temp.ID_reference1,
q$culture_growth_condition.culture_temp.ID_reference2,
q$environment_sampling_isolation_source.origin.ID_reference,
q$molecular_biology.GC_content.ID_reference,
q$molecular_biology.sequence.ID_reference,
q$references.ID_reference1.NA,
q$references.ID_reference2.NA,
q$references.ID_reference3.NA,
q$references.ID_reference4.NA,
q$strain_availability.strain_history.ID_reference,
q$strain_availability.straininfo_link.straininfo_strainnumber,
q$strain_availability.straininfo_link.straininfo_strainnumber1,
q$strain_availability.straininfo_link.straininfo_strainnumber2,
q$strain_availability.strains.ID_reference,
q$taxonomy_name.strains.ID_reference)

cor(num_vars)