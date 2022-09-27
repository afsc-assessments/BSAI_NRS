#Query the survey age comp data for unidentified and NRS in the EBS trawl survey


SELECT
haehnr.agecomp_ebs_standard_stratum.species_code,
haehnr.agecomp_ebs_standard_stratum.year,
haehnr.agecomp_ebs_standard_stratum.stratum,
haehnr.agecomp_ebs_standard_stratum.sex,
haehnr.agecomp_ebs_standard_stratum.age,
haehnr.agecomp_ebs_standard_stratum.agepop,
haehnr.agecomp_ebs_standard_stratum.meanlen,
haehnr.agecomp_ebs_standard_stratum.sdev
FROM
haehnr.agecomp_ebs_standard_stratum
WHERE
( haehnr.agecomp_ebs_standard_stratum.species_code = 10261
  AND haehnr.agecomp_ebs_standard_stratum.stratum = 999999 )
OR ( haehnr.agecomp_ebs_standard_stratum.species_code = 10260
     AND haehnr.agecomp_ebs_standard_stratum.stratum = 999999 )