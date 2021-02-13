#chi square calculation place of residence
rownames =c("urban", "rural")
colnames = c("less than 3 km (within reach)", "greater than 3 km (far reach)")
c <- matrix(c(2701,2862,982,2483),
            nrow = 2,
            byrow = TRUE,
            dimnames = list(rownames, colnames))
c
#HO=No relationship between the categorical variables.
chic <- chisq.test(c, correct = T) #<0.001
chic
barplot(c, beside = T, legend = T)



#chi square calculation Nearest facility.
rownames =c("Yes", "No")
colnames = c("less than 3 km (within reach)", "greater than 3 km (far reach)")
nearfacility <- matrix(c(1360,3571,2323,1774),
                       nrow = 2,
                       byrow = TRUE,
                       dimnames = list(rownames, colnames))
nearfacility
chinearfacility <- chisq.test(nearfacility) #<0.001
chinearfacility
barplot(nearfacility, beside = T, legend = T)


#chi square calculation Mode of transport.
rownames =c("Walking", "Private car/taxi", "Public transport", "Bicycle/motorcycle")
colnames = c("less than 3 km (within reach)", "greater than 3 km (far reach)")
modeoftransport <- matrix(c(368,2116,279,220,1316,680,1720,2329),
                          nrow = 4,
                          byrow = TRUE,
                          dimnames = list(rownames, colnames))
modeoftransport
chimodeoftransport <- chisq.test(modeoftransport)#<0.001
chimodeoftransport
barplot(modeoftransport, beside = T, legend = T)


#chi square calculation Facility type.
rownames =c("Referral/county hospital", "Private hospitals/clinics", "Government health cares", "NGO/FBO hospitals/clinics")
colnames = c("less than 3 km (within reach)", "greater than 3 km (far reach)")
facilitytype <- matrix(c(1711,2235,1064,1743,552,970,356,397),
                       nrow = 4,
                       byrow = TRUE,
                       dimnames = list(rownames, colnames))
facilitytype
chifacilitytype <- chisq.test(facilitytype)#<0.001
chifacilitytype
barplot(facilitytype, beside = T, legend = T)



#chi square calculation Education.
rownames =c("No education", "Pre-Primary", "Primary", "Secondary", "Post-secondary")
colnames = c("less than 3 km (within reach)", "greater than 3 km (far reach)")
education <- matrix(c(908,1066,141,256,1552,2132,756,1336,326,555),
                    nrow = 5,
                    byrow = TRUE,
                    dimnames = list(rownames, colnames))
education
chieducation <- chisq.test(education)#<0.001
chieducation
barplot(education, beside = T, legend = T)


#chi square calculation Employment status.
rownames =c("Formal employed", "Informal", "Not working", "Students")
colnames = c("less than 3 km (within reach)", "greater than 3 km (far reach)")
employmentstatus <- matrix(c(207,263,1115,1379,723,1173,1638,2530),
                           nrow = 4,
                           byrow = TRUE,
                           dimnames = list(rownames, colnames))
employmentstatus
chiemploymentstatus <- chisq.test(employmentstatus)#<0.001
chiemploymentstatus
barplot(employmentstatus, beside = T, legend = T)



#chi square calculation Age group.
rownames =c("age < 15 years and > 64 years ", "14 to 24 years", "25 to 54 years", "55 to 64 years")
colnames = c("less than 3 km (within reach)", "greater than 3 km (far reach)")
agegroup <- matrix(c(1273,1747,449,689,1564,2460,397,449),
                   nrow = 4,
                   byrow = TRUE,
                   dimnames = list(rownames, colnames))
agegroup
chiagegroup <- chisq.test(agegroup)#<0.001
chiagegroup
barplot(agegroup, beside = T, legend = T)



#chi square calculation Sex.
rownames =c("Male", "Female")
colnames = c("less than 3 km (within reach)", "greater than 3 km (far reach)")
Sex <- matrix(c(1142,1527,2541,3818),
              nrow = 2,
              byrow = TRUE,
              dimnames = list(rownames, colnames))
Sex
chisex <- chisq.test(Sex)#<0.001
chisex
barplot(Sex, beside = T, legend = T)
