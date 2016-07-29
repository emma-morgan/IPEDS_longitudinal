#Function to get survey information from file name

#********************************************************************************************
# Function to generate FY that will be used to merge across surveys when needed
# Function: IPEDS_FY
#********************************************************************************************

# The function IPEDS_FY creates fiscal year based on type of survey
# This will allow Tableau blending by FY for different survey files

#testing how this function works...
mydir <- choose.dir(caption="Select location of your dictionary files")
setwd(mydir)
list.files()
fileinfo.list <- list()
for (i in 1:length(list.files())) {
  fileinfo.list[[i]] <- IPEDS_FY(list.files()[i])
}
simplify2array(fileinfo.list)


IPEDS_FY <- function(myFile) {
  
  #make sure text of myfile is all lowercase to fit with descriptions
  myFile <- tolower(myFile)
  
  #Listed in order of appearance on IPEDS data center
  
  #Institutional Characteristics
  
  #Directory Information
  if (substring(myFile,1,2) == "hd") {
    surveyType <- "HD"
    surveyName <- "Directory Information"
    fall <- as.numeric(substring(myFile,3,6))
    FY <- fall+1
  }
  
  #
  else if (substring(myFile,1,2) == "ic") {
    
    #Student charges for academic year programs
    if (substring(myFile,7,9) == "_ay") {
      surveyType <- "IC_AY"
      surveyName <- "Student charges for academic year programs"
      fall <- as.numeric(substring(myFile,3,6))
      FY <- fall+1
    }
    
    #Student charges by program (vocational programs)
    else if (substring(myFile,7,9)=="py") {
      surveyType <- "IC_PY"
      surveyName <- "Student charges by program - vocational programs"
      fall <- as.numeric(substring(myFile,3,6))
      FY <- fall+1
    }
    
    #Educational offerings, organization, services, and athletic associations
    else {
      surveyType <- "IC"
      surveyName <- "Educational offerings, organization, services, athletic assoc."
      fall <- as.numeric(substring(myFile,3,6))
      FY <- fall+1
    }
  }

  #Admissions and test scores
  else if (substring(myFile,1,3) == "adm") {
    surveyType <- "ADM"
    surveyName <- "Admissions and Test Scores"
    fall <- as.numeric(substring(myFile,4,7))
    FY <- fall+1
  }
  
  #Fall Enrollment
  else if (substring(myFile,1,2) == "ef" && ! (substring(myFile,3,3) %in% c("f","i"))) {
    

    if (substring(myFile,7,7)=="a") {
      
      #Distance education status and level of student
      if (substring(myFile,8,12)=="_dist") {
        surveyType <- "EF_A_DIST"
        surveyName <- "Fall Enrollment - Distance education status and level of student"
        fall <- as.numeric(substring(myFile, 3, 6))
        FY <- fall+1
      }
      
      #Race/ethnicity, gender, attendance status, and level of student      
      else {
        surveyType <- "EF_A"
        surveyName <- "Fall Enrollment - Race/ethnicity, gender"
        fall <- as.numeric(substring(myFile, 3, 6))
        FY <- fall+1
      }
    }
    
    #Age category, gender, attendance status, and level of student
    else if (substring(myFile,7,7)=="b") {
      surveyType <- "EF_B"
      surveyName <- "Fall Enrollment - Age category, gender"
      fall <- as.numeric(substring(myFile, 3, 6))
      FY <- fall+1
    }
    
    else if (substring(myFile,7,7)=="c") {
      
      #Major field of study, race/ethnicity, gender, attendance status, and level of student
      if (substring(myFile,7,8)=="cp") {
        surveyType <- "EF_CP"
        surveyName <- "Fall Enrollment - major field of study, race/ethnicity, gender"
        fall <- as.numeric(substring(myFile, 3, 6))
        FY <- fall+1
      }
      
      #Residence and migration of first-time freshman
      else {
        surveyType <- "EF_C"
        surveyName <- "Fall Enrollment - Residence and Migration of First-time freshman"
        fall <- as.numeric(substring(myFile, 3, 6))
        FY <- fall+1
      }
    }
    
    else if (substring(myFile,7,7)=="d") {
      surveyType <- "EF_D"
      surveyName <- "Fall Enrollment - Total entering class, retention rates, and student-to-faculty ratio"
      fall <- as.numeric(substring(myFile, 3, 6))
      FY <- fall+1
    }
  }

  #12 month enrollment
  #12 month unduplicated headcount
  
  else if (substring(myFile,1,4)=="effy") {
    surveyType <- "EFFY"
    surveyName <- "12-month unduplicated headcount"
    FY <- as.numeric(substring(myFile, 5, 8))
    fall <- FY-1
  }
  
  #12 month instructional activity
  else if (substring(myFile,1,4)=="efia") {
    surveyType <- "EFIA"
    surveyName <- "12-month instructional activity"
    FY <- as.numeric(substring(myFile, 5, 8))
    fall <- FY-1
  }
  
  #Completions
  
  else if (substring(myFile,1,1) == "c") {
    if (substring(myFile,7,7) == "a") {
      surveyType <- "Completions_A"
      surveyName <- "Awards/degrees conferred by program, level, race/ethnicity, gender"
      FY <- as.numeric(substring(myFile,2,5))
      fall <- FY-1
    }
    else if (substring(myFile,7,7) == "b") {
      surveyType <- "Completions_B"
      surveyName <- "Number of students receiving awards/degrees by race/ethnicity and gender"
      FY <- as.numeric(substring(myFile,2,5))
      fall <- FY-1
    }
    else if (substring(myFile,7,7) == "c") {
      surveyType <- "Completions_C"
      surveyName <- "Number of students receiving awards/degrees by award level, gender, race/ethnicity, age category"
      FY <- as.numeric(substring(myFile,2,5))
      fall <- FY-1
    }
    else if (substring(myFile,7,9) == "dep") {
      surveyType <- "Completions_DEP"
      surveyName <- "Number of programs offered and number of programs offered via distance education, by award level"
      FY <- as.numeric(substring(myFile,2,5))
      fall <- FY-1
    }
  }
  
  #Graduation Rates
  
  else if (substring(myFile,1,2) == "gr") {
    if (substring(myFile,1,6) == "gr200_") {
      surveyType <- "GR_200"
      surveyName <- "200% Graduation Rates"
      FY <- as.numeric(substr(myFile,3,6))
      fall <- FY-1
    }
    else if (substring(myFile,7,9) == "_l2") {
      surveyType <- "GR_L2"
      surveyName <- "200% Graduation Rates"
      FY <- as.numeric(substr(myFile,3,6))
      fall <- FY-1
    }
    else {
      surveyType <- "GR_150"
      surveyName <- "150% Graduation Rates"
      FY <- as.numeric(substr(myFile,3,6))
      fall <- FY-1
    }
  }

  #Student Financial Aid and Net Price
  
  else if (substring(myFile,1,3) == "sfa") {
    if (substr(myFile,4,4)=="v") {
      surveyType <- "SFAV"
      surveyName <- "Military Servicemembers and Veteran's Benefits"
      fall <- as.numeric(paste("20", substr(myFile,5,6),sep=""))
      FY <- fall+1
    }
    else {
      surveyType <- "SFA"
      surveyName <- "Student Financial Aid and net price"
      fall <- as.numeric(paste("20", substr(myFile,4,5),sep=""))
      FY <- fall+1
    }
  }
  
  #Finance
  else if (substr(myFile,1,1)=="f") {
    if (substring(myFile,6,9)=="_f1a") {
      surveyType <- "F_F1A"
      surveyName <- "Finance - Public Institutions - GASB 34/35"
      fall <- as.numeric(paste("20", substr(myFile,4,5),sep=""))
      FY <- fall + 1
    }
    else if (substring(myFile,6,8)=="_f2") {
      surveyType <- "F_F2"
      surveyName <- "Finance - Private not-for profit or public FASB"
      fall <- as.numeric(paste("20", substr(myFile,4,5),sep=""))
      FY <- fall + 1
    }
    else if (substring(myFile,6,8)=="_f3") {
      surveyType <- "F_F2"
      surveyName <- "Finance - Private for-profit institutions"
      fall <- as.numeric(paste("20", substr(myFile,4,5),sep=""))
      FY <- fall + 1
    }
  }
  
  #Instructional Staff/Salaries
  else if (substring(myFile,1,3)=="sal") {
    if (substring(myFile,9,10)=="is") {
      surveyType <- "SAL_IS"
      surveyName <- "Number and salary outlays for FT nonmedical instructional staff by gender and academic rank"
    }
    else if (substring(myFile,9,11)=="nis") {
      surveyType <- "sal_nic"
      surveyName <- "Number and salary outlays for FT nonmedical noninstructional staff by occupation"
    }
    fall <- as.numeric(substring(myFile,4,7))
    FY <- fall+1
  }
  
  #Fall Staff
  
  else if (substring(myFile,1,1)=="s") {
    if (substring(myFile,7,8)=="oc") {
      surveyType <- "S_OC"
      surveyName <- "Staff by Occupational Category, Race/ethncity, and gender"
      fall <- as.numeric(substring(myFile,2,5))
      FY <- fall + 1
    }
    
    else if (substring(myFile,7,9)=="sis") {
      surveyType <- "S_SIS"
      surveyName <- "FT Instructional Staff by rank and faculty/tenure status"
      fall <- as.numeric(substring(myFile,2,5))
      FY <- fall+1
    }
    
    else if (substring(myFile,7,8)=="is") {
      surveyType <- "S_IS"
      surveyName <- "FT Instructional Staff by faculty/tenure status, academic rank, race/ethnicity, gender"
      fall <- as.numeric(substring(myFile,2,5))
      FY <- fall+1
    }
    
    else if (substring(myFile,7,8)=="nh") {
      surveyType <- "S_NH"
      surveyName <- "New hires by occupational category, race/ethnicity, and gender"
      fall <- as.numeric(substring(myFile,2,5))
      FY <- fall+1
    }
  }
  
  #Employees by Assigned Position
  else if (substr(myFile,1,3)=="eap") {
    surveyType <- "EAP"
    surveyName <- "Employees by assigned position - staff by occupational category"
    fall <- as.numeric(substring(myFile,4,7))
    FY <- fall+1
  }
  
  #Academic Libraries
  
  else if (substring(myFile,1,2)=="al") {
    surveyType <- "AL"
    surveyName <- "Academic Libraries"
    FY <- as.numeric(substring(myFile,3,6))
    fall <- FY-1
  }
 
  else {
    surveyType <- "UNIDENTIFIED SURVEY TYPE"
    surveyName <- "UNIDENTIFIED SURVEY NAME"
    FY <- "UNIDENTIFIED FY"
    fall <- "UNIDENTIFIED FALL"
  }
  
  #Named list values so it will be clearer to refer to them later on
  
  myreturn <- list()
  
  myreturn[["filename"]] <- myFile
  myreturn[["surveyType"]] <- surveyType
  myreturn[["surveyName"]] <- surveyName
  myreturn[["fall"]] <- fall
  myreturn[["FY"]] <- FY
  
  
  return (myreturn)
}