# Hi Guys
#Use the package ImportExport to read in tables from an access database
library(ImportExport)



#List of names compiled from the 2014 access database
#See VBA code below to generate list of availalbe tables
table_names = c("ADM2014", "AL2014", "C2014_A", "C2014_B", "C2014_C", "C2014DEP", "CUSTOMCGIDS2014",
                "DRVADM2014", "DRVAL2014", "DRVC2014", "DRVEF122014", "DRVEF2014", "DRVF2014", 
                "DRVGR2014", "DRVHR2014", "DRVIC2014", "EAP2014", "EF2014", "EF2014A", 
                "EF2014A_DIST", "EF2014B", "EF2014C", "EF2014CP", "EF2014D", "EFFY2014", "EFIA2014", 
                "F1314_F1A", "F1314_F2", "F1314_F3", "filenames14", "FLAGS2014", "GR200_14", "GR2014", 
                "GR2014_L2", "HD2014", "IC2014", "IC2014_AY", "IC2014_PY", "IC2014Mission", "S2014_IS", 
                "S2014_NH", "S2014_OC", "S2014_SIS", "SAL2014_IS", "SAL2014_NIS", "sectiontable14", 
                "SFA1314_P1", "SFA1314_P2", "SFAV1314", "Tables14", "valuesets14", "vartable14")

#Use access_import from the ImportExport package to read in a list of tables;
mydb_2014 <- access_import("Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/IPEDS201415.mdb", 
                           table_names = table_names, ntab = length(table_names), SQL_query = rep(F, times= length(table_names)), 
                           where_sql = c())

#Pull the list of tables from all access sheets
table_list <- mydb_2014[['Tables14']]

#Pull the list of variables from access sheets list
var_list <- mydb_2014[['vartable14']]

value_labels <- mydb_2014[['valuesets14']]

#Pull list of section tables - not sure exactly what this is

section_tables <- mydb_2014[['sectiontable14']]

file_names <- mydb_2014[['filenames14']]

mission_statements <- mydb_2014[['IC2014Mission']]

comparison_groups <- mydb_2014[['CUSTOMCGIDS2014']]

data_sheets <- mydb_2014[-which(names(mydb_2014) %in% c("Tables14",'vartable14','valuesets14',
                                                        'sectiontable14','filenames14','IC2014Mission',
                                                        'CUSTOMCGIDS2014'))]

#***************************************************************
#Alternatively, we can use the list of tables from the "Tables14" sheet to get an easy list of names
#***************************************************************

Tables2014 <- access_import("Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/IPEDS201415.mdb", 
                            table_names = c("Tables14"), ntab = 1, SQL_query = rep(F, times= 1), where_sql = c()) 

f <- sapply(Tables2014, is.factor)
Tables2014[f] <- lapply(Tables2014[f], as.character)
table_names_2 <- Tables2014[['TableName']]


#Second method of import - use table_names_2
mydb_2014 <- access_import("Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/IPEDS201415.mdb", 
                           table_names = table_names_2, ntab = length(table_names_2), SQL_query = rep(F, times= length(table_names_2)), 
                           where_sql = c())






#*************************************************************************
#able to generate a list of tables from access using visual basic function, listed below for reference
#http://www.consultdmw.com/access-VBA-list-objects.htm

# Function dmwListAllTables() As String
# '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dim tbl As AccessObject, db As Object
# Dim strMsg As String
#  
# On Error GoTo Error_Handler
#  
# Set db = Application.CurrentData
# For Each tbl In db.AllTables
# Debug.Print tbl.Name
# Next tbl
#  
# strMsg = " -- Tables listing complete -- "
#  
# Procedure_Done:
# dmwListAllTables = strMsg
# Exit Function
#  
# Error_Handler:
# strMsg = Err.Number & " " & Err.Description
# Resume Procedure_Done
#  
# End Function
# 
# 
# Sub printTables()
# 
# table_list = dmwListAllTables()
# Debug.Print (table_list)
# 
# End Sub