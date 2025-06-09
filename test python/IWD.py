#!/usr/bin/python
#####################################################################################################
#	File: 				IWD.py
#	Created by:			COFHE
#	Created on:			2022-11-17 09:26:08.835519
#	Purpose:			Generate set of classes and handler for downloading IPEDS data for IWD
#	Last Update:		
#	Last Update By:		
#	Last Update Desc:	
#####################################################################################################

import requests, sqlite3, re, os, time, datetime, zipfile, glob, argparse
from random import randrange
import getpass

IWD_DATA_PATH = os.path.join("C:\\",
							 "Users",
							 getpass.getuser(),
							 "Massachusetts Institute of Technology",
							 "IWD - Data")

def helper_delete(file_name):
	if os.path.exists(file_name):
		os.remove(file_name)
	else:
		print("\tCan''t delete. No file exists")

def helper_year(year,year_type):
	current_year = year
	if year_type == "normal":
		prior_year = str(int(year)-1)
	elif year_type == "fiscal":
		prior_year = str(int(year[0:2])-1)+str(int(year[2:4])-1)
	else:
		raise Exception("Only valid year types are normal and fiscal")
	return current_year,prior_year

def helper_unzip(file_name,extract_dir):
	"""
	Helper function that unzips a zip archive
	"""
	file_list = []
	fh = open(file_name, 'rb')
	z = zipfile.ZipFile(fh)
	for name in z.namelist():
		z.extract(name, extract_dir)
		file_list.append(name)
	fh.close()
	return file_list

def which_file_version(data_dir):
	"""
	If the ziparchive contains a _rv file, flag that file to use
	in processing.
	"""
	file_search_path = os.path.join(data_dir,"*.zip")

	for ziparchive in glob.glob(file_search_path):   
		
		file_list = helper_unzip(ziparchive,data_dir)
		
		#determine if a revision "_RV" file is present
		first_file=""
		revision_file=""
		for file_name in file_list:
			if "_rv" in file_name:
				print("\trevision: {0}".format(file_name))
				revision_file = file_name
			else:
				print("\toriginal: {0}".format(file_name))
				first_file = file_name
				
		
		#Pick which file we're going to use and process it    
		if revision_file!="":
			print ("\t * USE REVISION")
			os.remove(os.path.join(data_dir,first_file))
			
		else:
			print ("\t * DON'T USE REVISION")

def download(file_url,download_location):

	# Pause for 14 seconds.  It is a good idea to rate
	# limit calls to websites.  If doing development work,
	# I'd recommend not trying to download every file
	# in one pass
	wait_time = 14+randrange(30)
	time.sleep(wait_time)

	r = requests.get(file_url, stream = True)

	# When I'm scripting these, I like to keep track of what the program is going
	# so I can be sure I didn't do something like make 1000 requests when I meant
	# to make one.
	logfile = open("log.txt","a")
	logfile.write("\t".join([str(datetime.datetime.now()),
							 "MAKE REQUEST",
							 file_url,
							 str(r),
							 str(wait_time)
							 ])+"\n"
				  )
	logfile.close()

	# What to call the file we output
	file_name = file_url.split("/")[-1:][0]

	with open(os.path.join(download_location,file_name),"wb") as outfile:
		for chunk in r.iter_content(chunk_size = 1024):
			if chunk:
				outfile.write(chunk)


def load_metadata_db():
	"""
	This method builds a little sqlite database to hold 
	metadata on IPEDS files.  This db is loaded from a metadata
	file.  The reason we are using a db is we can use SQL
	statement to filter what set of things are downloaded.
	"""
	print("* Run load_metadata_db")
	connect = sqlite3.connect("IWD.db")
	cursor = connect.cursor()

	strsql = "drop table if exists IWD_files"
	cursor.execute(strsql)

	strsql = """
			create table IWD_files (
				IPEDS_Cycle text,
				IPEDS_Collection text,
				IPEDS_FILE text,
				year_type text
			)
			"""
	cursor.execute(strsql)

	infile = open("metadata.txt","r")
	for line in infile.readlines()[1:]:
		row = line.replace("\n","").replace("\r","").split("\t")

		strsql = "insert into IWD_files values(?,?,?,?)"
		cursor.execute(strsql,row)

	strsql = "select count(*) from IWD_files"
	cursor.execute(strsql)
	row = cursor.fetchone()
	print("\t{0} Rows Loaded".format(row[0]))

	connect.commit()
	connect.close()

class IWD_file():
	def __init__(self,ipeds_file, year):
		
		# based on the file name passed, look up metadata in the 
		# database
		connect = sqlite3.connect("IWD.db")
		cursor = connect.cursor()

		strsql = "select * from IWD_files where IPEDS_FILE=?"
		cursor.execute(strsql,[ipeds_file])
		row = cursor.fetchone()

		# If we don't find a file, end the program
		if row is None:
			raise Exception("Error - file {0} does not exist in metadata".format(ipeds_file))

		# IPEDS provides years in 3 formats 
		# 1) 2020	-> 2020
		# 2) 1920	-> 2020
		# 3) 20		-> 20
		self.year_type = row[3]

		if self.year_type=="standard":
			pass
		elif self.year_type=="fy":
			year = str(int(year[2:4])-1)+year[2:4]
		elif self.year_type =="2digit":
			year = year[2:4]
		else:
			raise Exception("Error in metadata.  Only valid year types are standard, fy, and 2digit")

		self.ipeds_file = ipeds_file.replace("[YEAR]",year)
		self.IPEDS_Cyle = row[0]
		self.IPEDS_Collection = row[1]
		self.data_file_name = "{0}.zip".format(self.ipeds_file)
		self.data_file_dl = "https://nces.ed.gov/ipeds/datacenter/data/{0}".format(self.data_file_name)
		self.data_dictionary_name = "{0}_Dict.zip".format(self.ipeds_file)
		self.data_dictionary_dl = "https://nces.ed.gov/ipeds/datacenter/data/{0}".format(self.data_dictionary_name)

		self.subsurvey_name = ipeds_file.replace("[YEAR]","").lower()

		self.data_directory = os.path.join(IWD_DATA_PATH,
											self.IPEDS_Collection,
											self.subsurvey_name,
											"Data")

		self.dictionary_directory = os.path.join(IWD_DATA_PATH,
												self.IPEDS_Collection,
												self.subsurvey_name,
												"Dictionary")

		connect.close()

	def __repr__(self):
		"""
		Built in method to print a file object
		"""
		repr_str = "* File Object\n" \
		+ "\tIPEDS File -> {0}\n".format(self.ipeds_file) \
		+ "\tIPEDS Cycle -> {0}\n".format(self.IPEDS_Cyle) \
		+"\tIPEDS Collection -> {0}\n".format(self.IPEDS_Collection) \
		+"\tIPEDS SubSurvery -> {0}\n".format(self.subsurvey_name) 
		return repr_str

	def makedirs(self):
		"""
		This will typically only run once.  It tests if the data and
		dictionary directories exist, and if not, it will make them.
		"""
		print("* Run makedirs")
		# Test if the collection folder exists:
		collection_path = os.path.join(IWD_DATA_PATH,self.IPEDS_Collection)
		if not os.path.exists(collection_path):
			os.makedirs(collection_path)
			print("\tMake directory: {0}".format(collection_path))

		# Test if the sub survey folder exists
		sub_survey_path = os.path.join(IWD_DATA_PATH,
											self.IPEDS_Collection,
											self.subsurvey_name)
		if not os.path.exists(sub_survey_path):
			os.makedirs(sub_survey_path)
			print("\tMake directory: {0}".format(sub_survey_path))

		# Test for existence of data and dictionary dirs
		data_dir = os.path.join(IWD_DATA_PATH,
								self.IPEDS_Collection,
								self.subsurvey_name,
								"Data")
		if not os.path.exists(data_dir):
			os.makedirs(data_dir)
			print("\tMake directory: {0}".format(data_dir))

		dict_dir = os.path.join(IWD_DATA_PATH,
								self.IPEDS_Collection,
								self.subsurvey_name,
								"Dictionary")
		if not os.path.exists(dict_dir):
			os.makedirs(dict_dir)
			print("\tMake directory: {0}".format(dict_dir))

		out_dir = os.path.join(IWD_DATA_PATH,
								self.IPEDS_Collection,
								self.subsurvey_name,
								"compiled")
		if not os.path.exists(out_dir):
			os.makedirs(out_dir)
			print("\tMake directory: {0}".format(out_dir))

	def download_file(self,debug = False):
		self.makedirs()
		print("Download Data")
		# Download data
		print("\tDownload {0} to {1}".format(self.data_file_dl,
											 self.data_directory))
		if debug:
			pass
		else:
			download(self.data_file_dl,self.data_directory)

		# Download data
		print("\tDownload {0} to {1}".format(self.data_dictionary_dl,
											 self.dictionary_directory))
		if debug:
			pass
		else:
			download(self.data_dictionary_dl,self.dictionary_directory)

	def process_file(self,debug = False):
		"""
		Unzips file and determines which to keep
		"""
		print("* File Processing")
		print("\tUnzip: {0}".format(os.path.join(self.data_directory,self.data_file_name)))
		helper_unzip(os.path.join(self.data_directory,self.data_file_name),
					self.data_directory
					)
		print("\tUnzip: {0}".format(os.path.join(self.dictionary_directory,self.data_dictionary_name)))
		helper_unzip(os.path.join(self.dictionary_directory,self.data_dictionary_name),
					self.dictionary_directory
					)
		print("* Determine which file to use")
		which_file_version(self.data_directory)

	def cleanup(self,debug = False):
		print("* Cleanup")
		if debug:
			print("\tDelete: {0}".format(os.path.join(self.data_directory,self.data_file_name)))
		else:
			helper_delete(os.path.join(self.data_directory,self.data_file_name))

		if debug:
			print("\tDelete: {0}".format(os.path.join(self.dictionary_directory,self.data_dictionary_name)))
		else:
			helper_delete(os.path.join(self.dictionary_directory,self.data_dictionary_name))

def main():
	parser = argparse.ArgumentParser()
	group = parser.add_mutually_exclusive_group(required=True)
	group.add_argument("-s","--survey", help="Download files from a survey")
	group.add_argument("-f","--file", help="Download a single file")
	group.add_argument("-c","--release", help="Download files from a release cycle")
	parser.add_argument("-y","--year", help="What year to use in download",required=True)
	parser.add_argument("-v","--verbose", help="Print to screen",action='store_true')
	parser.add_argument("-d","--debug", help="Debug",action='store_true')
	parser.add_argument("-r","--reload", help="Reload metadata",action='store_true')
	args, unknown = parser.parse_known_args()

	connect = sqlite3.connect("IWD.db")
	cursor = connect.cursor()

	if args.reload:
		load_metadata_db()

	if args.file:
		strsql = "select * from IWD_files where IPEDS_FILE=?"
		cursor.execute(strsql,[args.file])
		rows = cursor.fetchall()
	elif args.survey:
		strsql = "select * from IWD_files where IPEDS_Collection=?"
		cursor.execute(strsql,[args.survey])
		rows = cursor.fetchall()
	elif args.release:
		strsql = "select * from IWD_files where IPEDS_Cycle=?"
		cursor.execute(strsql,[args.release])
		rows = cursor.fetchall()
	
	if rows==[]:
		except_str = "No entry found in metadata db. Try either updating the db or checking the arguments you passed"
		raise Exception(except_str)

	if args.debug:
		print("DEBUG RUN")
		print("* Start up")
		for row in rows:
			print("\t->Process {0} for {1}".format(row[2],args.year))
			a_File = IWD_file(row[2], args.year)
	else:
		if args.verbose:
			print("* Start up")
			print("\tArgs Passed: {0}".format(args))
		for row in rows:
			if args.verbose:
				print("\t Process {0} for {1}".format(row[2],args.year))
			a_File = IWD_file(row[2], args.year)
			a_File.download_file()
			a_File.process_file()
			a_File.cleanup()

if __name__ == "__main__":
	# Survey Name/sub-survey abbreviation/Data
	# Survey Name/sub-survey abbreviation/Dictionary
    main()

