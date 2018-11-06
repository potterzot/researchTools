#!/usr/bin/env python3

"""
USAGE: get_paper.py doi 
Creates a new paper note with full metadata and downloads the pdf:
* Uses the Crossref API to fetch paper metadata and abstract
* Automatically download the pdf and saves it in standard naming convention
"""

# -*- coding: utf-8 -*-

import os, sys, getopt
import string
import re
import requests
import json
from datetime import date
from collections import OrderedDict
from itertools import chain #for flattening a list
from metapub import CrossRef as cr

#######################
# Global Variables

#Depending on operating system
if sys.platform == "linux" or sys.platform == "linux2":
  HOME_DIR = "/www/potterzot.github.io"
elif sys.platform == "darwin":
  # OS X
  pass
elif sys.platform == "win32":
  # Windows...
  HOME_DIR = "C:/Users/Nicholas Potter/code/potterzot.github.io"

#Universal
REPO_DIR = "_notes"
MD_DIR = "papers"
PDF_DIR = "Dropbox/personal-work/bibliography/library"

USAGE_TXT = 'USAGE: get_paper.py [OPTION] <list of DOIs>\n\n'
HELP_TXT = '\n' + USAGE_TXT + 'OPTIONS:\n\
  -h, --help:       print this message. \n\
  -d, --debug:      fetch DOI metadata and print to screen, but do not write to any files. \n\
  -m, --markdown:   create a markdown file to add notes to. \n\
  -b, --biblio:     specify the bibtex file to add an entry to. \n\
  \n\
Example: ./get_paper.py -m -b temp.bib 10.1257/jep.24.2.31\n\
'

##################
# Methods
def clean_txt(txt):
  """Takes txt and returns a sanitized utf-8 string."""
  r = txt.encode("utf-8", errors="backslashreplace").decode('utf-8').replace("\\u0144", "")
  return r


def doi2json(doi):
  """
  Takes a DOI string and returns a JSON string of metadata.
  """
  if "arxiv" in doi:
    print("This script does not yet support arXiv.")
    sys.exit(2)
  else:
    url = "https://dx.doi.org/" + doi

    headers = {"accept": "application/json"}
    r = requests.get(url, headers = headers)

    if repr(r)=="<Response [200]>": #success!
      #handle potential encoding errors
      rtxt = clean_txt(r.text)
      try:
        j = json.loads(rtxt)
      except Exception as e:
        print(e)
        print(r)
        print(repr(rtxt))
        print("Error, DOI {} not found.".format(doi))
        sys.exit(2)
    elif repr(r)=="<Response [503]>":
      print("Error 503: DOI service currently unavailable, try again in a bit.")
      sys.exit(2)
    elif repr(r)=="<Response [404]>":
      print("Error 404: Resource {} not found".format(url))
      sys.exit(2)
    else:
      print("Error {}.".format(repr(r)))
      sys.exit(2)

  return j


def extract_metadata(res):
  """Creates an OrderedDict of metadata for writing."""
  d = OrderedDict()

  d['type'] = res['type']
  d['authors'] = make_author_list(res)
  d['citation-authors'] = make_citation_authors(res)
  d['citation'] = make_citation(res)
  d['source-title'] = res['title'].strip(" ").capitalize()
  d['title'] = "Notes on '" + d['source-title'] + "', by " + ", ".join(d['authors'][0:-1])
  if len(d['authors']) > 1:
    d['title'] = d['title'] + ", and " + d['authors'][-1] 
  d['citation-authors']
  d['container'] = make_container(res)
  d['publisher'] = res['publisher'] if 'publisher' in res.keys() else '' 
  d['issn'] = res['issn'] if 'issn' in res.keys() else ''
  d['year'] = make_year(res)
  d['issue'] = res['issue'] if "issue" in res.keys() else ""
  d['volume'] = res['volume'] if "volume" in res.keys() else ""
  d['pages'] = res['page'] if "page" in res.keys() else ""
  d['subjects'] = make_subject(res)
  d['doi'] = res['DOI']
  d['link'] = res['URL']
  d['citationkey'] = make_citation_key(res)
  d['fetched'] = date.today().strftime("%Y%m%d")
  return d


def make_year(res):
  """
  Takes the DOI result and returns a string of the year.
  """
  return str(res['issued']['date-parts'][0][0])

def make_author_list(res):
  """Takes a list of author names and returns a cleaned list of author names."""
  try:
    r = [", ".join([clean_txt(x['family']), clean_txt(x['given'])]) for x in res['author']]
  except KeyError as e:
    print("No 'author' key, using 'Unknown Author'. You should edit the markdown file to change the name and citationkey.")
    r = ["Unknown Authors"]
  return r 


def make_citation(meta):
  """Creates a markdown citation from metadata."""
  pass

def make_citation_authors(res):
  """Takes a DOI json string and returns a string of authors in Chicago style."""
  if "author" in res.keys():
    first_author = res['author'][0]['family'] + ", " + res['author'][0]['given']
    last_author = res['author'][-1]['given'] + " " + res['author'][-1]['family']
    middle_authors = ", ".join(" ".join([x['given'], x['family']]) for x in res['author'][1:-1])
    #assemble authors
    author_string = first_author
    author_string = author_string + ", " + middle_authors if middle_authors != '' else author_string
    author_string = author_string + ", and " + last_author if len(res['author']) > 1 else author_string
    
    author_string = author_string + "." if author_string[-1] != "." else author_string
  else:
    author_string = "Unknown Authors"

  return clean_txt(author_string)
  

def make_citation_key(res):
  """Takes a DOI query and returns a string citation key."""
  year = str(make_year(res))

  try:
    last_names = [x['family'] for x in res['author']]
  except KeyError as e:
    last_names = ["Unknown"]
  if len(last_names) >= 3:
    key = last_names[0] + "ETAL" + year
  else:
    key = "".join(last_names) + year

  return clean_txt(key.replace(" ", ""))


def make_container(res):
  """Given DOI metadata return a journal name."""
  journal_dict = {
    "Am. J. Agr. Econ.": "American Journal of Agricultural Economics",
    "Annu. Rev. Resour. Econ.": "Annual Review of Resource Economics",
    "J. Appl. Econ.": "Journal of Applied Economics",
    "J Med Internet Res": "Journal of Medical Internet Research",
    "Pers Ubiquit Comput": "Perspectives on Ubiquitous Computation",
    "PLoS Comput Biol": "PLoS Computational Biology",
    "PLoS Med": "PLoS Medicine"}

  print(res['container-title'])
  s = res['container-title'] if "container-title" in res.keys() else "Unknown"
  if s is None: s = "Unknown"
  if len(s) == 0: s = ""
  
  return journal_dict[s] if s in journal_dict.keys() else s


def make_subject(res):
  """Takes the DOI metadata and returns a list of subjects."""
  subj_dict = {
    "Agricultural and Biological Sciences (miscellaneous)": "Agricultural and Biological Sciences",
    "Unsorted": "Unsorted"}
  keys = subj_dict.keys()
  s = res['subject'] if "subject" in res.keys() else ["Unsorted"]
  
  return [subj_dict[x] if x in keys else x for x in s]


def main(argv):
  """
  Takes a DOI and creates a markdown file with the article metadata, as well as attempting to download the pdf.
  """
  if len(argv) == 0 or argv == None:
      print(HELP_TXT)
      sys.exit(2)
  try:
    opts, args = getopt.getopt(argv,"dhmb:",["debug", "help", "markdown", "biblio="])
  except getopt.GetoptError:
    print(HELP_TXT)
    sys.exit(2)

  testing = False
  markdown = False
  bibtex_filename = "library.bib"
  
  for opt, arg in opts:
    if opt in ('-h', '--help'):
      print(HELP_TXT)
      sys.exit()
    elif opt in ('-d', '--debug'):
      testing = True
      print("Fetching of DOI and printing metadata, but not adding to bibtex or creating markdown.")
    elif opt in ('-b', '--biblio'):
      bibtex_filename = arg
      print(arg)
    elif opt in ('-m', '--markdown'):
      markdown = True
      print("Writing entry to bibtex file but not creating a markdown file.")

  if len(args) == 0:
    print("Error: At least one DOI must be provided.")
    sys.exit(2)

  for doi in args:
    #Get the metadata from crossref.org
    res = doi2json(doi)
    
    #extract
    meta = extract_metadata(res)
    
    #check that reference exists
    citation_found = True
    letters = list(string.ascii_lowercase)
    with open(bibtex_filename, 'r') as fobj:
        text = fobj.read().strip()
        tmp_key = meta['citationkey']
        for letter in letters:
            if tmp_key in text:
                tmp_key = meta['citationkey'] + letter
            else:
                meta['citationkey'] = tmp_key
                break

    #Then write if not testing
    if testing:
      print("DOI RESULTS:")
      for k,v in res.items():
        if type(v) is list:
          print(k + ': ' + str(v) + '\n')
        else:
          print(k + ': "' + str(v) + '"\n')
      print("PARSED METADATA:")
      for k,v in meta.items():
        if type(v) is list:
          print(k + ': ' + str(v) + '\n')
        else:
          print(k + ': "' + str(v) + '"\n')
    else:
      #Add the metadata to the bibtex reference
      with open(bibtex_filename, "a") as f:
        s = ",".join([
          "\n@article{" + meta['citationkey'],
          "\nauthor = {" + " and ".join(meta['authors']) + "}",
          "\ndoi = {" + meta['doi'] + "}",
          "\nissn = {" + meta['issn'] + "}",
          "\njournal = {" + meta['container'] + "}",
          "\npublisher = {" + meta['publisher'] + "}",
          "\nnumber = {" + meta['issue'] + "}",
          "\npages = {" + meta['pages'] + "}",
          "\ntitle = {" + meta['source-title'] + "}",
          "\nurl = {" + meta['link'] + "}",
          "\nvolume = {" + meta['volume'] + "}",
          "\nyear = {" + meta['year'] + "}\n}\n\n"
        ])
        f.write(s)
      
      print("reference {} added to {}!\n".format(meta['citationkey'], bibtex_filename))
      
      # create the markdown notes file
      if markdown:
        subject_dir = re.sub(r'\([^)]*\)', '', meta['subjects'][0]).strip().lower().replace(" ", "_").replace(",","")
        md_dir = "/".join([HOME_DIR, REPO_DIR, MD_DIR, subject_dir])
        os.mkdir(md_dir) if os.path.isdir(md_dir) == False else None
        filename = "/".join([md_dir, meta['citationkey'] + ".md"])
        with open(filename, "w") as f:
          f.write("---\nlayout: mathpost\n")
          for k,v in meta.items():
            try:
              if type(v) is list:
                f.write(k + ': ' + str(v) + '\n')
              else:
                f.write(k + ': "' + str(v) + '"\n')
            except UnicodeEncodeError as e:
              print("Unicode Error. Some character(s) may be wrong.")
              print(meta.keys())
              print(repr(k) + ": " + repr(v))
              print(e)
        
          f.write("---\n\n")
          citation = "**Citation:** " \
            + meta['citation-authors'] + ' "' \
            + meta['source-title'] + '". ' \
            + '*' + meta['container'] + '*'
          citation = citation + " " + meta['volume'] if meta['volume'] != '' else citation
          citation = citation + ", no. " + meta['issue'] if meta['issue'] != '' else citation
          citation = citation + " (" + meta['year'] + ")" if meta['year'] != '' else citation
          citation = citation + ": " + meta['pages'] if meta['pages'] != '' else citation
        
          f.write(citation + ". [[Paper link](" + meta['link'] + ")]")
          
          #add the reference to the "reading_list.md" file
          with open("/".join([HOME_DIR, REPO_DIR, MD_DIR, "reading_list.md"]), "a") as f:
            f.write("* [ ] **" + meta['citationkey'] \
              + "**: (" \
              + re.sub(r'\([^)]*\)', '', meta['subjects'][0]).strip() \
              + ") " + meta['link'] + "\n")
          
          print("reference {} added in {}!\n".format(meta['citationkey'], md_dir))
      

if __name__ == '__main__':
  main(sys.argv[1:])
