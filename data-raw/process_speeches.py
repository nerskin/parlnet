
# coding: utf-8

# This notebook aims to ingest the Hansard XML dumps, extract the information I care about, and write it to CSV files.

# In[75]:


import os
import re
import sys
import multiprocessing

from bs4 import BeautifulSoup
import  pandas as pd



class Speech:
    """Class to represent a speech, question, or answer in parliament"""
    
    def __init__(self,speech,type):
        """Should only be called from the constructor for Debate"""
        self.name = speech.find('name').get_text()
        try:
            self.name_id = speech.find('name.id').get_text()
        except:
            self.name_id = 'Missing'
        #try/except needed to account for format change in 2011
        try:
            self.text = speech.find('talk.text').get_text()
        except:
            self.text = "\n".join([x.get_text() for x in speech.find_all('para')])
        try:
            self.party = speech.find('party').get_text()
        except: 
            self.party = 'NA'#'party' is occasionally missing in the XML.
        self.type = type

class Division:
    
    def __init__(self,division,i):
        self.id = i
        self.result = division.find('division.result').get_text().strip()
        try:
            self.ayes = [name_tag.get_text() for name_tag in division.find('ayes').find_all('name')]
        except:
            self.ayes = []
        try:
            self.noes = [name_tag.get_text() for name_tag in division.find('noes').find_all('name')]
        except:
            self.noes = []
        parent = division.parent
        grandparent = parent.parent
        self.parent_title = parent.title.get_text()
        self.grandparent_title = grandparent.title.get_text()
        
class Subdebate:
    
    def __init__(self,subdebate,i):
        self.id = i
        self.title = subdebate.find('title')
        self.speeches = list()
        for i in ['speech','question','answer']:
            self.speeches += [Speech(speech,i) for speech in subdebate.find_all(i)]
        self.divisions = [Division(division,i) for i,division in enumerate(subdebate.find_all('division'))]

class Debate:
    """Class to represent a debate in parliament"""
    
    def __init__(self,debate,i):
        """Should only be called from the constructor for SittingDay"""
        self.id = i
        self.title = debate.find('title')
        self.subdebates = list()
        try:
            self.type = debate.type.get_text()
        except:
            self.type = 'NA'
        subdebate = True
        index = 1
        id = 1
        to_avoid = list()
        while subdebate:
            subdebate=debate.find_all('subdebate.'+str(index))
            if subdebate:
                for subd in subdebate:
                    if subd not in to_avoid:
                        self.subdebates.append(Subdebate(subd,i))
                    to_avoid.extend(subd.descendants)
                    i+=1
                index+=1
        
        
        
class SittingDay:
    """Class to represent a whole parliamentary sitting day"""
    
    def __init__(self,day):
        """takes a beautifulsoup object"""
        self.date = day.find('date').get_text()
        debates = day.find_all('debate')
        self.debates = [Debate(debate,i) for i,debate in enumerate(debates)]
        self.chamber = day.find('chamber').get_text()
        if self.chamber == 'House of Reps':
            self.chamber = 'HoR'
        
    def write_divisions(self,path = '.'):
        name = list()
        vote = list()
        date = list()
        parent_title = list()
        grandparent_title = list()
        debate_type = list()
        debate_id = list()
        subdebate_id = list()
        chamber = list()
        division_id = list()
        for debate in self.debates:
            for subdebate in debate.subdebates:
                for division in subdebate.divisions:
                    all_names = division.ayes + division.noes
                    for member in all_names:
                        name.append(member)
                        if member in division.ayes and member in division.noes:
                            vote.append('NA')#this must be an issue with the data - members cannot physically be on both sides
                        elif member in division.ayes:
                            vote.append('aye')
                        elif member in division.noes:
                            vote.append('no')
                        date.append(self.date)
                        chamber.append(self.chamber)
                        parent_title.append(division.parent_title)
                        grandparent_title.append(division.grandparent_title)
                        debate_type.append(debate.type)
                        debate_id.append(debate.id)
                        subdebate_id.append(subdebate.id)
                        division_id.append(division.id)
        try:
            data_frame = pd.DataFrame({'name':name,'vote':vote,'parent_title':parent_title,'grandparent_title':grandparent_title,
                                   'date':date,'chamber':chamber,'debate_type':debate_type,'debate_id':debate_id,'subdebate_id':subdebate_id,'division_id':division_id})
            data_frame.to_csv(path+'/'+self.date + '-' + self.chamber + '-' + 'divisions.csv',encoding = 'utf-8',index = False)
        except:
            with open('log.txt','a') as f:
                f.write(str([len(x) for x in [name,vote,date,parent_title,grandparent_title,
                                          debate_type,debate_id,subdebate_id,chamber,division_id]])+'\n')
    
    def write_speeches(self,path='.'):
        """Write all the debates to a csv file (one file per day)"""
        #ToDo: extend to also include votes
        text = list()
        name = list()
        party = list()
        date = list()
        debate_title = list()
        subdebate_title = list()
        debate_type = list()
        chamber=list()
        debate_id = list()
        subdebate_id = list()
        type = list()
        name_id = list()
        for debate in self.debates:
            for subdebate in debate.subdebates:
                for speech in subdebate.speeches:
                    text.append(speech.text)
                    name.append(speech.name)
                    party.append(speech.party)
                    date.append(self.date)
                    debate_title.append(debate.title)
                    subdebate_title.append(subdebate.title)
                    debate_type.append(debate.type)
                    chamber.append(self.chamber)
                    debate_id.append(debate.id)
                    subdebate_id.append(subdebate.id)
                    type.append(speech.type)
                    name_id.append(speech.name_id)
        data_frame = pd.DataFrame({'name':name,'party':party,'date':date,'text':text,'chamber':chamber,'debate_title':debate_title,
            'subdebate_title':subdebate_title,'debate_type':debate_type,'debate_id':debate_id,'subdebate_id':subdebate_id,'type':type,'name_id':name_id})
        data_frame.to_csv(path + '/' + self.date+ '-' + self.chamber +'-speeches.csv',encoding='utf-8',index=False)
        
    def write_proceedings(self,speeches_path='.',divisions_path='.'):
        self.write_speeches(speeches_path)
        self.write_divisions(divisions_path)


# FixMe:
# * ~~Split up debates into subdebates~~
# * ~~Add divisions~~
# * Add dictionary to associate MPs' names as listed in divisions with their names as listed elsewhere. 

# Since building the BeautifulSoup representation of each XML file is somewhat slow, it's worth parallelising the process:

# In[78]:


def xml_to_csv(x):
    try:
        data_file = x[0]
        speeches_path = x[1]
        divisions_path = x[2]
        with open(data_file,'r') as f:
            SittingDay(BeautifulSoup(f.read(),'lxml')).write_proceedings(speeches_path,divisions_path)
        return 'done'
    except Exception as e:
        print(str(e))
        with open('log.txt','a') as f:
            f.write(x[0]+'\n')
        return 'error'



if __name__ == '__main__':
    data_files = os.listdir('./xml_data/')
    data_files = [f for f in data_files if re.match('.*xml',f)]
    data_files = ['./xml_data/'+data_file for data_file in data_files]




    (speeches_path,divisions_path) = ('tidied_parliamentary_data/speeches','tidied_parliamentary_data/divisions')

    try:
        os.makedirs(speeches_path)
    except OSError:
        pass #nothing to do if the directories already exist


    try:
        os.makedirs(divisions_path)
    except OSError:
        pass #nothing to do if the directories already exist

    input = [(data_file,speeches_path,divisions_path) for data_file in data_files]


# In[79]:

    pool = multiprocessing.Pool(multiprocessing.cpu_count()-1)
    pool.map(xml_to_csv,input)

    print('done')
