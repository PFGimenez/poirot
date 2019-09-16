import random, string, requests
from random import randint
from subprocess import call
import difflib
import os,time,sys



class Waiting:
	states = (' / ',' -- ',' \\ ',' | ')
	step = 0
	def __init__(self,msg):
		self.msg = msg
		self.next()

	def next(self):
		message = "{}  {}".format(self.msg,Waiting.states[Waiting.step % 4])
		sys.stdout.write("\r"+message)	
	   	sys.stdout.flush() # Pour raffraichir l'affichage
		Waiting.step += 1
		time.sleep(0.05)

random.seed()

def randomnumber(n):
		return randint(1,n)

def randomword(length):
   return ''.join(random.choice(string.lowercase+string.uppercase+string.digits) for i in range(length))

def insert_spaces(s):
    s = list(s)
    for i in xrange(len(s)-1):
        if (randomnumber(10)==10):
            s[i] = s[i] + ' '
    return ''.join(s)

def comparer(a,b):
	n = 0
	#print('{} => {}'.format(a,b))  
	for i,s in enumerate(difflib.ndiff(a, b)):
		if s[0]==' ': continue
		elif s[0]=='-':n+=1
	return n

def requeteGET(url,argname,argvalue):
	payload = {argname: argvalue}
	r = requests.get(url, params=payload)
	return (r.text,r.elapsed.total_seconds())
	
def stats(url,arg):
	tab = []
	tabTime = []
	nbRequetes = 100
	progressBarGet = Waiting("Envoi des requetes aleatoires ")
	for j in range(1,nbRequetes):
		progressBarGet.next()
		chain = insert_spaces(randomword(randomnumber(50)))
		(response,time) = requeteGET(url,arg,chain)
		tab.append(response)
		tabTime.append(time)

	totalCmp = 0.0
	total = 0.0

	for a in tab : 
		for b in tab : 
			totalCmp += comparer(a,b)
			total += 1

	return (totalCmp/total,sum(tabTime)/nbRequetes)
