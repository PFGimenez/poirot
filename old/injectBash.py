import random, string, requests
import difflib
import sys
from random import randint
import time
random.seed()

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
	#print url+" : "+argvalue
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
def containsArgEgal(motif):
	return any(motif in s for s in sys.argv)

def extractArgEgal(motif):
	return [x for x in sys.argv if motif in x][0][(len(motif)):]

def usage():
	print "Usage : {} [-m|-n|-d|-t] --param=<mode parameter> --input=<input file> --url=<targeted URL> --arg=<GET targeted parameter> \n\t-m : motif\n\t-n : non motif\n\t-d : difference\n\t-t : time".format(sys.argv[0])
	exit(0)
def getArgsEgals():
	if containsArgEgal("--url=") and containsArgEgal("--arg=") and containsArgEgal("--param=") and containsArgEgal("--input="):
		url = extractArgEgal("--url=")
		arg = extractArgEgal("--arg=")
		param = extractArgEgal("--param=")
		inputs = extractArgEgal("--input=")
		return (url,arg,param,inputs)
	else:
		usage()

def getArgs():
	if ("-m" in sys.argv or "--motif" in sys.argv):
		mode="motif"
		(url,arg,param,inputs) = getArgsEgals()
	
	elif ("-n" in sys.argv or "--nmotif" in sys.argv):
		mode="nmotif"
		(url,arg,param,inputs) = getArgsEgals()

	elif ("-d" in sys.argv or "--difference" in sys.argv):
		mode="difference"
		(url,arg,param,inputs) = getArgsEgals()
	elif ("-t" in sys.argv or "--time" in sys.argv):
		mode="time"
		(url,arg,param,inputs) = getArgsEgals()
	else:
		usage()
	return (mode,url,arg,param,inputs)

(mode,url,arg,param,inputs) = getArgs()


if mode == "difference" or mode == "time":
	marge = float(param)
	(diff,time) = stats(url,arg)
	if mode=="difference":
		moyenne = diff
	else:
		moyenne = time
else:
	motif = param
if mode == "difference":
	chain = insert_spaces(randomword(randomnumber(50)))
	print (chain + "\n")
	payload = {arg: chain}
	r = requests.get(url, params=payload)
	motif = r.text


f = open(inputs,'r')
lignes  = f.readlines()
f.close()

for ligne in lignes:
	(r,t) =	requeteGET(url,arg,ligne.rstrip())
	if mode == "motif":
		if motif in r:
			print "REQ:"+ligne,
	elif mode == "nmotif":
		if not(motif in r):
			print "REQ:"+ligne,
	elif mode == "difference":
		val = comparer(r,motif)
		#print(val)
		if not(val >= moyenne-(moyenne*marge) and val <= moyenne+(moyenne*marge)):
			print "REQ:"+ligne,
	elif mode == "time":
		val = t
		if not(val >= moyenne-(moyenne*marge) and val <= moyenne+(moyenne*marge)):
			print "REQ:"+ligne,
		#print (r.text+ "("+str(r.elapsed.total_seconds())+"s)")
	
