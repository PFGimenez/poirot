import sys


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





