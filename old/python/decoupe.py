import sys

def decoupe(input_file):
	inputfile = open (input_file,"r")
	outputfile = open("out"+input_file,"w") 
	for l in inputfile.readlines ():
		for c in l:
			if (c != "\n"):
				outputfile.write(c+"\n")
	return outputfile.name

def compare (file1,file2):
	f1 = open (file1,"r")
	f2 = open (file2,"r")
	res = 0
	for line1,line2 in zip(f1,f2):
		if line1 != line2:
			res = res + 1
	return res

def findmin(filenames):
	fn = open(filenames,"r")
	i = 0
	heuristique = sys.maxint
	outtab = []
	for filename in fn.readlines ():
		outtab.append(decoupe(filename.rstrip('\n')))
		i = i + 1
	i = 0
	for i in range(len(outtab)-1) :
		for j in range (i+1,len(outtab)):
			minimum = compare(outtab[i],outtab[j])
			print "Comparaison de (" + str(i) + "," + str(j) + ") : " + str(minimum) + "\n"
			if(heuristique > minimum) :
				heuristique = minimum
	print "L'heuristique vaut : " + str(heuristique)
			
findmin("liste.txt")
