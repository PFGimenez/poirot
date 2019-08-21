import difflib
def comparer(a,b):
	n = 0
	print('{} => {}'.format(a,b))  
	for i,s in enumerate(difflib.ndiff(a, b)):
		if s[0]==' ': continue
		elif s[0]=='-':n+=1
	return n

a = "<html>\ntruc\n</html>"
b = "<html>\n\n</html>"

print comparer(a,b)


