import random, string, requests
from random import randint

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


for j in range(1,100):
	#chain = "\""
	chain = insert_spaces(randomword(randomnumber(50)))
	chain2 = insert_spaces(randomword(randomnumber(50)))
	#chain = chain + randomword(randomnumber(50))
	#chain = chain + "\""
	#call(["./thomas",chain])
	print (chain + "\n")
	payload = {'ip': chain}
	r = requests.get('http://localhost/injections/ping.php', params=payload)
	print (r.text)
	
