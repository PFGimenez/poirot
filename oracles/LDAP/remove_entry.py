#!/usr/bin/env python

import sys
import ldap
import ldap.modlist as modlist

server = 'ldap://localhost:389'
connect = ldap.initialize(server)
connect.simple_bind_s("cn=admin,dc=vuln,dc=com","secret")
deleteDN = "cn=user1, dc=vuln, dc=com"
try:
	connect.delete_s(deleteDN)
except ldap.LDAPError, e:
	print e
connect.unbind_s()
