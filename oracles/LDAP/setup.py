#!/usr/bin/env python

import sys
import ldap
import ldap.modlist as modlist

def main(user_server,domain,user_login,user_password):

    server = user_server
    connect = ldap.initialize(server)
    domain_list = domain.split(".")
    domain_name = domain_list[0]
    domain_code = domain_list[1]
    login_credentials = "cn=" + user_login + ",dc=" + domain_name + ",dc=" + domain_code
    connect.simple_bind_s(login_credentials,user_password)

    # we add the login information of a test user
    dn="cn=user1,dc="+domain_name+",dc="+domain_code
    attrs = {}
    attrs['objectclass'] = ['person']
    attrs['cn'] = 'user1'
    attrs['sn'] = 'Bob'
    attrs['description'] = 'hardtoguess'
    ldif = modlist.addModlist(attrs)
    connect.add_s(dn,ldif)

    connect.unbind_s()

if __name__ == "__main__":
    if(len(sys.argv) == 5):
        main(sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4])
    else:
        print("Usage: python setup.py [server] [domain] [domain_login] [domain_password]")