# screwed up lisp and rewrote in python

import re

with open("4.dat", "r") as f:
    data = [re.compile(r"[\s\n]+").split(x) for x in f.read().split("\n\n")]

for i, d in enumerate(data):
    data[i] = {p.split(':')[0]: p.split(':')[1] for p in d}

def pass_valid(d):
    try:
        if not 1920 <= int(d["byr"]) <= 2002:
            return False
        if not 2010 <= int(d["iyr"]) <= 2020:
            return False
        if not 2020 <= int(d["eyr"]) <= 2030:
            return False
        hgt = re.match(r"^(\d+)(\w+)$", d["hgt"])
        height, unit = hgt.groups()
        if unit == "cm":
            if not 150 <= int(height) <= 193:
                return False
        elif unit == "in":
            if not 59 <= int(height) <= 76:
                return False
        else:
            print('wrong unit')
            return False

        if not re.match("^#[0-9a-f]{6}$", d["hcl"]):
            return False

        if not d["ecl"] in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]:
            return False

        if not re.match("^\d{9}$", d["pid"]):
            return False
        
        return True
    except Exception:
        return False
    
data = [d for d in data if pass_valid(d)]

print(data)
print(len(data))
