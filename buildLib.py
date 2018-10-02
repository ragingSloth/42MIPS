import re

#identify the lanugage primitives provided as well as the syntax for invoking them
#once identified assemble the syntax into a library file for use by the compiler
def getFunctions(fName):
    index = 0
    matches = []
    f=open(fName)
    lines = f.readlines()
    for line in lines:
        match = re.findall('[A-Z]+[0-9]*:',line)
        if len(match) == 1:
            matches.append([match,index])
        index += 1
    f.close()
    return matches, lines

def getSyntax(functions,lines):
    for i in range(len(functions)):
        j = 1
        match = re.findall('[ ]*#[a-zA-Z0-9 ,$@()]*',lines[functions[i][1]+j])
        if len(match) == 1:
            functions[i].append(match[0]+'\n') 
            while True:
                j += 1
                match =  re.findall('[ ]*#[a-zA-Z0-9 ,$@()]*',lines[functions[i][1]+j])
                if len(match) <1:
                    break
                functions[i].append(match[0]+'\n')
    return functions


if __name__ == '__main__':
    matches, lines = getFunctions('MIPSbackend.asm')
    functions = getSyntax(matches,lines)
    functions = [x for x in functions if len(x) >= 3]
    f = open('lib.asm','w')
    for i in functions:
        n = i[0][0]
        f.write(':'+n[:-1])
        f.write('\n')
        for j in range(2,len(i)):
            i[j] = i[j].replace('#','')
            f.write('<')
            f.write(i[j])
            f.write('>\n')
    f.close()
