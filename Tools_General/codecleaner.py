from pathlib import Path

def rmv_endspace(s):
    s = s.replace(' \n', '\n')
    if s.find(' \n') != -1:
        return rmv_endspace(s)
    return s

if __name__ == '__main__':
    for f in list(Path('../').rglob('*.pro')):
        with open(f, 'r') as fobj:
            content = fobj.readlines()

        content_out = [rmv_endspace(l) for l in content]

        with open(f, 'w') as fobj:
            fobj.writelines(content_out)