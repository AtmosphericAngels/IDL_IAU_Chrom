from pathlib import Path
import re

eq_wo_spaces = re.compile('[^\ ]\=[^\ ]')



def set_spaceequalspace(content):
    out = []
    for i, s in enumerate(content):
        vd = (',' not in s)
        if i > 0:
            vd = (vd and
                  ('pro' not in content[i-1].lower()) and
                  ('function' not in content[i-1].lower()) and
                  ('$' not in content[i-1]))
        if vd:
            if r := re.search(eq_wo_spaces, s):
                out.append(s[:r.span()[0]+1] + " = " + s[r.span()[1]-1:])
            else:
                out.append(s)
        else:
            out.append(s)
    return out



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
        content_out = set_spaceequalspace(content_out)

        with open(f, 'w') as fobj:
            fobj.writelines(content_out)