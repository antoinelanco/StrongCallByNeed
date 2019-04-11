from igraph import *
import json

f= open("treeData.json","r")
j = f.read()
d = json.loads(j)[0]
f.close()

# print(d)

def f(l):
    if not "children" in l:
        return [l["name"]]
    else:
        t = []
        for i in l["children"]:
            t += f(i)
        t.append(l["name"])
        return t

def ff(s,l):
    return l.index(s)

def f7(seq):
    seen = set()
    seen_add = seen.add
    return [x for x in seq if not (x in seen or seen_add(x))]

l = f7(f(d))



g = Graph(directed=True)
le = len(l)
g.add_vertices(le)
g.vs["color"] = (["blue"] + (["pink"]*(le-2)) + ["red"])

def h(r,l):
    if "children" in r:
        tmp = []
        for i in r["children"]:
            tmp.append((ff(r["name"],l),ff(i["name"],l)))
            tmp += h(i,l)
        return tmp
    else:
        return []

ed = list(set(h(d,l)))
g.add_edges(ed)
g.vs["label"] = l
g.vs["label_dist"] = -2.5
layout = g.layout("kk")
plot(g, layout = layout, bbox = (1920, 1080), margin = 300)
