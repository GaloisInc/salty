from jinja2 import Environment, FileSystemLoader

env = Environment(loader=FileSystemLoader(''))

template = env.get_template('ltl_temp.tl')


layer_loc = {0:['A'], 1:['B'], 2:['C','D']}

rlayers = ['First', 'Second', 'Third']

n_layers = 3
n_locations = 4
n_uavs = 4
locations = []
points = [chr(i + 97).upper() for i in range(n_locations)]
locations += ["None"]
layers = []
layers += "First"

tmpinput = {}

tmpinput["controller_name"] = "Controller"

for i in range(n_locations):
    locations += [' | ']
    locations += [points[i]]

for i in range(1,n_layers):
    layers += [' | ']
    layers += [rlayers[i]]

tmpinput["types"] = "enum Pos = {} \nenum Layer = {}".format("".join(locations),
                                                     ''.join(layers))
input_string = []
for i in range(n_uavs):
    for j in range(i,n_uavs):
        if i != j:
            input_string += ["input uav{}_{}_collide : Bool = \
 False\n".format(i,j)]

tmpinput["inputs"] = "".join(input_string)

output_string = []
layers = ['First', 'Second', 'Third', 'Third']
for i in range(n_uavs):
    output_string += ["output uav{}_layer : Layer = {}\n".format(i,layers[i])]
    output_string += ["output uav{}_goto : Pos = None\n".format(i)]
    output_string += ["output uav{}_intent : Pos = None\n".format(i)]

tmpinput["outputs"] = "".join(output_string)

spec = '  !('
for i in range(n_uavs):
    for j in range(i, n_uavs):
        if i != j:
            spec += 'uav{}_{}_collide'.format(i,j)
            if i != n_uavs - 2 or j != n_uavs -1:
                spec += ' /\ '
            else:
                spec += ')'

tmpinput["env_trans"] = [spec]

#the layer/location relationship
specs = []
spec = ''
for i in range(n_uavs):
    for l in range(n_layers):
        lh = "  uav{}_layer == {} -> (".format(i, rlayers[l])
        rh = ""
        for j in range(len(layer_loc[l])):
            rh += 'uav{}_goto == {}'.format(i, layer_loc[l][j])
            if j == len(layer_loc[l]) - 1:
                rh += ' \/ uav{}_goto == None)'.format(i)
            else:
                rh += ' \/ '
        spec = lh + rh
        specs += [spec]

#must not be in the same layer if they collide
for i in range(n_uavs):
    for j in range(j,n_uavs):
        if i != j:
            spec = '  uav{z}_{r}_collide -> uav{z}_layer != uav{r}_layer'.format(z=i,r=j)
            specs += [spec]

#the intention/goto relationship
for i in range(n_uavs):
    for j in range(n_locations):
        spec = "  uav{i}_goto' == {j} -> uav{i}_intent == {j}".format(i=i, j=points[j])
        specs += [spec]


for i in range(n_uavs):
    for j in range(1,n_layers-1):
        spec = """  uav{i}_layer == {j} -> uav{i}_layer' ==\
 {j}""".format(i=i,j=rlayers[j])
        spec += """ \/ uav{i}_layer' == {ju} \/ uav{i}_layer' ==\
 {jd}""".format(i=i,ju=rlayers[j+1], jd=rlayers[j-1])
        specs += [spec]
    spec = """  uav{i}_layer == First -> uav{i}_layer' == First \/ uav{i}_layer' ==\
 Second""".format(i=i)
    specs += [spec]
    spec = """  uav{i}_layer == {layer} -> uav{i}_layer' == {layer} \/\
 uav{i}_layer' == {layerd}""".format(i=i, layer=rlayers[-1], layerd=rlayers[-2])
    specs += [spec]

sys_trans_strings = specs
tmpinput["sys_trans"] = sys_trans_strings

specs = []
for i in range(n_uavs):
    for j in range(n_locations):
        spec = "  uav{i}_goto' == {j}".format(i=i,j=points[j])
        specs += [spec]

sys_liveness_strings = specs
tmpinput["sys_liveness"] = sys_liveness_strings

with open("ctrl.salt",'w+') as f:
    f.write(template.render(**tmpinput))
