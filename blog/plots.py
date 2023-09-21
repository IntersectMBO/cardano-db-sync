import time
import psycopg2
import matplotlib.pyplot as plt
from itertools import accumulate
import operator
import numpy as np


def extract(row):
  return (row[1], row[2])

def to_hours (sec):
  return sec / 3600

def to_mins (sec):
  return sec / 60

def cumulative_sum(input_list): 
  cumulative_sum_iter = accumulate(input_list, operator.add)
  return list(cumulative_sum_iter)

def listfun (rows):
  tuples = map(extract, rows)
  unzipped = list(zip(*tuples))
  epochs = unzipped[0]
  secs = unzipped[1]
  cumul_secs = list(cumulative_sum(secs))
  cumul_hours = list(map (to_hours, cumul_secs))
  mins = list(map (to_mins, secs))
##  hours = map (to_hours, unzipped[1])
#  print (list(hours))
#  cumul_hours = cumulative_sum(hours)
#  print (cumul_hours)
  return (epochs, cumul_hours, mins)

def get3from4 (ls):
  return [ls[0], ls[1], ls[2]]

def getlast (ls):
  return [ls[len(ls) - 1]]

dbs = ["default_outputs",  "extra_column", "pruned"]
# dbs = ["default_outputs",  "extra_column", "pruned", "default_full"]
query_type = [1, 0, 0, 1]
colors = ['blue', 'red', 'green', 'black']
disk_size = [107, 120, 63, 251]
max_memory = [2, 2, 2, 16]


def utxo_query (address, typ):
  match typ:
    case 0:
     return ("select * from tx_out where address = \'" + address + "\' and tx_out.consumed_by_tx_in_id is null;")
    case _:
     return ("select * from tx_out where address = \'" + address + "\' and not exists " + """( SELECT tx_in.id
          from tx_in where
          tx_out.tx_id = tx_in.tx_out_id AND
          tx_out.index = tx_in.tx_out_index);""")

data = {}
addresses = {}
output_number = ['1', '100', '1000', '10000']
addresses["1"] = ['9XQrTpiaBYn1Qsg8GvodVJPTkRiVQLj4XwgwWUuVcrYMYyXioT1GyjRQgDK1oKjFce7Sw4YqgpAds5wxZFEoXQK4SZN8nhrVSuAo', 'addr128phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcrtw79hu', 'addr12yqg7sr6pp7ndc6vuq4fj64wgzp9zhh45jv4wngxcf0aqxruzkw47xrh6a0', 'addr1g8knx74trpqt3zz6lf2jw65gw0eh0j3xwv48088u2dpy0wue5wfpyzgq24f502', 'addr1g8nltc2ks84xlarvvajpdn6mgrasqk6vta79xtahj6gkxd37vp5shsm708', 'addr1g98zq74pz94csr2p9r294sezaw540shqx9rxw5wfvz99vmr5xyqq8adl3c', 'addr1gy5p8wv6sr8m8eu0ulmscyhwhvukawnfzxv8s4ck8wtdv009fhm4pphll83kk0eaarn', 'addr1gy5p8wv6sr8m8uw0j4jnk95mzup4jc65fqmm0n3n6vr3p288zzsfquk83n8szt42fvr', 'addr1gy5p8wv6sr8mgqjrwj7s75pft9y94ftwqey9vnlcqhew2xaumxqe2cdam3npgv60hqa', 'addr1q800047xyktwh4vcus6ch3t0mr4xuq3rf43ne0xl4vpcjuullay0hzpuff33jjkwjn5eyd4t8erj7jt3ll6kp3z7jfyqpwrdm8', 'addr1q80004tgvnnx6kkhkdv4d64a8h3qdwmwj0acrcj67yekmsfseh32qa5mpe4f3xaddknlp6slk7d6jg9v2npg3khzmfjs2ke8h2', 'addr1q800052j7xnplytmfukxjypt00gu4s6r67ya9we3j5vtpsdfgjldmxk8zgexlncrkx7xgm8qm27djjm5r3n82upmxznszvzf3m', 'addr1q80009g08w544qr2v56edk5tyeh6fhvalnjkf7ya4p58qne4nspy5yf7j87zk37agrxmp78rgp3a4wnvsntpjky2jjyq2u30ly', 'addr1q80009twy2hr8rh0m5htrtanydfnxh8kny0q5ses5xe56hs8aw4njh5qx5mh32f7tv770ke3pvzhgruq4l7rzzlkgstsyx6d8q', 'addr1q80009uv0n7uf2sfqm48qnve9z6fzcskzv057d5qkrg55k6vkfwjlmekk8pmma73vy7pq5yzztvjuv8p8pw8szwjw80s3larmh', 'addr1q8000c43qqf70zla8u99ly7amwsyc4qf8lq7z855k44h397wlrvres06nzvgawvdcuacdwfzyar3su3dtymhd8u6lnjq8wk8qw', 'addr1q8000ch4cxttsj5d8dcdnmfydy5nf7rezlu3punqz5dmjp5aelq2k4qs8x3cgck9s96t70j3skjhfjex5vv8m7tc705sg6dzrg', 'addr1q8000clce6ay5377tpq93t4v0ac6tl4hxmj39pfw9cqqe4e5c6qj52pe8d2vhg84tgtme54fdzg8cmxuxkdmjmemvwnqvz72kp', 'addr1q8000gh7zxn9upykvvsrvcs4hqdggrcgqhqmeggs9fdpp96v2n2hnrz9q5e3nnv6ss7lvs32t8cytv68w9dfyye2s92su752sk', 'addr1q8000gngx04tet2c35cv9fawvjurl25444yx4e300dhn8ln8z4arptfu0tpukngyz5ykee2hq6n3gp4e908x3pv465lqql7zuy']

addresses["100"] = ['addr1q833w5xxlu5js2u4zzucvp54jn7l6cnhpt5vz67kaw3kyn8unqrggmqcq4d3sgwtnea5kv3nx7pel6ga46wsz5l0u3ysyf85gt', 'addr1q8de54w53gzrjl4ssekw3jrj0pgj90jsug6q64s4ty4renu6pqrw2a0eqf2asdzcx9g5v36l0le5wlm7un7a9q8rsdpquvfqy3', 'addr1q8mk4jyjww2qsjue874dfn3h7z2lcqvwnlwyq2umeapnd70tf0hqhkl2qh4j0nuql59e5ntsa2sysawrmtznuvqqlhxqjt3l89', 'addr1q8pe3ud2ugq7zn65qhpyrym22lfeywsecsrux8dqygwfgwkmj0pw2nzgvq7qmav9y9huy5vnuxjegvckuskdaux6k6fq9zue5f', 'addr1q8r7zqzcsdfn493q09ha2rcu44qwq4e8yxmlv8rj0yntjls4ss7cmladpsjz90vugggsjpkgr4fvqe87dnd5jw0ft0vs4cdxn9', 'addr1q8rx5u2evam6lcsdkaw6tgp5ree6seymqq9qtp22wa2gmxyvxhx5356ctx5v9r84p90zmwg08flqgwty4zjcn3aau53sjm2fx3', 'addr1q8u7kgwzyv5cm20630vdasv2y9gr48exqu2muzl2390vvn570eje7tn9tad77w0junslej8ld0yy86nddqg2u4kgfgzqycmcd2', 'addr1q8v2g4lnan45gyq80x9pesjaywhn45n88mxhswjagfnr4vf6w07rz6nlusf6s7dzrhw79rfpkfvw5u5hygarvzhfqvuqrrsehw', 'addr1q8vryult99kj5naza0ejjy9q8w90n76n6nn86d0r60entm6shuz2h05wze9j2uhmj44fnn0ws9fxp8q6a6rpmap74elqlz5sye', 'addr1q8x5u370rvwpzwue0yq62zdft09zhl78pex8cyz6wqt2se7m06eecv9vjqlnve30m3f70wxl8z6870z8t5a37p6yy8ss0s8rer', 'addr1q8xvk5639uejl7s5aen5xrl6ks8y7aq44ctwh3cjz03vj68k6kadgsudjgqm4m3x65a7qpt5896w8m2wt3jesjqwe7ysxjwa6q', 'addr1q92gsssaee9v4dyq6e6y0zq0v3rxh8x59a2x5mqyngh85djylkedhkzxk73f9sqnnarn5fhyedhs9wuu4vr2zlrqsntsdv9vap', 'addr1q9479ym3y9vh43jfc772s3rdravkvqtzv8v3jkhkxzukgu9wugwvza3xsy82myff7rhp2tj6496sjx3csh8l9j5mutjq5daww2', 'addr1q94k28mjny703adh74ake3l79wx2dwunxdle7g5ldsh6p35z05adjhz7nw5c30s7cqtqcu202kfy7apn2agugs54qzpqj6lcm4', 'addr1q957vp9ly2azr2kj25wkle04yeuxp5w2z7urexhdlgx7s3a5q6yg2jmckvx5ua47na8aehy963cu5u8uhsca7xtkh3ws2gdd4p', 'addr1q97femvm3cnjfzhe93549zy594utsvcxntsnc66egkgyff78z2mlkm0xy8cldhdezq7eanm5ftaf0ggnpdphpcvff2gqwnta2p', 'addr1q98082gl9cgn03equxt3ngv0036hmdsac4s5saazum86wgxdnul9tamasrsh3lptz5q204xxhgl668qe4d9a8nd458uqgf304m', 'addr1q9mmnmy26echdldern0w9k3pj06n59g9w840lxxke2rwcha9y8qlgz92tafgawujk0qzyx6cxlsd9f2k0732x9k7xdxsnlcslr', 'addr1q9pfzhpg6y9efhwzu7a8jk4s98rq0920saadsyzhsc5w5ufs77v2wattlspuy3kd2tkj39luuh6yxa67t62eh7udja0qmxhmz7', 'addr1q9qvzqelzytxxq0c6fkz3x22r989gfqdq90j9jaupzrephg5hxhj6h8ju8qvv802erm0pp4vs69vclurccvghr2g3hjsnc7cap']

addresses["1000"] = ['addr1q80qee2gsa0dr5m73zfwec76s785szq5dywlk0q6wmrrhxe2ey5x0zac2qflv2vz27kg0u6rp6u6p8l3ahhr5h43q3hs5krgxc', 'addr1q80xrnzzu7rgpw65r3cj9qmlfdfdj5dplf38xweuxxtcrqu3am3v72q0vzta5mmrc0nlr2xfxxs2f58j7eqr2l5xmthqzay7zr', 'addr1q836qf2vqzv57uc42rupyw039fsvnlfuaxumry25x9fwcg43hmpsthwgqxya4j9k9rhq4ha72fzu2wuyueuw6lkz846sde0a0x', 'addr1q83mys0kcm3svuv2s7a5cceungt3tahx2cyuge6kr7k5rnp8zha6lzvqzma4wt0z5f5655ukayyudp0yye8vhnt76t5qh055ys', 'addr1q865gv9pc5zuhsgyumrjpvxymmh5a5ndqysty8k9wey2d230nrhsm0q0c2twn0nmp7rjh4zqdllmfxaw2daallch6d8qel5sr6', 'addr1q86jp8fpya6q9t4ad7x89p7gdqfeqhm39c46ymd865kuwtdzt3lv5xdqkqzlm8tughz750u76ewf46gf3lxajn8wrh4q4yzt8j', 'addr1q86vamqy020245w02fwgwc8j3qv7gvr2m23tmrqv2htzqkxuy8qgkg4wk4w08dpe24qklsm4v40zmcugs3x8zrqvrnqqp68t4u', 'addr1q872sx5q8afvy004ur4sdzp7vxkw3l988fyl9052d8xk7reph00q6we03dd9c0yg8p5psnru4zmzkzprz7zsfqw4hfdq6jy47d', 'addr1q87cx65lr2ay32zpuakmrc2g42zcf68j7nr04r79l5nd4gs6rv9d7mkkj0xyywj3qecg9wd7aqy4hjzfd3fke79feptsy5gcmd', 'addr1q87k79llvkz4kv720860ag5d0zlzheea5elzd8dnsjzmuvsacrmsa7huc6z3y45ekkzfzjdgmzke5e2yx6zr6ddhlpfq62g6xs', 'addr1q87zlevtssntccuu5afvg0j8hpdyes7k5w9k3lg7eu6jxsd46zeke4dvex433xpgufpz3q8tvh2vhcr6hnel8673khfsnzpy2f', 'addr1q88ahx5ttzem56jexzal2hcahglj9krpqmxm58pm3f60ra7xpkjgrn305ctrlhx3s87nq4nks5pwlvqt45lpr8jht8nqs3tcpv', 'addr1q88ar6whf4dyg786m8a9p5fmt9quah7jkvxvpyv67pj6gf0622wkwsh50fchdlxw5qvj4gf3gkp5ptcuwmcq9zwrj0us2ssehs', 'addr1q88tg69f9e43arseqavzhr7ueflzmcq9kjf3v3cffjujusytzse867whjq63k4uj8mtyuc5j5ny8wq5ha7qku8f7ac7sm6h8w4', 'addr1q893sk0cf5hl0nvrf0nyzclmk0vcsrp08a9tjvlmc9tysn9g845k9d2zdj22kpelpa9j4s7adz5v58m9w4u9zl9a9mgs0sqgya', 'addr1q89rmyu5dzuak897tfnve72tccqfkyvea7zpjytygyz0m9mqcs0wk87xanag8d7w2ju0p9r6dnjw5x0jv5aa073uracq5mg38j', 'addr1q8axvtsmmdw0qetpjl206cxfyrdqleyg9vghdfz9enwv94sl0khwp4q4nvgv9lc5pnwdz9qn52j9pxxtpa85pe2apeqs8j0l9z', 'addr1q8df44fnt0fujnmdg3ggt4vt3xg2nrh80045tjufrgrcry3g2s4uzs8ugpeuz0qegtea6sd8uuffljhg8ulg49904vjq35gdza', 'addr1q8dx3392e6zyxw4e6e8qued9e3t73d05v84e057fp7x2a2j3u36fsagqszj6yufkrcrnterqzts52sgydrjn4unads2se0lxh7', 'addr1q8g59w6mtcnperwy5yq7fg3g547f9vjq8709wj3xk0w28573g2a4kh3xrjxufggpuj3z3ftuj2eyq0u72a9zdv7u50fssflav4']

addresses["10000"] = ['addr1w9jx45flh83z6wuqypyash54mszwmdj8r64fydafxtfc6jgrw4rm3', 'addr1zxgx3far7qygq0k6epa0zcvcvrevmn0ypsnfsue94nsn3tvpw288a4x0xf8pxgcntelxmyclq83s0ykeehchz2wtspks905plm', 'addr1w999n67e86jn6xal07pzxtrmqynspgx0fwmcmpua4wc6yzsxpljz3', 'addr1qxs76zpnfyq5w0xrpjwrkkghch0ew024j83hx0dg00f9xjrx828mrjy00s2sd8awhvummze55m8hq4fqghdzqlcaqwlshwkm0m', 'addr1zxj47sy4qxlktqzmkrw8dahe46gtv8seakrshsqz26qnvzypw288a4x0xf8pxgcntelxmyclq83s0ykeehchz2wtspksr3q9nx', 'addr1q86ylp637q7hv7a9r387nz8d9zdhem2v06pjyg75fvcmen3rg8t4q3f80r56p93xqzhcup0w7e5heq7lnayjzqau3dfs7yrls5', 'addr1q9ad5mud938r7l24eaac4v57ydlj72hazgprts9f7daaz09mm95jvs5h94099yglxr7v6ug6pp43780huy040lm4utmqx8g5eu', 'addr1qys6p3j97mfxe72rxkh5d9lp3cdjqp37dd8t5yjq5vl9m93h0dy20dzu4wpwtfre93yd2ph2e57ppft6kyc0dg28yp7shh7t9v', 'addr1qycewgm43uc96vt3qjp434mqp4jfzttws0xjwqz4a364qu95mx98r9d2mpx5ka4xe5npakhrz2qz4n2tqzgvyngrkedqn3hctc', 'addr1q9cwvremt6n320s2e3agq0jyq82yhrk3htsu0w426xnz5us70z4w0jgvcdkkynmm8wmds66jd9kusnjfpu6raw5fqp0sr07p5w', 'addr1w9eu87z6ywets8talp9fv94kv6c7rjx9lnllv7pan39p53gkjg05e', 'addr1wxc45xspppp73takl93mq029905ptdfnmtgv6g7cr8pdyqgvks3s8', 'addr1w9yr0zr530tp9yzrhly8lw5upddu0eym3yh0mjwa0qlr9pgmkzgv0', 'addr1q88ysqegp378eag7fauv5zyjvvuy8mca3dc0925hv0nzl8qp0jwyns8qwzf5dqtdlwkv7qt5upzcyfmd5yl43s89txvse89ujz', 'addr1qyht4ja0zcn45qvyx477qlyp6j5ftu5ng0prt9608dxp6l2j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq4jxtdy', 'addr1qxtrqdumg8dleqcra3myptlq6n43m8s0mver0pwgqrr8awvkkcdaz26hglgm4qvc6fdy0rr4ck6q5q249drqc4fzyrgq68vuva', 'addr1z8flqry5fspwjtgpxua6ssq6ppz6mxetnr04ftvzd3v93gj75jq4yvpskgayj55xegdp30g5rfynax66r8vgn9fldndseu7ewa', 'addr1zxn9efv2f6w82hagxqtn62ju4m293tqvw0uhmdl64ch8uw6j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq6s3z70', 'addr1qx56qfea3ksrnp3q9r5fx3w8ppxs9ascqaks69yuvapc55l77lw2x9y7payhqmktcuse4uvv8jzexdkf6aa2hzkcnv3qjk4dy2', 'addr1w89s3lfv7gkugker5llecq6x3k2vjvfnvp4692laeqe6w6s93vj3j']

q = utxo_query ('addr1q88ar6whf4dyg786m8a9p5fmt9quah7jkvxvpyv67pj6gf0622wkwsh50fchdlxw5qvj4gf3gkp5ptcuwmcq9zwrj0us2ssehs', 0)
print (q)
q = utxo_query ('addr1q88ar6whf4dyg786m8a9p5fmt9quah7jkvxvpyv67pj6gf0622wkwsh50fchdlxw5qvj4gf3gkp5ptcuwmcq9zwrj0us2ssehs', 1)
print (q)

for i in range(len(dbs)):
  db = dbs[i]
  data[i] = {}
  qtype = query_type[i]
  data[i]["name"] = db
  data[i]["type"] = qtype

  conn = psycopg2.connect(database = db,
                        user = "kostas",
                        host= '/var/run/postgresql',
                        password = "",
                        port = 5432)

  cur = conn.cursor()
  cur.execute('SELECT * FROM epoch_sync_time;')
  rows = cur.fetchall()
#  print (rows)
  conn.commit()

# print (list(rows))
  data[i]["sync"] = listfun(rows)
  times = []
  for io in output_number:
    acctime = 0
    for address in addresses[io]:
      cur = conn.cursor()
      start_time = time.time()
      q = utxo_query (address, qtype)
      cur.execute(q)
      acctime = acctime + time.time() - start_time
      rows = cur.fetchall()
      conn.commit()
#      print (list(rows))
    times.append(1000 * acctime / len (addresses[io]))
  conn.close()
  data[i]["query"] = times

print (data)

for i in range(len(dbs)):
  db = dbs[i]
  color = colors[i]

  plt.xlabel("epoch")
  plt.ylabel("cumulative time(hours)")
  plt.plot(data[i]["sync"][0],data[i]["sync"][1], c=color, label=db)
plt.legend(loc='best')
plt.savefig("plots/plot_cumul_sync_time.png")
plt.clf()


for i in range(len(dbs)):
  db = dbs[i]
  color = colors[i]

  plt.xlabel("epoch")
  plt.ylabel("time(minutes)")
  plt.plot(data[i]["sync"][0],data[i]["sync"][2], c=color, label=db)
plt.legend(loc='best')
plt.savefig("plots/plot_sync_time.png")
plt.clf()

x = np.arange(1)
width = 0.1  # the width of the bars
multiplier = 0
fig, ax = plt.subplots() # layout='constrained')
for i in range(len(dbs)):
  db = dbs[i]
  clor = colors[i]
  offset = width * multiplier
  rects = ax.bar(x + offset, [disk_size[i]], width, label=db,align='center', color = clor)
  ax.bar_label(rects, padding=1, color = clor)
  multiplier += 1

plt.xlabel("disk")
plt.ylabel("size(GB)")
plt.legend(loc='best')
ax.set_xticks(x + (len(dbs)-1) * width/2, ["epoch 425"])
plt.savefig("plots/plot_disk_size.png")
plt.clf()


x = np.arange(1)
width = 0.1  # the width of the bars
multiplier = 0
fig, ax = plt.subplots() # layout='constrained')
for i in range(len(dbs)):
  db = dbs[i]
  clor = colors[i]
  offset = width * multiplier
  rects = ax.bar(x + offset, [max_memory[i]], width, label=db,align='center', color = clor)
  ax.bar_label(rects, padding=1, color = clor)
  multiplier += 1

plt.xlabel("")
plt.ylabel("max memory(GB)")
plt.legend(loc='best')
ax.set_xticks(x + (len(dbs)-1) * width/2, ["epoch 425"])
plt.savefig("plots/plot_max_memory.png")
plt.clf()


x = np.arange(3)
width = 0.25  # the width of the bars
multiplier = 0
fig, ax = plt.subplots(layout='constrained')
for i in range(len(dbs)):
  db = dbs[i]
  clor = colors[i]
  offset = width * multiplier
  rects = ax.bar(x + offset, get3from4(data[i]["query"]), width, label=db, color = clor)
  ax.bar_label(rects, padding=3, color = clor)
  multiplier += 1

plt.xlabel("outputs number")
plt.ylabel("time(ms)")
plt.legend(loc='best')
ax.set_xticks(x + (len(dbs)-1) * width/2, get3from4(output_number))
plt.savefig("plots/plot_query_time.png")
plt.clf()

x = np.arange(1)
width = 0.1  # the width of the bars
multiplier = 0
fig, ax = plt.subplots() # layout='constrained')
for i in range(len(dbs)):
  db = dbs[i]
  clor = colors[i]
  offset = width * multiplier
  rects = ax.bar(x + offset, getlast(data[i]["query"]), width, label=db, color = clor)
  ax.bar_label(rects, padding=3, color = clor)
  multiplier += 1

plt.xlabel("outputs number")
plt.ylabel("time(ms)")
plt.legend(loc='best')
ax.set_xticks(x + (len(dbs)-1) * width/2, getlast(output_number))
plt.savefig("plots/plot_query_time_many.png")
plt.clf()



"""

for i in range(len(dbs)):
  db = dbs[i]
  color = colors[i]

  plt.xlabel("outputs number")
  plt.ylabel("time(seconds)")
 ## plt.plot(output_number, data[i]["query"], c=colors[i], label=db)  # c = 
plt.legend(loc='best')
plt.savefig("plots/plot_query_time.png")
plt.clf()

"""
