#!/usr/bin/env python3
from PIL import Image, ImageDraw, ImageFont
import json, time, sys

if len(sys.argv) != 3:
    print("Usage: {} <caddy/access.log> <out.png>".format(sys.argv[0]), file=sys.stderr)
    sys.exit(1)

logfilename = sys.argv[1]
destfname = sys.argv[2]

font = ImageFont.truetype("/var/lib/caddy/DejaVuSans.ttf", 10)
# font = ImageFont.load_default()

resolution = 3600  # width in seconds of one bin
imgwidth = 900
imgheight = 200
ymultiplier = 1

requests = []
with open(logfilename) as f:
    for line in f:
        obj = json.loads(line)
        uri = obj["request"]["uri"]
        if uri in ["/play/run", "/play/core", "/play/asm"
                  ,"/compile/run", "/compile/core", "/compile/asm"
                  ,"/submit"]:
            requests.append((obj["ts"], obj["duration"]))

requests.sort(key=lambda r: r[0])

# bin => (number of requests, total duration)
binned = {}
for req in requests:
    key = int(req[0] / resolution)
    numreq, totaldur = binned[key] if key in binned else (0, 0.0)
    binned[key] = (numreq + 1, totaldur + req[1])

im = Image.new("RGB", (imgwidth, imgheight), (255, 255, 255))
draw = ImageDraw.Draw(im)

nowtime = time.time()
nowkey = int(time.time() / resolution)
for x in range(imgwidth - 1, -1, -1):
    prevkeytime = time.gmtime(nowtime - (imgwidth - 1 - (x - 1)) * resolution)
    keytime     = time.gmtime(nowtime - (imgwidth - 1 - x      ) * resolution)
    if prevkeytime.tm_mday != keytime.tm_mday:
        draw.line([(x, 0), (x, 10)], (100, 100, 100))
        draw.line([(x, 11), (x, imgheight - 1)], (230, 230, 230))
        year = "'" + str(keytime.tm_year)[2:]
        month = ("0" + str(keytime.tm_mon))[-2:]
        mday = ("0" + str(keytime.tm_mday))[-2:]
        draw.text((x+1, 0), year, font=font, fill=(0, 0, 0))
        draw.text((x+1, 12), month, font=font, fill=(0, 0, 0))
        draw.text((x+1, 24), mday, font=font, fill=(0, 0, 0))

    key = nowkey - (imgwidth - 1 - x)
    numreq, totaldur = binned[key] if key in binned else (0, 0.0)
    durheight = int(totaldur)
    if numreq > 0:
        draw.line([(x, imgheight - 1), (x, max(0, imgheight - numreq))], (255, 0, 0))
    if durheight > 0:
        draw.line([(x, imgheight - 1), (x, max(0, imgheight - durheight))], (0, 255, 0))

im.save(destfname, "PNG")
