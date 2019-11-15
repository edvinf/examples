import numpy

#
# Extracts SM2 scanmar telegrams from scanmar logfiles with 1 telegram for each line
#
def extract_sm2(telegramlines):
    header = ("timestamp hhmmss.ss", "status A/V", "sensortype", "sensorid", "measurementid", "measurementvalue", "qualityfactor", "checksum")
    data = []
    for l in telegramlines:
         d = l.split(",")
         if d[0]=="$PSCMSM2":
             assert len(d)==8
             
             timest = d[1]
             status = d[2]
             sensortype = d[3]
             sensorid = d[4]
             measid = d[5]
             
             if (d[6].strip()!=""):
                 measvalue = float(d[6])
             else:
                 measvalue = numpy.NaN
             
             tail = d[7].split("*")
             quality = tail[0]
             checks = tail[1].strip()
             
             data.append((timest, status, sensortype, sensorid, measid, measvalue, quality, checks))
    return (header, data)

def writecsv(header, data, stream):
    for h in header[:-1]:
        stream.write(h)
        stream.write(", ")
    stream.write(header[-1])
    stream.write("\n")
    for d in data:
        assert len(d) == len(header)
        for r in d[:-1]:
            stream.write(str(r))
            stream.write(", ")
        stream.write(d[-1])
        stream.write("\n")

def extract_telegrams(telegramtype, file, targetfile):
    f = open(file, "rU")
    telegramlines = f.readlines()
    f.close()
    
    if telegramtype == "SM2":
        print("Extracting SM2 formatted telegrams")
        header, data = extract_sm2(telegramlines)
    else:
        sys.stderr.write("Telegramtype: " + telegramtype + " not supported.\n")
        sys.exit(2)
    
    f = open(targetfile, "w")
    writecsv(header, data, stream=f)
    f.close()
    
    

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 4:
        sys.stderr.write("Usage: telegramtype scanmarfile targetfile\n")
    else:
        extract_telegrams(sys.argv[1], sys.argv[2], sys.argv[3])