#!/usr/bin/python

### readlet counting script
### UPDATED TO TRY TO WORK WITH DICTIONARIES as a device
### and also to merge (& maybe filter) all at once. YEAH BABY.

import pdb
pdb.set_trace()

def countReadlets(fnames,outfname,chromosome):
#    import numpy
    import pysam

    dictlist = []

    for samp in fnames:
        samfile = pysam.Samfile(samp,"rb") # parse the .bam (sequence alignment) file
        id_start_end = []
        for read in samfile.fetch(chromosome):
            id_start_end.append([read.qname, read.pos+1, read.aend])
        rdict = {}
        for rlet in id_start_end:
            start = rlet[1]
            end = rlet[2]
            for i in range(start, end):
                if i in rdict:
                    rdict[i] += 1
                else:
                    rdict[i] = 1
        dictlist.append(rdict)

    g = open("messages.txt", 'w')
    g.write("finished dictionary-ing\n")
    f = open(outfname, 'w')

    # to write out without filtering:
    allpos = set(keys(d) for d in dictlist)
    for pos in sorted(allpos):
        f.write("%s\t" % pos)
        for d in dictlist:
            f.write("%s\t" % d.get(pos,0) )
        f.write("\n")
    f.close()

    # to write out with filtering:
    #allsamps = {k:[d.get(k,0) for d in dictlist] for k in {k for d in dictlist for k in d}}
    #g.write("finished creating whole-study dictionary\n")
    #filtered = {k:allsamps[k] for k in allsamps if numpy.median(allsamps[k])>5}
    #g.write("finished filtering")

    #for pos, nums in filtered.iteritems():
    #    f.write("%s\t" % pos)
    #    for num in nums:
    #        if num==nums[-1]:
    #            f.write("%s" % num)
    #        else:
    #            f.write("%s\t" % num)
    #    f.write("\n")


### get arguments from command line
### use: python countReadlets.py --file myfile.bam --output outfile.txt --kmer 100 --chrom 22
import argparse
parser = argparse.ArgumentParser(description="get arguments from command line")
parser.add_argument("--files", nargs = "+", help="bam files to count & filter")
parser.add_argument("--output", help="output file to store results")
parser.add_argument("--chrom", help="chromosome to parse")
args = parser.parse_args()

countReadlets(args.files,args.output,args.chrom)
