#!/usr/bin/python

### readlet counting script
### UPDATED TO TRY TO WORK WITH DICTIONARIES as a device

def countReadlets(fname,outfname,chromosome):
    import pysam
    samfile = pysam.Samfile(fname,"rb") # parse the .bam (sequence alignment) file
    id_start_end = []
    for read in samfile.fetch(chromosome):
        id_start_end.append([read.qname, read.pos+1, read.aend]) #id_start_end will be a list of readlet alignments, containing read ID (readlets coming from same read have same ID), alignment start position, and alignment end position

    rdict = {}
    for rlet in id_start_end:
        start = rlet[1]
        end = rlet[2]
        for i in range(start, end):
            if i in rdict:
                rdict[i] += 1
            else:
                rdict[i] = 1

    f = open(outfname, 'w')

    for pos, num in sorted(rdict.iteritems()):
        f.write("%s\t%s\n" % (pos,num))


### get arguments from command line
### use: python countReadlets.py --file myfile.bam --output outfile.txt --kmer 100 --chrom 22
from optparse import OptionParser
opts = OptionParser()
opts.add_option("--file","-f",type="string",help="input file name (must be .bam, and must be indexed with samtools first)")
opts.add_option("--output","-o",type="string",help="output file name")
opts.add_option("--chrom","-c",type="string",help="chromosome to parse")
options,arguments = opts.parse_args()


countReadlets(options.file,options.output,options.chrom)
