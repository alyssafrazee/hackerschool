#!/usr/bin/python

### readlet counting script
### using numpy arrays instead of dictionaries - see if they are faster, more efficient, and allow us to filter all at once.
### and also to merge (& maybe filter) all at once. YEAH BABY.

# add chromosome lengths
chrlen = 249250621
fnames = ["orbFrontalF1-small.bam", "toyfile.bam"]

def countReadlets(fnames,outfname,chromosome):
    import numpy as np
    import pysam

    dictlist = []

    for samp in fnames:
        samfile = pysam.Samfile(samp,"rb") # parse the .bam (sequence alignment) file
        id_start_end = []
        for read in samfile.fetch(chromosome):
            id_start_end.append([read.qname, read.pos+1, read.aend])
        
        print "starting sample!"
        print len(id_start_end)
        
        datarow = np.zeros((chrlen, len(fnames)))
        for idx,rlet in enumerate(id_start_end):
            print "starting readlet" + " " + str(idx)
            for i in range(rlet[1], rlet[2]):
                    datarow[i] += 1
        
        print "finished one sample!"
        
        if fnames.index(samp)==0:
            datatable = datarow
        else:
            datatable = np.vstack((datatable, datarow))
            
    return datatable

#starr-seq



### get arguments from command line
### use: python countReadlets.py --file myfile.bam --output outfile.txt --kmer 100 --chrom 22
#import argparse
#parser = argparse.ArgumentParser(description="get arguments from command line")
#parser.add_argument("--files", nargs = "+", help="bam files to count & filter")
#parser.add_argument("--output", help="output file to store results")
#parser.add_argument("--chrom", help="chromosome to parse")
#args = parser.parse_args()

#countReadlets(args.files,args.output,args.chrom)


# also try sparsearray data structure