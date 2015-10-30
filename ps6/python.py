##########spark##########
########setup########
from operator import add
import numpy as np
#read data into Spark from the HDFS
lines = sc.textFile('/data/airline')
#repartition the dataset to better distribute data across the nodes
lines = lines.repartition(192).cache()


########2(a)########
filterLines=lines.filter(lambda line: 'NA' not in line.split(',')[15]).cache()


########2(b)########
#write map function to create key for each unique combination and set up for later \
#computations of total number of flights as well as number of flights that are more \
#than 30 minutes, 60 minutes, and 180 minutes late for each unique key
def mapper( line ):
  vals = line.split(',')
  #convert scheduled departure time into whole hours
  schDep = list( str(vals[5]) )
  if len(schDep)==4:
    hour = ''.join(schDep[0:2])
  elif len(schDep)==3:
    hour = schDep[0]
  else:
    hour = str(0)
  #create key for each unique combination
  key = '-'.join([vals[x] for x in [8,16,17,1,3]])+'-'+hour
  #count used to calculate total number of flights
  total=1.0
  #initialize counts for flights more than 30 minutes, 60 minutes, and 180 minutes late 
  valid1=0
  valid2=0
  valid3=0
  #if flight more than 30 minutes, 60 minutes, or 180 minutes late, set count to 1
  if vals[15]>30:
    valid1 = 1.0
  if vals[15]>60:
    valid2 = 1.0
  if vals[15]>180:
    valid3 = 1.0
  return(key, (total,valid1,valid2,valid3))
  
#reduce function to aggregate statistic; get total number of flights and number of flights \
#that are more than 30 minutes, 60 minutes, and 180 minutes late for each unique key    
def reducer( (a, b, c, d), (w, x, y, z) ):
  return( (a+w),(b+x),(c+y),(d+z) )

import time
#function to time aggregation operation
def timeAggr( line ):
  start = time.time() 
  #apply map function to RDD
  mappedLines = line.map(mapper)
  #apply reduce function to resulting RDD; returns a RDD
  tmp = mappedLines.reduceByKey(reducer)
  #collect to make RDD accessible
  results = tmp.collect()
  elapsed = time.time()-start
  return(tmp,results,elapsed)

tmp,results,elapsed = timeAggr(filterLines)

tmp
#PythonRDD[11] at collect at <stdin>:5

results[0:3]
#[(u'NW-STL-MSP-3-3-15', (38.0, 38.0, 38.0, 38.0)), (u'DL-ATL-SNA-11-4-19', (10.0, 10.0, \
#10.0, 10.0)), (u'CO-GSO-MCO-8-6-15', (4.0, 4.0, 4.0, 4.0))]

elapsed
#500.448312997818


########2(c)########
#use repartition(1) to get a single data file instead of multiple pieces from the different \
#nodes hosting the HDFS
tmp.repartition(1).saveAsTextFile('/data/airline')

exit()