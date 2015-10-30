##########EC2##########
########1########
######setup######
#login
ssh -i ~/.ssh/stat243-fall-2015-ssh_key.pem ubuntu@52.32.71.69
mkdir -p mnt/airline
cd mnt
#increse storage in /mnt
git clone "https://github.com/berkeley-stat243/stat243-fall-2015"
cd stat243-fall-2015/howtos
sudo ./setup-storage
cd ~/mnt/airline
#download airline dataset
wget http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz 
tar -xvzf 1987-2008.csvs.tgz

#start R
export PATH=${PATH}:/root/R/bin
R


########4########
col=1,2,3,4,5,6,7,8,9,10,15,16,17,18,19

time for ((i=1987; i<=2008; i++)) 
do 
bzcat $i.csv.bz2 | cut -d',' -f${col}| bzip2 > clean$i.csv.bz2 
done
# real	16m39.767s
# user	20m12.716s
# sys	0m26.456s


##########spark##########
########2########
######setup######
#setup Spark cluster
#create UNIX environment variables containing AWS credentials
export AWS_ACCESS_KEY_ID=`grep aws_access_key_id stat243-fall-2015-credentials.boto | cut \
-d' ' -f3`
export AWS_SECRET_ACCESS_KEY=`grep aws_secret_access_key stat243-fall-2015-credentials.boto \
| cut -d' ' -f3`
#change permissions on the private SSH key file
chmod 400 ~/.ssh/stat243-fall-2015-ssh_key.pem
cd spark-1.5.1/ec2
export NUMBER_OF_WORKERS=12
#start a spark cluster
./spark-ec2 -k sc.huang@berkeley.edu:stat243-fall-2015 -i ~/.ssh/stat243-fall-2015-ssh_key.pem \
--region=us-west-2 -s ${NUMBER_OF_WORKERS} -v 1.5.1 launch sparkvm-sc.huang
#login
./spark-ec2 -k sc.huang@berkeley.edu:stat243-fall-2015 -i ~/.ssh/stat243-fall-2015-ssh_key.pem \
--region=us-west-2 login sparkvm-sc.huang

mkdir /mnt/airline
cd /mnt/airline
#download airline dataset to master node
wget http://www.stat.berkeley.edu/share/paciorek/1987-2008.csvs.tgz
tar -xvzf 1987-2008.csvs.tgz

export PATH=$PATH:/root/ephemeral-hdfs/bin/
#set up directories in HDFS
hadoop fs -mkdir /data/airline
#copy the dataset onto it
hadoop fs -copyFromLocal /mnt/airline/*bz2 /data/airline
hadoop fs -ls /data/airline

#install python packages
yum install -y python27-pip python27-devel
pip-2.7 install 'numpy==1.9.2'
#start python in spark
export PATH=${PATH}:/root/spark/bin
pyspark


######2(c)######
hadoop fs -ls /data/airline
#Warning: $HADOOP_HOME is deprecated.

#Found 2 items
#-rw-r--r--   3 root supergroup          0 2015-10-29 04:58 /data/airline/_SUCCESS
#-rw-r--r--   3 root supergroup  351289530 2015-10-29 04:56 /data/airline/part-00000

#copy file to master node
hadoop fs -copyToLocal /data/airline/part* /mnt/airline

head part*
#(u'DH-CVG-MDW-12-5-21', (4.0, 4.0, 4.0, 4.0))
#(u'DL-SLC-SMF-5-5-22', (27.0, 27.0, 27.0, 27.0))
#(u'OO-MKE-CLE-9-6-7', (4.0, 4.0, 4.0, 4.0))
#(u'OH-ORF-CVG-3-5-13', (4.0, 4.0, 4.0, 4.0))
#(u'US-JAX-CLT-9-4-10', (61.0, 61.0, 61.0, 61.0))
#(u'WN-BUR-PHX-4-4-11', (41.0, 41.0, 41.0, 41.0))
#(u'EV-ORD-CVG-7-3-13', (3.0, 3.0, 3.0, 3.0))
#(u'HP-SEA-ANC-9-4-11', (4.0, 4.0, 4.0, 4.0))
#(u'OH-CVG-BNA-4-4-7', (4.0, 4.0, 4.0, 4.0))
#(u'NW-MEM-OKC-6-6-14', (38.0, 38.0, 38.0, 38.0))

./spark-ec2 --region=us-west-2 --delete-groups destroy sparkvm-sc.huang