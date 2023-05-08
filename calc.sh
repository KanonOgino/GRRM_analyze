#!/bin/bash

. ./input.txt

if [ ! -d ${path} ]; then
  mkdir ${path}
fi

cd ~/stable_ssh/zz_EQ_list/

for i in `ls | grep 'Cal*' | grep -v '.log' | sed ${nos1} | sed 's/.gjf//g' | sort -n`
do

sed -i s/exam_stable/EQ${i}_stable/g Cal_${nos4}_EQ_${i}.gjf

g09 Cal_${nos4}_EQ_${i}.gjf

done

cd ${path}
if [ -e ./zz_EQ* ]; then
  rm -r ./zz_EQ*
fi
cd ~/stable_ssh/
mv zz_EQ* ${path} 

#TS
cd ~/stable_ssh/zz_TS_list/

for i in `ls | grep 'Cal*' | grep -v '.log' | sed ${nos2}| sed 's/.gjf//g' | sort -n`
do

sed -i s/exam_stable/TS${i}_stable/g Cal_${nos4}_TS_${i}.gjf

g09 Cal_${nos4}_TS_${i}.gjf

done

cd ${path}
if [ -e ./zz_TS* ]; then
  rm -r ./zz_TS*
fi

cd ~/stable_ssh/
mv zz_TS* ${path} 

#DC
cd ~/stable_ssh/zz_DC_list/

for i in `ls | grep 'Cal*' | grep -v '.log' | sed ${nos3} | sed 's/.gjf//g' | sort -n`

do

sed -i s/exam_stable/DC${i}_stable/g Cal_${nos4}_DC_${i}.gjf

g09 Cal_${nos4}_DC_${i}.gjf

done

cd ${path}

if [ -e ./zz_DC* ]; then
  rm -r ./zz_DC*
fi

cd ~/stable_ssh
mv zz_DC* ${path} 

echo 'Normal Termination'

