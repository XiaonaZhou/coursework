#!/bin/bash

# r1 is a random number between 1 and 28
# r1 is the location of the first pair 
r1=$(($(shuf -i 1-28 -n 1 )))

# use r1 to find p1
p1=$(($(awk 'NR=='$r1'{print $1}' pairs.txt)))

# pair2.txt contains all useable pairs after selecting the first pair
grep [$p1] -v  pairs.txt > pairs2.txt

# l1 is total number of pairs in pairs2.txt
l1=$(($(awk '{temp+=1} END{print temp}' pairs2.txt)))

# r2 is a random number between 1 and l1
# r2 is the location of the second pair

r2=$(($(shuf -i 1-$l1 -n 1 )))

# use r2 to find p2
p2=$(($(awk 'NR=='$r2'{print $1}' pairs2.txt)))

# pair3.txt contains all useable pairs after selecting the second pair
grep [$p2] -v  pairs2.txt > pairs3.txt
l2=$(($(awk '{temp+=1} END{print temp}' pairs3.txt)))
r3=$(($(shuf -i 1-$l2 -n 1 )))

p3=$(($(awk 'NR=='$r3'{print $1}' pairs3.txt)))


p4=$(($(grep [$p3] -v  pairs3.txt)))

echo $p1 $p2 $p3 $p4



