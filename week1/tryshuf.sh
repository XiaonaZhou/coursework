#!/bin/bash
#get_fixed_random()
#{
 # openssl enc -aes-256-ctr -pass pass:"$1" -nosalt </dev/zero 2>/dev/null
#}

read SEED
shuf -i 1-28 -n 1 --random-source=<(echo $SEED)
