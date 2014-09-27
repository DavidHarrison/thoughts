#!/bin/sh
primeiter()
{
    # z - running product of primes
    z=$1
    num=$2
    # count - primes left to count
    count=$3
    echo "z: $z, num: $num, count: $count"
    # if the count is equal to zero, break
    if [ `echo "$count == 0" | bc` -ne 0 ]
    then
        `echo $num - 1 | bc`
    elif [ `isprime $z $num` ]
    then
        primeiter `echo $z * $num | bc` `echo $num + 1 | bc` `echo $count - 1 | bc`
    else
        primeiter $z `echo $num + 1 | bc` $count
    fi
}

isprime()
{
    z=$1
    num=$2
    # if the number is 2, then it is prime
    if [ `echo "$num == 2" | bc` -ne 0 ]
    then
        true
    gcdv=`gcd $z $num`
    #if the gcd equals one, the number is not prime
    # if the gcd is not 1, return true
    elif [ `echo "$gcdv == 1" | bc` -eq 0 ]
    then
        true
    # otherwise, return false
    else
        false
    fi
}

gcd()
{
    # if the second argument is zero, the first argument is the gcd
    if [ `echo "$2 == 0" | bc` -ne 0 ]
    then
        echo $1
    else
        gcd $2 `echo $1 % $2 | bc`
    fi
}

# primeiter 1 2 "$1"
if [ `isprime 1 2` ]
then
    echo "Prime"
else
    echo "Not Prime"
fi
