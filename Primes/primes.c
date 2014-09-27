#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

#define FALSE   0
#define TRUE    1

int main(int argc, char **argv);
void usage(void);
nn_t primeIter(nn_t prime_product, nn_t test_num, nn_t primes_left);

int main(int argc, char **argv)
{
    if (argc != 2) usage();
    printf("%d\n", primeIter(1, 2, atoll(argv[1])));
}

void usage(void)
{
    printf("usage: primes <nth prime>");
    exit(1);
}

nn_t primeIter(nn_t prime_product, nn_t test_num, nn_t primes_left)
{
    nn_t new_test_num;
    printf("z: %d, t: %d, n: %d\n", prime_product, test_num, primes_left);
    if (is_prime(prime_product, test_num))
    {
        if (primes_left == 1) return test_num;
        if (test_num == 2) new_test_num = 3;
        else new_test_num = test_num + 2;
        return primeIter(prime_product * test_num,
                         new_test_num,
                         primes_left - 1);
    } else {
        return primeIter(prime_product, test_num + 2, primes_left);
    }
}

int is_prime(nn_t prime_product, nn_t test_num)
{
    if (test_num == 2) return TRUE;
    return (gcd(prime_product, test_num) == 1);
}

int gcd(nn_t a, nn_t b)
{
    if (b == 0) return a;
    return gcd(b, a % b);
}
