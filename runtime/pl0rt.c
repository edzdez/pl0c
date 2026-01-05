#include <stdio.h>

void _write(long int v)
{
    printf("%ld\n", v);
}

void _read(long int *ptr)
{
    scanf("%ld", ptr);
}
