#define swapbytes16(x) \
{ \
        unsigned short int data = *(unsigned short int*)&(x); \
        data = ((data & 0xff00) >> 8) |    \
               ((data & 0x00ff) << 8);     \
        *(unsigned short int*)&(x) = data;       \
}
/* macro to swap the bytes in a 32-bit variable */
#define swapbytes32(x) \
{ \
        unsigned int data = *(unsigned int*)&(x); \
        data = ((data & 0xff000000) >> 24) |    \
               ((data & 0x00ff0000) >>  8) |    \
               ((data & 0x0000ff00) <<  8) |    \
               ((data & 0x000000ff) << 24);     \
        *(unsigned int*)&(x) = data;            \
}
/* macro to swap the bytes in a 64-bit variable */
#define swapbytes64(x) \
{ \
        unsigned int *words = (unsigned int *)&(x); \
        unsigned int temp0;  \
        unsigned int temp1;  \
        temp0 = words[0];    \
        swapbytes32(temp0);  \
        temp1 = words[1];    \
        swapbytes32(temp1);  \
        words[1] = temp0;    \
        words[0] = temp1;    \
}

#define MAX(a,b)       (a < b) ? (b) : (a)
