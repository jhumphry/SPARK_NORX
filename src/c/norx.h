/*

   A C API for the SPARK_NORX Ada 2012 / SPARK 2014 implementation of the NORX
   Authenticated Encryption Algorithm created by Jean-Philippe Aumasson, Philipp
   Jovanovic and Samuel Neves

   Copyright (c) 2016, James Humphry - see LICENSE file for details

*/
#ifndef SPARK_NORX_NORX_H
#define SPARK_NORX_NORX_H

#include <stddef.h>

void norx6441_aead_encrypt(
        unsigned char *c, size_t *clen,
        const unsigned char *a, size_t alen,
        const unsigned char *m, size_t mlen,
        const unsigned char *z, size_t zlen,
        const unsigned char *nonce, const unsigned char *key);

int norx6441_aead_decrypt(
        unsigned char *m, size_t *mlen,
        const unsigned char *a, size_t alen,
        const unsigned char *c, size_t clen,
        const unsigned char *z, size_t zlen,
        const unsigned char *nonce, const unsigned char *key);

#endif
