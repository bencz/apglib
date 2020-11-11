/*
 * CRYPT(3) IMPLEMENTATION FOR UCLIBC
 *
 * THE UCLIBC LIBRARY IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
 * MODIFY IT UNDER THE TERMS OF THE GNU LESSER GENERAL PUBLIC
 * LICENSE AS PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER
 * VERSION 2.1 OF THE LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
 *
 * THE GNU C LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
 * BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  SEE THE GNU
 * LESSER GENERAL PUBLIC LICENSE FOR MORE DETAILS.
 *
 * YOU SHOULD HAVE RECEIVED A COPY OF THE GNU LESSER GENERAL PUBLIC
 * LICENSE ALONG WITH THE GNU C LIBRARY; IF NOT, WRITE TO THE FREE
 * SOFTWARE FOUNDATION, INC., 59 TEMPLE PLACE, SUITE 330, BOSTON, MA
 * 02111-1307 USA.
 *
 */

#ifndef _CRYPT_H
#define _CRYPT_H        1

//#include <features.h>

//__BEGIN_DECLS

/* Encrypt characters from KEY using salt to perturb the encryption method.
 * If salt begins with "$1$", MD5 hashing is used instead of DES. */
/*extern char *crypt (const char *__key, const char *__salt) __THROW __nonnull ((1, 2));                                                               */
extern char *crypt (const char *__key, const char *__salt) ;

/* Setup DES tables according KEY.  */
/*extern void setkey (const char *__key) __THROW __nonnull ((1));                                                                                      */
extern void setkey (const char *__key) ;

/* Encrypt data in BLOCK in place if EDFLAG is zero; otherwise decrypt
   block in place.  */
/*extern void encrypt (char *__block, int __edflag) __THROW __nonnull ((1));                                                                           */
extern void encrypt (char *__block, int __edflag) ;

//__END_DECLS

#endif  /* crypt.h */
