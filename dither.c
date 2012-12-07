#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <stddef.h>
#include <malloc.h>

#define RANGE(a,b,c) { if (a < b) a = b;  if (a > c) a = c; }

unsigned char *DoColorDither(pic24, w, h, rdisp, gdisp, bdisp, idisp, maplen)
     unsigned char *pic24, *rdisp, *gdisp, *bdisp, *idisp;
     int   w, h, maplen;
{
  /* takes a 24 bit picture, of size w*h, dithers with the colors in
     rdisp, gdisp, bdisp (which have already been allocated),
     and generates an 8-bit w*h image, which it returns.
     returns NULL on error

     note: the rdisp,gdisp,bdisp arrays should be the 'displayed' colors,
     not the 'desired' colors */

  unsigned char *np, *ep, *newpic;
  short *cache;
  int r2, g2, b2;
  int *thisline, *nextline, *thisptr, *nextptr, *tmpptr;
  int  i, j, rerr, gerr, berr, pwide3;
  int  imax, jmax;
  int key;
  long cnt1, cnt2;
  int fserrmap[512];   /* -255 .. 0 .. +255 */

  /* compute somewhat non-linear floyd-steinberg error mapping table */
  for (i=j=0; i<=0x40; i++,j++)
    { fserrmap[256+i] = j;  fserrmap[256-i] = -j; }
  for (     ; i<0x80; i++, j += !(i&1) ? 1 : 0)
    { fserrmap[256+i] = j;  fserrmap[256-i] = -j; }
  for (     ; i<=0xff; i++)
    { fserrmap[256+i] = j;  fserrmap[256-i] = -j; }

  cnt1 = cnt2 = 0;
  pwide3 = w*3;  imax = h-1;  jmax = w-1;
  ep = pic24;

  /* attempt to malloc things */
  newpic = (unsigned char *)  malloc((size_t) (w * h));
  cache  = (short *) calloc((size_t) (2<<14), sizeof(short));
  thisline = (int *) malloc(pwide3 * sizeof(int));
  nextline = (int *) malloc(pwide3 * sizeof(int));
  if (!cache || !newpic || !thisline || !nextline) {
    if (newpic)   free(newpic);
    if (cache)    free(cache);
    if (thisline) free(thisline);
    if (nextline) free(nextline);

    return (unsigned char *) NULL;
  }

  np = newpic;

  /* get first line of picture */

  for (j=pwide3, tmpptr=nextline; j; j--, ep++) *tmpptr++ = (int) *ep;

  for (i=0; i<h; i++) {

    tmpptr = thisline;  thisline = nextline;  nextline = tmpptr;   /* swap */

    if (i!=imax) {  /* get next line */
      for (j=pwide3, tmpptr=nextline; j; j--, ep++) *tmpptr++ = (int) *ep;
    }

    /* dither a line */
    for (j=0, thisptr=thisline, nextptr=nextline; j<w; j++,np++) {
      int k, d, mind, closest;

      r2 = *thisptr++;  g2 = *thisptr++;  b2 = *thisptr++;

      /* map r2,g2,b2 components (could be outside 0..255 range)
	 into 0..255 range */

      if (r2<0 || g2<0 || b2<0) {   /* are there any negatives in RGB? */
	if (r2<g2) { if (r2<b2) k = 0; else k = 2; }
	else { if (g2<b2) k = 1; else k = 2; }

	switch (k) {
	case 0:  g2 -= r2;  b2 -= r2;  d = (abs(r2) * 3) / 2;    /* RED */
	         r2 = 0;
	         g2 = (g2>d) ? g2 - d : 0;
	         b2 = (b2>d) ? b2 - d : 0;
	         break;

	case 1:  r2 -= g2;  b2 -= g2;  d = (abs(g2) * 3) / 2;    /* GREEN */
	         r2 = (r2>d) ? r2 - d : 0;
	         g2 = 0;
	         b2 = (b2>d) ? b2 - d : 0;
	         break;

	case 2:  r2 -= b2;  g2 -= b2;  d = (abs(b2) * 3) / 2;    /* BLUE */
	         r2 = (r2>d) ? r2 - d : 0;
	         g2 = (g2>d) ? g2 - d : 0;
	         b2 = 0;
	         break;
	}
      }

      if (r2>255 || g2>255 || b2>255) {   /* any overflows in RGB? */
	if (r2>g2) { if (r2>b2) k = 0; else k = 2; }
              else { if (g2>b2) k = 1; else k = 2; }

	switch (k) {
	case 0:   g2 = (g2*255)/r2;  b2 = (b2*255)/r2;  r2=255;  break;
	case 1:   r2 = (r2*255)/g2;  b2 = (b2*255)/g2;  g2=255;  break;
	case 2:   r2 = (r2*255)/b2;  g2 = (g2*255)/b2;  b2=255;  break;
	}
      }

      key = ((r2&0xf8)<<6) | ((g2&0xf8)<<1) | (b2>>4);
      if (key >= (2<<14))
      { if (newpic) free(newpic);
        if (cache) free(cache);
	if (thisline) free(thisline);
	if (nextline) free(nextline);
	return (unsigned char *) NULL;}

      if (cache[key]) { *np = (unsigned char) (cache[key] - 1);  cnt1++;}
      else {
	/* not in cache, have to search the colortable */
	cnt2++;

        mind = 10000;
	for (k=closest=0; k<maplen && mind>7; k++) {
	  d = abs(r2 - rdisp[k])
	    + abs(g2 - gdisp[k])
	    + abs(b2 - bdisp[k]);
	  if (d<mind) { mind = d;  closest = k; }
	}
	cache[key] = closest + 1;
	*np = closest;
      }

      /* propogate the error */
      rerr = r2 - rdisp[*np];
      gerr = g2 - gdisp[*np];
      berr = b2 - bdisp[*np];

      RANGE(rerr, -255, 255);
      RANGE(gerr, -255, 255);
      RANGE(berr, -255, 255);
      rerr = fserrmap[256+rerr];
      gerr = fserrmap[256+gerr];
      berr = fserrmap[256+berr];

      if (j!=jmax) {  /* adjust RIGHT pixel */
	thisptr[0] += (rerr*7)/16;
	thisptr[1] += (gerr*7)/16;
	thisptr[2] += (berr*7)/16;
      }

      if (i!=imax) {	/* do BOTTOM pixel */
	nextptr[0] += (rerr*5)/16;
	nextptr[1] += (gerr*5)/16;
	nextptr[2] += (berr*5)/16;

	if (j>0) {  /* do BOTTOM LEFT pixel */
	  nextptr[-3] += (rerr*3)/16;
	  nextptr[-2] += (gerr*3)/16;
	  nextptr[-1] += (berr*3)/16;
	}

	if (j!=jmax) {  /* do BOTTOM RIGHT pixel */
	  nextptr[3] += rerr/16;
	  nextptr[4] += gerr/16;
	  nextptr[5] += berr/16;
	}
	nextptr += 3;
      }
    }
  }

  free(thisline);  free(nextline);
  free(cache);

  for (i = 0; i<w*h; i++) newpic[i] = idisp[newpic[i]];

  return newpic;
}
