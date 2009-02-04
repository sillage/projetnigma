
#include <stdlib.h>
#include <stdio.h>
#include<string.h>

typedef struct pixel pixel;
struct pixel
{

    int bleu;
    int rouge;
    int vert;
};

int add_char(int c, char *s,size_t i)
{
  size_t taille_s;
  taille_s=strlen(s);
  if (i<taille_s && s[i]=='0' )
  {
    if (c%2==0)
    {
      printf("c pair et s pair %i\n",c);
      return (c)%255;

	}
    else
    {
      printf("c impair et s pair %i\n",c);
      return (c+1)%255;
	}
  }
  else
  {
    if ( i<taille_s && s[i]=='1')
    {
      if (c%2==1)
      {
	printf("c impair et s impair %i\n",c);
	return (c)%255;

	  }
      else
      {
	printf("c pair et s impair %i\n",c);
	return (c+1)%255;
	  }
    }
    else
      if (i>=taille_s && i<taille_s+3)
	return -1;
      else
	return c;
  }
}
void image (char *s)
{
  FILE *src;
  FILE *dst;
  pixel p;
  int c;
  size_t i;
  int accu;
  accu =54;
  p.bleu=0;
  p.rouge=0;
  p.vert=0;
  src = fopen("kikoo.bmp","r");
  dst = fopen("dst.bmp","w+");
  c = 1;
  i=0;

  for (accu=0;accu!=54;accu++)
  {

    c= getc(src);
    putc(c,dst);
    if (accu ==3 || accu==2 || accu==4 ||accu==5)
      printf( "File Size: %i \n",c);
    if (accu ==6 || accu==7 || accu==8 ||accu==9)
      printf( "Reserved: %i \n",c);
    if (accu ==10 || accu==11 || accu==12 ||accu==13)
      printf( "DataOffset: %i \n",c);
    if (accu ==14 || accu==15 || accu==16 ||accu==17)
      printf( "HeaderSize: %i \n",c);
    if (accu ==18 || accu==19 || accu==20 ||accu==21)
      printf( "Width: %i \n",c);
    if (accu ==24 || accu==22 || accu==23 ||accu==25)
      printf( "Heigth: %i \n",c);
    if (accu ==26 || accu==27)
      printf( "Planes: %i \n",c);
    if (accu ==28 || accu==29)
      printf( "BitPerPixels: %i \n",c);
    if (accu ==30 || accu==31 || accu==32 ||accu==33)
      printf( "Compression: %i \n",c);
    if (accu ==37 || accu==36 || accu==35 ||accu==34)
      printf( "BitmapDataSize: %i \n",c);
    if (accu ==38 || accu==39 || accu==40 ||accu==41)
      printf( "HResolution: %i \n",c);
    if (accu ==45 || accu==44 || accu==43 ||accu==42)
      printf( "VResolution: %i \n",c);
    if (accu ==46 || accu==47 || accu==48 ||accu==49)
      printf( "Colors: %i \n",c);
    if (accu ==50 || accu==51 || accu==52 ||accu==53)
      printf( "ImportantColors: %i \n",c);

  }
  while (p.bleu!=-1||p.vert!=-1 ||p.bleu!=-1 )
  {
    p.bleu=getc(src);
    if (p.bleu==-1)
      break;
    p.vert=getc(src);
    if (p.vert==-1)
      break;
    p.rouge=getc(src);
    if (p.rouge==-1)
      break;
    putc( add_char(p.bleu,s,i++),dst);
    putc(add_char(p.vert,s,i++),dst);
    putc(add_char(p.rouge,s,i++),dst);
  }
  fclose (src);
  fclose (dst);
}


int    main (int argc , char **argv)
{
  argc=argc;
  if (argc>1)
    image(argv[1]);
  else
    printf("Connard tu veux crypter quoi!!\n");

  return 0;

}
