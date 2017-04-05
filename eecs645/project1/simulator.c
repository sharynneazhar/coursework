/*
 ============================================================================
 Author        : Sharynne Azhar <sharynneazhar@gmail.com>
 KUID          : 2513206
 Date          : 5 April 2017
 Description   : Simulates a level 1 (L1) cache
 To Compile    :
 ============================================================================
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <time.h>
#include <unistd.h>

/* Input file */
FILE *inputFile;

/* Size of the cache */
#define CacheSizeExp 15
#define CacheSize (1 << CacheSizeExp)

/* Size of address */
#define AddressBits 32

/* Associativity */
#define AssocExp 2
#define Assoc (1 << AssocExp)

/* Block Size */
#define BlockSizeExp 6
#define BlockSize (1 << BlockSizeExp)
#define BlockSizeMask (BlockSize - 1)

/* Line Index Size */
#define LineSizeExp (CacheSizeExp - BlockSizeExp - AssocExp)
#define LineSize (1 << LineSizeExp)
#define LineSizeMask (LineSize - 1)

/* Tag Size */
#define TagSizeExp (AddressBits - (BlockSizeExp + LineSizeExp))
#define TagSize (1 << TagSizeExp)
#define TagSizeMask (TagSize - 1)

typedef struct Block {
  int valid;
  int32_t tag;
} Block;

int hitCount = 0;
int missCount = 0;

Block cache[LineSize][Assoc] = {};

int main(int argc, const char * argv[]) {

  printf("\nCacheSizeExp: %d", CacheSizeExp);
  printf("\nCacheSize: %d", CacheSize);

  printf("\nAddressBits: %d", AddressBits);

  printf("\nAssocExp: %d", AssocExp);
  printf("\nAssoc: %d", Assoc);

  printf("\nBlockSizeExp: %d", BlockSizeExp);
  printf("\nBlockSize: %d", BlockSize);
  printf("\nBlockSizeMask: %d", BlockSizeMask);

  printf("\nLineSizeExp: %d", LineSizeExp);
  printf("\nLineSize: %d", LineSize);
  printf("\nLineSizeMask: %d", LineSizeMask);

  printf("\nTagSizeExp: %d", TagSizeExp);
  printf("\nTagSize: %d", TagSize);
  printf("\nTagSizeMask: %d", TagSizeMask);

  /* ---------------------------------------------------- */

  if (argv[1] == NULL) {
    printf("\nERROR: Missing file.\n");
    return 1;
  }

  // open the binary file passed in from input
  inputFile = fopen(argv[1], "rb");

  if (!inputFile) {
    printf("\nERROR: Unable to open file.\n");
    return 1;
  }

  // read the contents of the binary file
  int32_t address;
  while (fread(&address, 4, 1, inputFile)) {
    // determine the number of bits in the line index and tag
    int32_t line = (address & LineSizeMask) >> BlockSizeExp;
    int32_t tag = (address & TagSizeMask) >> (LineSizeExp + BlockSizeExp);

    bool hit = 0;

  }


  fclose(inputFile);

  return 0;
}
