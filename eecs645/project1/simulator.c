/*
 ============================================================================
 Author        : Sharynne Azhar <sharynneazhar@gmail.com>
 KUID          : 2513206
 Date          : 5 April 2017
 Description   : Simulates a level 1 (L1) cache
 To Run        : make simulator; ./simulator [input file]
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

/* Block Offset Size */
#define BlockSizeExp 6
#define BlockSize (1 << BlockSizeExp)
#define BlockSizeMask (BlockSize - 1)

/* Line Index Size */
#define LineSizeExp (CacheSizeExp - BlockSizeExp - AssocExp)
#define LineSize (1 << LineSizeExp)
#define LineSizeMask (LineSize - 1)

/* Tag Size */
#define TagSizeExp (AddressBits - BlockSizeExp - LineSizeExp)
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
  printf("\n=== Cache Parameters ===");
  printf("\n%-15s: %d", "CacheSizeExp", CacheSizeExp);
  printf("\n%-15s: %d", "CacheSize", CacheSize);
  printf("\n%-15s: %d", "AddressBits", AddressBits);
  printf("\n%-15s: %d", "AssocExp", AssocExp);
  printf("\n%-15s: %d", "Assoc", Assoc);
  printf("\n%-15s: %d", "BlockSizeExp", BlockSizeExp);
  printf("\n%-15s: %d", "BlockSize", BlockSize);
  printf("\n%-15s: %d", "BlockSizeMask", BlockSizeMask);
  printf("\n%-15s: %d", "LineSizeExp", LineSizeExp);
  printf("\n%-15s: %d", "LineSize", LineSize);
  printf("\n%-15s: %d", "LineSizeMask", LineSizeMask);
  printf("\n%-15s: %d", "TagSizeExp", TagSizeExp);
  printf("\n%-15s: %d", "TagSize", TagSize);
  printf("\n%-15s: %d", "TagSizeMask", TagSizeMask);
  printf("\n=========================\n\n");

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
  int32_t addressBuf;
  while (fread(&addressBuf, 4, 1, inputFile)) {
    // find the line index and tag of the address buffer
    int32_t line = (addressBuf & LineSizeMask) >> BlockSizeExp;
    int32_t tag = (addressBuf & TagSizeMask) >> (LineSizeExp + BlockSizeExp);

    // initialize hit flag to false
    bool hit = false;

    // loop through the block data
    for (int i = 0; i < Assoc; i++) {
      // if the tag in the address buffer and cache match, we have a hit
      if (cache[line][tag].tag == tag) {
        hitCount++;
        hit = true;
      }
    }

    if (!hit) {
      missCount++;
    }
  }

  // calculate hit ratio
  float hitRatio = (float) (100.0 * hitCount / (missCount + hitCount));

  printf("\n====== Cache Results ======");
  printf("\n%-16s: %d", "Hit Count", hitCount);
  printf("\n%-16s: %d", "Miss Count", missCount);
  printf("\n%-16s: %.02f%%", "Hit Probability", hitRatio);
  printf("\n===========================\n\n");

  fclose(inputFile);

  return 0;
}
