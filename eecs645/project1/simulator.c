/*
 ============================================================================
 Author        : Sharynne Azhar <sharynneazhar@gmail.com>
 KUID          : 2513206
 Date Modified : 11 April 2017
 Description   : Simulates a level 1 (L1) cache
 To Run        : make simulator; ./simulator [input file]
 ============================================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <time.h>
#include <unistd.h>

/* Size of address */
#define AddressBits 32

/* Associativity (0 for 1-way, 1 for 2 way) */
#define AssocExp 0
#define Assoc (1 << AssocExp)

/* Size of the cache */
#define CacheSizeExp 15
#define CacheSize (1 << CacheSizeExp)

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

/* Data structure representing a line index */
typedef struct line_t {
  int valid;
  int32_t tag;
} line_t;

/* 2D array to represent the cache */
line_t cache[LineSize][Assoc] = {};

int main(int argc, const char * argv[]) {

  if ((argc > 2) && argv[2] == "-info") {
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
    printf("\n=========================\n");
  }

  printf("\nRunning simulation...\n");

  // make sure a file is passed in from command line
  if (argv[1] == NULL) {
    printf("\nERROR: Missing file.\n");
    return 1;
  }

  // open the binary file passed in from input
  FILE *inputFile;
  const char * filename = argv[1];
  inputFile = fopen(filename, "rb");

  // check if file is valid
  if (!inputFile) {
    printf("\nERROR: Unable to open file.\n");
    return 1;
  }

  int hitCount = 0;     // number of cache hits
  int missCount = 0;    // number of cache misses
  int32_t addressRead;  // address being read in

  // read in the data from the binary file and store
  // note: 32 bit address is equal to 4 bytes
  while (fread(&addressRead, 4, 1, inputFile)) {
    // find the line index and tag of the address buffer
    int32_t line = (addressRead & LineSizeMask) >> BlockSizeExp;
    int32_t tag = (addressRead & TagSizeMask) >> (LineSizeExp + BlockSizeExp);

    // boolean value to indicate a hit (1) or miss (0)
    int isCacheHit = 0;

    // check for a cache hit
    for (int i = 0; i < Assoc; i++) {
      // test the address valid bit and tag
      // cache hit if the line passes both tests
      if ((cache[line][i].valid == 1) && (cache[line][i].tag == tag)) {
        isCacheHit = 1;
        hitCount++;
        break;
      }
    }

    // there was a cache miss, run replacement algorithm
    if (isCacheHit == 0) {
      missCount++;
      for (int i = 0; i < Assoc; i++) {
        if (cache[line][i].valid == 0) {
          cache[line][i].tag = tag;
          cache[line][i].valid = 1;
        } else {
          cache[line][i].valid = 0;
        }
      }
    }
  }

  // calculate hit ratio
  float hitRatio = (float) (100.0 * hitCount / (missCount + hitCount));

  printf("\n====== Cache Results ======");
  printf("\n%-16s: %d", "Hit Count", hitCount);
  printf("\n%-16s: %d", "Miss Count", missCount);
  printf("\n%-16s: %.02f%%", "Hit Probability", hitRatio);
  printf("\n===========================\n\n");

  // close file
  fclose(inputFile);

  return 0;
}
