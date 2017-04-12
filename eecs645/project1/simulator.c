/*
 ============================================================================
 Author        : Sharynne Azhar <sharynneazhar@gmail.com>
 KUID          : 2513206
 Date Modified : 11 April 2017
 Description   : Simulates a level 1 (L1) cache
 To Run        : make simulator; ./simulator [input file]
                 Note: use the flag "info=yes" to show cache description
 ============================================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* Size of address */
#define AddressBits 32

/* Size of the cache */
#define CacheSizeExp 15
#define CacheSize (1 << CacheSizeExp)

/* Block Offset Size */
#define BlockSizeExp 6
#define BlockSize (1 << BlockSizeExp)
#define BlockSizeMask (BlockSize - 1)

/* Data structure representing a line index */
typedef struct line_t {
  int valid;
  int32_t tag;
} line_t;

void runSim(FILE *inputFile, int AssocExp) {
  int hitCount = 0;     // number of cache hits
  int missCount = 0;    // number of cache misses
  int32_t addressRead;  // address being read in

  // determine number of bits in line index and tag based on associativity
  int32_t Assoc = (1 << AssocExp);

  int32_t LineSizeExp = (CacheSizeExp - BlockSizeExp - AssocExp);
  int32_t LineSize = (1 << LineSizeExp);
  int32_t LineSizeMask = (LineSize - 1);

  int32_t TagSizeExp = (AddressBits - BlockSizeExp - LineSizeExp);
  int32_t TagSize = (1 << TagSizeExp);
  int32_t TagSizeMask = (TagSize - 1);

  /* 2D array to represent the cache */
  line_t cache[LineSize][Assoc];

  // make sure cursor is at the beginning of the file
	fseek(inputFile, 0, SEEK_SET);

  // read in the data from the binary file and store
  while (fread(&addressRead, sizeof(int32_t), 1, inputFile)) {
    // find the line index and tag of the address buffer
    int32_t line = (addressRead >> BlockSizeExp) & LineSizeMask;
    int32_t tag = ((addressRead >> (BlockSizeExp + LineSizeExp)) & TagSizeMask);

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
      // boolean value to indicate if replacement was successful
      int isReplaced = 0;

      // go through and find first invalid or empty block
      for (int i = 0; i < Assoc; i++) {
        // update the block
        if ((cache[line][i].valid == 0) && (cache[line][i].tag == 0)) {
          cache[line][i].tag = tag;
          cache[line][i].valid = 1;
          isReplaced = 1;
          break;
        }
      }

      // if cache is full, then overwrite a victim
      if (isReplaced == 0) {
        int temp = (rand() % ((Assoc - 1) + 1 - 0) + 0);
        cache[line][temp].tag = tag;
        cache[line][temp].valid = 1;
      }

      // increase cache miss count
      missCount++;
    }
  }

  // calculate hit ratio
  float hitRatio = (float) (100.0 * hitCount / (missCount + hitCount));

  printf("\n===========================");
  printf("\n%-16s: %d", "Cache Size", CacheSize);
  printf("\n%-16s: %d", "Address Bits", AddressBits);
  printf("\n%-16s: %d", "Associativity", Assoc);
  printf("\n%-16s: %d", "Block Size", BlockSize);
  printf("\n%-16s: %d", "Line Size", LineSize);
  printf("\n%-16s: %d", "Tag Size", TagSize);
  printf("\n===========================");
  printf("\n%-16s: %d", "Hit Count", hitCount);
  printf("\n%-16s: %d", "Miss Count", missCount);
  printf("\n%-16s: %.02f%%", "Hit Probability", hitRatio);
  printf("\n===========================\n\n");
}

int main(int argc, const char **argv) {
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

  // run the simulation
  runSim(inputFile, 0);
  runSim(inputFile, 2);

  // close file
  fclose(inputFile);

  return 0;
}
