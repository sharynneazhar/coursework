//  Created by Frank M. Carrano and Tim Henry.
//  Copyright (c) 2013 __Pearson Education__. All rights reserved.

/** Listing 7-5.
    @file PVE.h */

#ifndef PVE_H
#define PVE_H

#include <stdexcept>
#include <string>

using namespace std;

class PVE : public logic_error
{
public:
   PVE(const string& message = "");
}; // end PrecondViolatedExcep

#endif
