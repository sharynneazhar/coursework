//*****************************************************************************
//--ALUSimulator.c
//
//		Author: 		    Gary J. Minden
// 		Modified By: 		Sharynne Azhar (2513206)
//		Organization:	  KU/EECS/EECS 645
//		Date:			      2017-04-22 (B70422)
//		Version:		    1.0
//		Description:	  This is the prototype for a simple ALU simulator
//
//*****************************************************************************
//

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "ALUSimulator.h"

extern void ALUSimulator( RegisterFile theRegisterFile,
													uint32_t OpCode,
													uint32_t Rs,
													uint32_t Rt,
													uint32_t Rd,
													uint32_t ShiftAmt,
													uint32_t FunctionCode,
													uint32_t ImmediateValue,
													uint32_t* Status) {

	uint32_t RdVal = 0;

	RegisterFile_Read(theRegisterFile,
										Rs, &theRegisterFile[Rs],
										Rt, &theRegisterFile[Rt]);

	if (OpCode == 0) {
		switch (FunctionCode) {
			case 0: // SLL, NOOP
				RdVal = (unsigned) theRegisterFile[Rt] << (unsigned) ShiftAmt;
				break;
			case 2: // SRL
				RdVal = (unsigned) theRegisterFile[Rt] >> (unsigned) ShiftAmt;
				break;
			case 3: // SRA
				RdVal = (signed) theRegisterFile[Rt] >> (unsigned) ShiftAmt;
				break;
			case 4: // SLLV
				RdVal = (unsigned) theRegisterFile[Rt] << (unsigned) theRegisterFile[Rs];
				break;
			case 6: // SRLV
				RdVal = (unsigned) theRegisterFile[Rt] >> (unsigned) theRegisterFile[Rs];
				break;
			case 32: // ADD
				RdVal = (signed) ((signed) theRegisterFile[Rs]) +  (signed) theRegisterFile[Rt];
				break;
			case 33: // ADDU
				RdVal = (unsigned) ((unsigned) theRegisterFile[Rs]) + (unsigned) theRegisterFile[Rt];
				break;
			case 34: //SUB
				RdVal = (signed) ((signed) theRegisterFile[Rs]) - (signed) theRegisterFile[Rt];
				break;
			case 35: //SUBU
				RdVal = (unsigned) ((unsigned) theRegisterFile[Rs]) - (unsigned) theRegisterFile[Rt];
				break;
			case 36: //AND
				RdVal = theRegisterFile[Rs] & theRegisterFile[Rt];
				break;
			case 37: //OR
				RdVal = theRegisterFile[Rs] | theRegisterFile[Rt];
				break;
			case 38: //XOR
				RdVal = theRegisterFile[Rs] ^ theRegisterFile[Rt];
				break;
			case 42: //SLT
				RdVal = ((signed) theRegisterFile[Rs] < (signed) theRegisterFile[Rt]);
				break;
			case 43: //SLTU
				RdVal = ((unsigned) theRegisterFile[Rs] < (unsigned) theRegisterFile[Rt]);
				break;
			default:
				printf(">> ERROR: Unknown FunctionCode.");
		}
	} else if (OpCode == 8) {
		RdVal = (signed) ((signed) theRegisterFile[Rs]) + (signed) (int32_t) (int16_t) ImmediateValue;
	} else if (OpCode == 9) {
		RdVal = (unsigned) ((unsigned) theRegisterFile[Rs]) + (unsigned) (int32_t) (int16_t) ImmediateValue;
	} else if (OpCode == 10) {
		RdVal = ((signed) theRegisterFile[Rs] < (signed) (int32_t) (int16_t) ImmediateValue);
	} else if (OpCode == 11) {
		RdVal = ((unsigned) theRegisterFile[Rs] < (unsigned) (int32_t) (int16_t) ImmediateValue);
	} else {
		printf(">> ERROR: Unknown OpCode.");
	}

	if (OpCode == 0) {
		RegisterFile_Write(theRegisterFile, true, Rd, RdVal);
	} else {
		RegisterFile_Write(theRegisterFile, true, Rt, RdVal);
	}

	// printf(">>ALU: Opcode: %02X; Rs: %02X; Rt: %02X; Rd: %02X;\n",
	// 			OpCode,
	// 			Rs,
	// 			Rt,
	// 			Rd);
	//
	// printf(">>>>ALU: ShiftAmt: %02X; FunctionCode: %02X; ImmediateValue: %04X;\n",
	// 			ShiftAmt,
	// 			FunctionCode,
	// 			ImmediateValue);

}
