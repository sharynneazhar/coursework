//*****************************************************************************
//--ALUSimulator.c
//
//		Author: 		    Gary J. Minden
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

  uint32_t RsVal;
	uint32_t RtVal;
	uint32_t RdVal = 0;

	RegisterFile_Read(theRegisterFile, Rs, &RsVal, Rt, &RtVal);

	switch (OpCode) {
		case 0:
			switch (FunctionCode) {
				case 0: // SLL, NOOP
					RdVal = Rt << ShiftAmt;
					break;
				case 2: // SRL
					RdVal = Rt >> ShiftAmt;
					break;
				case 3: // SRA
					RdVal = (signed) Rt >> ShiftAmt;
					break;
				case 4: // SLLV
					RdVal = Rt << Rs;
					break;
				case 6: // SRLV
					RdVal = Rt >> Rs;
					break;
				case 32: // ADD
					RdVal = Rs + Rt;
					break;
				case 33: // ADDU
					RdVal = Rs + Rt;
					break;
				case 34: //SUB
					RdVal = Rs - Rt;
					break;
				case 35: //SUBU
					RdVal = Rs - Rt;
					break;
				case 36: //AND
					RdVal = Rs & Rt;
					break;
				case 37: //OR
					RdVal = Rs | Rt;
					break;
				case 38: //XOR
					RdVal = Rs ^ Rt;
					break;
				case 42: //SLT
					RdVal = (Rs < Rt);
					break;
				case 43: //SLTU
					RdVal = ((unsigned) Rs < (unsigned) Rt);
					break;
				default:
					printf(">> ERROR: Unknown FunctionCode.");
			}
			break;
		case 8: // ADDI
			RdVal = Rs + ImmediateValue;
			break;
		case 9: // ADDIU
			RdVal = Rs + ImmediateValue;
			break;
		case 10: // SLTI
			RdVal = (Rs < ImmediateValue);
			break;
		case 11: // SLTIU
			RdVal = ((unsigned) Rs < (unsigned) ImmediateValue);
			break;
		default:
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
