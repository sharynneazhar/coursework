#
#	This is a small test program for the EECS_645
#	ALUSimulator. All instructions just use the
#	register file.
#
#----------------------------------------------------------------------
#
#
			.text
Start:		addi			$09,$02,100
			addi			$10,$05,-128
			add				$11,$01,$04
			sll				$12,$05,4
			or				$13,$04,$05
			xor				$14,$02,$03
			sub				$15,$03,$02
			