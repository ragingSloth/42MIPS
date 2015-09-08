break
PUSH:
    #li $a0 
    #jal PUSH
	sw $a0, 0($t1)
	sub $t1, $t1, 4
	jr $ra
DROP:
    #jal DROP 
	add $t1, $t1, 4
	sw $zero, 0($t1)
	jr $ra
DUP:
    #jal DUP
	lw $t2, 4($t1)
	sw $t2, 0($t1)
	sub $t1, $t1, 4
	jr $ra
	
ADD:
    #jal ADD
	lw $t2, 4($t1)
	lw $t3, 8($t1)
	add $t2, $t2, $t3
	add $t1, $t1, 4
	sw, $zero, 0($t1), 
	sw, $t2, 4($t1)
	jr $ra
SUBTRACT:
    #jal SUBTRACT
	lw $t2, 4($t1)
	lw $t3, 8($t1)
	sub $t2, $t2, $t3
	add $t1, $t1, 4
	sw, $zero, 0($t1)
	sw, $t2, 4($t1)
	jr $ra
MUL:
    #jal MUL
	lw $t2, 4($t1)
	lw $t3, 8($t1)
	mult $t2, $t3
	mflo $t2
	add $t1, $t1, 4
	sw, $zero, 0($t1)
	sw, $t2, 4($t1)
	jr $ra
OVER:
    #jal OVER
	lw $t2, 8($t1)
	sw $t2, 0($t1)
	add $t1, $t1, 4
	jr $ra
SWAP:
    #jal SWAP
	lw $t2, 4($t1)
	lw $t3, 8($t1)
	sw $t3, 4($t1)
	sw $t2, 8($t1)
	jr $ra
ROT:
    #jal ROT
	lw $t2, 4($t1)
	lw $t3, 8($t1)
	lw $t4, 12($t1)
	sw $t2, 8($t1)
	sw $t3, 12($t1)
	sw $t4, 4($t1)
	jr $ra
ZEROEQ:
    #jal ZEROEQ
	lw $t2, 4($t1)
	seq $t3, $t2, $zero
	sw $t3, 4($t1)
	jr $ra
GREATER0:
    #jal GREATER0
	lw $t2, 4($t1)
	sgt $t3, $t2, $zero
	sw $t3, 4($t1)
	jr $ra	
IF:
    #jal IF
    add $t3, $t1, 4
    lw $t2, ($t3)
    li $a0, @
    li $a1, @
    add $t1,$t1,4
    subi $t2,$t2,1
    bltzal $t2, ELSE
ELSE:
    #jal THEN
    add $a2, $a0, 0
    j SKIP

THEN:
    add $a2, $a1, 0
    j SKIP

SKIP:
    add $ra, $ra, $a2
    jr $ra

	
EXIT:

.data
	stack1: .word 0:1000
	control: .word 0:1000
