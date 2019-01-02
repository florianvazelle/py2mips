.data
nl: .asciiz "\n"

.text
.globl main
main:
move $fp, $sp
addi $sp, $sp, -4
addi $sp, $sp, -4
li $v0, 3
sw $v0, 0($sp)
li $v0, 8
lw $t0, -8($fp)
addi $sp, $sp, 4
	#multiplication
mult $t0, $v0
mflo $v0
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 28
sw $v0, 0($sp)
li $v0, 4
lw $t0, -8($fp)
addi $sp, $sp, 4
	#soustraction
sub $v0, $t0, $v0
lw $t0, -4($fp)
addi $sp, $sp, 4
	#egale a
beq $t0, $v0, target1
li $v0, 0
b suite1
	target1:
li $v0, 1
b suite1
	suite1:
	#vardef a
addi $sp, $sp, -4
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 2
sw $v0, 0($sp)
li $v0, 1
lw $t0, -8($fp)
addi $sp, $sp, 4
	#superieur a
bgt $t0, $v0, target2
li $v0, 0
b suite2
	target2:
li $v0, 1
b suite2
	suite2:
	#vardef b
addi $sp, $sp, -4
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 2
sw $v0, 0($sp)
li $v0, 1
lw $t0, -12($fp)
addi $sp, $sp, 4
	#inferieur a
blt $t0, $v0, target3
li $v0, 0
b suite3
	target3:
li $v0, 1
b suite3
	suite3:
	#vardef b2
addi $sp, $sp, -4
sw $v0, 0($sp)
	# Debut Pcondop
addi $sp, $sp, -4
	# Debut Pcondop
addi $sp, $sp, -4
addi $sp, $sp, -4
lw $v0, -8($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -24($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target4
li $v0, 0
b suite4
	target4:
li $v0, 1
b suite4
	suite4:
sw $v0, 0($sp)
addi $sp, $sp, -4
lw $v0, -4($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -24($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target5
li $v0, 0
b suite5
	target5:
li $v0, 1
b suite5
	suite5:
lw $t0, -20($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_6
li $v0, 0
b suite6
	target_6:
beq $t0, $t1, target6
li $v0, 0
b suite6
	target6:
li $v0, 1
b suite6
	suite6:
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 2
sw $v0, 0($sp)
li $v0, 1
lw $t0, -20($fp)
addi $sp, $sp, 4
	#superieur ou egale a
bge $t0, $v0, target7
li $v0, 0
b suite7
	target7:
li $v0, 1
b suite7
	suite7:
lw $t0, -16($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_8
li $v0, 0
b suite8
	target_8:
beq $t0, $t1, target8
li $v0, 0
b suite8
	target8:
li $v0, 1
b suite8
	suite8:
	#vardef c
addi $sp, $sp, -4
sw $v0, 0($sp)
	# Debut Pcondop
addi $sp, $sp, -4
	# Debut Pcondop
addi $sp, $sp, -4
addi $sp, $sp, -4
lw $v0, -8($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -28($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target9
li $v0, 0
b suite9
	target9:
li $v0, 1
b suite9
	suite9:
sw $v0, 0($sp)
addi $sp, $sp, -4
lw $v0, -4($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -28($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target10
li $v0, 0
b suite10
	target10:
li $v0, 1
b suite10
	suite10:
lw $t0, -24($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_11
li $v0, 0
b suite11
	target_11:
beq $t0, $t1, target11
li $v0, 0
b suite11
	target11:
li $v0, 1
b suite11
	suite11:
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 2
sw $v0, 0($sp)
li $v0, 1
lw $t0, -24($fp)
addi $sp, $sp, 4
	#inferieur a
blt $t0, $v0, target12
li $v0, 0
b suite12
	target12:
li $v0, 1
b suite12
	suite12:
lw $t0, -20($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_13
li $v0, 0
b suite13
	target_13:
beq $t0, $t1, target13
li $v0, 0
b suite13
	target13:
li $v0, 1
b suite13
	suite13:
	#vardef c2
addi $sp, $sp, -4
sw $v0, 0($sp)
	# Debut Pcondop
addi $sp, $sp, -4
	# Debut Pcondop
addi $sp, $sp, -4
addi $sp, $sp, -4
lw $v0, -12($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -32($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target14
li $v0, 0
b suite14
	target14:
li $v0, 1
b suite14
	suite14:
sw $v0, 0($sp)
addi $sp, $sp, -4
lw $v0, -4($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -32($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target15
li $v0, 0
b suite15
	target15:
li $v0, 1
b suite15
	suite15:
lw $t0, -28($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_16
li $v0, 0
b suite16
	target_16:
beq $t0, $t1, target16
li $v0, 0
b suite16
	target16:
li $v0, 1
b suite16
	suite16:
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 2
sw $v0, 0($sp)
li $v0, 1
lw $t0, -28($fp)
addi $sp, $sp, 4
	#superieur ou egale a
bge $t0, $v0, target17
li $v0, 0
b suite17
	target17:
li $v0, 1
b suite17
	suite17:
lw $t0, -24($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_18
li $v0, 0
b suite18
	target_18:
beq $t0, $t1, target18
li $v0, 0
b suite18
	target18:
li $v0, 1
b suite18
	suite18:
	#vardef c3
addi $sp, $sp, -4
sw $v0, 0($sp)
	# Debut Pcondop
addi $sp, $sp, -4
	# Debut Pcondop
addi $sp, $sp, -4
addi $sp, $sp, -4
lw $v0, -12($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -36($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target19
li $v0, 0
b suite19
	target19:
li $v0, 1
b suite19
	suite19:
sw $v0, 0($sp)
addi $sp, $sp, -4
lw $v0, -4($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -36($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target20
li $v0, 0
b suite20
	target20:
li $v0, 1
b suite20
	suite20:
lw $t0, -32($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_21
li $v0, 0
b suite21
	target_21:
beq $t0, $t1, target21
li $v0, 0
b suite21
	target21:
li $v0, 1
b suite21
	suite21:
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 2
sw $v0, 0($sp)
li $v0, 1
lw $t0, -32($fp)
addi $sp, $sp, 4
	#inferieur a
blt $t0, $v0, target22
li $v0, 0
b suite22
	target22:
li $v0, 1
b suite22
	suite22:
lw $t0, -28($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_23
li $v0, 0
b suite23
	target_23:
beq $t0, $t1, target23
li $v0, 0
b suite23
	target23:
li $v0, 1
b suite23
	suite23:
	#vardef c4
addi $sp, $sp, -4
sw $v0, 0($sp)
	# Debut Pcondop
addi $sp, $sp, -4
	# Debut Pcondop
addi $sp, $sp, -4
addi $sp, $sp, -4
li $v0, 4
sw $v0, 0($sp)
li $v0, 4
lw $t0, -40($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target24
li $v0, 0
b suite24
	target24:
li $v0, 1
b suite24
	suite24:
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 4
sw $v0, 0($sp)
li $v0, 5
lw $t0, -40($fp)
addi $sp, $sp, 4
	#egale a
beq $t0, $v0, target25
li $v0, 0
b suite25
	target25:
li $v0, 1
b suite25
	suite25:
lw $t0, -36($fp)
addi $sp, $sp, 4
	#ou
li $t1, 1
beq $v0, $t1, target26
beq $t0, $t1, target26
li $v0, 0
b suite26
	target26:
li $v0, 1
b suite26
	suite26:
sw $v0, 0($sp)
li $v0, 1
lw $t0, -32($fp)
addi $sp, $sp, 4
	#ou
li $t1, 1
beq $v0, $t1, target27
beq $t0, $t1, target27
li $v0, 0
b suite27
	target27:
li $v0, 1
b suite27
	suite27:
	#vardef d1
addi $sp, $sp, -4
sw $v0, 0($sp)
move $a0, $v0
li $v0, 1
syscall
la $a0, nl
li $v0, 4
syscall
li $v0, 0
jr $ra
