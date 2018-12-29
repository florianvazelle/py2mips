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
	# Debut Pcondop
addi $sp, $sp, -4
	# Debut Pcondop
addi $sp, $sp, -4
addi $sp, $sp, -4
lw $v0, -8($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -20($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target3
li $v0, 0
b suite3
	target3:
li $v0, 1
b suite3
	suite3:
sw $v0, 0($sp)
addi $sp, $sp, -4
lw $v0, -4($fp)
sw $v0, 0($sp)
li $v0, 0
lw $t0, -20($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target4
li $v0, 0
b suite4
	target4:
li $v0, 1
b suite4
	suite4:
lw $t0, -16($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_5
li $v0, 0
b suite5
	target_5:
beq $t0, $t1, target5
li $v0, 0
b suite5
	target5:
li $v0, 1
b suite5
	suite5:
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 2
sw $v0, 0($sp)
li $v0, 1
lw $t0, -16($fp)
addi $sp, $sp, 4
	#superieur ou egale a
bge $t0, $v0, target6
li $v0, 0
b suite6
	target6:
li $v0, 1
b suite6
	suite6:
lw $t0, -12($fp)
addi $sp, $sp, 4
	#et
li $t1, 1
beq $v0, $t1, target_7
li $v0, 0
b suite7
	target_7:
beq $t0, $t1, target7
li $v0, 0
b suite7
	target7:
li $v0, 1
b suite7
	suite7:
	#vardef c
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
