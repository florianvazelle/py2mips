.data
nl: .asciiz "\n"

.text
.globl main
main:
#hash((a . -4) (b . -8))
move $fp, $sp
addi $sp, $sp, -4
li $v0, 1
sw $v0, 0($sp)
li $v0, 1
lw $t0, -4($fp)
addi $sp, $sp, 4
	#egale a
beq $t0, $v0, target2
li $v0, 0
b suite2
	target2:
li $v0, 1
b suite2
	suite2:
beq $v0, $zero, suite1_1_0
li $v0, 5
	#vardef a
addi $sp, $sp, -4
sw $v0, 0($sp)
li $v0, 3
	#vardef b
addi $sp, $sp, -4
sw $v0, 0($sp)
addi $sp, $sp, -4
lw $v0, -4($fp)
sw $v0, 0($sp)
li $v0, 5
lw $t0, -12($fp)
addi $sp, $sp, 4
	#n'est pas egale a
bne $t0, $v0, target4
li $v0, 0
b suite4
	target4:
li $v0, 1
b suite4
	suite4:
beq $v0, $zero, suite3_2_0
addi $sp, $sp, -4
li $v0, 9
sw $v0, 0($sp)
li $v0, 8
lw $t0, -12($fp)
addi $sp, $sp, 4
	#addition
add $v0, $t0, $v0
	#vardef q
addi $sp, $sp, -4
sw $v0, 0($sp)
b suite3_2
	suite3_2_0:
li $v0, 1
beq $v0, $zero, suite3_2_1
li $v0, 0
	#vardef truc
addi $sp, $sp, -4
sw $v0, 0($sp)
	suite3_2_1:
	suite3_2:
	suite1_1_0:
	suite1_1:
move $a0, $v0
li $v0, 1
syscall
la $a0, nl
li $v0, 4
syscall
li $v0, 0
jr $ra
