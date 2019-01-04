.data
nl: .asciiz "\n"

.text
.globl main
main:
##(struct:Op add #(struct:Const num 1) #(struct:Const num 2)) 1
move $fp, $sp
li $v0, 1
beq $v0, $zero, suite1_1_0
li $v0, 5
	#vardef a
addi $sp, $sp, -4
sw $v0, 0($sp)
li $v0, 3
	#vardef b
addi $sp, $sp, -4
sw $v0, 0($sp)
li $v0, 0
beq $v0, $zero, suite2_2_0
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
b suite2_2
	suite2_2_0:
li $v0, 1
beq $v0, $zero, suite2_2_1
li $v0, 0
	#vardef truc
addi $sp, $sp, -4
sw $v0, 0($sp)
	suite2_2_1:
	suite2_2:
addi $sp, $sp, -4
li $v0, 1
sw $v0, 0($sp)
li $v0, 2
lw $t0, -16($fp)
addi $sp, $sp, 4
	#addition
add $v0, $t0, $v0
b suite1_1
	suite1_1_0:
li $v0, 32
	suite1_1:
move $a0, $v0
li $v0, 1
syscall
la $a0, nl
li $v0, 4
syscall
li $v0, 0
jr $ra
