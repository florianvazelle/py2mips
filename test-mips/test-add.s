.data
nl: .asciiz "\n"

.text
.globl main
main:
move $fp, $sp
addi $sp, $sp, -4
li $v0, 36
sw $v0, 0($sp)
addi $sp, $sp, -4
li $v0, 42
sw $v0, 0($sp)
li $v0, 5
lw $t0, -8($fp)
addi $sp, $sp, 4
#soustraction
sub $v0, $t0, $v0
lw $t0, -4($fp)
addi $sp, $sp, 4
#addition
add $v0, $t0, $v0
#vardef a
addi $sp, $sp, -4
sw $v0, 0($sp)
addi $sp, $sp, -4
lw $v0, -4($fp)
sw $v0, 0($sp)
li $v0, 57
lw $t0, -8($fp)
addi $sp, $sp, 4
#addition
add $v0, $t0, $v0
#vardef b
addi $sp, $sp, -4
sw $v0, 0($sp)
addi $sp, $sp, -4
addi $sp, $sp, -4
lw $v0, -8($fp)
sw $v0, 0($sp)
lw $v0, -4($fp)
lw $t0, -16($fp)
addi $sp, $sp, 4
#soustraction
sub $v0, $t0, $v0
sw $v0, 0($sp)
lw $v0, -4($fp)
lw $t0, -12($fp)
addi $sp, $sp, 4
#modulo
div $t0, $v0
mfhi $v0
#vardef c
addi $sp, $sp, -4
sw $v0, 0($sp)
addi $sp, $sp, -4
addi $sp, $sp, -4
lw $v0, -12($fp)
sw $v0, 0($sp)
li $v0, 8
lw $t0, -20($fp)
addi $sp, $sp, 4
#multiplication
mult $t0, $v0
mflo $v0
sw $v0, 0($sp)
addi $sp, $sp, -4
lw $v0, -12($fp)
sw $v0, 0($sp)
li $v0, 3
lw $t0, -20($fp)
addi $sp, $sp, 4
#soustraction
sub $v0, $t0, $v0
lw $t0, -16($fp)
addi $sp, $sp, 4
#addition
add $v0, $t0, $v0
#redefinition de c
addi $sp, $sp, 0
sw $v0, 0($sp)
addi $sp, $sp, 0
move $a0, $v0
li $v0, 1
syscall
la $a0, nl
li $v0, 4
syscall
li $v0, 0
jr $ra
