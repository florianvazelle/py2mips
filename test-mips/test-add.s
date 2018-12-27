.data
nl: .asciiz "\n"

.text
.globl main
main:
move $fp, $sp
li $v0, 36
addi $sp, $sp, -4
sw $v0, 0($sp)
li $v0, 42
addi $sp, $sp, -4
sw $v0, 0($sp)
lw $v0, -8($fp)
move $a0, $v0
li $v0, 1
syscall
la $a0, nl
li $v0, 4
syscall
li $v0, 0
jr $ra
