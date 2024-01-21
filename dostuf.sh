#!/bin/bash
rgbasm driver.asm -o test.o
rgblink test.o -o test.bin
cat <(head -c $((0x200)) Rockman_World_2_Japan.gbs) <(tail -c +$((0x1C001)) test.bin) > total.gbs
