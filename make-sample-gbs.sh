#!/bin/bash

set -e

rgbasm driver.asm -o test.o
rgblink test.o -o test.bin
(head -c $((0x200)) Rockman_World_2_Japan.gbs; tail -c +$((0x1C001)) test.bin) > total.gbs
