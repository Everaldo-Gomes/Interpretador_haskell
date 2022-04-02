#!/bin/bash

clear
rm Parser.hs
happy Parser.y
runghc Interpreter.hs
