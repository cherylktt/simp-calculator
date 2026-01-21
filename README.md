# SIMP Calculator

## Project Idea
The idea is to use the SIMP compiler built as the backbone for a simple calculator-like website. It is able to take in the following as input: non-SIMP instructions like 4+7, SIMP instructions like x = 4 + 7; return x; and the calling of built-in functions like fib(4). The frontend is handled through the use of [Play Framework](https://www.playframework.com/) and the execution is handled by CheerpJ within the browser directly.

![alt text](https://github.com/cherylktt/simp-calculator/blob/main/frontend-ui.png)

## Future Work
Currently, the pre-defined functions only cover fibonacci and factorial. To increase the number of functions available, there could be a separate page that allows user to input the necessary SIMP instructions for the function in a text box, and these instructions will be passed to a controller that will temporarily save them in a .simp file within the CheerpJ virtual file system, such that it can be retrieved and used in the calculator function within the same runtime. Some options for these additional functions include logarithm, exponential and square root.
