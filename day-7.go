package main

import (
	"fmt"
)

// Totally not going to write this myself:
// https://stackoverflow.com/a/30226442/257418
func permutations(arr []int) [][]int {
	var helper func([]int, int)
	res := [][]int{}

	helper = func(arr []int, n int) {
		if n == 1 {
			tmp := make([]int, len(arr))
			copy(tmp, arr)
			res = append(res, tmp)
		} else {
			for i := 0; i < n; i++ {
				helper(arr, n-1)
				if n%2 == 1 {
					tmp := arr[i]
					arr[i] = arr[n-1]
					arr[n-1] = tmp
				} else {
					tmp := arr[0]
					arr[0] = arr[n-1]
					arr[n-1] = tmp
				}
			}
		}
	}
	helper(arr, len(arr))
	return res
}

func runProgram(program []int, input <-chan int, output chan<- int) {
	// Helper functions.
	fetch := func(mode int, p int) int {
		switch mode {
			case 0: return program[program[p]]
			case 1: return program[p]
			default: panic(fmt.Sprintf("unknown mode %d", mode))
		}
	}
	fetchIndex := func(mode int, p int) int {
		switch mode {
			case 0: return program[p]
			case 1: panic("store into immediate")
			default: panic(fmt.Sprintf("unknown mode %d", mode))
		}
	}
	arithmetic := map[int]func(int, int) int{
		1: func(x, y int) int { return x + y },
		2: func(x, y int) int { return x * y },
		7: func(x, y int) int { if x < y { return 1 } else { return 0 } },
		8: func(x, y int) int { if x == y { return 1 } else { return 0 } },
	}

	pc := 0
	for {
		op := program[pc]
		modeA := op / 100 % 10
		modeB := op / 1000 % 10
		modeC := op / 10000 % 10
		opcode := op % 100
		switch opcode {
		case 1, 2, 7, 8:
			a := fetch(modeA, pc+1)
			b := fetch(modeB, pc+2)
			c := fetchIndex(modeC, pc+3)
			program[c] = arithmetic[opcode](a, b)
			pc += 4
		case 5, 6:
			a := fetch(modeA, pc+1)
			b := fetch(modeB, pc+2)
			condition := map[int]bool{5: a != 0, 6: a == 0}[opcode]
			if condition { pc = b } else { pc += 3 }
		case 3:
			a := fetchIndex(modeA, pc+1)
			program[a] = <-input
			pc += 2
		case 4:
			v := fetch(modeA, pc+1)
			output <- v
			pc += 2
		case 99:
			// The modules halt *after* giving an output, which means
			// we cannot detect a halted state until we feed input
			// into a halted module at some point in the loop. So it
			// needs to accept that input, and then inform that it has
			// halted (close) instead of responding with a new output.
			<-input
			close(output)
			return
		default:
			panic(fmt.Sprintf("unknown opcode %d", opcode))
		}
	}
}

func runSeries(program []int, phases []int, feedback bool) int {
	N := len(phases)

	// I bet Go's channels are great for this stuff.
	input := make([]chan int, N)
	output := make([]chan int, N)

	// Spawn goroutines for all five instances of the program...
	for i, p := range phases {
		input[i] = make(chan int)
		output[i] = make(chan int)
		go runProgram(append([]int(nil), program...), input[i], output[i])
		input[i] <- p
	}

	// Feed the initial input signal to A.
	input[0] <- 0

	// Coordinate their communication in a loop, tracking E's output.
	var end_output int
	for {
		for i, _ := range phases {
			value, ok := <-output[i]
			if feedback {
				if !ok { return end_output }
				if i == N-1 { end_output = value }
			} else {
				if i == N-1 { return value }
			}
			input[(i+1)%N] <- value
		}
	}
}

func bestSignal(program []int, phases []int, feedback bool) int {
	var best int
	for _, p := range permutations(phases) {
		signal := runSeries(program, p, feedback)
		if signal > best {
			best = signal
		}
	}
	return best
}

func main() {
	program := []int{3,8,1001,8,10,8,105,1,0,0,21,34,59,76,101,114,195,276,357,438,99999,3,9,1001,9,4,9,1002,9,4,9,4,9,99,3,9,102,4,9,9,101,2,9,9,102,4,9,9,1001,9,3,9,102,2,9,9,4,9,99,3,9,101,4,9,9,102,5,9,9,101,5,9,9,4,9,99,3,9,102,2,9,9,1001,9,4,9,102,4,9,9,1001,9,4,9,1002,9,3,9,4,9,99,3,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99}

	fmt.Println("*  Best serial thruster signal:",   bestSignal(program, []int{0,1,2,3,4}, false))
	fmt.Println("** Best feedback thruster signal:", bestSignal(program, []int{5,6,7,8,9}, true))
}
