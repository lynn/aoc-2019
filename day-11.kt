// https://adventofcode.com/2019/day/11 in Kotlin.
// Try it here: https://play.kotlinlang.org/

data class Point(val x: Int, val y: Int)
fun Point.turnLeft() = Point(y, -x)
fun Point.turnRight() = Point(-y, x)
operator fun Point.plus(other: Point) = Point(x + other.x, y + other.y)

data class Result(val visitedPanelCount: Int, val whitePanels: HashSet<Point>)

fun run(program: ArrayList<Long>, startOnWhite: Boolean): Result {
    while (program.size < 10000) { program.add(0) }
    var whitePanels: HashSet<Point> = hashSetOf()
    if (startOnWhite) { whitePanels.add(Point(0, 0)) }
    var visitedPanels: HashSet<Point> = hashSetOf(Point(0, 0))
    var pos: Point = Point(0, 0)
    var dir: Point = Point(0, -1)
    var pc: Int = 0
    var rel: Int = 0
    var choseColor: Boolean = false
    fun fetch(mode: Int, i: Long): Long =
        when (mode) {
            0 -> program[i.toInt()]
            1 -> i
            2 -> program[i.toInt() + rel]
            else -> throw RuntimeException("unknown mode $mode")
        }
    fun fetchIndex(mode: Int, i: Long): Int =
        when (mode) {
            0 -> i.toInt()
            1 -> throw RuntimeException("cannot store into immediate")
            2 -> i.toInt() + rel
            else -> throw RuntimeException("unknown mode $mode")
        }
    intcode@ while (true) {
        val op = program[pc]
        val opcode = (op % 100).toInt()
		val modeA = ((op / 100) % 10).toInt()
        val modeB = ((op / 1000) % 10).toInt()
        val modeC = ((op / 10000) % 10).toInt()

        fun arithmetic(f: (a: Long, b: Long) -> Long) {
            val a = fetch(modeA, program[pc + 1])
            val b = fetch(modeB, program[pc + 2])
            val c = fetchIndex(modeC, program[pc + 3])
            program[c] = f(a, b)
            pc += 4
        }
        fun jumpIf(f: (it: Long) -> Boolean) {
            val a = fetch(modeA, program[pc + 1])
            val b = fetch(modeB, program[pc + 2])
            pc = if (f(a)) b.toInt() else pc + 3
        }
        when (opcode) {
            1 -> arithmetic { a, b -> a + b }
            2 -> arithmetic { a, b -> a * b }
            3 -> { // scan
                val a = fetchIndex(modeA, program[pc + 1])
                program[a] = if (whitePanels.contains(pos)) 1 else 0
                pc += 2
            }
            4 -> { // print
                val a = fetch(modeA, program[pc + 1])
                if (!choseColor) {
                    if (a == 0L) whitePanels.remove(pos) else whitePanels.add(pos)
                    choseColor = true
                } else {
                    dir = if (a == 0L) dir.turnLeft() else dir.turnRight()
                    pos += dir
                    visitedPanels.add(pos)
                    choseColor = false
                }
                pc += 2
            }
            5 -> jumpIf { it != 0L }
            6 -> jumpIf { it == 0L }
            7 -> arithmetic { a, b -> if (a < b) 1 else 0 }
            8 -> arithmetic { a, b -> if (a == b) 1 else 0 }
            9 -> {
                val a = fetch(modeA, program[pc + 1])
                rel += a.toInt()
                pc += 2
            }
            99 -> { break@intcode }
            else -> throw RuntimeException("unknown opcode $opcode")
        }
    }
    return Result(visitedPanels.size, whitePanels)
}

fun main() {
    var program: ArrayList<Long> = arrayListOf(3,8,1005,8,318,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,29,1,107,12,10,2,1003,8,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,59,1,108,18,10,2,6,7,10,2,1006,3,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,93,1,1102,11,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,118,2,1102,10,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,145,1006,0,17,1006,0,67,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,173,2,1109,4,10,1006,0,20,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,201,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,224,1006,0,6,1,1008,17,10,2,101,5,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,256,2,1107,7,10,1,2,4,10,2,2,12,10,1006,0,82,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,294,2,1107,2,10,101,1,9,9,1007,9,988,10,1005,10,15,99,109,640,104,0,104,1,21102,1,837548352256,1,21102,335,1,0,1105,1,439,21102,1,47677543180,1,21102,346,1,0,1106,0,439,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235190374592,1,21101,393,0,0,1105,1,439,21102,3451060455,1,1,21102,404,1,0,1105,1,439,3,10,104,0,104,0,3,10,104,0,104,0,21102,837896909668,1,1,21102,1,427,0,1105,1,439,21102,1,709580555020,1,21102,438,1,0,1105,1,439,99,109,2,21201,-1,0,1,21102,1,40,2,21102,1,470,3,21102,460,1,0,1106,0,503,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,465,466,481,4,0,1001,465,1,465,108,4,465,10,1006,10,497,1101,0,0,465,109,-2,2105,1,0,0,109,4,1201,-1,0,502,1207,-3,0,10,1006,10,520,21101,0,0,-3,21202,-3,1,1,22101,0,-2,2,21101,1,0,3,21101,0,539,0,1106,0,544,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,567,2207,-4,-2,10,1006,10,567,21202,-4,1,-4,1105,1,635,22101,0,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,0,586,0,1105,1,544,22102,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,605,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,627,21202,-1,1,1,21101,627,0,0,105,1,502,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0)

    // *
    val result1 = run(program, false)
    println(result1.visitedPanelCount)

    // **
    val result2 = run(program, true)
    val minX = result2.whitePanels.minOf { it.x }
    val maxX = result2.whitePanels.maxOf { it.x }
    val minY = result2.whitePanels.minOf { it.y }
    val maxY = result2.whitePanels.maxOf { it.y }
    for (y in minY..maxY) {
        for (x in minX..maxX) {
            print(if (result2.whitePanels.contains(Point(x, y))) "#" else " ")
        }
        print("\n")
    }
}
