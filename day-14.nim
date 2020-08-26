# https://adventofcode.com/2019/day/14 in Nim.

import sequtils
import strutils
import sugar
import tables

proc ceilDiv(x: int64, y: int64): int64 =
  (x + y - 1) div y

type Graph[T] = seq[tuple[src: T, dst: T]]

iterator toposort*[T](graph: Graph[T]): T =
  var g = graph
  var srcs = g.map(e => e.src)
  var dsts = g.map(e => e.dst)
  var nodes = (srcs & dsts).deduplicate()
  while nodes.len > 0:
    var open = nodes.filter(n => not g.any(e => e.dst == n))
    if open.len == 0:
      raise newException(Exception, "cyclic graph")
    for o in open:
      yield o
    nodes.keepIf(n => n notin open)
    g.keepIf(e => e.src notin open)

type Chemical = string
type Quantity = tuple[count: int, chemical: Chemical]
proc parseQuantity(s: string): Quantity =
  var halves = s.split(" ")
  var count = parseInt(halves[0])
  var chemical = halves[1]
  return (count, chemical)

type Reaction = tuple[ingredients: seq[Quantity], product: Quantity]
proc parseReaction(s: string): Reaction =
  var halves = s.split(" => ")
  var ingredients = halves[0].split(", ").map(parseQuantity)
  var product = parseQuantity(halves[1])
  return (ingredients, product)

var reactions = stdin.readAll().split("\n").map(parseReaction)

var dependencies: Graph[Chemical] = @[]
for r in reactions:
  for i in r.ingredients:
    dependencies.add((r.product.chemical, i.chemical))

proc oreForFuel(fuel: int64): int64 =
  var insOf = newTable[Chemical, seq[Quantity]]()
  var nOf = newTable[Chemical, int]()
  for r in reactions:
    insOf[r.product.chemical] = r.ingredients
    nOf[r.product.chemical] = r.product.count
  var made = newTable[Chemical, int64]()
  var need = newTable[Chemical, int64]()
  need["FUEL"] = fuel

  for chem in toSeq(toposort(dependencies)):
    if chem == "ORE":
      continue
    let stillNeed = need.mgetOrPut(chem, 0) - made.mgetOrPut(chem, 0)
    let batches = ceilDiv(stillNeed, nOf[chem])
    made[chem] += batches * nOf[chem]
    for i in insOf[chem]:
      need[i.chemical] = need.getOrDefault(i.chemical) + batches * i.count
    
  return need["ORE"]

# *: How much ORE for one FUEL?
echo("* ", oreForFuel(1))

# **: Binary search for the point at which we need more than one trillion ORE.
var lo = 1'i64
var hi = 1_000_000_000_000'i64

while lo + 1 < hi:
  var guess = (lo + hi) div 2
  var ore = oreForFuel(guess)
  if ore > 1_000_000_000_000:
    hi = guess
  else:
    lo = guess

echo("** ", lo)
