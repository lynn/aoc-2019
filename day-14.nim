import sequtils
import strutils
import sugar

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
    var o = open[0]
    yield o
    nodes.keepIf(n => n != o)
    g.keepIf(e => e.src != o)

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
    dependencies.add((i.chemical, r.product.chemical))


echo(toSeq(toposort(dependencies)))
