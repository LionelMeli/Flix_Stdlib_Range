def nextInt32(e: Unit -> Option[Int32]): Option[Int32] = 
    match e() {
        case Some(v)    => Some(v+1)
        case None       => None
    }

def nextTuple2(e: Unit -> Option[(Int32,Int32)]): Option[(Int32,Int32)] = 
    match e() {
        case Some(v, _) if v >= 13                  => Some(v+1, 5)
        case Some(v, _) if (v >= 6) and (v < 13)    => Some(v+3, v+3)
        case Some(v, _) if v >= 0                   => Some(v+1, v+1)
        case Some(v, _) if v <= 0                   => Some(v+1, 0)
        case Some(_, _)                             => None
        case None                                   => None
    }




def cond(x:(Int32,Int32), y:(Int32,Int32)): Bool = fst(x) <= fst(y)

// The main entry point.
def main(): Unit \ IO =
    
    println("Begin" |> Console.bold |> Console.blue);
    /*
    let r1 = Range.Range(-9,0);
    let it1 = IteratorFactory.IteratorFactory({range=r1, producer = nextInt32, isToBoundReached = (x,y) -> if (x <= y ) true else false});
    foreach (x <- it1) println("Loop:${x}");
    */
    /*
    let r2 = Range.Range((-10,0),(20,0));
    let it2 = IteratorFactory.IteratorFactory({range=r2, producer = nextTuple2, isToBoundReached = (x:(Int32,Int32), y:(Int32,Int32)) -> if (fst(x) <= fst(y)) true else false});
    foreach (x <- it2) println("Loop:${x}");
    */


    System.StdOut.print("?s");

    /*
    let it3 = IteratorReversedFactory.Wrapper("Hello World");
    foreach (x <- it3) println("Loop:${x}");
    */

    println("End." |> Console.bold |> Console.blue)
