mod Stdlib.Test.Range {

    def nextInt32(e: Unit -> Option[Int32]): Option[Int32] = {
        match e() {
            case Some(v)    => Some(v+1)
            case None       => None
        }
    }

    def prevInt32(e: Unit -> Option[Int32]): Option[Int32] = {
        match e() {
            case Some(v)    => Some(v-1)
            case None       => None
        }
    }

    def nextInt32ByStep(e: Unit -> Option[Int32]): Option[Int32] = {
        match e() {
            case Some(v) if v >= 15                 => Some(v+5)
            case Some(v) if v >= 10                 => Some(v+2)
            case Some(v) if v >= 0                  => Some(v+3)
            case Some(v)                            => Some(v+1)
            case None                               => None
        }
    }

    @Test
    def iteratorBuilder01(): Bool = region rc {
        let expected = List#{-2,-1,0,1,2,3,4,5,6,7,8,9};
        let r = Range.Range(-2,9);
        let it = IteratorFactory.IteratorFactory({range=r, producer = nextInt32, isToBoundReached = (x,y) -> if (x <= y ) true else false});

        let v = MutList.new(rc);
        foreach (x <- it) { debug(x); MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def iteratorBuilder02(): Bool = region rc {
        let expected = List#{9,8,7,6,5,4,3,2,1,0,-1};
        let r = Range.Range(9,-2);
        let it = IteratorFactory.IteratorFactory({range=r, producer = prevInt32, isToBoundReached = (x,y) -> if (x > y ) true else false});

        let v = MutList.new(rc);
        foreach (x <- it) { debug(x); MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def iteratorBuilder03(): Bool = region rc {
        let expected = List#{-2,-1,0,3,6,9,12,14,16,21,26};
        let r = Range.Range(-2,27);
        let it = IteratorFactory.IteratorFactory({range=r, producer = nextInt32ByStep, isToBoundReached = (x,y) -> if (x <= y ) true else false});

        let v = MutList.new(rc);
        foreach (x <- it) { debug(x); MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

}
