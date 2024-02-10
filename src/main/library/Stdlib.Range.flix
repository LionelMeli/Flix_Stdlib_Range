pub enum Range[a](a,a)

pub type alias Producer[a] = (Unit -> Option[a]) -> Option[a]
pub type alias Predicat[a] = a -> a -> Bool

pub enum IteratorFactory[a]({range = Range[a], producer = Producer[a], isToBoundReached = Predicat[a]})

pub enum IteratorReversedFactory[a]{
    case List(List[a]), Wrapper(a)
}

instance Iterable[IteratorFactory] {
    pub def iterator(rc: Region[r], x: IteratorFactory[a]): Iterator[a, r, r] \ r = 
        Stdlib.Range.iteratorBuilder(rc, x)
}

instance Iterable[IteratorReversedFactory] {
    pub def iterator(rc: Region[r], x: IteratorReversedFactory[a]): Iterator[a, r, r] \ r = 
        Stdlib.Range.iteratorReversedBuilder(rc, x)
}

mod Stdlib.Range {
    
    pub def iteratorBuilder(rc: Region[r], it: IteratorFactory[a]): Iterator[a, r, r] \ r = {
        let IteratorFactory.IteratorFactory(r) = it;
        let Range.Range(fromBound, toBound) = r.range;

        let curr = ref Some(fromBound) @ rc;
        let producer = deref (ref r.producer @ rc);

        let iterF = () -> match (deref curr) {
            case Some(i) if r.isToBoundReached(i, toBound)  => { let c = deref curr; curr:= producer(() -> c); Some(i)}
            case _                                          => None
        };
        Iterator.iterate(rc, iterF)
    }

    pub def iteratorReversedBuilder(rc: Region[r], it: IteratorReversedFactory[a]): Iterator[a, r, r] \ r = {
        match it {
            case IteratorReversedFactory.List(list)  => let l = ref List.reverse(list) @ rc; List.iterator(rc, deref l)
            case IteratorReversedFactory.Wrapper(t)  => typematch t {
                                                            case s: String  =>  let length =  String.length(s)-1;
                                                                                Iterator.range(rc, 0, length) |> 
                                                                                Iterator.map(i -> unchecked_cast(Char.toString(String.charAt(length - i, s)) as a))
                                                            case _: _       =>  Iterator.empty(rc)
                                                        }
            case _                                  => Iterator.empty(rc)
        }
    }

}