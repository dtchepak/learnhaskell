using System;
using System.Collections.Generic;
using System.Linq;

public static class Folding {

    public interface IHeadTailList<A> {
        bool IsEmpty();
        A Head();
        IHeadTailList<A> Tail();
    }

    public class MyList<A> : IHeadTailList<A> {
        readonly int headIndex = 0;
        readonly A[] items;
        public MyList(params A[] items) { this.items = items; }
        MyList(int headIndex, params A[] items) { 
            this.headIndex = headIndex;
            this.items = items; 
        }
        public bool IsEmpty() { return headIndex >= items.Length; }
        public A Head() { return items[headIndex]; }
        public IHeadTailList<A> Tail() { return new MyList<A>(headIndex+1, items); }
    }

    public class CList<A> : IHeadTailList<A> {
        readonly bool empty;
        readonly A head;
        readonly CList<A> tail;
        CList(A head, CList<A> tail) { this.head = head; this.tail = tail; this.empty = false; }
        CList() { this.empty = true; }

        public A Head() { return head; }
        public IHeadTailList<A> Tail() { return tail; }
        public bool IsEmpty() { return empty; }
        public static readonly CList<A> Empty = new CList<A>();

        public static CList<A> operator +(A head, CList<A> tail) {
            return new CList<A>(head, tail);
        }
    }

    public static MyList<A> Cons<A>(this A head, MyList<A> tail) {
        return null;
    }

    public static A FoldLeft<A,B>(Func<A, B, A> f, A accum, IHeadTailList<B> list) {
        if (list.IsEmpty()) return accum;
        return FoldLeft(f, f(accum, list.Head()), list.Tail());
    }

    public static A FoldLeft2<A,B>(Func<A, B, A> f, A accum, IEnumerable<B> list) {
        var r = accum;
        foreach (var item in list) {
            r = f(r, item);
        }
        return r;
    }

    public static A FoldLeft<A,B>(Func<A,B,A> f, A accum, IEnumerable<B> list) {
        if (list.IsEmpty()) return accum;
        return FoldLeft(f, f(accum, list.First()), list.Skip(1));
    }

    public static A FoldRight<A,B>(Func<B,A,A> f, A seed, IEnumerable<B> list) {
        if (list.IsEmpty()) return seed;
        return f(list.First(), FoldRight(f, seed, list.Skip(1)));
    }

    public static A FoldRight<A,B>(Func<B,A,A> f, A seed, IHeadTailList<B> list) {
        if (list.IsEmpty()) return seed;
        return f(list.Head(), FoldRight(f, seed, list.Tail()));
    }

    public static bool IsEmpty<T>(this IEnumerable<T> items) { return !items.Any(); }

    public static void Main() {
        Console.WriteLine("Hello world!");
        
        var list = Enumerable.Range(1, 1000).ToArray();

        //var result = FoldLeft((a,b) => a+b, 0, list);
        //
        // mono folding.exe  5.95s user 0.01s system 99% cpu 5.966 total

        //var mylist = new MyList<int>(list);
        //var result = FoldLeft((a,b) => a+b, 0, mylist);
        //
        // mono folding.exe  0.04s user 0.00s system 95% cpu 0.042  

        //var list = Enumerable.Range(1, 1000).ToArray();
        //var result = FoldLeft2((a,b) => a+b, 0, list);
        //
        // mono folding.exe  0.04s user 0.00s system 95% cpu 0.043 total

        // =====================

        //var result = FoldRight((a,b) => a+b, 0, list);
        //
        // mono folding.exe  6.26s user 0.01s system 99% cpu 6.279 total

        //var mylist = new MyList<int>(list);
        //var result = FoldRight((a,b) => a+b, 0, mylist);
        //
        // mono folding.exe  0.04s user 0.00s system 94% cpu 0.042 total
        
        // =====================

        var clist = 1 + (2 + (3 + (4 + (5 + (6 + (7 + (8+ (9 + (10 + CList<int>.Empty)))))))));
        var result = FoldLeft((a,b) => a+b, 0, clist);

        Console.WriteLine(result);
    }
}
