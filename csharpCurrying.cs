using System;
using System.Collections.Generic;
using System.Linq;

public class Program {
    public static A Foldl<A,B>(Func<A,B,A> f, A initial, IEnumerable<B> xs) {
        var acc = initial;
        foreach (var x in xs) {
            acc = f(acc, x);
        }
        return acc;
    }

    public static A Foldl<A,B>(Func<A,Func<B,A>> curriedFn, A initial, IEnumerable<B> xs) {
        var acc = initial;
        foreach (var x in xs) {
            var f = curriedFn(acc); // Call with acc to get remainder of function
            acc = f(x);
        }
        return acc;
    }

    public static A ConstUncurried<A,B>(A a, B b) { return a; }

    //`const` should actually be `Func<B,A> Const(A a)`, but we're using `int`
    //as that's all we need for this example, and it makes the type defs less verbose.
    public static Func<int,int> Const(int a) { return b => ConstUncurried(a,b); }


    public static void Main() {
        var xs = new[] {1,2,3,4};

        //Create a lambda with 2 arguments.
        var length0 = Foldl<int,int>((acc, x) => acc+1, 0, xs);
        Console.WriteLine("length0: " + length0.ToString());
        //
        //Create a lambda with 2 arguments.
        var length1 = Foldl<int,int>(acc => x => acc+1, 0, xs);
        Console.WriteLine("length1: " + length1.ToString());

        //Explicitly use `acc` and `x`
        var length2 = Foldl<int,int>((acc, x) => Const(acc+1)(x), 0, xs);
        Console.WriteLine("length2: " + length2.ToString());

        //Drop `x` but having our lambda return a `Func<int,int>`, 
        //rather than a `Func<int,int,int>`.
        var length3 = Foldl<int,int>(acc => Const(acc+1), 0, xs);
        Console.WriteLine("length3: " + length3.ToString());

        //Drop `acc` by having using a `Func<int,Func<int,int>>` as our lambda,
        //instead of a `Func<int,int,int>`.
        var length4 = Foldl<int,int>(acc => Compose<int,int,Func<int,int>>(Const,Succ)(acc), 0, xs);
        Console.WriteLine("length4: " + length4.ToString());

        var length5 = Foldl<int,int>(Compose<int,int,Func<int,int>>(Const,Succ), 0, xs);
        Console.WriteLine("length5: " + length5.ToString());
    }

    public static Func<A,C> Compose<A,B,C>(Func<B,C> f, Func<A,B> g) {
        return x => f(g(x));
    }

    public static int Succ(int x) { return x+1; }
}
