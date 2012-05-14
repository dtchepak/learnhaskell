using System;
using System.Collections.Generic;
using System.Linq;

public class Program {
    public static void Main() {
        var f0 = (acc,x) => acc + 1;

        Console.WriteLine(Eval(f0, 10, 123));

        var result0 = Eval(
    }

    static int Const(int a, int b) { return a; }
    static int Eval(Func<int,int,int> f, int acc, int x) { return f(acc,x); }

    static Func<int,int> Const(int a) { return b => Const(a,b); }
    static int Eval(Func<int,Func<int,int>> f, int acc, int x) { return f(acc)(x); }
}
