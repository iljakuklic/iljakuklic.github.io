---
title: Non-regular types in Java
---

Ironically, the first real post on this blog is about a programming language I hardly ever use.
I could not think of technical reason why Java could not support non-regular types and
polymorphic recursion as these two features are usually enabled by the type erasure property.
But is it really the case? Googling did not seem to reveal the answer so I decided to find out
myself. The short answer is yes, you can have non-regular data types in Java. The long answer
is the rest of this post.

<!--more-->


### Setting up the scene

Before we get to the meat of non-regular types, let's define a couple of helper classes first.
Bear in mind that the samples are probably far from being idiomatic Java code.
Hopefully they will be good enough to illustrate the point.

The `Pair`{.lang-java} class represents just a pair of things of the same type,
denoted by the type argument `T`{.lang-java}.
To facilitate creation of instances of this class,
a smart constructor `mk()`{.lang-java} is provided.
For example, a pair with strings "foo" and "bar" in it is
constructed using `Pair.mk("foo", "bar")`{.lang-java}.
The components of the pair can be accessed by directly using
the `left`{.lang-java} and `right`{.lang-java} attributes.
The pair class is designed as an immutable data structure:
all the member variables are qualified as `final`{.lang-java}.

~~~~~~~ { .lang-java }
class Pair<T> {

    final public T left;
    final public T right;

    public Pair(T left, T right) {
        assert left != null && right != null: "Cheating!";
        this.left = left;
        this.right = right;
    }

    public static <T> Pair<T> mk(T left, T right) { return new Pair<T>(left, right); }
};
~~~~~~~~~~~~~~

Another utility class we will use is just a simple callback function interface.
It is defined as follows:

~~~~~ { .lang-java }
interface Proc<T> {
    public void run(T x);
};
~~~~~~~~~~

### A regular tree

I'd like to illustrate non-regular types on the example of binary trees.
But before diving into the non-regular types proper, let me introduce an
example of a classic binary tree so we have something to compare with.
In our case, data will be stored in leaves.

A regular binary tree `RTree`{.lang-java} is either a leaf node with data
or it contains a pair of child trees.
That is, for the sake of implementation simplicity,
exactly one of `data`{.lang-java} and `sub`{.lang-java} fields is non-null
in each tree node.
Notice that the definition of `RTree<T>`{.lang-java}
recursively refers to `RTree<T>`{.lang-java} (the same thing we are defining).
This will be important later on as it's the main point of difference between
this definition and the definition of a non-regular binary tree.

~~~~~ { .lang-java }
class RTree<T> {
    final public T data;
    final public Pair<RTree<T>> sub;

    // private constructor... not to be used directly
    private RTree(T data, Pair<RTree<T>> sub) {
        this.data = data; this.sub = sub;
    }
~~~~~~~~~~

The constructor is defined as private to enforce some invariants.
The client of this class will have to use one of two smart constructors available:

* `mkLeaf()`{.lang-java} constructs a leaf node with data in it.
  In this case, the `sub`{.lang-java} field will be set to null.
* `mkNode()`{.lang-java} constructs an internal node with two subtrees.
  In this case, the `data`{.lang-java} field will be set to null.

The `isLeaf()`{.lang-java} function just checks if this is a leaf or
an internal node of the tree.

~~~~~ { .lang-java }
    // leaf constructor
    public static <T> RTree<T> mkLeaf(T data) {
        return new RTree<T>(data, null);
    }
    // node constructor
    public static <T> RTree<T> mkNode(RTree<T> left, RTree<T> right) {
        return new RTree<T>(null, Pair.mk(left, right));
    }
    // check if this is a leaf node
    public boolean isLeaf() { return sub == null; }
~~~~~~~~~~

Last but not least, we have the `each()`{.lang-java} procedure that executes
action specified by `proc`{.lang-java} for every leaf node, passing in the
data associated with the node.
It is implemented by a straightforward recursion on the structure of the tree.

~~~~~ { .lang-java }
    public void each(Proc<T> proc) {
        if (isLeaf())
            proc.run(data);
        else {
            sub.left.each(proc);
            sub.right.each(proc);
        }
    }
};
~~~~~~~~~~

This concludes the regular case. Hopefully no surprises so far.
Let's move on to the fun part.

### A non-regular tree

Definition of a non-regular data structure might look like this:

`NTree`{.lang-java} is either a leaf with data content or it contains an `NTree`{.lang-java}
with a pair of `T`{.lang-java}s in it.
Notice that the definition of `NTree<T>`{.lang-java} now does not contain
`NTree<T>`{.lang-java} but rather `NTree<Pair<T>>`{.lang-java}.
This is what makes `NTree`{.lang-java} a non-regular type.
Again, `data`{.lang-java} is only non-null for leaf nodes and `deep`{.lang-java}
is only non-null for intermediate nodes.

~~~~~ { .lang-java }
class NTree<T> {
    final public NTree<Pair<T>> deep;
    final public T data;

    // private constructor... not to be used directly
    private NTree(T data, NTree<Pair<T>> deep) {
        this.data = data; this.deep = deep;
    }
~~~~~~~~~~

Again, we have two smart constructors. However, they behave a bit differently this time:

* `mkData()`{.lang-java} just stores given data point.
* `mkDeep()`{.lang-java} turns a tree of pairs elements of type `T`{.lang-java}
  into a tree of elements of type `T`{.lang-java}.
  It kind of removes a level of pairing from the type, so pairs of `T`{.lang-java}s can be treates
  as just `T`{.lang-java}s inside this kind of tree.

As before, we have a function to check for the kind of node we're dealing with.
This time, it's called `isData()`{.lang-java}.

~~~~~ { .lang-java }
    // data constructor
    public static <T> NTree<T> mkData(T data) {
        return new NTree<T>(data, null);
    }
    // deeper tree constructor
    public static <T> NTree<T> mkDeep(NTree<Pair<T>> deep) {
        assert deep != null: "Cheating!";
        return new NTree<T>(null, deep);
    }
    // check if this is a data node
    public boolean isData() { return deep == null; }
~~~~~~~~~~

Iteration over nodes turns out to be slightly trickier in non-regular settings as well.
The reason is that we have to turn a procedure that operates on things into one
that operates on pairs of things first.

~~~~~ { .lang-java }
    public void each(final Proc<T> proc) {
        if (isData())
            proc.run(data);
        else {
            final Proc<Pair<T>> deepProc = new Proc<Pair<T>>() {
                public void run(Pair<T> sub) {
                    proc.run(sub.left);
                    proc.run(sub.right);
                }
            };
            deep.each(deepProc);
        }
    }
};
~~~~~~~~~~

### What is `NTree`?

Let me point this out: we have defined two **very different data structures**.
On one hand, we have `RTree<T>`{.lang-java} which is just a normal binary tree as we know it.
On the other hand `NTree<T>`{.lang-java} is a weight-balanced binary tree.
That implies it is able to maintain an interesting invariant:
the number of its elements is statically guaranteed to be a power of two
(unless you are [cheating](#cheating)).

How does that happen? <!-- First observe that the data node has either type `T`{.lang-java},
or type `Pair<T>`{.lang-java}, or type `Pair<Pair<T>>`{.lang-java}, and so on. -->

Consider the following definitions:

~~~~~ { .lang-java }
String             x0 = "a";
Pair<String>       x1 = Pair.mk("b", "c");
Pair<Pair<String>> x2 = Pair.mk(Pair.mk("d", "e"), Pair.mk("f", "g"));
~~~~~~~~~~

By nesting zero or more pairs, we can only store a power-of-two number of strings.

We can turn these into corresponding `NTree`{.lang-java}s by applying the
`mkData()`{.lang-java} constructor:

~~~~~ { .lang-java }
NTree<String>             s0 = NTree.mkData(x0);
NTree<Pair<String>>       s1 = NTree.mkData(x1);
NTree<Pair<Pair<String>>> s2 = NTree.mkData(x2);
~~~~~~~~~~

Now, `s0`{.lang-java}, `s1`{.lang-java}, and `s2`{.lang-java} have different types.
We can reconcile it by applying the appropriate number of `mkDeep()`{.lang-java} constructors:

~~~~~ { .lang-java }
NTree<String> t0 = s0;
NTree<String> t1 = NTree.mkDeep(s1);
NTree<String> t2 = NTree.mkDeep(NTree.mkDeep(s2));
~~~~~~~~~~

So anywhere a `NTree<String>`{.lang-java} is expected, you can have a string,
or a pair of strings, or a pair of pairs of strings etc.
The nesting of `Pair`{.lang-java} classes is what induces the weight-balanced binary
tree structure. `NTree`{.lang-java} can then be used sum over an arbitrary number of
nesting of `Pair`{.lang-java}s into one type.

The output can be checked using a helper print procedure like this:

~~~~~ { .lang-java }
public static void printTreeData(NTree<String> t) {
    final Proc<String> print = new Proc<String>() {
        public void run(String str) {
            System.out.println(str);
        }
    };
    t.each(print);
    System.out.println();
}
~~~~~~~~~~

~~~~~ { .lang-java }
printTreeData(t0);
printTreeData(t1);
printTreeData(t2);
~~~~~~~~~~

#### Cheating {#cheating}

As we're dealing with a language with that has a type system that is not known to be
the most principled in the universe, there is a number of ways to violate the
stated invariant.
Here is a probably incomplete list:

* Null references. Each user-defined type in Java implicitly includes an extra value:
  `null`{.lang-java}. Therefore a value of type `NTree<String>`{.lang-java} is not really
  a balanced tree as it can also be null. I tried to make this less of an issue using
  `assert`{.lang-java} turning it into a runtime error.
* Reflection and unsafe type casts.
* Non-termination. Having a function that promises to return a tree that does not terminate
  does not actually return a tree.
* Exceptions. Similar to the previous point.

Use of some of these, such as casts, can be avoided.
However, for example a null reference can be easily introduced by accident,
making the techniques described here slightly less useful in Java setting.

### Alright, what is this good for?

Admittedly, the code shown is rather a toy example.
Use of non-regular types for encoding and statically checking invariants has been
demonstrated many times, mostly in functional programming setting.

In particular, you can:

* [Enforce balancedness](http://www.cs.kent.ac.uk/people/staff/smk/redblack/Typed.hs)
  of red-black trees.
* Implement [Finger Trees](http://comonad.com/reader/wp-content/uploads/2010/04/Finger-Trees.pdf).
  These are virtually lists, where the first element is a pair of trees of depth 1, second
  element is a pair of trees of depth 2, and so on.
  This hard-to-maintain invariant can be expressed with non-regular types as well.
* Have a "triangular" list of lists, where the first element is a list of one element,
  second element has two elements and so on.
  Useful for symmetric binary relations.

### Side note: non-regular types in C++

C++ templates are usually considered to be more powerful than generics in Java.
Nonetheless, in this case they fall short. If you tried to transliterate the definition
of `NTree<T>`{.lang-java} into C++, strange things would happen.

Due to the lack of type erasure in C++, the compiler will attempt to generate code for
`NTree<Pair<T>>`{.lang-java} while generating code for `NTree<T>`{.lang-java}.
That triggers generation of code for `NTree<Pair<Pair<T>>>`{.lang-java} and so on,
sending the compiler into the oblivion of mindless code generation until it gives up by
hitting an implementation-defined limit on number of nested template instantiations.

### Conclusion

By having only one run-time representation for all the instances of a polymorphic class,
Java is able to support non-regular data types.
This causes the type system to be able to encode non-trivial invariants
one might not expect to be able to express in Java.

That's it, good night.
