---
layout: post
categories: Haskell
title: 惰性求值
description: 惰性求值是non-strict sematics的一种实现方式，WHNF刻画了按需求值的层次。
---

**据说**Haskell是一种惰性求值的语言，

表达式的求值策略是按需求值，即call-by-need。

<br/>

但是到底什么才是按需的，

求值到什么程度。

刚开始学习的时候，总是很模糊。

<br/>

随着学习的深入，才慢慢了解，

Haskell规范中，并没有涉及惰性求值，

只说它是一种**non-strict language**，

求值策略取决于具体的实现。

<br/>

这是怎么回事呢？

又要从头开始了。

<br/>

## **non-strictness**

non-strict function，是**指称语义**中的概念，

指称语义将每一段代码，与一个数学对象相对应，

借此来研究程序的语义。

<br/>

一开始我们认为，程序中的函数，

会有直接的数学函数与之对应。

<br/>

其实不然，

因为，程序中的很多函数，在处理某些参数的时候，

行为是“未定义的”，得不到有用的信息，

这样的数学函数称为**部分函数**（partial function）。

<br/>

这里尽量[不要提及“不能终止的”](http://www.vex.net/~trebla/haskell/lazy.xhtml)，

因为指称语义并不考虑求值过程。

<br/>

例如：

partialFn :: Integer -> Integer

partialFn 0 = 0

partialFn n = test $ n + 1

<br/>

可以看到partial只在参数为0的时候是有意义的。

所以，与程序中的函数对应的，

就不是普通意义上的数学函数了，

还需要**扩充定义域和值域**。

<br/>

我们为每个集合增加一个新的值“**⊥**”，称为bottom，

用来表示“未定义”。

函数f(n)={0,n=0;⊥,n≠0}，就能表示partialFn了。

<br/>

Haskell用undefined来表示⊥，

undefined也确实包含在所有类型中，

\> :t undefined

\> undefined :: a

<br/>

通常情况下，

如果一个数学函数的参数是⊥，则结果就是⊥。

这样的函数称为**strict function**。

<br/>

而Haskell中函数则不同。

例如：

nonStrictFn :: Integer -> Integer

nonStrictFn 0 _ = 0

<br/>

nonStrictFn 0 undefined
\> 0

<br/>

函数nonStrictFn在参数包含undefined的时候，

并没有得到undefined，而是得到了一个明确的值0，

它对应的数学函数是**non-strict function**。

<br/>

函数的指称语义是non-strict function，

这样的语言，称为non-strict language。

<br/>

## **lazy evaluation**

求值，是**操作语义**中的概念。

**能实现non-strict指称语义的求值策略并不是唯一的**。

<br/>

GHC是Haskell目前最流行的编译器，

它使用了**惰性求值**（lazy evaluation）。

<br/>

例如：

x = 1

y = 2

z = (x, y)

<br/>

\> :sprint x

\> x = _

\> :sprint y

\> y = _

\> :sprint z

\> y = _

<br/>

这里“:sprint”只是打印表达式，而不会求值它。

“_”用来表示“未求值的”，或称为一个“**thunk**”。

<br/>

thunk在使用的时候，**并不一定被完全求值**。

\> let first = (u, _) = u

\> first z

\> 1

<br/>

\> :sprint x

\> x = 1

\> :sprint y

\> y = _

\> :sprint z

\> y = (1, _)

<br/>

first函数，只求值了第一个参数，

第二个参数，仍然是未求值的。

<br/>

实际上，**thunk的求值是分层次的**，

为了尽量少的求值，每一次只将表达式求值为**WHNF**（weak head normal form），

其中的子表达式，仍然是未求值的thunk。

<br/>

如果不能满足需要，

就会继续将对应的sub-thunk，再求值为WHNF。

<br/>

## **Weak head normal form**

一个WHNF（weak head normal form），

是表达式求值的一种结果形式。

<br/>

WHNF，只将表达式求值到，

最外层的**值构造器**或者**lambda抽象**为止。

<br/>

例如：以下表达式是WHNF，

(1 + 1, 2 + 2)，最外层是一个值构造器，(,)

\x -> 2 + 2，最外层是一个lambda抽象，

'h' : ("e" ++ "llo")，最外层是一个值构造器，(:)

<br/>

以下表达式不是WHNF：

1 + 2，最外层是加法函数调用，(+)

(\x -> x + 1) 2，最外层是一个匿名函数调用

"he" ++ "llo"，最外层是列表的连接函数调用，(++)

<br/>

因此，

表达式(1 + 1, 2 + 2)会按需首先求值为(thunk1,thunk2),

如果还需要thunk1的值，thunk1会求值为2。

<br/>

例如：

testFn1 _ = 1

testFn2 (_, _) = 2

tettFn3 (u, _) = 3

这3个函数会导致表达式(1 + 1, 2 + 2)进行**不同层次**的求值。

<br/>

## **seq**

为了控制求值程度，

GHC内置了seq函数。

<br/>

\> :t seq

\> seq :: a -> b -> b

<br/>

它表示，**在结果求值为WHNF之前**，

**会先将seq的第一个参数求值为WHNF**。

<br/>

例如：

u = 1

v = (u, u)

<br/>

x = 1

y = seq x (x, x)

<br/>

\> :sprint u

\> u = _

\> :sprint v

\> v = _

<br/>

\> :sprint x

\> x = _

\> :sprint y

\> y = _

<br/>

我们定义一个函数，用来把参数求值WHNF，

\> let whnf (_, _) = ()

<br/>

则，

\> whnf v

\> ()

\> whnf y

\> ()

<br/>

\> :sprint u

\> u = _

\> :sprint v

\> v = (_, _)

<br/>

\> :sprint x

\> x = 1

\> :sprint y

\> y = (1, 1)

<br/>

使用seq控制求值深度，

可以防止创建过多的thunk。

<br/>

## **结语**

当我们在REPL中输入一个表达式时，

实际上调用了print函数，

它会对表达式完全求值。

<br/>

因此，表达式求值的层次问题，

**是很难从REPL中看出来的**。

<br/>

另一方面，WHNF将求值过程分成了一系列节点，

能清晰的刻画求值程度，

这可能是我们学习Haskell入门的起点吧。

<br/>

参考：

[Haskell/Denotational semantics]()

[Haskell/Laziness](https://en.wikibooks.org/wiki/Haskell/Laziness)

[Haskell: What is Weak Head Normal Form?](http://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form)

[Parallel and Concurrent Programming in Haskell](http://book.douban.com/subject/24294415/)

[How Lazy Evaluation Works in Haskell](https://hackhands.com/lazy-evaluation-works-haskell/)

[Lazy Evaluation of Haskell](http://www.vex.net/~trebla/haskell/lazy.xhtml)
