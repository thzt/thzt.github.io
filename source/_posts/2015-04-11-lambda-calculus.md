---
layout: post
categories: Lisp
title: Lisp程序员眼中的λ演算
description: λ演算是程序设计语言的理论基础，它是一套满足特定规则的符号系统，简洁的语法下面蕴含着强大的表达能力。
---

形式系统是一套符号化的系统，

这些符号，遵循了某些规则，

模型化了待研究的现实世界。

<br/>

例如：

(lambda (x) x)

是由符号“(”，“)”，“lambda”和“x”构成的。

它们符合“S表达式”的语法规则，

以及Lisp函数调用的推导规则，

表示了一个数学函数f(a)=a。

<br/>

区分符号和它指称的事物，

是很重要的。

<br/>

我们要研究的这些符号构成了一种语言，

称为**目标语言**。

而符号所指称的事物也构成了一种语言，

称为**元语言**。

<br/>

例如：

符号1，我们通常认为它是自然数1。

但这只是通俗的说法。

<br/>

事实上，符号1，符号2，符号3，...属于目标语言范围，

而自然数1，自然数2，自然数3，...属于元语言范围。

<br/>

符号1和自然数1是不同的，

如果愿意，我们当然可以用符号a表示自然数1。

<br/>

## **λ演算——形式语法**

λ演算系统，是一个形式系统，

用来研究编程语言。

<br/>

在内容上，大体分为几个部分。

**形式语法，公理语义，不动点，操作语义，指称语义**。

<br/>

**形式语法**，

用来说明合法表达式的组成方式。

定义如下：

M ::= x | MM | λx.M

<br/>

这是一个递归定义，

它表示，在λ演算系统中，合法的表达式，

要么是一个变量x，

要么是一个函数调用（application），

要么是一个函数抽象（lambda abstraction）。

<br/>

例如：如下表达式就是合法的，

x，变量x

(λx.x)5，函数调用

λx.y，函数抽象

<br/>

我们看到合法的lambda表达式，

在语法上和Lisp中的S表达式很相似。

<br/>

x <=> x

(λx.x)5 <=> ((lambda (x) x) 5)

λx.y <=> (lambda (x) y)

<br/>

事实上，Lisp语言的语法，

确实受到了λ演算的影响。

<br/>

在不影响歧义的情况下，

我们将采用Lisp语言来讨论λ演算。

因为，他们可以看做只是语法不同的两套形式系统。

<br/>

## **λ演算——公理语义**

**公理语义**，

是一套等式证明系统，

用来区分一个形式系统中的两个表达式，

是不是等价。

<br/>

正如不同的语法规则，给出了不同的形式语言，

不同的等价性条件，给出了有不同公理语义的形式系统。

<br/>

给λ演算添加不同的等价性规则，

会导致不同的λ演算系统。

<br/>

最常用的两种等价规则是，

**α等价**，和**β等价**。

<br/>

**α等价**指出，

函数的形参只是占位符，

替换形参和函数体中相应名字的符号，

所产生的新表达式与原表达式等价。

<br/>

例如：

(lambda (x) x)和(lambda (y) y)

是等价的。

<br/>

这里还没有谈到这两个表达式的指称语义。

无论这些符号指称什么，

在公理语义的约束下，都是等价的。

<br/>

**β等价**指出，

函数调用表达式，等价于，

把函数体中的形参替换成实参后的表达式。

<br/>

例如：

((lambda (x) (+ a x)) y)等价于(+ a y)

<br/>

需要注意的是，

实参含有自由变量，

可能会与替换后环境中的绑定变量冲突。

<br/>

例如：

(

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x) 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (y) 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(+ x y)))

&nbsp;&nbsp;&nbsp;&nbsp;(- a y)

)

<br/>

如果我们单纯把形参x替换成(- a y)

结果如下：

(lambda (y) 

&nbsp;&nbsp;&nbsp;&nbsp;(+ (- a y) y))

其中，函数体(+ (- a y) y)中，

第一个y就会被绑定的符号y所捕获。

<br/>

这容易引起歧义，

因此，在这种情况下，我们需要为绑定变量更名。即，

(lambda (z) 

&nbsp;&nbsp;&nbsp;&nbsp;(+ (- a y) z))

<br/>

## **λ演算——不动点**

我们看到λ演算中，所有函数都是匿名的，

这样在函数体内部引用函数本身，是很困难的事情。

<br/>

例如，阶乘函数，

(define fact

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (n)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(if (= n 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(* n (fact (- n 1))))))

函数内部引用了函数本身。

<br/>

改写成等式形式，

fact = (lambda (n)

&nbsp;&nbsp;&nbsp;&nbsp;(if (= n 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(* n (fact (- n 1)))))

我们发现，在等式两边都出现了fact。

<br/>

这使得我们回忆起了**代数学**中，

求解以下方程式。

x = G(x)。

<br/>

其中，

G(f) = (lambda (n)

&nbsp;&nbsp;&nbsp;&nbsp;(if (= n 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(* n (f (- n 1)))))

或，

G = (lambda (f)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (n)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(if (= n 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(* n (f (- n 1))))))

<br/>

待求取的函数fact，是这个方程的解。即，

fact = G(fact)。

我们称，方程x = G(x)的解为函数G的**不动点**。

<br/>

那么这个方程有解吗？

幸运的是，人们已经找到了求解不动点的办法。

<br/>

方法如下：

人们发现存在一个称为**Y组合子**（Y combinator）的函数，

可以得到任意函数G的不动点。即，

令x = YG，则x = G(x)

<br/>

因此，上例中，

fact = YG

<br/>

其中，

Y = (lambda (k)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;((lambda (g) (g g))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (f)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (n)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;((k (f f))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;n)))))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;

这里需要注意，**Y本身不是G的不动点**，

YG才是。

<br/>

## **λ演算——操作语义**

**操作语义**，

是一套推导规则，

据此表达式可以规约成更简单的形式。

<br/>

与等式证明系统不同的是，

推导规则具有方向性。

<br/>

相同之处在于，

在不同个数规约规则的限制下，

得到了不同的λ演算系统。

<br/>

最常用的规约规则，称为**β规约**。

它是β等价的有向形式。

<br/>

记法如下：

((lambda (x) (+ a x)) y) -> (+ a y)

<br/>

另外，某些表达式，β规约**不可终止**的。

例如：

(

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(x x))

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(x x))

)

->

(

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(x x))

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(x x))

)

-> ...

<br/>

对于β规约可终止的表达式，人们发现，

按不同的次序，对表达式进行规约，

总是可以得到相同的最终表达式，

称为**范式**（normal form）。

这种性质，称为**汇聚性**（confluence）。

<br/>

## **λ演算——指称语义**

**指称语义**，

是通过为每一个表达式指定一个数学对象，

作为该表达式的指称，

来说明表达式语义的一种办法。

<br/>

能这样做，是建立在**语义合成性**（compositionality）前提之下的。

即，表达式的语义，只由它的子表达式语义决定。

<br/>

例如：

对于加法表达式，

(+ 1 2)

<br/>

符号+，指称数学函数，加法函数，

符号1，指称自然数1，

符号2，指称自然数2。

<br/>

整个表达式指称，自然数1与自然数2的加法操作。

<br/>

记为：
E\[\[+\]\] = 加法函数

E\[\[1\]\] = 自然数1

E\[\[2\]\] = 自然数2

E\[\[e1+e2\]\] = E\[\[e1\]\]+E\[\[e2\]\]

其中，等式右边的+表示自然数加法操作。

<br/>

## **结语**

λ演算虽然语法简单，但是内涵丰富，

这很符合**Scheme语言**的设计哲学。

<br/>

从模型论的角度来看，

如果说物理学是对现实世界的建模，

那么程序设计语言，就是对计算的建模。

<br/>

λ演算和其他编程语言一样，采用形式方法，

用满足特定规则的一组符号，

建立了计算模型。

<br/>

图灵机，递归函数论等，采用了其他方式建模，

虽然复杂一些，但是与λ演算具有相同的计算能力。

<br/>

λ演算很值得学习，

会了解到很多数学上的基础问题，

集合论，证明论，递归论，模型论，

都有不同程度的应用。














