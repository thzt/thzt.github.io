---
layout: post
categories: Lisp
title: 尾递归优化
description: 据说Scheme是一门经过尾递归优化的语言，但是具体是怎样优化的，有什么好处，我们又不太清楚，本文梳理了一下这些头绪。
---

Scheme是一门支持Proper tail recursion的语言，

实际上这是对语言本身的约束，

任何实现都必须满足，

我们可以依赖它写出可移植的程序。

<br/>

那么，到底什么是尾递归呢？

什么样的才是Proper呢？

它给我们带来了什么好处呢？

<br/>

这还要从函数的尾调用说起。

我们发现，

Proper tail recursion和continuation有很深的关联。

<br/>

下文为了叙述方便，

我们用术语尾递归优化（tail recursion optimization）,

来介绍Proper tail recursion的技术细节。

<br/>

## **尾调用**

这是一个很常见的概念，

但是为了完整性，这里还是要说一说。

<br/>

我们看两个函数，f和g，他们的定义如下，

<br/>

(define (f a)

&nbsp;&nbsp;&nbsp;&nbsp;(g 2)

&nbsp;&nbsp;&nbsp;&nbsp;(display a))

<br/>

(define (g b)

&nbsp;&nbsp;&nbsp;&nbsp;(display b))

<br/>

(f 1)

<br/>

结果：

21

<br/>

我们分析一下实际的调用过程，

求值(f 1)，会导致f的函数体被求值，

于是，先求值(g 2)，导致g的函数体被求值，输出2，

然后**函数g返回**了，返回到f的函数体中，

再接着执行下一条语句，输出1。

<br/>

我们看到，对g的调用，不是f的最后一个调用。

称为**g不是f的尾调用**。

<br/>

我们改一下例子，

<br/>

(define (f a)

&nbsp;&nbsp;&nbsp;&nbsp;(display a)

&nbsp;&nbsp;&nbsp;&nbsp;(g 2))

<br/>

(define (g b)

&nbsp;&nbsp;&nbsp;&nbsp;(display b))

<br/>

(f 1)

<br/>

结果：

12

<br/>

现在g是f的尾调用了。

<br/>

为什么要强调尾调用呢？

因为，如果g是f的尾调用，

**g就可以不返回到f中**，

**而直接返回到f该返回的地方**。

<br/>

调用g的时候，就不会增长调用栈，

而是废弃原来f的调用环境即可。

<br/>

不必要的调用栈不会增加，

使得尾递归可以在常量的调用栈空间中执行，

我们就可以放心的使用尾递归来替代循环了。

<br/>

## **调用栈和调用图**

从语言的实现角度来看，

每一次函数调用会初始化一个新的frame，

frame中保存着形参与实参的绑定。

<br/>

例如：

(f 1)会产生一个frame，[(a 1)]

<br/>

而环境，是一个frame的**链表**，

top-level环境中只有一个frame，表示了变量f和g的绑定，

[(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)]

<br/>

所以，进入f的函数体后，

新创建的frame会添加到**f定义时的环境**头部，

(**[(a 1)]** [(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])

环境中有两个frame了。

<br/>

f的函数体是在这个新环境中求值的。

f函数体的执行环境是对定义时环境的扩展，

这是**词法作用域**规则的简单实现。

<br/>

我们再调用g，看看环境会怎样变化，

调用g会创建一个新的frame，[(b 2)]

这个frame会添加到**g定义时的环境**头部，

(**[(b 2)]** [(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])

<br/>

注意，环境并没有变成，

([(b 2)] **[(a 1)]** [(f #&lt;procedure>) (g #&lt;procedure&gt;)])

新的frame并不会添加到**调用g时的环境**中去。

<br/>

当g返回时，

环境又变成了f的执行环境，

(**[(a 1)]** [(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])

<br/>

跟踪运行环境的变化，我们发现，

在实现词法作用域之后，环境并不是一个栈结构的。

<br/>

([(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])&nbsp;&nbsp;&nbsp;&nbsp;;top-level： frame0

(**[(a 1)]** [(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])&nbsp;&nbsp;&nbsp;&nbsp;;进入f： frame1 <- frame0

(**[(b 2)]** [(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])&nbsp;&nbsp;&nbsp;&nbsp;;进入g： frame2 <- frame0

(**[(a 1)]** [(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])&nbsp;&nbsp;&nbsp;&nbsp;;回到f： frame1 <- frame0

([(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])&nbsp;&nbsp;&nbsp;&nbsp;;回到top-level： frame0

<br/>

我们可以把frame0看成树根，

frame1，frame2看成子节点，

于是，**环境构成了一棵树**，

这就是为什么我们之前说环境是frame的**链表**，而不是**列表**的原因了。

<br/>

既然这样，

那么尾调用也就不必服从弹栈规则了，

g返回，可以让执行环境返回到f该返回的状态。

<br/>

([(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])    ;top-level： frame0

(**[(a 1)]** [(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])    ;进入f： frame1 <- frame0

(**[(b 2)]** [(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)])    ;进入g： frame2 <- frame0

([(f #&lt;procedure&gt;) (g #&lt;procedure&gt;)]))    ;**直接返回到top-level**： frame0

<br/>

这种技术，称为**尾调用优化**。

<br/>

## **尾递归的执行环境**

我们来分析一下尾递归的执行环境，

请看阶乘函数的尾递归版本，

<br/>

(define (fact n **result**)

&nbsp;&nbsp;&nbsp;&nbsp;(if (= n 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**result**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fact (- n 1) (* result n))))

<br/>

(+ 4 (fact 3 1))

<br/>

结果：

10

<br/>

top-level环境：([(fact #&lt;procedure&gt;)])

要计算(+ 4 (fact 3 1))，先要求值(fact 3 1)，

调用(fact 3 1)，进入函数体：(**[(n 3) (result 1)]** [(fact #&lt;procedure&gt;)])

再次调用(fact 2 3)：(**[(n 2) (result 3)]** [(fact #&lt;procedure&gt;)])，

词法作用域规则，扩展定义环境，

然后调用(fact 1 6)：(**[(n 1) (result 6)]** [(fact #&lt;procedure&gt;)])，

这里要返回result了，值为6。

<br/>

可是要返回到哪里呢？

我们看到以上的一系列调用都是**尾调用**，

所以，直接返回到了最开始调用(fact 3 1)的地方，

执行环境变成了，([(fact #<procedure>)])，

于是，在这环境中求值(+ 4 6) -> 10

<br/>

## **与continuation的关联**

我们看到，要想实现这样的调用结构，

需要把环境中的绑定关系分配在**内存堆**中，

这样就可以让函数的调用者，显式控制返回环境了。

<br/>

实现了尾调用优化后，

函数的调用者多了一种选择，

或者让函数执行后，**返回到调用前的执行环境**，

或者**返回到调用者该返回的执行环境**。

<br/>

这其实是一种continuation的操作。

即，f调用g，g的continuation，

或者是f调用g后的continuation，

或者是f的continuation。

<br/>

我们用CPS改写一下上面的例子，

<br/>

**非尾调用**的情况，

(define \*cont\*

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x) x))

<br/>

(define (f a cont)

&nbsp;&nbsp;&nbsp;&nbsp;(g 2

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(lambda (v)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(cont (display a)))**))

<br/>

(define (g b cont)

&nbsp;&nbsp;&nbsp;&nbsp;(cont (display b)))

<br/>

(f 1 \*cont\*)

<br/>

结果：

21

<br/>

我们看到非尾调用的g的continuation，

是执行display，再执行f的continuation。即，

(lambda (v)

&nbsp;&nbsp;&nbsp;&nbsp;(cont (display a)))

<br/>

我们再看**尾调用**情况，

(define \*cont\*

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x) x))

<br/>

(define (f a cont)

&nbsp;&nbsp;&nbsp;&nbsp;(display a)

&nbsp;&nbsp;&nbsp;&nbsp;(g 2 **cont**))

<br/>

(define (g b cont)

&nbsp;&nbsp;&nbsp;&nbsp;(cont (display b)))

<br/>

(f 1 \*cont\*)

<br/>

结果：

12

<br/>

尾调用g的continuation，

是f的continuation。

<br/>

这给了我们一种方案，在写解释器的时候，

可以使用CPS来进行尾调用优化。

<br/>

## **结语**

为了进行尾调用优化，

语言实现必须对调用图进行控制，

或者说，显式的控制continuation。

<br/>

与其把这种显式的控制隐藏在语言的实现里面，

**不如开放给语言的使用者**，

因为开放出来并不是特别困难。

<br/>

这种非弹栈形式的跳转，

称为**非局部跳转**（non-local jump），

类似C语言的setjmp和longjmp。

<br/>

call/cc就是这样产生的，

并不是为了追求另类，

**而是实现尾调用优化的直接后果**。

<br/>

而且，call/cc捕获的continuation是**first-class**的，

可以当做参数传递，或者返回，

这极大的丰富的Scheme语言的表现力，

让程序员可以最大限度的控制跳转范围。

<br/>

参考：

[An Introduction to Scheme and its Implementation](http://www.cs.rpi.edu/academics/courses/fall00/ai/scheme/reference/schintro-v14/schintro_toc.html)

[Essentials of Programming Languages](http://book.douban.com/subject/4030015/)

[Lisp in small pieces](http://book.douban.com/subject/1456904/)

[Concepts in Programming Languages](http://book.douban.com/subject/2587705/)

[Compiling with continuations](http://book.douban.com/subject/1762126/)

[The Scheme Programming Language](http://www.scheme.com/tspl4/)

[RnRS](http://scheme-reports.org/)
