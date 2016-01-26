---
layout: post
categories: Lisp
title: call/cc是怎样实现的
description: first-class continuation是一个迷人的特性，对比goto语句而言，它更安全，但是它到底是怎么实现的呢？
---

想象力比知识更重要。

——爱因斯坦

<br/>

**在大脑中第一个构想出来的人是天才，**

**后人只是一遍又一遍实现它最初的设想罢了。**

<br/>

call/cc如此，

lisp语言本身又何尝不是？

<br/>

一堆括号字母和空格，

构筑了美妙的外观。

<br/>

才有了后来者各种各样的实现。

<br/>

**这是一种自顶向下的设计思路，**

用构想作为目的，

用实现来支撑。

<br/>

与测试驱动开发，

有异曲同工之妙。

<br/>

## **放飞自己的想象力**

**假如**，我们有了一堆符号，

如何手动控制程序跳转？

<br/>

这个跳转方式，既然可以手动触发，

那一定是可以调用的。(k)

<br/>

k是哪来的？

一定是从什么地方创建的。

<br/>

跳转到哪里？

一定是跳转到创建它的位置之后。

<br/>

这个k是怎么过来的？

它一定当做参数传递过来的。

<br/>

k需要传递参数过去吗？

最好是需要，我们不想纯粹依赖副作用编程。(k 1)

<br/>

**假如**我们已经有k了，

并且在一个函数执行过程中调用了它，

(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;0

&nbsp;&nbsp;&nbsp;&nbsp;(k 2)

&nbsp;&nbsp;&nbsp;&nbsp;3)

执行完(k 2)后面的3还执行吗？

不执行了。

当前执行的这个函数还返回吗？

我们希望直接跳转到k定义的位置了。

<br/>

那么函数的执行过程就要重新理解了，

先求值函数体，

然后跳转到给定的位置。

<br/>

函数执行的结果，

并不一定是“返回”到调用的位置了。

<br/>

这个跳转可否理解成调用了k呢？

在函数调用处定义k，

执行完以后，用函数体的值v，调用k，

(k v)。

<br/>

嗯嗯，

就这么干，先从函数体执行后，

可以控制跳转位置开始。

<br/>

## **把以后要做什么当做参数传过去**

我们先看看，

旧观念中的“函数返回”是怎么绑架我们思维的。

**为什么函数一定要“返回”？**

<br/>

实际上，从机器的角度来看，

并不存在自动的返回机制，

调用一个函数，会把调用前的代码位置，先存起来。

然后去执行函数体的中代码，这可能在代码段的其他位置，

执行完后，再把以前存起来的位置恢复，

就完成了“返回”操作。

<br/>

现在我们不想这么干了，

我们不想让底层实现自动决定如何返回。

<br/>

例如：

(define (fn x)

&nbsp;&nbsp;&nbsp;&nbsp;(+ x 1))

&nbsp;&nbsp;&nbsp;&nbsp;

(define (gn y)

&nbsp;&nbsp;&nbsp;&nbsp;(fn y))

&nbsp;&nbsp;&nbsp;&nbsp;

(gn 2)

<br/>

我们调用了gn，gn又调用了fn，

fn执行完以后，返回gn，然后gn又返回，

像fn这样返回以后，调用者也返回的调用，称为**尾调用**。

<br/>

尾调用fn，本来没有必要返回gn内部，

直接返回gn该返回的位置就行了。

<br/>

**这就要求我们把函数执行完以后，**

**把“要做什么”当做参数传过去。**

<br/>

(define (final-cont v)

&nbsp;&nbsp;&nbsp;&nbsp;(display v))

<br/>

(define (fn x cont)

&nbsp;&nbsp;&nbsp;&nbsp;(cont (+ x 1)))

<br/>

(define (gn y cont)

&nbsp;&nbsp;&nbsp;&nbsp;(fn y (lambda (v)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(cont v))))

<br/>

我们在REPL中，一个表达式求值以后，就是打印它。

所以，我们创建一个最终要做的事情，final-cont

<br/>

先调用fn试试。

(fn 2 final-cont)

果然打印出了3。

<br/>

因为我们把打印这件事当做函数传过去了，

随时都可以调用。

<br/>

至于(cont (+ x 1))执行完后，fn不是还要返回的吗？

我们暂时可以认为是无用的数据，丢弃了，

后面再深入讨论。

<br/>

然后再调用(gn 2)试试。

(gn 2 final-cont)

就会去调用

(fn y (lambda (v)

&nbsp;&nbsp;&nbsp;&nbsp;(cont v)))

这里cont是final-cont

<br/>

然后调用fn了，(cont (+ x 1))

fn中的cont就是

(lambda (v)

&nbsp;&nbsp;&nbsp;&nbsp;(final-cont v))

<br/>

结果也是打印了3，正确输出。

<br/>

这是一个常函数，为什么要执行呢，

这是模拟fn执行完以后返回gn。

实际上，因为fn是尾调用，

我们只需要把gn中的cont传递给fn即可。

<br/>

(define (gn y cont)

&nbsp;&nbsp;&nbsp;&nbsp;(fn y cont))

<br/>

gn中的cont本来是final-cont，

是gn执行完以后要做的事情，

现在不加改变的，传递给了fn，

是不是相当于fn直接返回到gn该返回的位置了呢？

非常巧妙。

<br/>

其中，作为参数传递的cont，称为**Continuation**，

这种把“要做什么”当做参数传递的手法，称为**Continuation传递风格（CPS）**。

<br/>

## **用call/cc设置跳转点**

我们实际上不想每次都传递continuation，

只想在需要的时候调用它，

怎样产生我们需要的跳转点呢？

<br/>

call/cc就是做这个的。

<br/>

;before

(call/cc (lambda(k)

&nbsp;&nbsp;&nbsp;&nbsp;(define (fn x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k (+ x 1)))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;(define (gn y)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fn y))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;(gn 2))

;after

<br/>

用call/cc产生了一个跳转点，

它把call/cc位置处“以后要做什么”，包装成了参数k。

<br/>

对的，k虽然是函数的参数，

但是它也可以是一个函数。

<br/>

**其实k不是函数，是一个包装了continuation的对象，**

**它的调用机制，就是把包装的continuation提取出来调用一下。**

<br/>

反正k可以当做函数的参数传递，

像这样可以当做参数传递，可以作为函数的返回值的，k

称为first-class的，**first-class continuation**。

<br/>

我们看下执行流程，

先调用call/cc，设置了跳转点。

<br/>

然后，就进入(lambda (k) ...)中了，

其中k是call/cc处的continuation，

可以表示为

k = (lambda (v)

&nbsp;&nbsp;&nbsp;&nbsp;;after

)

<br/>

拿着call/cc的值，再执行after操作，

不就是“要做什么”的意思吗？

<br/>

进入(lambda (k) ...)以后，

定义了两个函数，fn和gn，

然后调用gn。

<br/>

gn调用了fn，fn又调用了k，

那么call/cc就直接返回了，程序跑到了k所示的跳转点了，

接着执行after操作。

<br/>

## **一种实现方式**

有了用例，

实现起来就简单多了。

<br/>

call/cc有很多方式实现，

我们只看下简单的解释实现。

<br/>

首先解释器的入口eval-exp要改，

(eval-exp '1 \*env\* \*cont\*)

需要传递一个最原始的“以后要做什么”，

(define \*cont\* (lambda (v)

&nbsp;&nbsp;&nbsp;&nbsp;(display v)))

<br/>

然后，遇到(call/cc ...)，我们这样处理，

<br/>

(define (eval-call/cc exp env cont)

&nbsp;&nbsp;&nbsp;&nbsp;(display "eval-call/cc\n")

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(let ((fn (cadr exp))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(continuation (make-continuation cont)))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(eval-function-call-list `(,fn ,continuation) env cont)))

<br/>

先拿到call/cc后面的那个lamabda，

然后用一个包装过的对象调用它，

**k就是这个包装过的对象continuation了**。

<br/>

我们再看看continuation对象调用的时候怎么处理，

<br/>

(define (eval-continuation-call exp env cont)

&nbsp;&nbsp;&nbsp;&nbsp;(display "eval-continuation-call\n")

&nbsp;&nbsp;&nbsp;&nbsp;(eval-exp (car exp) env

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (continuation)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(let ((wrapped-cont (continuation-cont continuation)))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(eval-exp (cadr exp) env

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (arg)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(wrapped-cont arg)))))))

<br/>

嵌套很深嘛，

没关系，其实只是解开continuation对象的封装，

把原始的cont拿出来，

然后先求值(k (+ x 1))中的(+ x 1)，

求值完了以后，

再调用包装中的cont。

<br/>

**这里比较新颖的地方是，因为整个解释器已经改成了CPS方式**，

**所以，顺序结构都要改成回调方式，**

(let ((arg (eval-exp (cadr exp) env)))

&nbsp;&nbsp;&nbsp;&nbsp;(wrapped-cont arg))

<br/>

要变成，

(eval-exp (cadr exp) env

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (arg)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(wrapped-cont arg)))

<br/>

然后呢，

还需要做什么呢？

没有了。

就完了。

<br/>

## **偷偷借用的Scheme尾调用优化机制**

**我们前面埋了一个雷。**

<br/>

重新来看看，

(define (final-cont v)

&nbsp;&nbsp;&nbsp;&nbsp;(display v))

<br/>

(define (fn x cont)

&nbsp;&nbsp;&nbsp;&nbsp;(cont (+ x 1)))

<br/>

(define (gn y cont)

&nbsp;&nbsp;&nbsp;&nbsp;(fn y cont))

<br/>

(gn 2)

<br/>

感觉上的执行过程是这样的，

gn调用了fn，fn调用cont，

cont返回，fn返回，gn返回，

回到了top-level。

<br/>

并非我们想的，

gn调用fn，fn调用cont，

cont直接返回到top-level。

<br/>

其实，在Scheme语言中，后者是对的。

确实直接返回到了top-level。

<br/>

因为语言规范指定，

**Scheme必须实现尾调用优化，**

指的就是这个。

<br/>

如果是尾调用，那么不用返回到调用处了，

只需要返回到调用者该返回的地方即可。

<br/>

这样我们解释器里面实现的call/cc，

更理直气壮了。

哪怕我们把\*cont\*传的再远，

也会直接返回到top-level，

不会导致一系列的调用栈弹栈操作。

<br/>

因为解释器实现中所有的函数调用都是尾调用。

<br/>

## **结语**

实际上，call/cc的编译实现还是比较麻烦的，

<br/>

本来调用结构是**栈型**的，

函数调用时，新建一个frame，添加到环境顶端，

返回时，弹栈。

<br/>

后来，为了实现闭包，

因为闭包具有无限生存期，

这个frame有可能以后还会用到，

所以，我们必须用**链表**来表示环境了，

函数返回后，并不会删除frame，只是暂时不链接到它了，

等待垃圾回收器来处理。

<br/>

再以后，

我们的执行过程，可以往前跳转了，

跳转到设置好的点，再分叉执行，

结果，环境就是一个**树型**结构了。

<br/>

每调用一个函数，

树增加了一个子节点，

函数返回，或者调用k，返回到以前的某个父节点，

因为还可能再回来，也可能重新执行一遍，

所以，再回来和重新执行必须同时保存下来，

成了两个分支。

<br/>

然而，这种**树型调用图**，

比**goto语句**更容易控制，

这也是call/cc的巧妙之处。

<br/>

当然call/cc用的时候，最好也封装一下，

免得k传递的到处都是。

不是吗，工具早就有了，

用的好不好，体现了工程师的水平。

<br/>

参考：

[源码](https://github.com/thzt/scheme-interpreter/tree/master/continuation)

[Essentials of Programming Languages](https://github.com/ultimate-lambda/book-reviews/wiki/Essentials-of-Programming-Languages)

[Concepts in Programming Languages](https://github.com/ultimate-lambda/book-reviews/wiki/Concepts-in-Programming-Languages)

[Lisp in small pieces](http://book.douban.com/subject/1456904/)

[Compiling with Continuations](http://book.douban.com/subject/1762126/)
