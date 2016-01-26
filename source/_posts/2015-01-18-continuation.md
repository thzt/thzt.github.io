---
layout: post
categories: Lisp
title: Continuation入门指南
description: 介绍了scheme语言中的first-class continuation，以及call/cc。
---

我在学写scheme语言的过程中，发现有3个难点，

lexical scope，continuation和macro。

这些概念其实是和编程语言无关的，

认识它们对于我们理解计算的本质有很大的帮助。

<br/>

continuation实现了类似goto语句的功能，

只是goto可以跳转到任何的代码标签处开始执行，

而continuation只能跳转到执行过的工作状态。

好的编程实践指导我们尽量不要使用goto，因为它会影响求值环境，

然而，continuation不会，它更函数式，没有副作用（side effect）。

<br/>

本文只是简单的介绍一下continuation的概念，

以及它在scheme语言中的应用（call/cc），

本文并不追求严谨，只能算是抛砖引玉吧。

<br/>

《The Scheme Programming Language 4th》第3.3节介绍了continuation的概念，

为了说明call/cc，作者举了一个小例子。

(let ([x (call/cc (lambda (k) k))])

&nbsp;&nbsp;(x (lambda (ignore) "hi")))
  
=> "hi"

如果你像我一样，感到很困惑，那么希望本文对你有所帮助。

<br/>

### **continuation的概念**

scheme是一门简洁的语言，

scheme程序不包含语句，它只由表达式构成，

程序的执行，就是表达式的求值过程。

<br/>

另外，除了关心表达式的值是怎样得出来的，

我们还会关心这个值是怎样被使用的。

<br/>

我们可以把整个程序看做一个链条，而每个表达式看做各个环节，

执行程序相当于沿着链条从头走到尾。

如果把注意力集中到当前求值的表达式上。

我们会发现，前面的环节中，所有表达式都已经求值了。

后面的链条，正期待当前表达式的值，继续执行。

<br/>

这样的话，我们就可以把“期待当前表达式的值，继续执行”，看做一个单参函数了。

这个单参函数就相当于continuation。

于是，我们在每个局部都拥有了全局观点，

整个程序相当于，求值当前表达式，然后再调用这个表达式的continuation。

<br/>

continuation相当于一个单参函数，并且在scheme中它是first class的，

因此，和其他函数一样，它不仅可以被调用，还可以作为其他函数的参数。

和函数不同的是，调用它不会返回一个值，

而会跳转到这个表达式刚刚求值完的工作状态，

以调用值作为表达式的值，程序继续向下执行。

<br/>

不同表达式的continuation是不同的，代表了不同的工作状态。

甚至，同一个表达式在程序执行过程的不同时期，continuation也是不同的。

<br/>

### **call/cc**

scheme语言内置提供了call/cc函数，用来获得当前的continuation。

例如：

(+ 0 (call/cc

&nbsp;&nbsp;(lambda (k)

&nbsp;&nbsp;&nbsp;&nbsp;(k 1))))

=> 1

call/cc获得的是如下expression的continuation。

(+ 0 expression)

<br/>

由于expression的后续程序，期望使用expression的值进行加0操作。

所以，这个continuation就相当于

(lambda (x)

&nbsp;&nbsp;(+ 0 x))

<br/>

下面我们对call/cc的求值规则进行详细说明，

（1）call/cc接受一个单参函数fn作为参数

（2）求值call/cc表达式，会使用当前的continuation调用fn，即(fn continuation)。因此，fn的形参k就是call/cc表达式的continuation

（3）另外，我们规定，fn的continuation就是call/cc的continuation

<br/>

再看一下上面的例子。

(+ 0 (call/cc

&nbsp;&nbsp;(lambda (k)

&nbsp;&nbsp;&nbsp;&nbsp;(k 1))))

call/cc的参数fn，是一个函数(lambda (k) (k 1))

为了求值(+ 0 (call/cc fn))我们会先求值(call/cc fn)，

根据call/cc的求值规则（2），fn的形参k，就是call/cc的continuation。

而根据函数的求值规则，(fn continuation)相当于(k 1)，即，使用1作为参数调用call/cc的continuation，

根据continuation的定义，程序会跳转到call/cc的工作状态，使用1作为call/cc表达式的值，继续向下执行。

即，(+ 0 1) => 1

<br/>

第二种情况，如果continuation没有被调用呢？

(+ 0 (call/cc

&nbsp;&nbsp;(lambda (k)
  
&nbsp;&nbsp;&nbsp;&nbsp;2)))
	
根据call/cc的求值规则（2），(fn continuation) => 2

根据call/cc的求值规则（3），fn的continuation就是call/cc的continuation，

再根据continuation的定义，程序会跳转到call/cc的工作状态，使用2作为call/cc表达式的值，继续向下执行。

即，(+ 0 2) => 2

<br/>

最后，我们来分析本文开头那个例子，

(let ([x (call/cc (lambda (k) k))])

&nbsp;&nbsp;(x (lambda (ignore) "hi")))
  
=> "hi"

call/cc的continuation是什么呢？

(lambda (z)

&nbsp;&nbsp;(let [(x z)]
  
&nbsp;&nbsp;&nbsp;&nbsp;(x (lambda (ignore) "hi"))))
	
即，首先将call/cc表达式的值z绑定到x，然后使用(lambda (ignore) "hi")作为参数调用x。

请注意区分，continuation中的let表达式和原来要求值的let表达式是不同的。

<br/>

整个程序是这样执行的，

为了求值let表达式，我们要先将call/cc表达式的值绑定到x，然后用(lambda (ignore) "hi")作为参数调用x，

这使得我们必须先求值call/cc。

根据上面分析的call/cc求值过程，k就是continuation，而(fn continuation)就是((lambda (k) k) continuation) <=> k，

k是fn的返回值，并没有被调用，属于上面的第二种情况，

程序会跳转到call/cc的工作状态，使用k作为call/cc表达式的值，继续向下执行。

于是，程序变成了，

(let ([x k])

&nbsp;&nbsp;(x (lambda (ignore) "hi")))

然后，x会绑定为k，用(lambda (ignore) "hi")作为参数调用x。

<br/>

可是，x是continuation，用(lambda (ignore) "hi")作为参数调用x，

类似上面的第一种情况，根据continuation的定义，程序会跳转到call/cc的工作状态，使用(lambda (ignore) "hi")作为call/cc表达式的值，继续向下执行。

(let ([x (lambda (ignore) "hi")])

&nbsp;&nbsp;(x (lambda (ignore) "hi")))

=> "hi"

所以，整个let表达式的值就是"hi"了。

<br/>

### **总结**

以上的例子比较简单，执行过程中只求值了一次call/cc，即continuation就一个。

如果执行过程中，多次调用了call/cc，会产生多个continuation，这时continuation的跳转就需要进行区分了。

例如，下面著名的“阴阳谜题”，你可以试试看，

(let* [(yin ((lambda (foo) (newline) foo)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(call/cc (lambda (bar) bar))))

&nbsp;&nbsp;&nbsp;&nbsp;(yang ((lambda (foo) (write-char #/*) foo)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(call/cc (lambda (bar) bar))))]

&nbsp;&nbsp;(yin yang))

<br/>

起初，我以为continuation是scheme的语言特性，但其实很多语言都实现了它，只不过可能不是first class的，

因此，这是和语言无关的概念，值得我们深入学习。

例如，C语言的setjmp和longjmp就能实现类似的跳转功能。
