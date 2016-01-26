---
layout: post
categories: Lisp
title: CPS的地位
description: 我已经忘记为什么认为CPS是一种用来把递归函数转换成尾递归的方法了，可惜这只是CPS冰山之一角。
---

Scheme语言中，可以使用call/cc，

得到当前表达式的Continuation。

<br/>

例如：

(+ 1 (call/cc (lambda (k)

&nbsp;&nbsp;&nbsp;&nbsp;(* 2 (k 3)))))

=> 4

<br/>

CPS，是Continuation Passing Style的缩写，

它是一种编码风格，

函数执行完以后，并不通过返回值，

而是调用它自己的Continuation来完成计算。

<br/>

例如，

函数通过返回值的方式，通常这样写，

(define (add x y)

&nbsp;&nbsp;&nbsp;&nbsp;(+ x y))

<br/>

(display (add 1 2))

=> 3

<br/>

改写成CPS方式，

(define (add x y **k**)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(k** (+ x y)**)**)

<br/>

(add 1 2 **display**)

=> 3

<br/>

其中display这个单参函数是表达式add的Continuation，

在调用add函数的时候，我们把它的Continuation当做附加参数传递过去了，

等add函数体的计算(+ x y)执行完后，

并没有通过返回值，而是直接调用了它的Continuation——k。

<br/>

## **CPS的误区**

CPS是一种代码风格，

它可以用来做任何适当的事情，

并不是为了call/cc而出现的。

<br/>

CPS可以**显式的指明程序的计算过程**，

在函数式语言的编译器中，常用来作为代码的**中间表示**（IR）。

<br/>

例如：Standard ML of New Jersey编译器，大致有以下几个处理过程。

（1）词法分析，语法分析，类型检查，建立抽象语法树

（2）转换成lambda演算的某种表示

（3）对用lambda演算表示的程序进行优化

（4）转换成CPS的某种表示

（5）优化CPS

（6）闭包变换（closure conversion），消除函数的自由变量

（7）去除嵌套作用域，得到一系列无嵌套的相互递归函数

（8）处理寄存器溢出，每个函数至多含有n个变量，n是目标机器寄存器数

（9）生成目标机器指令

<br/>

CPS与尾递归，也没有必然的联系。

虽然很多地方都喜欢通过把函数转换成尾递归化来介绍CPS。

<br/>

## **转换成CPS**

有关CPS的研究很多，

将任意函数转换成CPS，

已经不是困难的事情了。

<br/>

我们不做形式化的探讨，

只手动处理，借几个例子来体会一下。

<br/>

**例一：**含有自由变量的函数

(define y 1)

<br/>

(define (f x)

&nbsp;&nbsp;&nbsp;&nbsp;(+ x **y**))

<br/>

(display (f 2))

=> 3

<br/>

我们看到f的函数体中(+ x y)，y是自由的，

它没有在函数体中定义。

<br/>

根据词法作用域规则，

y，应该去**定义f的环境中**查找。

得到y => 1。

<br/>

我们通过**为f增加附加参数k**的方法，
将具有返回值的函数转换成CPS。

(define y 1)

<br/>

(define (f x **k**)

&nbsp;&nbsp;&nbsp;&nbsp;**(k** (+ x y)**)**)

<br/>

(f 2 display)

=> 3

<br/>

**例二：**阶乘函数

<br/>

(define (fact n)

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;n

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(* (fact (- n 1)) n)))

<br/>

(display (fact 5))

=> 120

<br/>

fact函数是递归的，为了转换成CPS，在增加附加参数k时候，

所有fact调用的地方都要修改，(define (fact n k)

难以修改的地方，可以思考一下实际的计算顺序，作为方向。

<br/>

(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;n

这里，返回n后要执行k所示的剩余计算，

相当于(k n)。

<br/>

(* (fact (- n 1)) n)

考虑运算顺序，先执行(fact (- n 1))，

增加附加参数以后，变成了(fact (- n 1) ?)，

其中“?”表示该表达式后续的剩余计算，是什么呢，

考虑计算顺序，如果计算完成，则要先乘以n，在执行k所示的剩余计算。

<br/>

所以，

? =>

(lambda (v)

&nbsp;&nbsp;&nbsp;&nbsp;(k (* v n)))

<br/>

整理得，

(define (fact n **k**)

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(k** n**)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fact (- n 1) 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(lambda (v)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(k (* v n)))**)))

<br/>

(fact 5 display)

=> 120

<br/>

**例三：**斐波那契函数

<br/>

(define (fib n)

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(+ (fib (- n 1)) (fib (- n 2)))))

<br/>

(display (fib 5))

=> 5

<br/>

先为函数增加附加参数k，

(define (fib n k)

<br/>

再改变非递归情况的返回值方式，

(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;1

<br/>

(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;(k 1)

<br/>

然后考虑(+ (fib (- n 1)) (fib (- n 2)))的运算顺序，

是从计算(fib (- n 1))开始的，

增加附加参数后，改写为，(fib (- n 1) ?)

<br/>

“?”是什么呢，它表示(fib (- n 1))表达式后续的计算：

先计算(fib (- n 2))，再把结果值相加，最后执行k所示的计算。

<br/>

? =>

(lambda (v1)

&nbsp;&nbsp;&nbsp;&nbsp;**(fib (- n 2) ??)**)

=>

(lambda (v1)

&nbsp;&nbsp;&nbsp;&nbsp;(fib (- n 2) 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(lambda (v2)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(k (+ v1 v2)))**))

<br/>

整理得，

(define (fib n k)

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(k** 1**)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fib (- n 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(lambda (v1)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fib (- n 2) 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(lambda (v2)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(k (+ v1 v2)))))**)))

<br/>

(fib 5 display)

=> 5

<br/>

总结一下，

**函数调用的返回值变成了，Continuation的参数**，

然后**在Continuation中处理后续的计算**。

<br/>

(fn x)

=> (fn x (lambda (v) ...))

其中v是原来(fn x)的返回值，

(fn x)的后续计算，写在“...”处。

<br/>

## **闭包变换**

限于目前大多数计算机的体系结构，

函数求值时，并不直接支持对词法变量的查找规则。

<br/>

我们可以继续改变CPS，

使得每个函数都不包含自由变量。

这种改变方式，称为**闭包变换**，

这种代码风格，称为Closure Passing Style。

<br/>

对于任意含有自由变量的函数，我们都可以进行闭包变换，

**并不一定是已经表示成CPS风格的程序**，

只是CPS变换与闭包变换有些相似之处。

<br/>

我们知道，

词法作用域规则，是当我们查找函数中自由变量的时候，

到定义该函数的环境中去查找。

<br/>

为了在运行时得到函数定义时的环境，

我们需要再增加一个或几个参数，来表示函数定义时的环境。

<br/>

为了连贯性，我们直接使用上文CPS转换后的结果，作为下一步闭包变换的起点。

<br/>

**例一：**含有自由变量的函数

(define y 1)

<br/>

(define (f x k)

&nbsp;&nbsp;&nbsp;&nbsp;(k (+ x y)))

<br/>

(f 2 display)

=> 3

<br/>

我们再增加一个参数e，来表示函数定义时的环境。

(define y 1)

(define (f x k **e**)

&nbsp;&nbsp;&nbsp;&nbsp;**(define y (vector-ref e 0))**

&nbsp;&nbsp;&nbsp;&nbsp;(k (+ x y)))

<br/>

**(define e0 (vector y f))**

(f 2 display **e0**)

=> 3

<br/>

注：

(vector y f))，用vector表示环境，y是第0个元素，f是第1个元素。

(vector-ref e 0))，取环境中第0个元素，y

<br/>

**例二：**阶乘函数

(define (fact n k)

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k n)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fact (- n 1) 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (v)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k (* v n))))))

<br/>

(fact 5 display)

=> 120

<br/>

我们先将Continuation中的lambda表达式写成定义式，便于理解，

(define (fact n k)

&nbsp;&nbsp;&nbsp;&nbsp;**(define (k1 v)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(k (* v n)))**

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k n)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fact (- n 1) **k1**)))

<br/>

我们看到，在Continuation函数中也包含了自由变量，

所以，情况比较复杂，

我们需要再传递一个参数，来表示Continuation定义时的环境，

用来区分fact函数定义时的环境。

<br/>

(define (k0 v **e**)

&nbsp;&nbsp;&nbsp;&nbsp;(display v))

<br/>

(define (fact n k **fact-e k-e**)

&nbsp;&nbsp;&nbsp;&nbsp;(define (k1 v **e**)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define n (vector-ref e 0))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define k (vector-ref e 1))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define k-e (vector-ref e 3))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k (* v n) **k-e**))

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;**(define fact (vector-ref fact-e 1))**

&nbsp;&nbsp;&nbsp;&nbsp;**(define k1-e (vector n k fact-e k-e k1 fact))**

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k n **k-e**)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fact (- n 1) k1 **fact-e k1-e**)))

<br/>

**(define fact-e0 (vector k0 fact))**

**(define k-e0 fact-e0)**

<br/>

(fact 5 k0 **fact-e0 k-e0**)

=> 120

<br/>

关键点在于，从定义时的环境中得到词法变量。

<br/>

**例三：**斐波那契函数

(define (fib n k)

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fib (- n 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (v1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fib (- n 2) 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (v2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k (+ v1 v2))))))))

<br/>

(fib 5 display)

=> 5

<br/>

先将Continuation中的lambda表达式写成定义式

(define (fib n k)

&nbsp;&nbsp;&nbsp;&nbsp;**(define (k1 v1)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define (k2 v2)**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(k (+ v1 v2)))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(fib (- n 2) k2))**

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k 1)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fib (- n 1) **k1**)))

<br/>

将函数定义时的环境，当做附加参数传递，

<br/>

(define (k0 v **fib-e k-e**)

&nbsp;&nbsp;&nbsp;&nbsp;(display v))

<br/>

(define (fib n k **fib-e k-e**)

&nbsp;&nbsp;&nbsp;&nbsp;(define (k1 v1 **fib-e k-e**)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(define (k2 v2 **fib-e k-e**)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define k (vector-ref (vector-ref k-e 2) 1))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define v1 (vector-ref k-e 0))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define k-k-e (vector-ref (vector-ref k-e 2) 3))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k (+ v1 v2) **fib-e k-k-e**))

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define fib (vector-ref fib-e 1))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define n (vector-ref k-e 0))**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(define k2-e (vector v1 fib-e k-e k2 fib n))**

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fib (- n 2) k2 **fib-e k2-e**))

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;**(define fib (vector-ref fib-e 1))**

&nbsp;&nbsp;&nbsp;&nbsp;**(define k1-e (vector n k fib-e k-e k1 fib))**

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;(if (<= n 2)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(k 1 **fib-e k-e**)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(fib (- n 1) k1 **fib-e k1-e**)))

<br/>

**(define fib-e0 (vector k0 fib))**

**(define k-e0 fib-e0)**

<br/>

(fib 5 k0 **fib-e0 k-e0**)

=> 5

<br/>

至此，三个例子中，所有的函数都没有自由变量了。

<br/>

## **结语**

本文只是手动处理了CPS变换和闭包变换。

CPS的用处还不止于此。

然而，我认为，**重要的是这种附加参数的思想**。

<br/>

学会了思想，我们才能灵活运用它。

发现更神奇的计算本质。

<br/>

本文参考：《Compiling with Continuations》




