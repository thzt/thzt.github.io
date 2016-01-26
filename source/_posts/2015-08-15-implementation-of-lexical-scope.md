---
layout: post
categories: Lisp
title: 词法作用域是怎样实现的
description: 随着行业的发展，高级的概念，可能就慢慢变得基本了，还好，我们仍然爱好学习。
---

自从Scheme引入词法作用域以来，

越来越多的编程语言，加入了这个特性。

<br/>

函数调用时，

所引用的自由变量，

要去**函数定义时的环境**中去查找，

而不是在**执行环境**中查找。

<br/>

这对寻找自由变量到底在哪定义，

是很方便的。

<br/>

可是，

词法作用域到底有什么好的，

又是怎样实现的呢？

<br/>

这还要从环境说起。

<br/>

## **环境的表示**

调用一个函数，它的形参有可能会**遮挡**外层的变量，

等函数执行完后，这个被遮挡的值还要恢复，

所以我们必须把要被遮挡的值存起来。

<br/>

我们可以用一个列表表示环境。

<br/>

(

&nbsp;&nbsp;&nbsp;&nbsp;[(x 3)]

&nbsp;&nbsp;&nbsp;&nbsp;[(x 1) (y 2)]

)

<br/>

它表示一个环境，每个元素称为一个**frame**，

例如，这个环境包含了两个frame，

frame[(x 3)]中的x，遮挡了frame[(x 1) (y 2)]中的x。

<br/>

**frame是用来表示形参和实参的绑定关系的**，

**整个环境用来表示调用栈**。

<br/>

当调用函数时，会创建一个新的frame，

例如，[(x 4) (y 5) (z 6)]

并把frame放到环境顶端。

<br/>

(

&nbsp;&nbsp;&nbsp;&nbsp;[(x 4) (y 5) (z 6)]

&nbsp;&nbsp;&nbsp;&nbsp;[(x 3)]

&nbsp;&nbsp;&nbsp;&nbsp;[(x 1) (y 2)]

)

<br/>

函数执行完后，环境要恢复成以前的样子，

<br/>

(

&nbsp;&nbsp;&nbsp;&nbsp;[(x 3)]

&nbsp;&nbsp;&nbsp;&nbsp;[(x 1) (y 2)]

)

<br/>

## **动态作用域：top-level变量**

Emacs Lisp 24.1之前，还是动态作用域的，

之后可以选择性的使用，只需要设置buffer-local变量，

lexical-binding不为nil即可。

<br/>

我们来看看动态作用域到底带来了哪些麻烦。

<br/>

环境：()

<br/>

(define a 1)

<br/>

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1)]

)

<br/>

(define (fn x)

&nbsp;&nbsp;&nbsp;&nbsp;(+ x a))

<br/>

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;)]

)

<br/>

注意，

**定义并不增加frame，而是改变当前frame。**

<br/>

(fn 2)

=> (+ x a)

<br/>

**调用函数了，在环境中增加一个frame，**

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(x 2)] 

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;)]

)

<br/>

在环境中，找到了x的值是2，a的值是1，

因此，(+ x a) => 3

<br/>

函数返回后，

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;)]

)

<br/>

## **动态作用域：被遮挡的top-level变量**

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;)]

)

<br/>

(define (gn a)

&nbsp;&nbsp;&nbsp;&nbsp;(fn 2))

<br/>

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

(gn 3)

=> (fn 2)

<br/>

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(a 3)]

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

(gn 3)

=> (fn 2)

=> (+ x a)

<br/>

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(x 2)]

&nbsp;&nbsp;&nbsp;&nbsp;[(a 3)]

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

在环境中，找到了x的值是2，a的值是3，

因此，(+ x a) => 5

<br/>

(fn 2)调用结束后，

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(a 3)]

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

(gn 3)调用结束后，

环境：(

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

## **动态作用域：意料之外**

在使用动态作用域时，我们发现，

(fn 2)的值，是根据它被使用的位置决定的，

**函数中的自由变量a，在不同的执行环境中，可能有不同的值。**

<br/>

这样，我们就总要注意，

不要**意外遮挡**了(fn 2)中的a。

<br/>

但是，fn如果是别人写的呢？

我们并不知道fn中的有一个名字为a的变量不能被遮挡。

<br/>

这也对fn的实现者提出了挑战，

不能写带有自由变量的函数，否则这个自由变量的值，

随着函数调用位置的不同是不同的。

<br/>

此外，**跟踪一个自由变量的值**，

也是很麻烦，我们必须把调用栈写出来，

或者在脑子里记住调用顺序。

<br/>

这对较大规模的工程来说，是非常耗时的。

<br/>

## **词法作用域：被遮挡的top-level变量**

词法作用域的环境变化规则是不同的，

假如函数调用前的环境是#<env>，
	
**（1）函数调用时，frame要添加到该函数定义时的环境#<env-fn>中去，**
	
**（2）函数返回后，整个环境要恢复成调用函数之前的环境#<env>。**

<br/>

第（1）点是为了保证自由变量在函数定义时的环境中查找。

第（2）点是因为#<env>和#<env-fn>通常是不同的。

<br/>

环境#<env-fn>：()

<br/>

(define a 1)

<br/>

环境#<env-fn>：(
	
&nbsp;&nbsp;&nbsp;&nbsp;[(a 1)]

)

<br/>

(define (fn x)

&nbsp;&nbsp;&nbsp;&nbsp;(+ x a))

<br/>

环境#<env-fn>：(
	
&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;)]

)

<br/>

(define (gn a)

&nbsp;&nbsp;&nbsp;&nbsp;(fn 2))

<br/>

环境#<env-fn>：(
	
&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

(gn 3)

=> (fn 2)

<br/>

环境#<env>：(
	
&nbsp;&nbsp;&nbsp;&nbsp;[(a 3)]

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

(gn 3)

=> (fn 2)

=> (+ x a)

<br/>

注意，这里fn定义时的环境是，

环境#<env-fn>：(
	
&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

**并且frame是最后被修改的状态**。

<br/>

所以调用fn后，

环境#<env-fn-extended>：(
	
&nbsp;&nbsp;&nbsp;&nbsp;[(x 2)]

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

因此，(+ x a) => 3

<br/>

(fn 2)调用结束后，**要恢复成调用之前环境，**

环境#<env>：(
	
&nbsp;&nbsp;&nbsp;&nbsp;[(a 3)]

&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

(gn 3)调用结束后，

环境#<env-fn>：(
	
&nbsp;&nbsp;&nbsp;&nbsp;[(a 1) (fn \#&lt;procedure&gt;) (gn \#&lt;procedure&gt;)]

)

<br/>

## **词法作用域：实现方式**

怎样实现词法作用域呢？

这里的关键在于拿到函数定义时的环境。

<br/>

**最好把函数的形参列表，函数体，和定义时的环境打包成一个对象，**

(define-record-type closure 

&nbsp;&nbsp;&nbsp;&nbsp;(fields param body env))

<br/>

这个打包好的对象，就称为“**闭包**”。

<br/>

(define (eval-lambda exp env)

&nbsp;&nbsp;&nbsp;&nbsp;(display "eval-lambda\n")

&nbsp;&nbsp;&nbsp;&nbsp;(let ((param (caadr exp))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(body (caddr exp)))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(make-closure param body env)))

<br/>

当调用一个函数时，

我们先将定义时的环境提取出来，

然后用形参与实参的绑定关系构成一个frame来扩展它，

最后让函数体在这个扩展后的环境中执行即可。

<br/>

(define (eval-function-call-list exp env)

&nbsp;&nbsp;&nbsp;&nbsp;(display "eval-function-call-list\n")

&nbsp;&nbsp;&nbsp;&nbsp;(let* ((closure (eval-exp (car exp) env))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(arg (eval-exp (cadr exp) env))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(body (closure-body closure))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(**lexical-env** (closure-env closure))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(param (closure-param closure))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(frame (create-frame)))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(extend-frame frame param arg)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(let ((executing-env (extend-env **lexical-env** frame)))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(eval-exp body executing-env))))

<br/>

## **结语**

要理解一个概念，

只从使用者的角度来看，

是不够的。

<br/>

**我们还要学会切换角色，**

从实现者的角度来看。

<br/>

当然，实现的方式并不是唯一的，

考虑不同的实现，也能增广我们的见闻。

<br/>

参考：

[源码](https://github.com/thzt/scheme-interpreter/tree/master/lexical-scope)

[The Structure And Interpretation of Computer Programs](https://github.com/ultimate-lambda/book-reviews/wiki/The-Structure-And-Interpretation-of-Computer-Programs)

[Essentials of Programming Languages](https://github.com/ultimate-lambda/book-reviews/wiki/Essentials-of-Programming-Languages)

[Concepts in Programming Languages](https://github.com/ultimate-lambda/book-reviews/wiki/Concepts-in-Programming-Languages)
