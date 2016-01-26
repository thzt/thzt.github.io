---
layout: post
categories: Lisp
title: 简介卫生宏
description:  Scheme语言是第一个实现卫生宏的Lisp方言，我们来瞧一瞧。
---

宏（macro）是Lisp语言进行元编程的手段，

它分为两种，读取宏（read macro）和编译宏（compilation macro）。

可以用来编写读取期和编译期运行的代码。

<br/>

宏，事实上进行了代码的转换。

每一个宏都与一个转换器（transformer）相关联。

代码转换后，会替换到原来的位置。

这个过程，称为宏展开（expansion）。

<br/>

区分读取期，编译期和运行期，是很重要的。

这是3个独立的阶段，

尤其在运行期，不存在任何未展开的宏。

<br/>

我们使用“a -> b”来表示，表达式a展开为b。

而使用“a => b”表示，表达式a的值为b。

<br/>

例子：

'a -> (quote a)，其中“'”是一个读取宏。

读取宏可以将任意表达式转换为S表达式。

<br/>

很多常见的表达式调用，其实是编译宏。

(and test1 test2) -> (if test1 test2 #f)

编译宏可以将一个S表达式转换为另一个S表达式。

<br/>

我们通常说的宏，指的是编译宏。

Scheme语言暂时不支持自定义的读取宏。

<br/>

## **定义一个宏**

Scheme语言中使用define-syntax来定义宏，

定义一个宏，有时也称为绑定一个关键字（keyword binding）。

宏展开的结果，最终都转换成了一些Scheme语言内置的表达式。

<br/>

例如：宏and是这样的定义的，

(define-syntax and

&nbsp;&nbsp;&nbsp;&nbsp;(syntax-rules ()

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(and) #t]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(and test) test]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(and test1 test2 ...)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(if test1

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(and test2 ...)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#f)]))

<br/>

其中，(syntax-rules ...)的值是一个转换器。

define-syntax将关键字and和这个转换器相关联。

<br/>

Scheme语言中有很多表达式可以返回转换器，

而syntax-rules是最常用的一个。

它内置了一个模板语言（pattern language）

<br/>

syntax-rules后面紧跟的一个括号，里面可以设置辅助关键字。

至于辅助关键字，可以参考Scheme语言手册了解使用情况。

<br/>

后续的每一个表达式，都具有“[模式，模板]”这样的结构。

对于[(and) #t]来说，

模式 = (and)

模板 = #t

<br/>

模式和模板中的“...”是syntax-rules模板语言的一部分。

具体用法，也可以参考Scheme语言手册。

<br/>

如果宏调用匹配了某个模式，就会按照相应模板展开。

下面3个宏调用，展开结果如下：

(and) -> #t

(and x) -> x

(and x y) -> (if x (and y) #f) -> (if x y #f)

<br/>

宏是可以递归展开的，一直到结果表达式中不再含有宏为止。

<br/>

模式中的第一个元素，因为肯定是宏的名字，

所以也可以替换成通配符“_”，

[(_) #t]

[(_ test) test]

<br/>

## **卫生宏**

Scheme是第一个支持卫生宏（hygienic macro）的Lisp方言。

也是第一个支持卫生宏的编程语言。

<br/>

“卫生”这个词表示，宏展开后，不会污染原来的词法作用域。

我们还是举例来说明吧，最后，我们再总结规律。

<br/>

例1：宏展开后，原表达式处于新的词法环境中。

(let-syntax [(insert-binding (syntax-rules ()

&nbsp;&nbsp;&nbsp;&nbsp;[(_ x) (let [(a 1)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(+ x a))]))]

&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(insert-binding (+ a 3))))

=> 6

<br/>

其中，let-syntax用来绑定局部关键字。

就像let可用来绑定局部变量一样。

<br/>

在let-syntax表达式内部，我们定义了宏insert-binding。

它绑定到syntax-rules求值后得到的转换器上。

<br/>

根据定义，我们知道(insert-binding x) -> (let \[(a 1)\] (+ x a))

原表达式x，处于含有新的绑定a => 1的词法环境中。

<br/>

如果原表达式x中含有a，就出现问题了。

我们的例子就是这种情况。

<br/>

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(insert-binding (+ a 3)))

->

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(let [(a 1)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(+ (+ a 3) a)))

=> 5

<br/>

结果出错了。

从(+ a 3)所在的原始词法环境来看，

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(insert-binding (+ a 3)))

(+ a 3)中a的值，应该是2才对。

宏展开污染了原始的词法环境。

<br/>

这是不“卫生”的。

Scheme通过给绑定的值改名字来实现卫生宏。

<br/>

宏展开(insert-binding x) -> (let \[(a 1)\] (+ x a))

改成了(insert-binding x) -> (let \[(:g0001 1)\] (+ x :g0001))

其中，:g0001是语言实现生成的唯一名字，不会与任何已有的名字冲突。

<br/>

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(insert-binding (+ a 3)))

->

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(let [(:g0001 1)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(+ (+ a 3) :g0001)))

=> 6

<br/>

这样就得到了正确的结果。

<br/>

例2：宏展开后，引入了不在原来词法作用域中的标识符。

(let [(a 1)]

&nbsp;&nbsp;&nbsp;&nbsp;(let-syntax [(insert-free (syntax-rules ()

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_ x) (+ x a)]))]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(insert-free (+ a 3)))))

=> 6

<br/>

同样根据定义，我们知道(insert-free x) -> (+ x a)

<br/>

所以，

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(insert-free (+ a 3)))

->

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(+ (+ a 3) a))

=> 7

<br/>

结果又出错了。

哪里出现问题了？

<br/>

宏定义的模式/模板[(_ x) (+ x a)]))]中的a，应该是第一行的绑定，(let [(a 1)]

而展开式(let \[(a 2)\] (+ (+ a 3) a))覆盖了外层对a的绑定。

<br/>

因此，宏展开式的行为，将取决于展开后的环境，

展开到不同的环境中，行为是不同的。

失去了宏调用的“引用透明性”。

<br/>

Scheme是怎么解决的呢？

语言规范指出，宏展开式中的自由标识符，处于宏定义时的词法作用域中。

即，宏展开式(+ x a)中，a具有宏定义环境中的值a => 1，(insert-free x) -> (+ x 1)

<br/>

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(insert-free (+ a 3)))

->

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(+ (+ a 3) 1))

=> 6

<br/>

## **规律总结**

我们遇到了一个问题。

我们知道，“+”在Scheme中表示加法函数，

它和a地位相同，也是一个变量，只不过它的值是一个函数。

<br/>

那么，以上两个例子中，变量+的值分别来自哪个词法作用域呢？

<br/>

例1中，

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(insert-binding (+ a 3)))

->

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(let [(:g0001 1)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(+ (+ a 3) :g0001)))

<br/>

经过分析，我们知道了，

(+ (+ a 3) :g0001)))中第一个+来自宏定义处的词法作用域，

第二个+来自宏替换处的词法作用域。

<br/>

例2中，

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(insert-free (+ a 3)))

->

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(+ (+ a 3) 1))

<br/>

同样经过分析，我们知道了，

(+ (+ a 3) 1))中第一个+来自宏定义处的词法作用域，

第二个+来自宏替换处的词法作用域。

<br/>

因此，

我们找到了一个规律，这也是卫生宏的目的所在。即，

**宏展开式中的所有标识符，仍处于其来源处的词法作用域中。**

<br/>

我们试着分析一下这两个例子。

例1展开式，

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;**(let [(:g0001 1)]**

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**(+** (+ a 3) **:g0001))**)

粗体来源于宏定义处，普通字体来源于宏替换处。

展开后，它们仍然处于各自来源处的词法作用域中。

<br/>

例2展开式，

(let [(a 2)]

&nbsp;&nbsp;&nbsp;&nbsp;**(+** (+ a 3) **1)**)

上述规则同样满足。

<br/>

宏展开式中的标识符，虽然来源不同，但互不污染。

这就达到卫生宏的目的了。

<br/>

## **结语**

在Lisp编程中，宏展开后造成了非预期的污染，是经常出现问题的地方。

Common Lisp目前并不支持卫生宏，

但是可以实现自己的宏定义，用自己的宏来定义宏，达到简洁可控的目的。

<br/>

当然，卫生宏也造成了表达能力的损失，

在特殊情况下，可以使用syntax-case以及datum->syntax来弥补。
