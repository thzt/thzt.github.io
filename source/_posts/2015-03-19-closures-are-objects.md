---
layout: post
categories: Lisp
title: 闭包就是对象
description: 闭包和对象是实现封装的两种形式，函数式简洁的与面向对象之间交相辉映。
---

## **作用域**

作用域，是自由变量的**查找**规则。

<br/>

如果变量具有词法作用域，

语言实现会相继到**更外层的词法范围**内查找绑定值。

<br/>

如果变量具有动态作用域，

语言实现会回溯到**更早的嵌套调用**中查找绑定值。

<br/>

## **词法作用域**

(define test-lexical-binding

&nbsp;&nbsp;&nbsp;&nbsp;(**let** [(x 1)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(lambda (y) (+ x y))))

<br/>

(test-lexical-binding 2) 

=> 3

<br/>

其中，**let**表达式返回了一个函数，作为test-lexical-binding的值。

根据函数调用规则，我们知道，

(test-lexical-binding 2) 

-> ((lambda (y) (+ x y)) 2) 

-> (+ x 2)

变量x是自由变量。

<br/>

如果x具有词法作用域，

则x的值，就是x所在函数，在定义时，外层作用域的值。

(lambda (y) (+ x y))的外层是let表达式，

(let [(x 1)]

&nbsp;&nbsp;&nbsp;&nbsp;...)

<br/>

因此，x => 1，(+ x 2) => 3

<br/>

## **动态作用域**

(define parameter-object

&nbsp;&nbsp;&nbsp;&nbsp;(**make-parameter** 1))

<br/>

(define (test-dynamic-binding)

&nbsp;&nbsp;&nbsp;&nbsp;(parameter-object))

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;

(**parameterize** [(parameter-object 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(test-dynamic-binding)) 

=> 2

<br/>

(test-dynamic-binding) 

=> 1

<br/>

其中，(make-parameter 1)返回一个包含值1的参数对象**#&lt;parameter object&gt;**。

参数对象是一个**无参函数**，调用后会得到它当前状态的包含值。

(parameter-object)的值取决于参数对象所处的动态作用域环境。

<br/>

我们可以使用parameterize来更改参数对象的包含值，

并且parameterize表达式内部会在新的动态作用域环境中求值。

<br/>

(**parameterize** [(parameter-object 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(test-dynamic-binding)) 

-> (test-dynamic-binding) 

-> (parameter-object)

<br/>

(parameter-object)要查找调用过程中最近的绑定值，

为了查找调用过程中最近的绑定，我们沿着刚才的推导向上找，

找到了parameterize对它的更改，值为2。

所以，

(**parameterize** [(parameter-object 2)]

&nbsp;&nbsp;&nbsp;&nbsp;(test-dynamic-binding)) 

=> 2

<br/>

而最后的直接调用(test-dynamic-binding) ，

调用过程中最近的绑定是对参数对象parameter-object的定义，

(define parameter-object

&nbsp;&nbsp;&nbsp;&nbsp;(**make-parameter** 1))

所以，(test-dynamic-binding) => 1

<br/>

## **词法闭包**

如果变量具有动态作用域，我们就要一直**记着函数的调用过程**。

这在复杂的程序中，是很困难又容易出错的事情。

因此，Scheme中的变量，默认具有词法作用域。

<br/>

词法作用域，保存了变量定义时的**环境**。

起到了**封闭和隔离**的作用。

<br/>

例如：

(define-values (get-value set-value!)

&nbsp;&nbsp;&nbsp;&nbsp;(**let** [(field 0)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(values (**lambda** () field)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(**lambda** (new-value) (set! field new-value)))))

<br/>

(get-value)

=> 0

<br/>

(set-value! 1)

<br/>

(get-value)

=> 1

<br/>

其中，**values**表达式用来同时返回多值，而**define-values**用来定义多值。

<br/>

get-value和set-value!函数分别用来读取和修改词法作用域中的变量field。

field对于get-value和set-value!来说是**共享的**，

而其它任何函数都无法修改和访问它。

<br/>

正因为有这样的封闭性，我们将函数连同定义时的环境一起，称为**闭包**。

<br/>

## **对象**

熟悉面向对象编程的人们，可能会清晰的认识到。

对象同样也是封闭和隔离了它包含的字段。

因此，**在这种封装意义上来说，闭包就是对象**。

<br/>

那么面向对象语言中的其它概念，是否也有相似的对应关系呢？

有的。

<br/>

例如：

(define create-object

&nbsp;&nbsp;&nbsp;&nbsp;(**lambda** (init)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(**let** [(field init)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(values (**lambda** () field)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(**lambda** (new-value) (set! field new-value))))))

<br/>

(define-values (get-value set-value!)

&nbsp;&nbsp;&nbsp;&nbsp;(create-object 1))

<br/>

(get-value)

=> 1

<br/>

(set-value! 2)

<br/>

(get-value)

=> 2

<br/>

我们定义了个函数create-object，它可以用来生成对象。

相当于一个**对象工厂**，面向对象编程中与之对应的概念就是**“类”**。

<br/>

例如：

(define-values create-object

&nbsp;&nbsp;&nbsp;&nbsp;(**let** [(static 1)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(**lambda** (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(**let** [(field x)]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(values (**lambda** () (+ static field))

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(**lambda** (new-value) (set! field new-value)))))

<br/>

最外层的let表达式返回了一个函数create-object，

我们来使用create-object创建两个对象。

<br/>

(define-values (get-value1 set-value1!)

&nbsp;&nbsp;&nbsp;&nbsp;(create-object 2))

<br/>

(get-value1)

=> 3

<br/>

(set-value1 3)

<br/>

(get-value1)

=> 4

<br/>

(define-values (get-value2 set-value2!)

&nbsp;&nbsp;&nbsp;&nbsp;(create-object 3))

<br/>

(get-value2)

=> 4

<br/>

(set-value1 4)

<br/>

(get-value1)

=> 5

<br/>

结果，最外层let表达式中的变量static，可以同时被两个对象访问。

在面向对象编程中，与之对应的概念就是**“类的静态变量”**。

<br/>

## **思想比手段更重要**

我们看到[let返回lambda]，就是一个“对象”，

[lambda返回[let返回lambda]]，就是一个“类”，

[let返回[lambda返回[let返回lambda]]]，就为类增加了“静态变量”。

<br/>

这是多么简洁而有力的结论呀。

出自——《Let Over Lambda》2008年

<br/>

我们想到，

**闭包和对象，只是用不同的方法实现了封装**。

而这种封装思想，才是更值得关注的。

<br/>

编程范型之争愈演愈烈，

函数式和面向对象之间似乎水火不容，

我们可不要在讨论手段的同时，偏废了思想。

<br/>

## **结语**

<br/>

封装，具有深刻的内涵，

它有几层含义，表达了很多与编程范型无关的思想，

“[封装的内涵](https://thzt.github.io/blog/2015/01/29/encapsulation/)”和大家一起详细探讨了这些内容。





















