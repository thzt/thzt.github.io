---
layout: post
categories: Logic
title: 类型理论之拙见
description: 
---

类型系统，是指一种根据所计算出值的种类，对词语进行分类，

从而证明某程序行为不会发生的，

可行语法手段。——《TAPL》

<br/>

可见类型系统是形式方法的一种，

也是一种证明方法。

<br/>

当我们把自己写好的程序提交给别人时，

如果别人质疑『你怎么证明它是对的』，

是不好回答的。

<br/>

因为通过测试用例来检测程序行为，

本身就是在进行不完全归纳，

我们只能断定验证过的事情是正确，

却不能断定一般性质。

<br/>

类型理论与数学，逻辑学，计算机科学相关，

甚至渗透到了其他学科之中。

<br/>

类型系统是程序语言之上的一套逻辑系统，

可以对程序进行推理，来断定某些性质。

不同的逻辑系统，『诱导』出了不同的类型系统。

<br/>

形式系统有个特点，那就是稍微改变一点约束条件，

就会得到一系列好玩的附加特性，

有大量丰富的逻辑系统可以玩。

例如，直觉主义逻辑，模态逻辑，时态逻辑，等等。

<br/>

可是，仅从代码进行静态分析，来断定程序运行时的所有行为，是不可判定的。

因此只能保证well typed的程序没有某类错误，

每个类型系统有各自要阻止的行为。

<br/>

类型系统种类繁多，支持各种好玩的特性，

例如，支持Polymorphism的类型系统，某类型可以由其他类型参数化，

支持Dependent type的系统，类型可以由值来决定，

子类型允许我们适当放宽类型要求，

递归类型，存在类型，全称类型。

<br/>

1934年，Curry意识到简单类型化lambda演算中的类型，与直觉主义逻辑之间的关系，

后面的研究发现，人们把这种对应关系推广为了Curry-Howard-Lambek Correspondance，

它将程序语言的类型，逻辑系统中的命题，和指称语义笛卡尔闭范畴，联系起来了。

一个合法项的存在，就证明了对应它类型的一个命题为真，程序即构造出来的证明。

<br/>

类型理论的实用内容还有很多，

Gradual typing的动静结合，例如flow，

以及Rust和Linear typeing的应用，

另外还有，Hindley–Milner类型推导算法。

<br/>

类型系统有一些性质可以衡量，

例如，type soundness，type safety，

以及检查类型的方式，static check，dynamic check，

包括某些语言是explicitly typed，而某些是implicitly typed，

某些语言是被stronger checked，有些则是weaker checked。

<br/>

现在是学习时间了。

<br/>

参考：

[Curry-Howard-Lambek correspondence](https://wiki.haskell.org/Curry-Howard-Lambek_correspondence)

[类型和程序设计语言](https://book.douban.com/subject/1318672/)

[Type System - Luca Cardelli](http://lucacardelli.name/papers/typesystems.pdf)

[程序设计语言理论基础](https://book.douban.com/subject/1944729/)
