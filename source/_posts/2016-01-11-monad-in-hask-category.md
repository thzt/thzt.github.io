---
layout: post
categories: Haskell
title: Hask范畴上的Monad
description: 一个单子（Monad）说白了不过就是自函子范畴上的一个幺半群而已，这有什么难以理解的？
---

范畴论是一个迷人的领域，

它是一门研究**数学结构**以及**结构之间关系**的理论。

<br/>

不知道我们学群论时，

是否感觉到了群同态与集合间映射的相似性。

学拓扑学时，

是否感觉到了连续映射与微分流形间光滑映射的相似性。

<br/>

范畴论统一了这些相似结构。

然而，这还要从抽象代数说起。

<br/>

## 幺半群（monoid）

在抽象代数中，**幺半群**是这样定义的。

<br/>

集合S和S上满足结合律的封闭二元运算"•"，

所形成的代数结构称为**半群**，记为(S, •)，简记为S

<br/>

设S是半群，元素e∈S，称为半群S的**幺元素**，

如果对于每一个x∈S，有xe=ex=x

<br/>

如果半群S有幺元素e，则它是唯一的。

含有幺元素的半群称为**幺半群**。

<br/>

注：

半群G如果有幺元素，且每个元素均可逆，

则称G为**群**

<br/>

## 图示法（diagram）

一个幺半群M，可以描述为一个集合M，和两个函数

µ : M × M -> M

η : 1 -> M

<br/>

其中，1 = {0}是只有一个元素的集合。

<br/>

<pre>           1 × µ<br>M × M × M -------&gt; M × M<br>    |                |<br>    | µ × 1          | µ<br>    |                |<br>    v                v<br>  M × M   -------&gt;   M<br>             µ</pre>

<pre>        η × 1          1 × η<br>1 × M -------&gt; M × M &lt; ------- M × 1<br>  |              |               |<br>  | α            | µ             | β<br>  |              |               |<br>  v              v               v<br>  M      =       M        =      M</pre>

<br/>

用元素来表示图表，可以写为，

<br/>

<pre>&lt;x,y,z&gt; |-------&gt;    &lt;x,yz&gt;<br>   -                   -<br>   |                   |<br>   |                   |<br>   v                   v<br>&lt;xy,z&gt;  |-------&gt; (xy)z=x(yz)</pre>

<pre>&lt;0,x&gt; |-------&gt; &lt;e,x&gt;    &lt;x,e&gt; &lt;-------| &lt;x,0&gt;<br>  -               -        -               -<br>  |               |        |               |<br>  |               |        |               |<br>  v               v        v               v<br>  x       =      ex       xe       =       x</pre>

<br/>

可以看出，(xy)z=x(yz)表示了群乘法的结合律，

x=ex,xe=x表示了幺元e，因此图表展示了幺半群的结构。

<br/>

## 范畴（category）

一个**范畴**C由一系列**对象**（object）和**箭头**（arrow）组成。

对于每一个箭头f，有两个对象与之关联，

称为箭头f的定义域（domain）和值域（codomain）。

并且，满足以下几条规则，

（1）对于每一个对象a，存在恒等箭头（identity arrow），i:a->a

（2）箭头满足结合律，对于任意的箭头f,g,h有(f•g)•h=f•(g•h)

（3）箭头的集合在箭头组合运算下是封闭的

<br/>

注：

f•g表示g和f的组合运算，它也是一个箭头，其中g的值域是f的定义域

<br/>

例：

所有的集合，以集合作为对象，集合间的映射作为箭头，构成了一个范畴，

所有的群，以群作为对象，群同态作为箭头，构成了一个范畴，

所有的拓扑空间，以拓扑空间作为对象，拓扑空间之间的连续映射为箭头，构成了一个范畴，

所有的微分流形，以微分流形作为对象，流形间的光滑映射为箭头，构成了一个范畴，

Haskell中，以类型作为对象（类型是值的集合），函数作为箭头，构成了一个范畴（**Hask范畴**）。

<br/>

## 函子（functor）

如果把范畴看做对象，则函子可以看做箭头。

<br/>

一个**函子**F是范畴C到范畴D的箭头，F:C -> D，

它满足以下条件，

F把C中的对象c映射为D中的对象F c，把C中的箭头f映射为D中的箭头F f。

且满足分配律，F (f•g)=(F f)•(F g)

<br/>

注：

等式左边的"•"表示C中的箭头组合运算，

等式右边的"•"表示D中的箭头组合运算。

<br/>

范畴C到自身的函子，称为**自函子**（endofunctor）。

<br/>

Hask范畴的自函子把Haskell中的类型a映射为另一个类型f a，

把类型a到类型b的函数，映射为类型f a到类型f b的函数。

<br/>

class Functor f where

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;fmap :: (a -> b) -> f a -> f b

<br/>

class Functor f => Applicative f where

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;pure :: a -> f a

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(<*>) :: f (a -> b) -> f a -> f b

<br/>

我们看到，pure和fmap放在一起，

构成了一个Hask范畴的自函子。

<br/>

## 自然变换（natural transformation）

如果把函子看做对象，则自然变换可以看做箭头。

<br/>

若F和G是范畴C到D的函子，则**自然变换**τ是一个箭头，τ: F -> G，

它满足以下条件，

<br/>

<pre>     f<br>a -------&gt; b</pre>

<pre>      F f<br>F a -------&gt; F b<br>|            |<br>| τ a        | τ b<br>|            |<br>v            v<br>G a -------&gt; G b<br>      G f</pre>

<br/>

注：

F a是D中与a对应的对象，F b是D中与b对应的对象，F f是D中与f对应的箭头

<br/>

## 函子范畴（functor category）

以范畴C到D的函子为对象，以函子间的自然变换为箭头，

构成了一个范畴，称为**函子范畴**。

<br/>

易知，自然变换可以进行组合运算，

设µ a : F a -> G a，η a : G a -> H a

则可以定义一个新的自然变换(η • µ) a = F a -> H a

可证自然变换的组合运算满足结合律。

<br/>

注：

函子范畴的对象，不是一个集合，

函子范畴的箭头，也不是映射。

<br/>

## Monad

范畴C上的monad，是一个**三元组**(F,µ,η)，其中

F是范畴C上的自函子，

µ是F^2到F的自然变换，µ:F^2->F，

η是单位自函子I到F的自然变换，η:I->F

<br/>

且满足以下条件

<br/>

<pre>           F • µ<br>F • F • F -------&gt; F • F<br>    |                |<br>    | µ • F          | µ<br>    |                |<br>    v                v<br>  F • F   -------&gt;   F<br>             µ</pre>

<pre>        η • F          F • η<br>I • F -------&gt; F • F &lt; ------- F • I<br>                 |                <br>  ||             | µ            ||<br>                 |               <br>                 v                <br>  F      =       F        =      F</pre>

<br/>

在Haskell中可以这样表示：

<br/>

{- 自函子F，作用在对象上时 -}

fObj :: (Applicative f) => a -> f a

fObj = pure

<br/>

{- 自函子F，作用在箭头上时 -}

fArr :: (Applicative f) => (a -> b) -> (f a -> f b)

fArr = fmap

<br/>

{- 自函子F^2 -}

<br/>

f2Obj :: (Applicative f) => a -> f (f a)

f2Obj = fObj . fObj

<br/>

f2Arr :: (Applicative f) => (a -> b) -> (f (f a) -> f (f b))

f2Arr :: fArr . fArr

<br/>

{- 单位自函子，作用到对象上时 -}

iObj :: a -> a

iObj = id

<br/>

{- 单位自函子，作用到箭头上时 -}

iArr :: (a -> b) -> (a -> b)

iArr = id

<br/>

{- 自然变换µ:F^2->F，（µ a:F^2 a->F a） -}

µ :: (Applicative f) => a -> f (f a) -> f a

<br/>

{- 自然变换η:I->F，（η a:I a->F a） -}

η :: (Applicative f) => a -> a -> (f a)

<br/>

## 自函子范畴上的幺半群

以范畴C上的自函子为对象，自然变换为箭头，

构成的函子范畴，称为**自函子范畴**。

<br/>

对比Monad定义中的自函子F与幺半群中的集合M，

<br/>

**结合律**：

<pre>           1 × µ<br>M × M × M -------&gt; M × M<br>    |                |<br>    | µ × 1          | µ<br>    |                |<br>    v                v<br>  M × M   -------&gt;   M<br>             µ</pre>

<pre>           F • µ<br>F • F • F -------&gt; F • F<br>    |                |<br>    | µ • F          | µ<br>    |                |<br>    v                v<br>  F • F   -------&gt;   F<br>             µ</pre>

**幺元**：

<pre>        η × 1          1 × η<br>1 × M -------&gt; M × M &lt; ------- M × 1<br>  |              |               |<br>  | α            | µ             | β<br>  |              |               |<br>  v              v               v<br>  M      =       M        =      M</pre>

<pre>        η • F          F • η<br>I • F -------&gt; F • F &lt; ------- F • I<br>                 |                <br>  ||             | µ            ||<br>                 |               <br>                 v                <br>  F      =       F        =      F</pre>

<br/>

可知，自函子F相当于群的集合M，自然变换µ相当于群乘法，单位自函子相当于幺元，它们构成了一个**幺半群**，

即**Monad是Hask自函子范畴上的幺半群**。

<br/>

注：

M × M × M表示集合M的笛卡尔积，

而F • F • F表示自函子F的组合。

<br/>

## 幺半群范畴（monoidal category）

幺半群在范畴论中是有了新的意义，

比群论中的概念更一般化。

<br/>

我们可以为范畴增加一个满足结合律的二元函子，

构成一个『范畴论意义上的』**幺半群**（monoid）。

<br/>

说一个范畴是具有幺半群结构的（monodial），

如果它有一个像笛卡尔积，或者直和，张量积，那样的『乘积』，

并且，这个『乘积』满足结合律，还有一个单位元。

<br/>

即，一个**严格幺半群范畴**（strict monoidal category）是范畴B上的一个结构&lt;B,□,e&gt;，

其中□是一个满足结合律的二元函子，□: B × B -> B，

□ (□ × 1) = □ (1 × □) : B × B × B -> B

而且存在对象e是二元函子□的单位元，

□ (e × 1) = id(B) = □ (1 × e)

<br/>

然后就可以在任意的幺半群范畴&lt;B,□,e&gt;中定义幺半群了。

<br/>

幺半群范畴B上的**幺半群**由三部分组成，&lt;c,µ,η&gt;，

其中c是B中的对象，µ : c □ c -> c，η : e -> c是范畴B中的箭头，

且满足以下条件

<pre>               σ                  µ □ 1<br>c □ (c □ c) -------&gt; (c □ c) □ c -------&gt; c □ c<br>    |                                       |<br>    | 1 □ µ                                 | µ<br>    |                                       |<br>    v                                       v<br>  c □ c ----------------------------------&gt; c<br>                        µ</pre>

<pre>        η □ 1          1 □ η<br>e □ c -------&gt; c □ c &lt; ------- c □ e<br>  |              |               |<br>  | α            | µ             | β<br>  |              |               |<br>  v              v               v<br>  c      =       c        =      c</pre>

<br/>

## 结语

『All told, a monad in X is just a monoid in the category of endofunctors of X,

with the product × replaced by composition of endofunctors

and unit set by the identity endofunctor.』

<br/>

一语成谶，很多人都是因为这句话入坑的，

然而理解它真的很不容易，

原来这个『幺半群』应该在范畴论意义上进行理解，

已经不是集合论基础上群论的内容了。

<br/>

在写这篇文章时，我甚至还没有入门，有错误在所难免，

但是多年坚持下来，似乎对这个问题有些眉目了，

于是就赶紧整理了一下，希望接下来以此为起点继续努力，勇往直前。

<br/>

**参考**：

《[近世代数引论](http://book.douban.com/subject/4201293/)》

《[Categories for the Working Mathematician 2nd](http://book.douban.com/subject/1823110/)》

[Implementing a category-theoretic Hask-monad in Haskell](https://medium.com/@brettwines/implementing-a-category-theoretic-hask-monad-in-haskell-7bf662f2e98b)

[A monad is just a monoid in the category of endofunctors, what's the problem?](http://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem)

[A MONAD IS JUST A MONOID IN THE CATEGORY OF ENDOFUNCTORS. WHAT'S THE PROBLEM ?](http://slides.com/julientournay/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-what-s-the-problem/fullscreen)
