---
layout: post
categories: Logic
title: 形式证明与逻辑推理
description: 到底什么是证明，什么是推理，它们能被精确定义吗，有没有研究它们的学科
---

<script src="/thirdpart/jQuery/jquery-1.11.1.js"></script>
<link href="/thirdpart/KaTeX/katex.min.css" rel="stylesheet"/>
<script src="/thirdpart/KaTeX/katex.min.js"></script>
<script src="/javascripts/katex.js"></script>

小时候，我就对侦探非常着迷，

买了很多介绍破案的漫画书，故事书，小说。

<br/>

什么《大宇神秘惊奇系列》啊，

《名侦探柯南》啊，

《福尔摩斯探案全集》啊，等等。

<br/>

可是，对于**什么是推理**，

以及怎样进行推理，

并没有清晰系统的认识。

<br/>

学生时代，从平面几何开始，

我们就知道了证明题。

经过一步一步的推导，

最后证明结论成立。

<br/>

可是，对于**什么是证明**，

并没有人能说出精确的定义。

<br/>

这一切，难道真是只是个谜吗？

是人类的未知领域吗？

<br/>

其实不然。

**逻辑学**就是研究推理和证明的学科，

研究思维的形式，规律和方法。

<br/>

其中，数理逻辑是逻辑学与数学的交叉学科，

用数学的方法研究逻辑，

我想，答案应该在这里吧。

<br/>

## **大局观**

数理逻辑虽然博大精深，

但是研究方法却非常简洁优美。

<br/>

给定一套逻辑系统，

分别从两个侧面来描述这个系统的性质。

语法层面，语义层面。

<br/>

**语法**，指的是构成这个逻辑系统的符号规则，

由公理和定理的推导规则组成，

让我们可以从一串合法的符号得到另一串合法符号，

称之为**形式证明**。

<br/>

**语义**，指的是用什么样的数学对象可以解释这些符号，

由论域和解释函数组成，我们得到的是一些代数结构，

而且，从已知符号串的语义性质得到了其他符号串的性质，

称之为**逻辑推理**。

<br/>

学校中的数理逻辑教科书，介绍了**命题演算**和**一阶谓词演算**这两个典型的逻辑系统。

它们各自的语义解释，恰好描述了日常生活中推理问题。

<br/>

总之，数理逻辑，用一套符号，对生活中常见的逻辑问题，进行了数学建模，

研究它，希望得到与证明和推理相关的更多性质和结论。

<br/>

## **形式证明**

为了说明问题，而又不引入过多的逻辑学概念，

我们从命题逻辑开始。

<br/>

命题逻辑的形式化演算系统大体上可分为两种类型，

一是**希尔伯特式**的公理化演算系统，

二是**甘岑(Gentzen)式**的自然推理系统。

<br/>

这两个系统各有所长，

前者更能体现公理化的思想，但其推理过程比较繁琐，

后者形式推理比较自然，但是规则较多。

<br/>

下面只说**命题演算的自然推理系统**。

<br/>

**语法：**

（1）可数个命题符号：<span data-katex="p_1,p_2,\cdots"></span>

（2）5个联接词符号：<span data-katex="\neg,\lor,\land,\to,\leftrightarrow"></span>

（3）2个辅助符号：<span data-katex="),("></span>

<br/>

**公式：（BNF）**

<span data-katex="\alpha::=p|(\neg\alpha)|(\alpha_1\lor\alpha_2)|(\alpha_1\land\alpha_2)|(\alpha_1\to\alpha_2)|(\alpha_1\leftrightarrow\alpha_2)"></span>

<br/>

**推导规则：**

（1）<span data-katex=""></span>包含律：<span data-katex="\frac{\alpha\in\Gamma}{\Gamma\vdash\alpha}"></span>

（2）<span data-katex="\neg"></span>消去律：<span data-katex="\frac{\Gamma,\neg\alpha\vdash\beta;\Gamma,\neg\alpha\vdash\neg\beta}{\Gamma\vdash\alpha}"></span>

（3）<span data-katex="\to"></span>消去律：<span data-katex="\frac{\Gamma\vdash(\alpha\to\beta);\Gamma\to\alpha}{\Gamma\vdash\beta}"></span>

（4）<span data-katex="\to"></span>引入律：<span data-katex="\frac{\Gamma,\alpha\vdash\beta}{\Gamma\vdash\alpha\to\beta}"></span>

（5）<span data-katex="\lor"></span>消去律：<span data-katex="\frac{\Gamma,\alpha\vdash\gamma;\Gamma,\beta\vdash\gamma}{\Gamma,\alpha\lor\beta\vdash\gamma}"></span>

（6）<span data-katex="\lor"></span>引入律：<span data-katex="\frac{\Gamma\vdash\alpha}{\Gamma\vdash\alpha\lor\beta;\Gamma\vdash\beta\lor\alpha}"></span>

（7）<span data-katex="\land"></span>消去律：<span data-katex="\frac{\Gamma\vdash\alpha\land\beta}{\Gamma\vdash\alpha;\Gamma\vdash\beta}"></span>

（8）<span data-katex="\land"></span>引入律：<span data-katex="\frac{\Gamma\vdash\alpha;\Gamma\vdash\beta}{\Gamma\vdash\alpha\land\beta}"></span>

（9）<span data-katex="\leftrightarrow"></span>消去律：<span data-katex="\frac{\Gamma\vdash\alpha\leftrightarrow\beta;\Gamma\vdash\alpha}{\Gamma\vdash\beta}"></span>，<span data-katex="\frac{\Gamma\vdash\alpha\leftrightarrow\beta;\Gamma\vdash\beta}{\Gamma\vdash\alpha}"></span>

（10）<span data-katex="\leftrightarrow"></span>引入律：<span data-katex="\frac{\Gamma,\alpha\vdash\beta;\Gamma,\beta\vdash\alpha}{\Gamma\vdash\alpha\leftrightarrow\beta}"></span>

<br/>

**例子：**

使用这些推理规则，我们就可以从一些合法的符号串，

推导出另一些合法的符号串了。

<br/>

（1）<span data-katex="\alpha\to\beta,\beta\to\gamma,\alpha\vdash\alpha\to\beta"></span>：<span data-katex=""></span>包含律

（2）<span data-katex="\alpha\to\beta,\beta\to\gamma,\alpha\vdash\alpha"></span>：<span data-katex=""></span>包含律

（3）<span data-katex="\alpha\to\beta,\beta\to\gamma,\alpha\vdash\beta"></span>：<span data-katex="\to"></span>消去律，式（1），式（2）

（4）<span data-katex="\alpha\to\beta,\beta\to\gamma,\alpha\vdash\beta\to\alpha"></span>：<span data-katex=""></span>包含律

（5）<span data-katex="\alpha\to\beta,\beta\to\gamma,\alpha\vdash\gamma"></span>：<span data-katex="\to"></span>消去律，式（3），式（4）

（6）<span data-katex="\alpha\to\beta,\beta\to\gamma,\alpha\vdash\alpha\to\gamma"></span>：<span data-katex="\to"></span>引入律，式（5）

<br/>

有了这些以后，我们就可以定义什么是一个**证明**了。

**证明序列：**

若有限序列，<span data-katex="\Gamma_1\vdash\alpha_1,\Gamma_2\vdash\alpha_2,\cdots ,\Gamma_n\vdash\alpha_n"></span>满足，

（1）<span data-katex="\Gamma_1,\Gamma_2,\cdots ,\Gamma_n"></span>为有限公式集

（2）<span data-katex="\alpha_1,\alpha_2,\cdots ,\alpha_n"></span>为公式

（3）每个<span data-katex="\Gamma_i\vdash\alpha_i(1\leq i\leq n)"></span>都是它之前若干个<span data-katex="\Gamma_j\vdash\alpha_j(1\leq j<i\leq n)"></span>应用某条推导规则得到的

<br/>

则称这个有限序列为<span data-katex="\Gamma_n\vdash\alpha_n"></span>的一个**（形式）证明序列**。

此时，也称<span data-katex="\alpha_n"></span>可由<span data-katex="\Gamma_n"></span>**（形式）证明**，

记为<span data-katex="\Gamma_n\vdash_N\alpha_n"></span>，其中<span data-katex="N"></span>表示自然推理系统。

<br/>

## **逻辑推理**

上文提到的是命题演算的自然推理系统，

这是一个形式系统，我们介绍了它的语法和推导规则，

根据这些推导规则，可以从一些合法的符号串推导出另一些，

在这个基础上，我们定义了什么叫做（形式）证明。

<br/>

如何**解释**这些符号呢？

它们有什么含义呢？

<br/>

我们给每一个合法的**公式**指定一个**逻辑命题**，作为这个公式的解释。

为每一个**联接词符号**指定一个**真值函数**，作为这个联接词符号的解释。

<br/>

**命题：**

命题是可以判断真假值的句子。

<br/>

**真值函数：**

<span data-katex="\{0,1\}"></span>上的<span data-katex="n"></span>元函数，<span data-katex="f:\{0,1\}^n\to\{0,1\}"></span>

称为一个<span data-katex="n"></span>元真值函数。

<br/>

我们将每个联接词与一个真值函数一一对应起来，

那么，复合命题的真假值就可以通过子命题的真假值计算出来了。

<br/>

**指派：**

设<span data-katex="\alpha"></span>为一个命题，<span data-katex="\alpha"></span>中出现的所有命题变元构成了一个序列<span data-katex="p_1,p_2,\cdots ,p_n"></span>，

对该序列指定的任一真假值序列<span data-katex="t_1,t_2,\cdots ,t_n"></span>称为<span data-katex="\alpha"></span>关于<span data-katex="p_1,p_2,\cdots ,p_n"></span>的一个**指派**，

其中<span data-katex="t_i=0,1"></span>。

<br/>

**真值表：**

命题在所有可能的指派下，所取值列成的表，称为真值表。

<br/>

**永真式：**

如果命题关于其中出现命题变元的所有指派均为真，则称该命题是一个永真式。

<br/>

有了这些以后，我们就可以定义**推理**了。

设<span data-katex="\alpha_1,\alpha_2,\cdots ,\alpha_n,\beta"></span>都是命题，

称**推理『<span data-katex="\alpha_1,\alpha_2,\cdots ,\alpha_n"></span>推出<span data-katex="\beta"></span>』是有效的**，

如果对<span data-katex="\alpha_1,\alpha_2,\cdots ,\alpha_n,\beta"></span>中出现的命题变元的任一指派，

若<span data-katex="\alpha_1,\alpha_2,\cdots ,\alpha_n"></span>都为真，则<span data-katex="\beta"></span>也为真，

记为<span data-katex="\alpha_1,\alpha_2,\cdots ,\alpha_n\models\beta"></span>

否则，称推理『<span data-katex="\alpha_1,\alpha_2,\cdots ,\alpha_n"></span>推出<span data-katex="\beta"></span>』是无效的。

<br/>

**例子：**

<span data-katex="\alpha\to\beta,\alpha\models\beta"></span>

<span data-katex="\alpha\lor\beta,\neg\alpha\models\beta"></span>

<br/>

## **证明与推理之间的关系**

命题演算的自然推理系统，有很多性质，其中，

<br/>

**可靠性**

<span data-katex="\Gamma\vdash\alpha\Rightarrow\Gamma\models\alpha"></span>

<br/>

**完备性**

<span data-katex="\Gamma\models\alpha\Rightarrow\Gamma\vdash\alpha"></span>

<br/>

它们表明，如果一个公式可以被证明，那么它所对应命题的推理就是有效的，

如果某些命题的推理是有效的，那么它就可以被证明。

<br/>

然而，形式化系统这种研究方法，并不是完美无缺的。

**哥德尔不完全性定理**

如果<span data-katex="\Gamma"></span>是一个有穷并包含初等算术<span data-katex="\Pi"></span>的形式理论，那么<span data-katex="\Gamma"></span>是一个不完全的形式理论。

<br/>

**哥德尔协调性定理**

如果形式理论<span data-katex="\Gamma"></span>包含初等算术<span data-katex="\Pi"></span>，那么<span data-katex="\Pi"></span>的协调性不能在<span data-katex="\Gamma"></span>中被证明。

<br/>

## **结语**

证明和推理也是可以研究的，

并且，一直以来都是人们的感兴趣的研究对象。

<br/>

逻辑学对自动定理证明，程序设计语言中的类型系统，

协议验证，软硬件的安全等领域，

有很重要的理论价值。

<br/>

以命题逻辑和一阶谓词逻辑为基础，

人们构造出了各式各样种类繁多的逻辑系统，

包括模态逻辑，直觉主义逻辑，时序逻辑，动态逻辑，

多值逻辑，模糊逻辑，非单调逻辑，λ演算，组合逻辑等等。

<br/>

现代逻辑学已经应用到了越来越多的学科之中。
