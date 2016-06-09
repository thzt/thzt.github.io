---
layout: post
categories: Logic
title: 代数数据类型的语法和语义
description: 
---

<script src="/thirdpart/jQuery/jquery-1.11.1.js"></script>
<link href="/thirdpart/KaTeX/katex.min.css" rel="stylesheet"/>
<script src="/thirdpart/KaTeX/katex.min.js"></script>
<script src="/javascripts/katex.js"></script>

### **代数数据类型**

一个代数数据类型，由值的一些集合，以及这些集合之间的一些函数构成。

这些函数都是一阶函数，不能以其他函数作为参数。

<br/>

### **泛代数**

泛代数（universal algebra）也称为等式逻辑（equational logic），

是用于研究代数数据类型的一个数学框架。

<br/>

在泛代数中，代数数据类型的语法由代数项（algebraic term）描述，公理语义用项之间的等式集（a set of equations）描述，

而指称语义对应于一个<span data-katex="\Sigma"></span>代数，操作语义通过给等式设定方向来表示。

- - -

## **代数数据类型的语法**

### **代数项和签名**

一个代数项（algebraic term）由符号和类型来定义，

这些信息放在一起称为代数项的签名（signature）。

<br/>

构成代数项的基本类型称为sort。

<br/>

一个签名<span data-katex="\Sigma=\left \langle S,F \right \rangle"></span>，由以下几个部分构成，

（1）以sort为元素构成的集合

（2）sort上函数符号的集合<span data-katex="F=\left \{ f:s_1\times \cdots \times s_k\rightarrow s \right \}"></span>

其中，<span data-katex="s_1,\cdots ,s_k,s\in S"></span>，<span data-katex="f"></span>称为类型化的函数符号，

每个函数符号的类型是唯一的。

<br/>

例如，自然数表达式的签名是<span data-katex="\Sigma_N=\left \langle S,F \right \rangle"></span>，

其中<span data-katex="S=\left \{ nat \right \}"></span>，只包含一个sort，

<span data-katex="F"></span>给出以下几个函数符号，

<span data-katex="0:nat"></span>，<span data-katex="1:nat"></span>，<span data-katex="+:nat\times nat\rightarrow nat"></span>，<span data-katex="*:nat\times nat\rightarrow nat"></span>。

习惯上为了节省空间，通常把签名写成一个表格形式，

<span data-katex="sorts:nat"></span>

<span data-katex="fctns:0,1:nat"></span>

<span data-katex="+,*:nat\times nat\rightarrow nat"></span>

<br/>

### **变量的指派**

一个指派（sort assignment），是如下一个有限集合，用来指定变量的类型，

<span data-katex="\Gamma=\left \{ x_1:s_1,\cdots ,x_k:s_k \right \}"></span>

不能为同一个变量指派不同的sort。

<br/>

### **合法代数项的集合**

基于签名<span data-katex="\Sigma"></span>和指派<span data-katex="\Gamma"></span>，可以定义一个sort为<span data-katex="s"></span>的代数项的集合<span data-katex="Terms^s\left ( \Sigma,\Gamma \right )"></span>，

它满足以下几个条件，

（1）如果<span data-katex="x:s\in \Gamma"></span>则<span data-katex="x\in Terms^s\left ( \Sigma,\Gamma \right )"></span>

（2）如果<span data-katex="f:s_1\times \cdots \times s_k\rightarrow s"></span>且<span data-katex="M_i\in Terms^{s_i}\left ( \Sigma,\Gamma \right )"></span>，

<span data-katex="i=1,\cdots ,n"></span>，则<span data-katex="fM_1\cdots M_k\in Terms^s\left ( \Sigma,\Gamma \right )"></span>

- - -

## **代数数据类型的指称语义**

### **<span data-katex="\Sigma"></span>代数**

<span data-katex="\Sigma"></span>代数是一种数学结构，它为代数项提供了含义或指称语义。

<br/>

一个<span data-katex="\Sigma"></span>代数，包含了一个或多个集合，称为载体（carrier），

以及一些特征元素，和载体上的一些一阶函数，

<span data-katex="f:A_1\times \cdots \times A_k \rightarrow A"></span>

<br/>

例如，<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{N}=\left \langle N,0,1,+,* \right \rangle"></span>

具有载体<span data-katex="N"></span>，它是自然数集，

具有特征元素，<span data-katex="0,1\in N"></span>，

以及函数，<span data-katex="+,*:N \times N \rightarrow N"></span>。

其中，特征元素可以看成零元函数。

<br/>

带有多个载体的例子是<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{A}_{pcf}=\left \langle N,B,0,1,\cdots ,+,true,false,Eq?,\cdots ,\right \rangle"></span>

其中<span data-katex="N"></span>是自然数集，<span data-katex="B"></span>是布尔值集，

<span data-katex="0,1,\cdots"></span>是自然数，<span data-katex="+"></span>是加法函数。

<br/>

### **代数项的解释**

我们说<span data-katex="\mathscr{A}=\left \langle \left \{ A^s \right \}_{s\in S}, \mathscr{I} \right \rangle"></span>是与所有合法代数项<span data-katex="\left \{ Terms^s\left ( \Sigma,\Gamma \right ) \right \}_{s\in S}"></span>对应的<span data-katex="\Sigma"></span>代数，

指的是如下对应关系成立，

（1）每一个sort，<span data-katex="s\in S"></span>，恰好对应一个载体<span data-katex="A^s"></span>

（2）每一个函数符号<span data-katex="f:s_1\times \cdots \times s_k\rightarrow s"></span>，恰好对应一个函数<span data-katex="\mathscr{I}(f):A^{s_1}\times \cdots \times A^{s_k}\rightarrow A^s"></span>

习惯上把<span data-katex="\mathscr{I}(f)"></span>写成<span data-katex="f^{\mathscr{A}}"></span>。

<br/>

### **含变量代数项的解释**

<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{A}"></span>的环境<span data-katex="\eta"></span>，是把变量映射到<span data-katex="\mathscr{A}"></span>的各载体中元素的一个映射。

<span data-katex="\eta :\mathscr{V} \rightarrow\cup _sA^s"></span>

需要环境的原因是，对于含变量<span data-katex="x"></span>的项<span data-katex="M"></span>，叙述<span data-katex="M"></span>的含义必须先给<span data-katex="x"></span>指定一个确定的值。

如果对于每个<span data-katex="x:s\in \Gamma"></span>，都有<span data-katex="\eta (x)\in A^s"></span>，就说环境<span data-katex="\eta"></span>满足指派<span data-katex="\Gamma"></span>。

<br/>

假定<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{A}"></span>的一个环境<span data-katex="\eta"></span>满足指派<span data-katex="\Gamma"></span>，

则可以把环境<span data-katex="\eta"></span>下的任何项<span data-katex="M\in Terms\left ( \Sigma ,\Gamma \right )"></span>的含义<span data-katex="\mathscr{A}[[M]]\eta"></span>定义如下，

（1）<span data-katex="\mathscr{A}[[M]]\eta =\eta (x)"></span>

（2）<span data-katex="\mathscr{A}[[fM_1\cdots M_k]]\eta =f^{\mathscr{A}}(\mathscr{A}[[M_1]]\eta ,\cdots ,\mathscr{A}[[M_k]]\eta )"></span>

<br/>

若<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{A}"></span>在上下文中是明确的，通常省略<span data-katex="\mathscr{A}"></span>而直接写<span data-katex="[[M]]\eta"></span>，

若<span data-katex="M"></span>中没有变量，则<span data-katex="\mathscr{A}[[M]]\eta"></span>不依赖于<span data-katex="\eta"></span>，可以写为<span data-katex="\mathscr{A}[[M]]"></span>。

- - -

## **语法和语义的关系**

### **可靠性与完备性**

代数数据类型的公理语义是由代数项之间的等式集给出的，签名和等式集合称代数规范（algebraic specification）。

一个代数规范，或者可以使用等式证明系统推导出代数项之间的其他等式，或者可以检验代数项对应的<span data-katex="\Sigma"></span>代数是否满足这些等式的要求。

<br/>

代数项对应的<span data-katex="\Sigma"></span>代数并不是唯一的。

从一个代数规范推导得到的等式，在该规范对应的任何<span data-katex="\Sigma"></span>代数中都成立，就称该代数证明系统是可靠的（sound）。

一个代数规范对应的任何<span data-katex="\Sigma"></span>代数中都成立的等式，在该规范中都可证，就称该代数证明系统是完备的（complete）。

<br/>

### **等式的可满足性**

等式（equation）是一个公式<span data-katex="M=N[\Gamma ]"></span>，其中<span data-katex="M,N\in Terms^s\left ( \Sigma ,\Gamma \right )"></span>，<span data-katex="s\in S"></span>。

如果环境<span data-katex="\eta"></span>满足指派<span data-katex="\Gamma"></span>，且<span data-katex="[[M]]\eta =[[N]]\eta"></span>，

就说<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{A}"></span>在环境<span data-katex="\eta"></span>下满足<span data-katex="M=N[\Gamma ]"></span>，记为

<span data-katex="\mathscr{A},\eta \models M=N[\Gamma]"></span>

<br/>

对于含变量的项，我们更感兴趣的是一个等式是否在变量所有可能的取值情况下都成立，

而不是在一个特别的环境中成立。

如果对于满足<span data-katex="\Gamma"></span>的任何一个环境<span data-katex="\eta"></span>都有<span data-katex="\mathscr{A},\eta \models M=N[\Gamma]"></span>，

就可以说，<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{A}"></span>满足等式<span data-katex="M=N[\Gamma ]"></span>，记为

<span data-katex="\mathscr{A} \models M=N[\Gamma]"></span>

<br/>

可满足性也可以扩展到等式集和代数集，

设<span data-katex="E"></span>是一个等式集，如果<span data-katex="\mathscr{A}"></span>满足所有等式，就说<span data-katex="\mathscr{A}"></span>满足<span data-katex="E"></span>。

类似的，若<span data-katex="C"></span>是一类<span data-katex="\Sigma"></span>代数，且对每个<span data-katex="\mathscr{A}\in C"></span>都有<span data-katex="\mathscr{A}\models M=N[\Gamma ]"></span>，则<span data-katex="C\models M=N[\Gamma ]"></span>

<br/>

若任何一个<span data-katex="\Sigma"></span>代数都满足代数项之间的等式<span data-katex="M=N[\Gamma ]"></span>，就说该等式是永真的（valid），写为<span data-katex="\models M=N[\Gamma ]"></span>。

例如，<span data-katex="x=x[x:s]"></span>就是永真的。

<br/>

若<span data-katex="\mathscr{A}"></span>满足签名<span data-katex="\Sigma=\left \langle S,F \right \rangle"></span>上的所有等式，就说<span data-katex="\Sigma"></span>代数是平凡的（trivial）。

<br/>

### **语义蕴含（semantic implication）**

若满足等式集<span data-katex="E"></span>的每一个<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{A}"></span>都满足等式<span data-katex="M=N[\Gamma ]"></span>，

则称签名<span data-katex="\Sigma"></span>上的等式集<span data-katex="E"></span>在语义上蕴含等式<span data-katex="M=N[\Gamma ]"></span>，记为，

<span data-katex="E\models M=N[\Gamma ]"></span>

<br/>

有了签名<span data-katex="\Sigma"></span>和等式集<span data-katex="E"></span>，我们定义代数规范<span data-katex="Spec=\left \langle \Sigma,E \right \rangle"></span>，

则满足代数规范的，在所有<span data-katex="\Sigma"></span>代数中都成立的等式，就是那些由等式集<span data-katex="E"></span>语义蕴含的等式。

<br/>

### **语义理论**

如果等式集<span data-katex="E"></span>在语义蕴含下封闭（closed），则把它称为一个理论（theory）。

更准确的说，如果<span data-katex="E\models M=N[\Gamma ]"></span>，则<span data-katex="M=N[\Gamma ]\in E"></span>，那么等式集<span data-katex="E"></span>就称为一个语义理论（semantic theory）。

一个<span data-katex="\Sigma"></span>代数<span data-katex="\mathscr{A}"></span>的理论<span data-katex="Th(\mathscr{A})"></span>，就是在<span data-katex="\mathscr{A}"></span>中成立的所有等式的集合。

可以证明一个<span data-katex="\Sigma"></span>代数的理论是一个语义理论。

<br/>

### **形式证明**

一个证明系统的推导规则如下，<span data-katex="\frac{antecedent}{consequent}"></span>，

使得从前件（antecedent）的任何实例出发，通过使用该证明系统的公理和其他规则，

可以推导出后件（consequent）的相应实例。

<br/>

例如：<span data-katex="\frac{M=N[\Gamma ],N=P[\Gamma ],P=Q[\Gamma ]}{M=Q[\Gamma ]}"></span>

<br/>

若证明了一条规则是可推导的，则能够把它当做系统的一条证明规则来使用。

如果某条规则没有前件，则称它是证明系统的一条公理（axiom）。

<br/>

我们说等式<span data-katex="M=N[\Gamma ]"></span>是可证的（provable），记为<span data-katex="E\vdash M=N[\Gamma ]"></span>

如果从<span data-katex="E"></span>到<span data-katex="M=N[\Gamma ]"></span>存在一个等式序列，

使得每个等式或者是公理（axiom），或者是<span data-katex="E"></span>中的等式，

或者是从序列中之前出现的一个或多个等式经一步推导得到的结果。

<br/>

### **语法理论**

若在可证性下等式集<span data-katex="E"></span>是封闭的，则称<span data-katex="E"></span>是一个语法理论（syntactic theory）。

换句话说，如果<span data-katex="E\vdash M=N[\Gamma ]"></span>，则<span data-katex="M=N[\Gamma ]\in E"></span>，那么等式集<span data-katex="E"></span>就称为一个语法理论。

<span data-katex="E"></span>的语法理论<span data-katex="Th(E)"></span>就是从<span data-katex="E"></span>可证的所有等式的集合。

<br/>

### **等式证明系统的性质**

可靠性（soundness）：若<span data-katex="E\vdash M=N[\Gamma ]"></span>，则<span data-katex="E\models M=N[\Gamma ]"></span>

演绎完备性（deductive completeness）：若<span data-katex="E\models M=N[\Gamma ]"></span>，则<span data-katex="E\vdash M=N[\Gamma ]"></span>

- - -

## **结语**

《[Foundations for Programming Languages](https://book.douban.com/subject/1761918/)》是一本好书，

可是中文的翻译《[程序设计语言理论基础](https://book.douban.com/subject/1944729/)》简直是晦涩难懂，

把sort翻译为『类子』，把signature翻译为『基调』，容易让人误以为和同调代数有什么联系。

原版书拿到后，看起来轻松了不少，这里只是对第三章部分内容做了个小结，以便卸下包袱轻装上阵。