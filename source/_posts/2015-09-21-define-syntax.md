---
layout: post
categories: Lisp
title: Scheme元编程
description: 很多语言都已经支持了元编程，那么Lisp就丧失优势了吗？并没有。因为，S表达式仍然是表达同相性的巧妙选择。
---

[同相性](https://en.wikipedia.org/wiki/Homoiconicity)，指的是，

程序和程序所操作的数据采用了统一编码。

<br/>

Lisp语言使用了S表达式，

例如，(fn x)

既可以看做是程序，用参数x调用函数fn，

也可以看做是数据，由符号fn和符号x构成的列表。

<br/>

**同相性使得我们，可以像处理数据一样处理代码。**

做一些代码转换之类的工作，十分简单。

<br/>

例如，

当遇到(fn x)时，

我们可以让它先转换成，

(begin

&nbsp;&nbsp;&nbsp;&nbsp;(display x)

&nbsp;&nbsp;&nbsp;&nbsp;(gn x))
	
然后再执行。

<br/>

甚至也可以用来定义变量，

(define-with-display (f a)

&nbsp;&nbsp;&nbsp;&nbsp;(g a))

<br/>

转换成，

(define (f a)

&nbsp;&nbsp;&nbsp;&nbsp;(display a)

&nbsp;&nbsp;&nbsp;&nbsp;(g a))

<br/>

这种代码层面的转换称为“宏”(macro)。

<br/>

## **定义一个宏**

Scheme是Lisp的一个简洁方言，

它使用define-syntax来定义宏。

<br/>

本质上，宏是一个特殊的标识符，

它关联了转换器函数。

<br/>

表达式的求值过程，分为了3个阶段，

读取期，宏展开期，运行期。

<br/>

在遇到宏调用的时候，

Scheme会先调用与之关联的转换器，进行代码转换，(宏展开期)

**然后再求值结果表达式**。(运行期)

<br/>

在解释器中，宏展开和表达式求值可能是交替进行的，

而在编译器中，他们是两个独立的阶段。

<br/>

(define-syntax or

&nbsp;&nbsp;&nbsp;&nbsp;(syntax-rules ()

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_) #f]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_ e) e]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_ e1 e2 e3 ...)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; (let ([t e1]) (if t t (or e2 e3 ...)))]))

<br/>

以上代码定义了一个宏，or，

它用来对(or ...)表达式进行变换。

<br/>

(or)转换成了#f

(or a)转换成了a

(or a b)转换成了(let ([t a]) (if t t (or b)))

<br/>

我们看到，

宏展开是支持递归调用的。

<br/>

## **模式匹配**

syntax-rules使用了模式匹配来定义转换器，

它的每一条语句给定了形如“[模式 模板]”的转换规则，

如果模式匹配成功了，

就按着模板的方式进行转换。

<br/>

[(_ e) e]

<br/>

其中，

模式是(_ e)，

模板是e，

“_”表示通配符。

<br/>

这个模式匹配了(or e)，

转换结果为e，

即它能把(or a)转换成a。

<br/>

我们再来看(_ e1 e2 e3 ...)，

其中的省略号“...”，

**并不是为了演示方便故意省略了。**

<br/>

“...”是一个标识符，是模式匹配的一部分，

它用来代表“和前面一样的匹配”。

模板中也出现了“...”，

它会根据模式中“...”来进行填充。

<br/>

Scheme中使用的模式匹配，是一个庞大的主题，

甚至**模式匹配已经构成了一门新的语言，**

TSPL4中进行了详细的解释，[Syntax-Rules Transformers](http://www.scheme.com/tspl4/syntax.html#./syntax:h2)

<br/>

## **转换器函数**

另外一种定义宏的方式是，

显式的指定宏展开器函数。

<br/>

(define-syntax r

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(display x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(display "\n")

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#t))

<br/>

我们用lambda定义了一个匿名函数，

并让它与宏标识符r相关联。

<br/>

我们直接在REPL中看看r是什么，

<br/>

\#&lt;syntax r&gt;

\#t

<br/>

第一行是(display x)副作用，

可见x的值是#&lt;syntax r&gt;，称为**语法对象**(syntax object)。

<br/>

然后r被转换成#t，

第二行是REPL中打印了#t的值。

<br/>

为了处理转换器中匹配到的语法对象，

Scheme语言提供了syntax-case特殊形式。

<br/>

(define-syntax or

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(syntax-case x ()

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_) #'#f]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_ e) #'e]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_ e1 e2 e3 ...)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; #'(let ([t e1]) (if t t (or e2 e3 ...)))])))

<br/>

它使用了与syntax-rules相同的模式匹配规则，

不同的是，我们还需要**显式构造模板中的语法对象**。

<br/>

对于宏调用(or a b)来说，x的值是#&lt;syntax (or a b)&gt;，

**syntax-case会先求值x，然后解开语法对象的封装**，得到(or a b)，

再进行模式匹配。

<br/>

## **语法对象**

语法对象，包装了标识符的作用域信息。

<br/>

我们知道Scheme的宏是卫生的(hygienic)，

**宏展开后的标识符还处在其来源处的词法作用域中，**

为了达成这个目的，作用域信息就要被保存起来。

<br/>

Scheme的不同实现有不同的做法，

[Petite Chez Scheme](http://www.scheme.com/download/)使用了**语法对象**进行封装。

<br/>

语法对象由syntax特殊形式创建，(syntax e)

\#'e是它的简写，

在程序的读取阶段会被展开为(syntax e)。

<br/>

前文我们说，

“模式匹配构成了一门新的语言”，并不为过，

因为#'有很多规则(**坑**)需要我们了解。

<br/>

（1）出现在“模式”中的变量，称为模式变量(pattern variable)，

**模式变量的值是它匹配的值**。

例如：(_ a b)匹配(or x y)，a和b就是模式变量，a的值是x，b的值是y

<br/>

（2）#'e的值是一个语法对象，**e可以是模式变量也可以不是**。

如果e是模式变量，则值为#&lt;syntax e匹配的值&gt;，

如果e不是模式变量，则值为#&lt;syntax e&gt;。

<br/>

（3）“模板”中的模式变量，必须出现在#'或者#'(...)中，**不能裸写**。

[Pattern variables, however, can be referenced only within syntax expressions](http://www.scheme.com/tspl4/syntax.html#./syntax:h3)

<br/>

（4）**#'(a b)不是一个语法对象，而是由语法对象构成的列表**，(#'a #'b)

例如：[(_ a) #'(a b)]，结果是(#&lt;syntax a匹配的值&gt; #&lt;syntax b&gt;)

注意到b不是模式变量。

<br/>

（5）多层#'，读取器会先将每一层展开成(syntax ...)再求值。

例如：#'#'a实际上是(syntax (syntax a))，

求值为(#&lt;syntax syntax&gt; (#&lt;syntax syntax&gt; #&lt;syntax a匹配的值&gt;))。

注意到syntax不是模式变量。

<br/>

## **可以定义宏的宏**

syntax-rules是用来定义宏的，

然而，它也是一个宏，它最终被展开为syntax-case。

<br/>

(define-syntax syntax-rules

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(syntax-case x ()

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_ (i ...) ((keyword . pattern) template) ...)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; #'(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; (syntax-case x (i ...)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; [(_ . pattern) #'template] ...))])))

<br/>

syntax-rules的目的，

是为了避免显式的书写lambda和#'。

<br/>

像这种生成syntax-case的宏还有很多，

例如，with-syntax。

<br/>

(define-syntax with-syntax

&nbsp;&nbsp;&nbsp;&nbsp;(lambda (x)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(syntax-case x ()

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[(_ ((p e) ...) b1 b2 ...)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; #'(syntax-case (list e ...) ()

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; [(p ...) (let () b1 b2 ...)])])))

<br/>

with-syntax的目的，

是把匹配条件写在一起，

最后输出到一个模板中。

<br/>

从这里我们可以看到，

syntax-case第一个参数的值，

可以是语法对象的列表。

<br/>

syntax-case会对列表中的语法对象，

解除#&lt;syntax ...&gt;的封装，

然后再进行模式匹配。

<br/>

## **结语**

Lisp的宏非常强大，

很多人只是听说过，

没有切身使用过，

隐约觉得宏可以解决任何问题。

<br/>

其实不然，

**Lisp宏只是做了一些代码的变换，**

**简化了已完成功能的描述方式。**

<br/>

本文对Scheme宏的定义和使用做了简单介绍，

希望能揭开它的神秘面纱。
