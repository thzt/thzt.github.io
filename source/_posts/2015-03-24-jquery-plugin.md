---
layout: post
categories: Design
title: jQuery插件的把玩方式
description:  Web前端各种类库，插件扑面而来，怎样抵挡这些诱惑呢？
---

类库，框架，层出不穷。

Web前端，似乎经历着历史上已经发生过很多次的事情。

<br/>

很多人找不到自己的努力方向了，

Backbone，Ember，AngularJS，ReactJS，...

到底该学哪个？

<br/>

一阵阵枪林弹雨，扫的我们抬不起头来，

各大公司为了抢占资源，霸占程序员的时间，

总是宣传它们的东西是最好的。

<br/>

其实，我们不必太较真，

我们只需要抓住已有知识的精髓即可，

举一反三，用创造力改变世界。

<br/>

编程，就好像古时候写诗一样。

伟大的诗人，不一定会背所有的诗词。

他们的伟大之处在于借鉴。

<br/>

学习别人好的思想，拿来灵活运用，恰如其分的表达，

就足够了，

至于是否伟大，让后人去说吧。

<br/>

比如说，jQuery过时了吗，

前端各种MVC框架，MVVM框架，说的比什么都好听。

好像不用框架就没办法生存似的。

<br/>

作为一个学过辩证法的人，

我们马上就意识到了，这可能是有人在故意炒作它。

另一方面，我们也知道这些框架确实也有可取之处。

<br/>

要做到不骄不躁，

只有强大自身，这一个办法了。

<br/>

慢慢的，我们就发现了，

框架也只不过是别人活学活用的成果罢了。

<br/>

## **扩展性**

我们来学习一下jQuery的设计思想。

<br/>

JavaScript经常做的，

大概就是选取页面的元素，然后做一些事情了。

<br/>

而怎样选取元素呢，CSS已经给我们提供办法了，

那就是CSS选择器selector，

这样选中的其实是满足条件的HTML元素集合。

<br/>

如果能在集合上定义自己的操作就好了，

我们就可以把选中的集合整体看做一个对象，

调用这个对象的方法。

<br/>

这其实就是面向对象的思想呀。

<br/>

有了这个想法之后，我们先设计测试用例。

也就是说，我们要考虑，假如我们已经有了某个类库，

我们将如何使用它呢？

<br/>

这其实是编程活动中，最具创造力的环节。

也是自顶向下编程，或者测试驱动开发思想的一种应用。

<br/>

jQuery作者是这样设计的，

类库的使用者接口如下，

$(selector).method(parameter);

<br/>

类库的开发者接口如下，

扩展静态方法，

$.extend({

&nbsp;&nbsp;&nbsp;&nbsp;staticMethod:function(){}

});

<br/>

扩展实例方法，

$.prototype.extend({

&nbsp;&nbsp;&nbsp;&nbsp;instanceMethod:function(){}

});

<br/>

这种思路，很值得借鉴，

它说明，最灵活的设计，是那些只包含扩展方法的设计。

类库中所有的功能，都是用统一的方式扩展出来的。

<br/>

类库自带的功能越多，

以后修改这些功能的可能性就越大。

<br/>

## **实现**

至于如何实现，网上可以找到很多教程，

下面只写了一下我自己的实现。

<br/>

(function(global,document){

&nbsp;&nbsp;&nbsp;&nbsp;jQuery.prototype=InstanceCreation.prototype;

&nbsp;&nbsp;&nbsp;&nbsp;jQuery.extend=jQuery.prototype.extend=extend;

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;function jQuery(selector){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return new InstanceCreation(selector);

&nbsp;&nbsp;&nbsp;&nbsp;}

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;function InstanceCreation(selector){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;var instance=this;

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;instance[0]=document.querySelector(selector);

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return this;

&nbsp;&nbsp;&nbsp;&nbsp;}

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;function extend(material){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;var depository=this;

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;for(var property in material){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;if(!material.hasOwnProperty(property)){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;continue;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;}

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;depository[property]=material[property];

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;}

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return this;

&nbsp;&nbsp;&nbsp;&nbsp;};

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;//export:

&nbsp;&nbsp;&nbsp;&nbsp;global.$=jQuery;

}(window,document));

<br/>

对实现不详细描述，还有其他原因，

因为，实现是灵活的，而用户接口是用来表达思想的。

任何一种实现都能达到目的，

但并非所有人都能设计出具有表现力的用户接口。

<br/>

贴到这里，而不是放置Github超链接，

是为了有个直观的印象，jQuery核心其实很小。

所有其他功能都是扩展出来的。

<br/>

## **插件**

类似jQuery这样，

提供基础核心，再提供对核心的扩展方式，

称为插件式开发。

<br/>

我们只需要使用

$.prototype.extend({

&nbsp;&nbsp;&nbsp;&nbsp;pluginName:function(){}

});

<br/>

$(selector)就有了一个pluginName方法了。$(selector).pluginName

<br/>

正因为有这么便捷的功能，

jQuery插件的数量才惊人的多。

<br/>

我们还可以把页面切分为几个部分，

每个部分是一个插件对象，只对这一块元素进行操作，

这符合面向对象的思想。

<br/>

$(container1).pluginName1(...);

$(container1).pluginName2(...);

<br/>

我们把问题简化了，

对整个页面的控制，转换成对这些自定义插件的控制了。

<br/>

## **缺点和改进**

经过一段时间使用后，

我们发现jQuery插件有一个缺点，

$(container).pluginName(...);

<br/>

一个插件的所有操作，都必须放到同一个pluginName方法中。

如果我们想初始化和取值，就不得不这样操作，

$(container).pluginName('init',data);

$(container).pluginName('getValue');

<br/>

后果就是pluginName这个函数中，我们就要写很多switch语句。

$.prototype.extend({

&nbsp;&nbsp;&nbsp;&nbsp;pluginName:function(operationName,arg0){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;switch(operationName){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;case 'init':

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;//...

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;break;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;case 'getValue':

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;//...

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;break;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;//...

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;}

&nbsp;&nbsp;&nbsp;&nbsp;}

});

<br/>

还算清晰，

不过最致命的就是，如果我们想添加一个对插件的操作，

我们必须去改同一个文件，在switch中添加case子句。

这太烦人了。

<br/>

而且，取data的值也比较麻烦。

按理说它是init操作的第1个参数，

但是它实际上是作为pluginName的第2个参数传递的。

<br/>

我们为什么不能设计一个工具，来改变现状呢？

让我们踏上程序设计之路吧。

假如我们已经设计好了，想这样用。pluginManager.extend

<br/>

(function($){

&nbsp;&nbsp;&nbsp;&nbsp;$.pluginManager.extend('pluginName',{

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;init:initPlugin

&nbsp;&nbsp;&nbsp;&nbsp;});

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;function initPlugin(){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;var $selector=this;

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;//arguments[0]===data

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;//...

&nbsp;&nbsp;&nbsp;&nbsp;}

}(jQuery));

<br/>

(function($){

&nbsp;&nbsp;&nbsp;&nbsp;$.pluginManager.extend('pluginName',{

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;getValue:getValueFromPlugin

&nbsp;&nbsp;&nbsp;&nbsp;});

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;function getValueFromPlugin(){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;var $selector=this;

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;//...

&nbsp;&nbsp;&nbsp;&nbsp;}

}(jQuery));

<br/>

我们避免了为插件添加操作的麻烦，

还把data参数的位置调节好了。

并且将插件操作的具体实现，分离到了不同的文件中。

<br/>

这能实现吗？

可以的，我已经做好了。

但是，实现并不重要，关键是思想。

<br/>

## **持续改进**

并不是技术本身想要更新，

是创新的业务需要，促进了创新的技术出现，

不适应性越强烈，改进工具的可能性就越大。

<br/>

随着时间的发展，

我们发现了另一个问题。

<br/>

插件在使用过程中，可能会设置一些缺省值，

例如：初始化，

$(container).pluginName('init',data);

$(container).pluginName('init');

都是有可能的。

<br/>

每个插件中都单独处理缺省值，是很啰嗦的事情。

function initPlugin(){

&nbsp;&nbsp;&nbsp;&nbsp;var $selector=this,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;data=arguments[0]||'Hello';

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;//...
}

<br/>

啰嗦也就罢了，

最不能容忍的是，代码中出现了硬编码。

'Hello'是业务数据，跟插件本身无关。

<br/>

还记得吗，

类库自带的功能越多，

以后修改这些功能的可能性就越大。

<br/>

所以，我们得想一个办法把他们分离出去。pluginManager.filter

(function($){

&nbsp;&nbsp;&nbsp;&nbsp;$.pluginManager.extend('pluginName',{

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;init:initPlugin

&nbsp;&nbsp;&nbsp;&nbsp;});

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;function initPlugin(){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;var $selector=this,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;data=arguments[0];

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;//...

&nbsp;&nbsp;&nbsp;&nbsp;}

}(jQuery));

<br/>

(function($){

&nbsp;&nbsp;&nbsp;&nbsp;$.pluginManager.**filter**('pluginName',{

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;init:filterInit

&nbsp;&nbsp;&nbsp;&nbsp;});

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;function filterInit(){

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;var $selector=this;

<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return [

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;arguments[0]||'Hello'

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;];

&nbsp;&nbsp;&nbsp;&nbsp;}

}(jQuery));

<br/>

因此，我们增加了一层，

作为插件核心的开发者，接受所有需要配置的参数。

<br/>

作为实际插件的使用者，为了避免多次配置，

可以一次性为插件设置默认值。

filter将pluginName插件的init操作实际调用的实参，

转换一下，传递给了核心。

<br/>

这样的话，插件核心的开发者，就可以提供更松散的功能，

不用假设实际的使用情况，

实际和具体业务相关的处理，放到了filter里面。

<br/>

另外，我们看到filter是很灵活的，

它可以将任何实参，以任何方式转换成核心需要的实参，

哪怕核心需要的实参是一个函数。

<br/>

这能实现吗？

也是可以的，我也已经做好了。

但是，实现并不重要，关键是思想。

<br/>

## **结语**

我们分析了jQuery的设计思想，

并拿来随机应变，设计了自己的插件管理器。

<br/>

使用框架了吗？

并没有。

连jQuery核心也是自己写的。

<br/>

但是，在思考过程中，无处不闪烁着许多优秀框架的思想。

<br/>

因此，我们也应该知道了，

插件管理器本身，并没有什么，

关键是思考过程，以及我们想到什么样的办法解决实际问题。

<br/>

我们的所能掌握的知识，总是不够的。

但这不影响我们去创造好用的工具。

<br/>

比如，为了借鉴MVVM的思想，不必使用knockout.js，

我们可以自己写一个bindTemplate来进行模板的读写操作。

<br/>

设置值

$(container).bindTemplate('setData',{

&nbsp;&nbsp;&nbsp;&nbsp;attr:'data-model',

&nbsp;&nbsp;&nbsp;&nbsp;data:json,

&nbsp;&nbsp;&nbsp;&nbsp;set:setFieldValue

});

<br/>

读取值

$(container).bindTemplate('getData',{

&nbsp;&nbsp;&nbsp;&nbsp;attr:'data-model',

&nbsp;&nbsp;&nbsp;&nbsp;get:getFieldValue

});

<br/>

我们可以把json对象，绑定到HTML对象上，再从HTML中取回同样结构的json对象。

而与HTML元素的排列方式无关。

json=[

&nbsp;&nbsp;&nbsp;&nbsp;0,

&nbsp;&nbsp;&nbsp;&nbsp;{

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;val:1

&nbsp;&nbsp;&nbsp;&nbsp;}

];

<br/>

&lt;span data-model="[0]"&gt;&lt;/span&gt;

&lt;div>

&nbsp;&nbsp;&nbsp;&nbsp;&lt;span data-model="[1].val"&gt;&lt;/span&gt;

&lt;/div&gt;

<br/>

setFieldValue与getFieldValue我设置成了函数，

用来询问对于某个HTML元素，如何绑定值，如何获取值，

以获得最大的灵活性。

<br/>

并且，利用$.pluginManager.filter，接口可以简化成，

$(container).bindTemplate('setData',{

&nbsp;&nbsp;&nbsp;&nbsp;data:json

});

$(container).bindTemplate('getData');

<br/>

源代码如下：

[jQuery core](https://github.com/thzt/web.frontend.component/blob/master/library/jquerycore/jquerycore.js)

[plugin Manager](https://github.com/thzt/web.frontend.component/blob/master/library/pluginmanager/pluginmanager.js)

还好Github可以控制版本，如果以后文件更改了，还能找到今天的版本。

<br/>

于是，阳光还是那么灿烂，日子还是那么美好。

关键是生活的方式。









