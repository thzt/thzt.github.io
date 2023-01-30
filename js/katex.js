$(function () {
	var katex = window.katex;
	$('[data-katex]').each(function () {
		var $element = $(this),
			tex = $element.attr('data-katex');
		katex.render(tex, $element[0]);
	});
});
