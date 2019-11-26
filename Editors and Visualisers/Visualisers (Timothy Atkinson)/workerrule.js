importScripts("bower_components/viz.js/viz.js");
importScripts("gp2converter.js");

onmessage = function(e) {
	console.log(e.data.src);
  var result = Viz(convert_rule(e.data.src), e.data.options);
  postMessage(result);
}
