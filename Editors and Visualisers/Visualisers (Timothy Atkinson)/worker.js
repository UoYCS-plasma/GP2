importScripts("bower_components/viz.js/viz.js");
importScripts("gp2converter.js");

onmessage = function(e) {
	console.log(e.data.src);
  var result = Viz(convert_graph(e.data.src, "digraph", "G"), e.data.options);
  postMessage(result);
}
