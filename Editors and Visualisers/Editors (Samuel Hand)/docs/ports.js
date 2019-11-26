app.ports.editorInit.subscribe(setupEditor);

app.ports.layoutGraph.subscribe(layoutGraph)

app.ports.findFit.subscribe(findFit);

function findFit(node)
{
    let svg = document.getElementById(node.svg);

    let point = svg.createSVGPoint();
    point.x = node.width;
    point.y = 0;

    let point2 = svg.createSVGPoint();
    point2.x = 0;
    point2.y = 0;

    let position = point.matrixTransform(svg.getScreenCTM().inverse());
    let origin = point2.matrixTransform(svg.getScreenCTM().inverse());
    let width = position.x - origin.x;

    app.ports.fitDone.send({id:node.id, width:width, svg:node.svg});
}


function layoutGraph(graph)
{
    var viz = new Viz();
    var renderOptions = {
        engine: "dot",
        format: "dot",
        yInvert: false,
        images: [],
        files: []
    };
    viz.renderString(graph, renderOptions)
        .then(dot => {
            app.ports.layoutDone.send(dot);
        });
}

function getPosition(e, svg)
{
    let position = {};

    let point = svg.createSVGPoint();
    point.x = e.clientX;
    point.y = e.clientY;
    position = point.matrixTransform(svg.getScreenCTM().inverse());

    return position;
}

function setupEditor(id)
{
    let svg = document.getElementById(id);

    svg.addEventListener("mousemove", (e) => {
        let position = getPosition(e, svg);
        let moveEvent = new CustomEvent("svgmousemove", { detail : position, bubbles : true });
        e.target.dispatchEvent(moveEvent);
    });

    svg.addEventListener("click", (e) => {
        let position = getPosition(e, svg);
        let clickEvent = new CustomEvent("svgclick", { detail : position, bubbles : true });
        e.target.dispatchEvent(clickEvent);
    });

    svg.addEventListener("dblclick", (e) => {
        let position = getPosition(e, svg);
        let dblclickEvent = new CustomEvent("svgdblclick", { detail : position, bubbles : true });
        e.target.dispatchEvent(dblclickEvent);
    });

    svg.addEventListener("mousedown", (e) => {
        let position = getPosition(e, svg);
        let downEvent;

        switch(e.button) {
            case 0:
                downEvent = new CustomEvent("svgleftdown", { detail : position, bubbles : true });
                break;

            case 2:
                downEvent = new CustomEvent("svgrightdown", { detail : position, bubbles : true });
                break;
        }

        if(downEvent != null)
            e.target.dispatchEvent(downEvent);
    });

    svg.addEventListener("mouseup", (e) => {
        let position = getPosition(e, svg);
        let upEvent;

        switch(e.button) {
            case 0:
                upEvent = new CustomEvent("svgleftup", { detail : position, bubbles : true });
                break;

            case 2:
                upEvent = new CustomEvent("svgrightup", { detail : position, bubbles : true });
                break;
        }

        if(upEvent != null)
            e.target.dispatchEvent(upEvent);
    });

    svg.addEventListener("contextmenu", (e) => {
        e.preventDefault();
    });
}
