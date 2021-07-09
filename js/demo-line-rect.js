let state = {
    pt1: {x: 50, y: 100},
    pt2: {x: 150, y: 200},
    mouse: {x: 0, y: 0},
    dragging: null,
}

let config = {
    elementId: 'demo-line-rect',
    bgShade: false,
    linePtRad: 6,
    markPtRad: 5,
    linePtPlain: 'rgb(0,0,0)',
    linePtHover: 'rgb(200,0,0)',
    dragMargin: 20,
    lrPixMargin: 100,
    tbPixMargin: 100,
    rectWidth: 2,
    lineWidth: 1.5
}

function draw() {
    let canvas = document.getElementById('demo-line-rect');
    if (canvas.getContext) {
        drawUnchecked(canvas);
    }
}

function ptClose(pt1, pt2, d) {
    let dx = pt1.x - pt2.x;
    let dy = pt1.y - pt2.y;
    let d2 = dx * dx + dy * dy;
    return d2 <= (d * d);
}

function drawUnchecked(canvas) {
    let ctx = canvas.getContext('2d');

    // clear background
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    // paint background debug shade
    if (config.bgShade) {
        ctx.fillStyle = 'rgba(0, 0, 0, 0.05)';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
    }

    // paint the line segment
    {
        let line = new Path2D();
        line.moveTo(state.pt1.x, state.pt1.y);
        line.lineTo(state.pt2.x, state.pt2.y);
        ctx.lineWidth = 1.5;
        ctx.stroke(line);
    }

    // paint the pixel square
    {
        let pxRect = new Path2D();
        pxRect.rect(
            config.lrPixMargin,
            config.tbPixMargin,
            canvas.width-2*config.lrPixMargin,
            canvas.height-2*config.tbPixMargin
        );
        ctx.lineWidth = 2.0;
        ctx.stroke(pxRect);
    }

    //-------------------------------------------------------------------------

    // compute the intersected area in the rectangle

    let pc = (state.pt1.x < state.pt2.x) ? state.pt1 : state.pt2;
    let qc = (state.pt1.x < state.pt2.x) ? state.pt2 : state.pt1;
    let p = canvasToPx(pc);
    let q = canvasToPx(qc);

    let m = (q.y - p.y) / (q.x - p.x);

    let p1 = null;
    if (p.x >= 1 || q.x <= 0) {
        p1 = {x: 0, y: 0};
    } else if (p.x >= 0) {
        p1 = p;
    } else {
        p1 = {x: 0, y: m * (-p.x) + p.y};
    }

    let q1 = null;
    if (p.x >= 1 || q.x <= 0) {
        q1 = {x: 0, y: 0};
    } else if (q.x <= 1) {
        q1 = q;
    } else {
        q1 = {x: 1, y: m * (1-p.x) + p.y};
    }

    let a = null;
    let b = null;
    if (p1.y < q1.y) {
        a = {x: p1.x, y: p1.y};
        b = {x: q1.x, y: q1.y};
    } else {
        a = {x: q1.x, y: q1.y};
        b = {x: p1.x, y: p1.y};
    }

    let a1 = null;
    if (b.y <= 0) {
        a1 = {x: b.x, y: b.y};
    } else if (a.y >= 1) {
        a1 = {x: a.x, y: 1};
    } else if (a.y >= 0) {
        a1 = {x: a.x, y: a.y};
    } else {
        a1 = {x: (-p1.y) / m + p1.x, y: 0};
    }

    let b1 = null;
    if (b.y <= 1 || a.y >= 1) {
        b1 = {x: b.x, y: Math.min(b.y, 1)};
    } else {
        b1 = {x: (1 - p1.y)/m + p1.x, y: 1};
    }

    let g = {x: b.x, y: 1};

    //-------------------------------------------------------------------------

    // draw extra points
    {
        let p1c = pxToCanvas(p1);
        let q1c = pxToCanvas(q1);
        let a1c = pxToCanvas(a1);
        let b1c = pxToCanvas(b1);
        let gc = pxToCanvas(g);
        let p1Circle = new Path2D();
        let q1Circle = new Path2D();
        let a1Circle = new Path2D();
        let b1Circle = new Path2D();
        let gCircle = new Path2D();
        p1Circle.arc(p1c.x, p1c.y, config.markPtRad, 0, 2*Math.PI);
        q1Circle.arc(q1c.x, q1c.y, config.markPtRad, 0, 2*Math.PI);
        a1Circle.arc(a1c.x, a1c.y, config.markPtRad, 0, 2*Math.PI);
        b1Circle.arc(b1c.x, b1c.y, config.markPtRad, 0, 2*Math.PI);
        gCircle.arc(gc.x, gc.y, config.markPtRad, 0, 2*Math.PI);
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.fill(p1Circle);
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.fill(q1Circle);
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.fill(a1Circle);
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.fill(b1Circle);
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.fill(gCircle);
    }

    // draw the "area region"
    {
        let h1 = pxToCanvas(a1);
        let h2 = pxToCanvas(b1);
        let h3 = pxToCanvas(g);
        let h4 = pxToCanvas({x: g.x, y: 0});
        let h5 = pxToCanvas({x: a1.x, y: 0});
        let ar = new Path2D();
        ar.moveTo(h1.x, h1.y);
        ar.lineTo(h2.x, h2.y);
        ar.lineTo(h3.x, h3.y);
        ar.lineTo(h4.x, h4.y);
        ar.lineTo(h5.x, h5.y);
        ar.closePath();
        ctx.fillStyle = 'rgba(128,0,128,0.2)';
        ctx.fill(ar);
    }

    // paint the circles at either end of the line segment
    {
        let pt1Circle = new Path2D();
        let pt2Circle = new Path2D();
        pt1Circle.arc(state.pt1.x, state.pt1.y, config.linePtRad, 0, 2*Math.PI);
        pt2Circle.arc(state.pt2.x, state.pt2.y, config.linePtRad, 0, 2*Math.PI);
        let pt1Fill =
            (ptClose(state.mouse, state.pt1, config.linePtRad) ||
             state.dragging == state.pt1)
            ? config.linePtHover
            : config.linePtPlain;
        let pt2Fill =
            (ptClose(state.mouse, state.pt2, config.linePtRad) ||
             state.dragging == state.pt2)
            ? config.linePtHover
            : config.linePtPlain;
        ctx.fillStyle = pt1Fill;
        ctx.fill(pt1Circle);
        ctx.fillStyle = pt2Fill;
        ctx.fill(pt2Circle);
    }

}

function canvasToPx(pt) {
    let canvas = document.getElementById(config.elementId);
    let pxWidth = canvas.width - 2*config.lrPixMargin;
    let pxHeight = canvas.height - 2*config.tbPixMargin;
    let x = (pt.x - config.lrPixMargin) / pxWidth;
    let y = 1.0 - (pt.y - config.tbPixMargin) / pxHeight;
    return {x: x, y: y};
}

function pxToCanvas(pt) {
    let canvas = document.getElementById(config.elementId);
    let pxWidth = canvas.width - 2*config.lrPixMargin;
    let pxHeight = canvas.height - 2*config.tbPixMargin;
    let x = pt.x * pxWidth + config.lrPixMargin;
    let y = (1.0 - pt.y) * pxHeight + config.tbPixMargin;
    return {x: x, y: y};
}

function requestRedraw() {
    window.requestAnimationFrame(draw);
}

function mousedown(event) {
    state.mouse.x = event.offsetX;
    state.mouse.y = event.offsetY;
    if (ptClose(state.mouse, state.pt1, config.linePtRad)) {
        state.dragging = state.pt1;
    } else if (ptClose(state.mouse, state.pt2, config.linePtRad)) {
        state.dragging = state.pt2;
    } else {
        state.dragging = null;
    }
    requestRedraw();
}

function mouseup(event) {
    state.dragging = null;
    requestRedraw();
}

function mousemove(event) {
    if (state.dragging) {
        let dx = event.offsetX - state.mouse.x;
        let dy = event.offsetY - state.mouse.y;
        let newx = state.dragging.x + dx;
        let newy = state.dragging.y + dy;
        let cp = clipToDragBounds({x: newx, y: newy});
        state.dragging.x = cp.x;
        state.dragging.y = cp.y;
    }
    state.mouse.x = event.offsetX;
    state.mouse.y = event.offsetY;
    requestRedraw();
}

function mouseleave(event) {
    state.dragging = null;
    requestRedraw();
}

function clipToDragBounds(pt) {
    let canvas = document.getElementById(config.elementId);
    let r = {x: pt.x, y: pt.y};
    let maxx = canvas.width - config.dragMargin;
    let maxy = canvas.height - config.dragMargin;
    r.x = (r.x < config.dragMargin) ? config.dragMargin : r.x;
    r.y = (r.y < config.dragMargin) ? config.dragMargin : r.y;
    r.x = (r.x > maxx) ? maxx : r.x;
    r.y = (r.y > maxy) ? maxy : r.y;
    return r;
}

function register() {
    let old_onload = window.onload;
    window.onload = function() {
        if (old_onload) {
            old_onload();
        }
        let canvas = document.getElementById(config.elementId);
        canvas.addEventListener('mousemove', mousemove);
        canvas.addEventListener('mousedown', mousedown);
        canvas.addEventListener('mouseup', mouseup);
        canvas.addEventListener('mouseleave', mouseleave);
        requestRedraw();
    }
}

if (window) {
    register();
}
