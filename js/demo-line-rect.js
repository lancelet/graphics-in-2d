let state = {
    pt1: {x: 75, y: 100},
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
    lineWidth: 1.5,
    font: "21px STIXGeneral",
    subscriptFont: "14px STIXGeneral",
    fontOffset: 20,
    subscriptOffset: 6
}

let drawConfig = {
    draw_p: true,
    draw_q: true,
    draw_p1: false,
    draw_q1: false,
    draw_a: true,
    draw_b: true,
    draw_a1: true,
}

function draw() {
    let canvas = document.getElementById('demo-line-rect');
    if (canvas.getContext) {
        drawUnchecked(canvas, drawConfig);
    }
}

function ptClose(pt1, pt2, d) {
    let dx = pt1.x - pt2.x;
    let dy = pt1.y - pt2.y;
    let d2 = dx * dx + dy * dy;
    return d2 <= (d * d);
}

function drawUnchecked(canvas, dc) {
    let ctx = canvas.getContext('2d');

    // clear background
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    // paint background debug shade
    if (config.bgShade) {
        ctx.fillStyle = 'rgba(0, 0, 0, 0.05)';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
    }

    // paint axes
    {
        let ye = pxToCanvas({x: -0.2, y: 1.2});
        let or = pxToCanvas({x: -0.2, y: -0.2});
        let xe = pxToCanvas({x: 1.2, y: -0.2});
        let l = ye.x;
        let r = xe.x;
        let t = ye.y;
        let b = or.y;

        let axes = new Path2D();
        axes.moveTo(l, t);
        axes.lineTo(l, b);
        axes.lineTo(r, b);
        ctx.lineWidth = 2;
        ctx.stroke(axes);

        let ar = 12;

        let xarrow = new Path2D();
        xarrow.moveTo(r-ar, b-(ar/3));
        xarrow.lineTo(r, b);
        xarrow.lineTo(r-ar, b+(ar/3));
        ctx.stroke(xarrow);
        textBelow(ctx, r, b, 'x', '', false);

        let yarrow = new Path2D();
        yarrow.moveTo(l-(ar/3), t+ar);
        yarrow.lineTo(l, t);
        yarrow.lineTo(l+(ar/3), t+ar);
        ctx.stroke(yarrow);
        textLeft(ctx, l, t, 'y', '', false);
    }

    // paint u0, u1, v0, v1
    {
        let u0top = pxToCanvas({x: 0, y: 0});
        let u0bot = pxToCanvas({x: 0, y: -0.25});
        let u1top = pxToCanvas({x: 1, y: 0});
        let u1bot = pxToCanvas({x: 1, y: -0.25});
        let v0lef = pxToCanvas({x: -0.25, y: 0});
        let v0rig = pxToCanvas({x: 0, y: 0});
        let v1lef = pxToCanvas({x: -0.25, y: 1});
        let v1rig = pxToCanvas({x: 0, y: 1});
        let u0line = new Path2D();
        let u1line = new Path2D();
        let v0line = new Path2D();
        let v1line = new Path2D();
        u0line.moveTo(u0top.x, u0top.y);
        u0line.lineTo(u0bot.x, u0bot.y);
        u1line.moveTo(u1top.x, u1top.y);
        u1line.lineTo(u1bot.x, u1bot.y);
        v0line.moveTo(v0lef.x, v0lef.y);
        v0line.lineTo(v0rig.x, v0rig.y);
        v1line.moveTo(v1lef.x, v1lef.y);
        v1line.lineTo(v1rig.x, v1rig.y);
        ctx.lineWidth = 1;
        ctx.setLineDash([6, 3]);
        ctx.stroke(u0line);
        ctx.stroke(u1line);
        ctx.stroke(v0line);
        ctx.stroke(v1line);
        textBelow(ctx, u0bot.x, u0bot.y, 'u', '0', false);
        textBelow(ctx, u1bot.x, u1bot.y, 'u', '1', false);
        textLeft(ctx, v0lef.x, v0lef.y, 'v', '0', false);
        textLeft(ctx, v1lef.x, v1lef.y, 'v', '1', false);
    }

    // paint the line segment
    {
        let line = new Path2D();
        line.moveTo(state.pt1.x, state.pt1.y);
        line.lineTo(state.pt2.x, state.pt2.y);
        ctx.lineWidth = 1.5;
        ctx.setLineDash([]);
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
    //
    let u_0 = 0.0;
    let u_1 = 1.0;
    let v_0 = 0.0;
    let v_1 = 1.0;

    let pc = (state.pt1.x < state.pt2.x) ? state.pt1 : state.pt2;
    let qc = (state.pt1.x < state.pt2.x) ? state.pt2 : state.pt1;
    let p = canvasToPx(pc);
    let q = canvasToPx(qc);

    let m = (q.y - p.y) / (q.x - p.x);

    let p1 = null;
    if (p.x >= u_1 || q.x <= u_0) {
        p1 = {x: u_0, y: v_0};
    } else if (p.x >= u_0) {
        p1 = p;
    } else {
        p1 = {x: u_0, y: m * (u_0 - p.x) + p.y};
    }

    let q1 = null;
    if (p.x >= u_1 || q.x <= u_0) {
        q1 = {x: u_0, y: v_0};
    } else if (q.x <= u_1) {
        q1 = q;
    } else {
        q1 = {x: u_1, y: m * (u_1 - p.x) + p.y};
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
    if (b.y <= v_0) {
        a1 = {x: b.x, y: b.y};
    } else if (a.y >= v_1) {
        a1 = {x: a.x, y: v_1};
    } else if (a.y >= v_0) {
        a1 = {x: a.x, y: a.y};
    } else {
        a1 = {x: (v_0 - p1.y) / m + p1.x, y: v_0};
    }

    let b1 = null;
    if (a1.y >= v_1) {
        b1 = {x: a1.x, y: v_1};
    } else if (b.y <= v_1) {
        b1 = {x: b.x, y: b.y};
    } else {
        b1 = {x: (v_1 - p1.y) / m + p1.x, y: v_1};
    }

    let g = {x: b.x, y: 1};

    //-------------------------------------------------------------------------

    // draw extra points
    {
        let p1c = pxToCanvas(p1);
        let q1c = pxToCanvas(q1);
        let ac = pxToCanvas(a);
        let bc = pxToCanvas(b);
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
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.fill(b1Circle);
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.fill(gCircle);

        // label all the points
        if (dc.draw_p) {
            textBelow(ctx, pc.x, pc.y, 'p', '', true);
        }
        if (dc.draw_q) {
            textBelow(ctx, qc.x, qc.y, 'q', '', true);
        }
        if (dc.draw_p1) {
            textBelowRight(ctx, p1c.x, p1c.y, 'p', '1', true);
        }
        if (dc.draw_q1) {
            textBelowLeft(ctx, q1c.x, q1c.y, 'q', '1', true);
        }
        if (dc.draw_a) {
            textBelowRight(ctx, ac.x, ac.y, 'a', '', true);
        }
        if (dc.draw_b) {
            textBelowRight(ctx, bc.x, bc.y, 'b', '', true);
        }
        if (dc.draw_a1) {
            textBelowLeft(ctx, a1c.x, a1c.y, 'a', '1', true);
            ctx.fill(a1Circle);
        }
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

function textBelow(ctx, x, y, text, subscript, bold) {
    drawText(ctx, x, y+config.fontOffset, text, subscript, bold);
}

function textBelowRight(ctx, x, y, text, subscript, bold) {
    drawText(ctx, x+(config.fontOffset/1.5), y+config.fontOffset, text, subscript, bold);
}

function textBelowLeft(ctx, x, y, text, subscript, bold) {
    drawText(ctx, x-(config.fontOffset/1.1), y+config.fontOffset, text, subscript, bold);
}

function textLeft(ctx, x, y, text, subscript, bold) {
    drawText(ctx, x-config.fontOffset, y+(config.fontOffset/2), text, subscript, bold);
}

function drawText(ctx, x, y, text, subscript, bold) {
    ctx.fillStyle = 'rgb(0,0,0)';
    ctx.textAlign = 'center';
    if (bold) {
        ctx.font = 'bold ' + config.font;
    } else {
        ctx.font = 'italic ' + config.font;
    }
    ctx.fillText(text, x, y);
    ctx.font = config.subscriptFont;
    ctx.textAlign = 'left';
    ctx.fillText(subscript, x + config.subscriptOffset, y + 0.6 * config.subscriptOffset);
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
