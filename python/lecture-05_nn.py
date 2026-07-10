"""
Crisp, modern animation of a neural network being trained (backpropagation),
shown as a clear sequence of phases with pauses in between:

    1. forward pass  (a highlight sweeps left -> right to the output)
    2. pause
    3. compare       (an accuracy % appears at the output)
    4. pause
    5. adjust        (a highlight sweeps right -> left; weight numbers change)
    6. pause  -> next round, looping back to a fresh random start.

It is a vector SVG driven by a little vanilla JavaScript, generated from
Python.
"""

import json
import numpy as np
from IPython.display import HTML

# 4 input "pixels" -> two small hidden layers -> 1 output.
LAYERS = [4, 3, 3, 1]
K = 4                      # weight snapshots -> (K-1) training rounds shown
W, H = 800, 360            # SVG coordinate space

# phase durations in milliseconds
PH = dict(fwd=1300, p1=450, cmp=850, p2=450, adj=1100, p3=550, reset=900)

# accuracy shown per round (rises, ends close to but not at 100%)
ACC = [0.24, 0.62, 0.95]


def _xs():
    return list(np.linspace(90, W - 150, len(LAYERS)))  # room at right for %


def _positions():
    pos = []
    for x, n in zip(_xs(), LAYERS):
        vspace = 66
        ys = H / 2 + (np.arange(n) - (n - 1) / 2) * vspace
        pos.append([(float(x), float(y)) for y in ys])
    return pos


def _radius(li):
    if li == 0:
        return 17
    if li == len(LAYERS) - 1:
        return 20
    return 15


def _keyframes():
    rng = np.random.default_rng(7)
    gaps = list(zip(LAYERS[:-1], LAYERS[1:]))
    kf = [[None] * len(gaps) for _ in range(K)]
    for g, (s, t) in enumerate(gaps):
        start = rng.uniform(-0.85, 0.85, size=(s, t))
        end = rng.uniform(-0.85, 0.85, size=(s, t))
        for k in range(K):
            u = k / (K - 1)
            noise = rng.uniform(-1, 1, size=(s, t)) * 0.45 * (1 - u)
            kf[k][g] = np.clip(start * (1 - u) + end * u + noise, -0.97, 0.97)
    return [[np.round(m, 2).tolist() for m in layer] for layer in kf]


def _tcolor(w):
    return "#047857" if w >= 0 else "#be123c"


def _ecolor(w):
    return "#10b981" if w >= 0 else "#f43f5e"


def _build_svg(pos, kf0):
    gaps = list(zip(LAYERS[:-1], LAYERS[1:]))
    edges, chips, nodes = [], [], []

    for g, (s, t) in enumerate(gaps):
        for i in range(s):
            x1, y1 = pos[g][i]
            for j in range(t):
                x2, y2 = pos[g + 1][j]
                w = kf0[g][i][j]
                aw = abs(w)
                edges.append(
                    f'<line id="e_{g}_{i}_{j}" x1="{x1:.1f}" y1="{y1:.1f}" '
                    f'x2="{x2:.1f}" y2="{y2:.1f}" stroke="{_ecolor(w)}" '
                    f'stroke-width="{1 + 4 * aw:.2f}" '
                    f'stroke-opacity="{0.20 + 0.55 * aw:.2f}" '
                    f'stroke-linecap="round"/>'
                )
                frac = 0.32 + 0.38 * (j / max(1, t - 1))
                lx = x1 + frac * (x2 - x1)
                ly = y1 + frac * (y2 - y1)
                chips.append(
                    f'<text id="w_{g}_{i}_{j}" class="wt" x="{lx:.1f}" '
                    f'y="{ly:.1f}" fill="{_tcolor(w)}">{w:+.2f}</text>'
                )

    for li, layer in enumerate(pos):
        r = _radius(li)
        is_out = (li == len(LAYERS) - 1)
        ring = "#6366f1" if is_out else "#94a3b8"
        rw = 2.2 if is_out else 1.5
        for k, (x, y) in enumerate(layer):
            if is_out:
                nodes.append(
                    f'<circle class="pulse" cx="{x:.1f}" cy="{y:.1f}" r="{r}" '
                    f'fill="none" stroke="#6366f1"/>'
                )
            nodes.append(
                f'<circle id="n_{li}_{k}" data-r="{r}" data-ring="{ring}" '
                f'cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="url(#nodeFill)" '
                f'stroke="{ring}" stroke-width="{rw}" filter="url(#soft)"/>'
            )
            if is_out:
                nodes.append(
                    f'<circle id="outfill" cx="{x:.1f}" cy="{y:.1f}" '
                    f'r="{r - 2}" fill="#6366f1" opacity="0"/>'
                )

    # accuracy readout to the right of the output node
    ox, oy = pos[-1][0]
    cx = ox + 82
    accuracy = (
        f'<g id="acc" opacity="0">'
        f'<text id="acc_val" class="acc-val" x="{cx}" y="{oy - 2}">62%</text>'
        f'<text class="acc-lbl" x="{cx}" y="{oy + 22}">accuracy</text>'
        f'</g>'
    )

    svg = (
        f'<svg id="__ID___svg" viewBox="0 0 {W} {H}" '
        f'xmlns="http://www.w3.org/2000/svg" class="net">'
        '<defs>'
        '<radialGradient id="nodeFill" cx="38%" cy="34%" r="80%">'
        '<stop offset="0%" stop-color="#ffffff"/>'
        '<stop offset="100%" stop-color="#eef2f7"/>'
        '</radialGradient>'
        '<filter id="soft" x="-40%" y="-40%" width="180%" height="180%">'
        '<feDropShadow dx="0" dy="1.5" stdDeviation="2.2" '
        'flood-color="#0f172a" flood-opacity="0.18"/>'
        '</filter>'
        '</defs>'
        f'<g>{"".join(edges)}</g>'
        f'<g>{"".join(nodes)}</g>'
        f'{accuracy}'
        f'<g>{"".join(chips)}</g>'
        '</svg>'
    )
    return svg


_TEMPLATE = """
<div class="nnwrap">
  <style>
    .nnwrap{
      --accent:#6366f1;
      max-width:680px;margin:0 auto;padding:18px 18px 14px;
      background:linear-gradient(180deg,#ffffff,#f8fafc);
      border:1px solid #e5e9f0;border-radius:18px;
      box-shadow:0 10px 30px -12px rgba(15,23,42,.25);
      font-family:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;
    }
    .nnwrap .net{width:100%;height:auto;display:block;}
    .nnwrap .wt{
      font-size:12px;font-weight:600;text-anchor:middle;
      dominant-baseline:central;font-variant-numeric:tabular-nums;
      paint-order:stroke;stroke:#f8fafc;stroke-width:3px;
      stroke-linejoin:round;pointer-events:none;
    }
    .nnwrap .acc-val{
      font-size:26px;font-weight:700;text-anchor:middle;
      dominant-baseline:central;font-variant-numeric:tabular-nums;
      fill:#475569;
    }
    .nnwrap .acc-lbl{
      font-size:11px;font-weight:600;text-anchor:middle;
      dominant-baseline:central;fill:#64748b;letter-spacing:2px;
    }
    .nnwrap .pulse{transform-box:fill-box;transform-origin:center;
      animation:nnpulse 2.6s ease-in-out infinite;}
    @keyframes nnpulse{
      0%,100%{opacity:.5;transform:scale(1);}
      50%{opacity:0;transform:scale(1.45);}
    }
    .nnbar{display:flex;align-items:center;gap:14px;margin-top:6px;}
    .nnbtn{
      width:46px;height:46px;flex:0 0 auto;border:none;cursor:pointer;
      border-radius:50%;background:var(--accent);color:#fff;
      display:grid;place-items:center;
      box-shadow:0 6px 16px -4px rgba(99,102,241,.6);
      transition:transform .15s ease,background .15s ease,box-shadow .15s ease;
    }
    .nnbtn:hover{background:#4f46e5;transform:translateY(-1px);
      box-shadow:0 8px 20px -4px rgba(79,70,229,.7);}
    .nnbtn:active{transform:translateY(0) scale(.96);}
    .nnbtn svg{width:20px;height:20px;fill:#fff;}
    .nntrack{flex:1;height:6px;border-radius:99px;background:#e5e9f0;
      overflow:hidden;}
    .nnfill{height:100%;width:0%;border-radius:99px;
      background:linear-gradient(90deg,#818cf8,#6366f1);}
    @media (prefers-reduced-motion:reduce){.nnwrap .pulse{animation:none;}}
  </style>

  __SVG__

  <div class="nnbar">
    <button class="nnbtn" id="__ID___btn" aria-label="Play animation">
      <svg id="__ID___play" viewBox="0 0 24 24"><path d="M8 5v14l11-7z"/></svg>
      <svg id="__ID___pause" viewBox="0 0 24 24" style="display:none">
        <path d="M6 5h4v14H6zM14 5h4v14h-4z"/></svg>
    </button>
    <div class="nntrack"><div class="nnfill" id="__ID___fill"></div></div>
  </div>

  <script>(function(){
    var DATA=__DATA__;
    var root=document.currentScript.parentNode;
    var q=function(id){return root.querySelector("#"+id);};
    var btn=q("__ID___btn"), icoPlay=q("__ID___play"),
        icoPause=q("__ID___pause"), fill=q("__ID___fill"),
        svg=q("__ID___svg"), accEl=q("acc"), accVal=q("acc_val"),
        outfill=q("outfill");

    var L=DATA.layers, KF=DATA.kf, XS=DATA.xs, PH=DATA.ph, ACC=DATA.acc;
    var iters=KF.length-1;
    var segs=[["fwd",PH.fwd],["p1",PH.p1],["cmp",PH.cmp],
              ["p2",PH.p2],["adj",PH.adj],["p3",PH.p3]];
    var iterDur=segs.reduce(function(a,s){return a+s[1];},0);
    var total=iters*iterDur+PH.reset;
    var minX=XS[0]-30, maxX=XS[XS.length-1]+30, BAND=135;
    var outX=XS[XS.length-1];

    function cl(t){return t<0?0:(t>1?1:t);}
    function ease(t){return t*t*(3-2*t);}
    function lerp(a,b,t){return a+(b-a)*t;}
    function ec(w){return w>=0?"#10b981":"#f43f5e";}
    function tc(w){return w>=0?"#047857":"#be123c";}
    function fmt(w){return (w>=0?"+":"")+w.toFixed(2);}
    function h2(x){return ("0"+x.toString(16)).slice(-2);}
    function hexLerp(a,b,t){a=a.slice(1);b=b.slice(1);var o="";
      for(var k=0;k<3;k++){var av=parseInt(a.substr(k*2,2),16),
        bv=parseInt(b.substr(k*2,2),16);
        o+=h2(Math.round(av+(bv-av)*t));}return "#"+o;}
    function interp(A,B,f){var R=[];for(var g=0;g<A.length;g++){var m=[];
      for(var i=0;i<A[g].length;i++){var r=[];
        for(var j=0;j<A[g][i].length;j++){
          r.push(A[g][i][j]+(B[g][i][j]-A[g][i][j])*f);}m.push(r);}
      R.push(m);}return R;}

    var playing=false, raf=null, startT=null, lastTs=0, elapsed=0;

    function draw(ms){
      var t=ms%total;
      fill.style.width=(t/total*100).toFixed(1)+"%";

      var e, phase, lp, dim=1;
      if(t<iters*iterDur){
        e=Math.floor(t/iterDur); var lt=t-e*iterDur, acc=0;
        for(var s=0;s<segs.length;s++){
          if(lt<acc+segs[s][1]){phase=segs[s][0];lp=(lt-acc)/segs[s][1];break;}
          acc+=segs[s][1];
        }
      } else { phase="reset"; e=iters; lp=(t-iters*iterDur)/PH.reset; }

      // current weights
      var curW;
      if(phase==="adj") curW=interp(KF[e],KF[e+1],ease(lp));
      else if(phase==="p3") curW=KF[e+1];
      else if(phase==="reset"){curW=interp(KF[iters],KF[0],ease(lp));
        dim=0.35+0.65*Math.abs(2*lp-1);}
      else curW=KF[e];

      // traveling highlight
      var flowX=null;
      if(phase==="fwd") flowX=lerp(minX,maxX,ease(lp));
      else if(phase==="adj") flowX=lerp(maxX,minX,ease(lp));

      // accuracy value + reveal (shown only once the signal reaches output)
      var eIdx=Math.min(e,ACC.length-1);
      var accCur=(phase==="reset")?ACC[ACC.length-1]:ACC[eIdx];

      var vis=0, outReveal=0;
      if(phase==="fwd"){
        outReveal=cl((flowX-outX)/50);
        vis=(flowX>=outX)?1:0;          // appears when the signal arrives
      }
      else if(phase==="reset"){vis=0; outReveal=0;}
      else {vis=1; outReveal=1;}

      svg.style.opacity=dim.toFixed(3);

      // edges + weight labels
      for(var g=0;g<L.length-1;g++){
        var mx=(XS[g]+XS[g+1])/2;
        var hlE=(flowX===null)?0:cl(1-Math.abs(mx-flowX)/BAND);
        for(var i=0;i<L[g];i++){
          for(var j=0;j<L[g+1];j++){
            var w=curW[g][i][j], aw=Math.abs(w);
            var e_=q("e_"+g+"_"+i+"_"+j);
            var col=ec(w); if(hlE>0.02) col=hexLerp(col,"#6366f1",0.75*hlE);
            e_.setAttribute("stroke",col);
            e_.setAttribute("stroke-width",(1+4*aw+2*hlE).toFixed(2));
            e_.setAttribute("stroke-opacity",Math.min(1,0.20+0.55*aw+0.4*hlE).toFixed(2));
            var tx=q("w_"+g+"_"+i+"_"+j);
            tx.setAttribute("fill",tc(w));
            tx.textContent=fmt(w);
            tx.style.fontSize=(12+3*hlE).toFixed(1)+"px";
          }
        }
      }

      // nodes
      for(var l=0;l<L.length;l++){
        var nx=XS[l];
        var hlN=(flowX===null)?0:cl(1-Math.abs(nx-flowX)/BAND);
        for(var k=0;k<L[l];k++){
          var nd=q("n_"+l+"_"+k); if(!nd) continue;
          var br=parseFloat(nd.getAttribute("data-r"));
          var ring=nd.getAttribute("data-ring");
          nd.setAttribute("r",(br+2.5*hlN).toFixed(2));
          nd.setAttribute("stroke",hlN>0.02?hexLerp(ring,"#6366f1",hlN):ring);
        }
      }

      // output prediction tint
      outfill.setAttribute("opacity",(outReveal*(0.12+0.5*accCur)).toFixed(3));

      // accuracy readout
      accEl.setAttribute("opacity",vis.toFixed(3));
      accVal.textContent=Math.round(accCur*100)+"%";
      accVal.setAttribute("fill",hexLerp("#475569","#059669",cl(accCur)));
      var fs=26; if(phase==="cmp") fs=26+2*(0.5+0.5*Math.sin(lp*Math.PI*2));
      accVal.style.fontSize=fs.toFixed(1)+"px";
    }

    function frame(ts){
      if(startT===null) startT=ts;
      lastTs=ts; draw(elapsed+(ts-startT));
      raf=requestAnimationFrame(frame);
    }
    function play(){
      if(playing) return; playing=true; startT=null;
      icoPlay.style.display="none"; icoPause.style.display="";
      btn.setAttribute("aria-label","Pause animation");
      raf=requestAnimationFrame(frame);
    }
    function pause(){
      if(!playing) return; playing=false;
      if(raf) cancelAnimationFrame(raf);
      if(startT!==null) elapsed+=(lastTs-startT); startT=null;
      icoPlay.style.display=""; icoPause.style.display="none";
      btn.setAttribute("aria-label","Play animation");
    }
    btn.addEventListener("click",function(){ playing?pause():play(); });

    if("IntersectionObserver" in window){
      new IntersectionObserver(function(es){
        es.forEach(function(en){ if(!en.isIntersecting && playing) pause(); });
      },{threshold:0.12}).observe(root);
    }

    draw(0);
  })();</script>
</div>
"""


def run():
    pos = _positions()
    kf = _keyframes()
    svg = _build_svg(pos, kf[0])
    uid = "nn7"

    data = json.dumps({
        "layers": LAYERS, "kf": kf, "xs": [round(float(x), 1) for x in _xs()],
        "ph": PH, "acc": ACC,
    })

    html = (
        _TEMPLATE
        .replace("__SVG__", svg)
        .replace("__DATA__", data)
        .replace("__ID__", uid)
    )
    return HTML(html)
