"""
Build-a-decision-tree widget for Quarto, in the style of the lecture-4
class-separability plots.

Public function
---------------
decision_tree_builder_3d(df, feature_x, feature_y, feature_z, target, ...)

    Emits a self-contained interactive widget: a *fixed-shape* depth-2
    decision tree drawn as an editable figure (each internal node has a
    feature dropdown + a threshold box; each of the four leaves has a
    class dropdown), wired live to a 3D Plotly scatter of the data. As the
    student fills in the tree, the axis-aligned split planes are redrawn on
    the 3D plot, mistakes are highlighted, and a running accuracy is shown.

    The whole thing is client-side (Plotly.js + a little vanilla JS), so it
    drops straight into a static Quarto site -- no server, no Python at view
    time. This mirrors how boundary_plot_toggle_features ships its checkbox
    UI: precompute everything in Python, hand the browser a small JS app.

The tree shape is fixed (3 internal nodes, 4 leaves); only the *values*
inside it are the student's to fill in. That is exactly enough capacity to
solve this dataset's XOR-style rule (split on the binary feature, then on a
numeric one), which is the intended lesson.
"""

import json
import uuid

import numpy as np


# --------------------------------------------------------------------------
# Helpers (same spirit as the underscore helpers in the lecture module)
# --------------------------------------------------------------------------

# Plotly's default qualitative palette, hardcoded so this module doesn't need
# the Python plotly package (the rendered widget loads Plotly.js in-browser).
_PLOTLY_PALETTE = [
    "#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A",
    "#19D3F3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52",
]


def _default_color_map(categories):
    pal = _PLOTLY_PALETTE
    return {c: pal[i % len(pal)] for i, c in enumerate(categories)}


def _padded_range(lo, hi, pad_frac=0.05):
    lo, hi = float(lo), float(hi)
    span = hi - lo
    pad = (abs(lo) * pad_frac if lo != 0 else 1.0) if span == 0 else span * pad_frac
    return (lo - pad, hi + pad)


def _points_payload(groups, feature_x, feature_y, feature_z, target, color_map):
    """Per-class point cloud as plain JSON-able dicts for the browser."""
    return [
        {
            "name": str(c),
            "color": color_map[c],
            "x": g[feature_x].astype(float).tolist(),
            "y": g[feature_y].astype(float).tolist(),
            "z": g[feature_z].astype(float).tolist(),
        }
        for c, g in groups.items()
    ]


# --------------------------------------------------------------------------
# The widget
# --------------------------------------------------------------------------

def decision_tree_builder_3d(
    df,
    feature_x,
    feature_y,
    feature_z,
    target,
    title=None,
    color_map=None,
    default_tree=None,
    seed=None,
    save_path=None,
):
    """Interactive "fill in the decision tree" widget over a fixed depth-2 tree.

    Parameters
    ----------
    df : DataFrame with the three feature columns and the target column.
    feature_x, feature_y, feature_z : column names mapped to the 3D axes
        (and to feature indices 0, 1, 2 inside the tree editor, in that order).
    target : the class-label column.
    title : heading shown above the widget.
    color_map : optional {class_value: color}; defaults to the Plotly palette.
    default_tree : optional starting tree. Shape:
        {
          "nodes": {
            "0": {"feat": 0, "thr": 15.0, "left": "1",  "right": "2"},
            "1": {"feat": 1, "thr": 15.0, "left": "l0", "right": "l1"},
            "2": {"feat": 1, "thr": 15.0, "left": "l2", "right": "l3"},
          },
          "leaves": {"l0": <class>, "l1": <class>, "l2": <class>, "l3": <class>},
        }
        `feat` is the feature index (0=x, 1=y, 2=z). A point goes LEFT when
        feature <= thr, else RIGHT (same convention as sklearn). If omitted,
        a deliberately-imperfect default is used so there's something to fix.
    save_path : if given, a standalone .html file is written there (handy to
        embed in Quarto via an <iframe>). Otherwise an IPython HTML object is
        returned for inline display; if IPython isn't available the raw HTML
        string is returned.
    """
    categories = list(df[target].unique())
    color_map = color_map or _default_color_map(categories)
    groups = {c: df[df[target] == c] for c in categories}

    bounds = [
        (df[feature_x].min(), df[feature_x].max()),
        (df[feature_y].min(), df[feature_y].max()),
        (df[feature_z].min(), df[feature_z].max()),
    ]
    ranges = [list(_padded_range(lo, hi)) for lo, hi in bounds]

    # Per-feature metadata so the editor can show a yes/no choice for binary
    # features (exactly two distinct values) instead of a threshold box.
    def _fmt(v):
        return int(v) if float(v).is_integer() else round(float(v), 3)

    feature_names = [feature_x, feature_y, feature_z]
    feature_meta, feature_medians = [], []
    for nm in feature_names:
        vals = sorted(df[nm].dropna().unique().tolist())
        feature_medians.append(round(float(np.median(df[nm])), 3))
        if len(vals) == 2:
            v0, v1 = float(vals[0]), float(vals[1])
            labels = (["no", "yes"] if {v0, v1} == {0.0, 1.0}
                      else [str(_fmt(v0)), str(_fmt(v1))])
            feature_meta.append({"binary": True, "values": [v0, v1],
                                 "labels": labels, "mid": (v0 + v1) / 2})
        else:
            feature_meta.append({"binary": False})

    if default_tree is None:
        # Tempting-but-wrong start: splits only on the numeric features and
        # ignores the binary one, so the XOR structure scores ~50%. The
        # thresholds start at random spots within each feature's range (pass
        # `seed` to make that reproducible).
        rng = np.random.default_rng(seed)

        def rand_thr(i):
            lo, hi = bounds[i]
            return round(float(rng.uniform(lo, hi)), 1)

        default_tree = {
            "nodes": {
                "0": {"feat": 0, "thr": rand_thr(0), "left": "1", "right": "2"},
                "1": {"feat": 1, "thr": rand_thr(1), "left": "l0", "right": "l1"},
                "2": {"feat": 1, "thr": rand_thr(1), "left": "l2", "right": "l3"},
            },
            "leaves": {
                "l0": categories[0], "l1": categories[0],
                "l2": categories[-1], "l3": categories[-1],
            },
        }

    payload = {
        "points": _points_payload(
            groups, feature_x, feature_y, feature_z, target, color_map
        ),
        "featureNames": [feature_x, feature_y, feature_z],
        "featureLabels": [f.replace("_", " ") for f in
                          (feature_x, feature_y, feature_z)],
        "featureMeta": feature_meta,
        "featureMedians": feature_medians,
        "classes": [str(c) for c in categories],
        "ranges": ranges,
        "tree": default_tree,
    }

    uid = uuid.uuid4().hex[:8]
    base_title = title or "Build the decision tree"

    html = _TEMPLATE
    html = html.replace("__UID__", uid)
    html = html.replace("__TITLE__", base_title.replace("<", "&lt;"))
    html = html.replace("__PAYLOAD__", json.dumps(payload))

    if save_path is not None:
        with open(save_path, "w", encoding="utf-8") as f:
            f.write("<!doctype html><html><head><meta charset='utf-8'>"
                    "</head><body>" + html + "</body></html>")
        return save_path

    try:
        from IPython.display import HTML
        return HTML(html)
    except ImportError:
        return html


# --------------------------------------------------------------------------
# Self-contained HTML/JS template. Placeholders: __UID__, __TITLE__,
# __PAYLOAD__. All ids are namespaced by __UID__ so several widgets can live
# on one Quarto page.
# --------------------------------------------------------------------------

_TEMPLATE = r"""
<div id="tb-__UID__" class="tb-widget">
  <style>
    #tb-__UID__ { font-family: system-ui, -apple-system, "Segoe UI", sans-serif;
                  color: #1c2433; max-width: 920px; }
    #tb-__UID__ .tb-head { display:flex; align-items:baseline; gap:14px;
                  flex-wrap:wrap; margin-bottom:10px; }
    #tb-__UID__ .tb-title { font-size:1.15rem; font-weight:650; }
    #tb-__UID__ .tb-score { font-variant-numeric: tabular-nums; font-weight:600;
                  padding:2px 10px; border-radius:999px; background:#eef1f6; }
    #tb-__UID__ .tb-score b { font-weight:750; }
    #tb-__UID__ .tb-grid { display:grid; grid-template-columns: 1fr;
                  gap:8px; }
    @media (min-width: 760px) {
      #tb-__UID__ .tb-grid { grid-template-columns: 360px 1fr; }
    }
    #tb-__UID__ .tb-tree { position:relative; width:360px; height:430px;
                  margin:0 auto; }
    #tb-__UID__ .tb-tree svg { position:absolute; inset:0; width:100%;
                  height:100%; pointer-events:none; }
    #tb-__UID__ .node { position:absolute; box-sizing:border-box;
                  border:1.5px solid #9aa6b8; border-radius:10px;
                  background:#fff; padding:6px 8px; width:150px;
                  box-shadow:0 1px 2px rgba(20,30,50,.08); }
    #tb-__UID__ .node.leaf { width:120px; text-align:center;
                  border-style:dashed; }
    #tb-__UID__ .node .row { display:flex; gap:4px; margin-top:4px; }
    #tb-__UID__ .node select, #tb-__UID__ .node input {
                  font:inherit; font-size:.8rem; padding:2px 4px;
                  border:1px solid #c4ccd8; border-radius:6px; width:100%;
                  background:#fff; }
    #tb-__UID__ .node .cap { font-size:.66rem; letter-spacing:.04em;
                  text-transform:uppercase; color:#7a8699; }
    #tb-__UID__ .node input { width:64px; }
    #tb-__UID__ .node .binrow { font-size:.78rem; margin-top:6px;
                  color:#33405a; }
    #tb-__UID__ .node .binrow b { color:#1c2433; }
    #tb-__UID__ .edge-lab { position:absolute; font-size:.7rem;
                  color:#5b6678; background:#fff; padding:0 3px; }
    #tb-__UID__ .tb-controls { margin:6px 0 0; font-size:.82rem;
                  display:flex; gap:14px; align-items:center; flex-wrap:wrap; }
    #tb-__UID__ .tb-hint { font-size:.78rem; color:#69748a;
                  margin-top:6px; line-height:1.4; }
    #tb-__UID__ .plot { width:100%; height:430px; }
    #tb-__UID__ button.tb-reset { font:inherit; font-size:.78rem;
                  padding:3px 10px; border:1px solid #c4ccd8; border-radius:6px;
                  background:#f6f8fb; cursor:pointer; }
    #tb-__UID__ button.tb-reset:hover { background:#eef1f6; }
  </style>

  <div class="tb-head">
    <span class="tb-title">__TITLE__</span>
    <span class="tb-score">Accuracy: <b id="acc-__UID__">--</b>
      <span id="cnt-__UID__" style="color:#69748a;font-weight:500"></span></span>
    <button class="tb-reset" id="reset-__UID__">Reset tree</button>
  </div>

  <div class="tb-grid">
    <div>
      <div class="tb-tree" id="tree-__UID__"></div>
      <div class="tb-controls">
        <label><input type="checkbox" id="mis-__UID__" checked>
          Mark mistakes</label>
        <label><input type="checkbox" id="planes-__UID__" checked>
          Show split planes</label>
      </div>
      <div class="tb-hint">For a numeric feature, a point goes <b>left</b>
        when its value is <b>&le; threshold</b>, otherwise <b>right</b>. Pick a
        <b>yes/no</b> feature and the threshold turns into its two branches
        automatically.</div>
    </div>
    <div class="plot" id="plot-__UID__"></div>
  </div>
</div>

<script>
(function () {
  var DATA = __PAYLOAD__;
  var UID = "__UID__";
  var $ = function (id) { return document.getElementById(id + "-" + UID); };

  // ---- fixed depth-2 tree geometry (figure layout in px) -----------------
  // internal nodes: 0 (root), 1 (left), 2 (right); leaves: l0..l3
  var NODE = {
    "0": {x:105, y:8,   kind:"node"},
    "1": {x:18,  y:150, kind:"node"},
    "2": {x:192, y:150, kind:"node"},
    "l0":{x:5,   y:330, kind:"leaf"},
    "l1":{x:95,  y:330, kind:"leaf"},
    "l2":{x:185, y:330, kind:"leaf"},
    "l3":{x:275, y:330, kind:"leaf"}
  };
  var EDGES = [["0","1","L"],["0","2","R"],
               ["1","l0","L"],["1","l1","R"],
               ["2","l2","L"],["2","l3","R"]];
  var INTERNAL = ["0","1","2"], LEAVES = ["l0","l1","l2","l3"];

  // ---- build the editable tree figure ------------------------------------
  var tree = $("tree");
  var svgNS = "http://www.w3.org/2000/svg";
  var svg = document.createElementNS(svgNS, "svg");
  tree.appendChild(svg);

  function featOptions(sel) {
    return DATA.featureLabels.map(function (lab, i) {
      return '<option value="' + i + '"' + (i === sel ? " selected" : "") +
             ">" + lab + "</option>";
    }).join("");
  }
  function classOptions(sel) {
    return DATA.classes.map(function (c) {
      return '<option value="' + c + '"' + (c === sel ? " selected" : "") +
             ">" + c + "</option>";
    }).join("");
  }

  // Build element ids the same way $() reads them: "<name>-<UID>".
  function nid(name) { return name + "-" + UID; }
  function el(tag, attrs) {
    var e = document.createElement(tag);
    if (attrs) for (var a in attrs) {
      if (a === "class") e.className = attrs[a];
      else if (a === "id") e.id = attrs[a];
      else e[a] = attrs[a];
    }
    return e;
  }

  function buildNodes() {
    INTERNAL.forEach(function (k) {
      var n = DATA.tree.nodes[k], pos = NODE[k];
      var div = el("div", {"class": "node"});
      div.style.left = pos.x + "px"; div.style.top = pos.y + "px";

      var cap = el("div", {"class": "cap"}); cap.textContent = "if";

      var sel = el("select", {id: nid(k + "_feat")});
      sel.innerHTML = featOptions(n.feat);

      // numeric row:  [ <= ] [ threshold input ]
      var numrow = el("div", {"class": "row", id: nid(k + "_numrow")});
      var le = el("span", {"class": "cap"});
      le.style.alignSelf = "center"; le.innerHTML = "&le;";
      var thr = el("input", {id: nid(k + "_thr"), type: "number", step: "0.5"});
      thr.value = n.thr;
      numrow.appendChild(le); numrow.appendChild(thr);

      // binary row:  branches: <lab0> | <lab1>   (no threshold to type)
      var binrow = el("div", {"class": "binrow", id: nid(k + "_binrow")});
      binrow.style.display = "none";

      div.appendChild(cap);
      div.appendChild(sel);
      div.appendChild(numrow);
      div.appendChild(binrow);
      tree.appendChild(div);
    });

    LEAVES.forEach(function (k) {
      var pos = NODE[k];
      var div = el("div", {"class": "node leaf"});
      div.style.left = pos.x + "px"; div.style.top = pos.y + "px";
      var cap = el("div", {"class": "cap"}); cap.textContent = "predict";
      var sel = el("select", {id: nid(k)});
      sel.innerHTML = classOptions(DATA.tree.leaves[k]);
      div.appendChild(cap); div.appendChild(sel);
      tree.appendChild(div);
    });
  }

  // Switch a node between numeric (threshold) and binary (yes/no) UI based on
  // its selected feature. For a binary feature there's only one meaningful
  // split, so we hide the threshold box, pin the split at the midpoint, and
  // relabel the branches with the feature's two values. `resetValue` only
  // applies when the user actively changes the feature (so we don't clobber
  // the default tree's thresholds at startup).
  function applyNodeMode(k, resetValue) {
    var feat = +$(k + "_feat").value;
    var meta = DATA.featureMeta[feat];
    var numrow = $(k + "_numrow"), binrow = $(k + "_binrow"), thr = $(k + "_thr");
    if (meta && meta.binary) {
      numrow.style.display = "none";
      binrow.style.display = "block";
      binrow.innerHTML = "branches: <b>" + meta.labels[0] +
                         "</b> &nbsp;|&nbsp; <b>" + meta.labels[1] + "</b>";
      thr.value = meta.mid;            // split between the two values
    } else {
      binrow.style.display = "none";
      numrow.style.display = "flex";
      if (resetValue) thr.value = DATA.featureMedians[feat];
    }
  }

  function center(k) {
    var pos = NODE[k];
    var w = pos.kind === "leaf" ? 120 : 150;
    var h = pos.kind === "leaf" ? 52 : 78;
    return {x: pos.x + w / 2, y: pos.y, yb: pos.y + h, cx: pos.x + w / 2};
  }
  function drawEdges() {
    while (svg.firstChild) svg.removeChild(svg.firstChild);
    EDGES.forEach(function (e) {
      var a = center(e[0]), b = center(e[1]);
      var line = document.createElementNS(svgNS, "line");
      line.setAttribute("x1", a.cx); line.setAttribute("y1", a.yb);
      line.setAttribute("x2", b.cx); line.setAttribute("y2", b.y);
      line.setAttribute("stroke", "#9aa6b8");
      line.setAttribute("stroke-width", "1.5");
      svg.appendChild(line);
    });
  }

  // Labels depend on the live tree (binary -> value names; numeric -> the
  // threshold), so they're rebuilt on every render.
  var labelLayer = null;
  function updateEdgeLabels(t) {
    if (!labelLayer) { labelLayer = el("div"); tree.appendChild(labelLayer); }
    labelLayer.innerHTML = "";
    EDGES.forEach(function (e) {
      var parent = e[0], side = e[2];
      var n = t.nodes[parent];
      var meta = DATA.featureMeta[n.feat];
      var text;
      if (meta && meta.binary) {
        text = side === "L" ? meta.labels[0] : meta.labels[1];
      } else {
        var thr = (Math.round(n.thr * 100) / 100);
        text = (side === "L" ? "\u2264 " : "> ") + thr;
      }
      var a = center(parent), b = center(e[1]);
      var lab = el("div", {"class": "edge-lab"});
      lab.textContent = text;
      lab.style.left = ((a.cx + b.cx) / 2 - 10) + "px";
      lab.style.top = ((a.yb + b.y) / 2 - 8) + "px";
      labelLayer.appendChild(lab);
    });
  }

  // ---- read the tree from the inputs -------------------------------------
  function readTree() {
    var nodes = {};
    INTERNAL.forEach(function (k) {
      var src = DATA.tree.nodes[k];
      nodes[k] = {
        feat: +$(k + "_feat").value,
        thr: parseFloat($(k + "_thr").value),
        left: src.left, right: src.right
      };
    });
    var leaves = {};
    LEAVES.forEach(function (k) { leaves[k] = $(k).value; });
    return {nodes: nodes, leaves: leaves};
  }

  function predict(p, t) {
    var cur = "0";
    while (t.nodes[cur]) {
      var n = t.nodes[cur];
      cur = (p[n.feat] <= n.thr) ? n.left : n.right;
    }
    return t.leaves[cur];
  }

  // ---- split planes: one axis-aligned rectangle per internal split -------
  function splitSurfaces(t) {
    var rects = [];
    function recurse(k, box) {
      var n = t.nodes[k];
      if (!n) return;
      var f = n.feat, thr = n.thr;
      var others = [0, 1, 2].filter(function (i) { return i !== f; });
      var u = others[0], v = others[1];
      var u0 = box[u][0], u1 = box[u][1], v0 = box[v][0], v1 = box[v][1];
      var gu = [[u0, u1], [u0, u1]];
      var gv = [[v0, v0], [v1, v1]];
      var gf = [[thr, thr], [thr, thr]];
      var coords = [null, null, null];
      coords[f] = gf; coords[u] = gu; coords[v] = gv;
      rects.push({
        type: "surface", x: coords[0], y: coords[1], z: coords[2],
        showscale: false, opacity: 0.35,
        colorscale: [[0, "#ff7f0e"], [1, "#ff7f0e"]],
        hoverinfo: "skip", showlegend: false
      });
      var left = box.map(function (b) { return b.slice(); });
      var right = box.map(function (b) { return b.slice(); });
      left[f][1] = thr; right[f][0] = thr;
      recurse(n.left, left); recurse(n.right, right);
    }
    recurse("0", DATA.ranges.map(function (r) { return r.slice(); }));
    return rects;
  }

  // ---- point traces (true class colour) + mistake overlay ----------------
  function pointTraces(t, markMistakes) {
    var traces = DATA.points.map(function (grp) {
      return {
        type: "scatter3d", mode: "markers",
        x: grp.x, y: grp.y, z: grp.z, name: grp.name,
        marker: {size: 5, color: grp.color, opacity: 0.9,
                 line: {color: "black", width: 1}}
      };
    });
    if (markMistakes) {
      var mx = [], my = [], mz = [];
      DATA.points.forEach(function (grp) {
        for (var i = 0; i < grp.x.length; i++) {
          var p = [grp.x[i], grp.y[i], grp.z[i]];
          if (predict(p, t) !== grp.name) {
            mx.push(p[0]); my.push(p[1]); mz.push(p[2]);
          }
        }
      });
      if (mx.length) traces.push({
        type: "scatter3d", mode: "markers", x: mx, y: my, z: mz,
        name: "mistakes", hoverinfo: "skip",
        marker: {size: 11, symbol: "circle-open", color: "#d62728",
                 opacity: 1, line: {color: "#d62728", width: 3}}
      });
    }
    return traces;
  }

  function accuracy(t) {
    var ok = 0, tot = 0;
    DATA.points.forEach(function (grp) {
      for (var i = 0; i < grp.x.length; i++) {
        tot++;
        if (predict([grp.x[i], grp.y[i], grp.z[i]], t) === grp.name) ok++;
      }
    });
    return {ok: ok, tot: tot};
  }

  var LAYOUT = {
    margin: {l: 0, r: 0, t: 0, b: 0},
    scene: {
      xaxis: {title: {text: DATA.featureLabels[0]}, range: DATA.ranges[0]},
      yaxis: {title: {text: DATA.featureLabels[1]}, range: DATA.ranges[1]},
      zaxis: {title: {text: DATA.featureLabels[2]}, range: DATA.ranges[2]},
      aspectmode: "cube",
      camera: {eye: {x: 1.5, y: 1.5, z: 1.3}}
    },
    legend: {x: 0.01, y: 0.99, bgcolor: "rgba(255,255,255,0.5)"},
    showlegend: true
  };

  function render() {
    var t = readTree();
    updateEdgeLabels(t);
    var data = pointTraces(t, $("mis").checked);
    if ($("planes").checked) data = data.concat(splitSurfaces(t));
    Plotly.react($("plot"), data, LAYOUT, {displayModeBar: false,
                                           responsive: true});
    var a = accuracy(t);
    var pct = (100 * a.ok / a.tot).toFixed(1);
    $("acc").textContent = pct + "%";
    $("cnt").textContent = "(" + a.ok + " / " + a.tot + " correct)";
  }

  function wire() {
    INTERNAL.forEach(function (k) {
      $(k + "_feat").addEventListener("change", function () {
        applyNodeMode(k, true); render();
      });
      $(k + "_thr").addEventListener("input", render);
    });
    LEAVES.forEach(function (k) { $(k).addEventListener("change", render); });
    $("mis").addEventListener("change", render);
    $("planes").addEventListener("change", render);
    $("reset").addEventListener("click", function () {
      INTERNAL.forEach(function (k) {
        $(k + "_feat").value = DATA.tree.nodes[k].feat;
        $(k + "_thr").value = DATA.tree.nodes[k].thr;
        applyNodeMode(k, false);
      });
      LEAVES.forEach(function (k) { $(k).value = DATA.tree.leaves[k]; });
      render();
    });
  }

  function init() {
    buildNodes();
    drawEdges();
    INTERNAL.forEach(function (k) { applyNodeMode(k, false); });
    wire();
    render();
  }

  // ---- ensure Plotly is loaded, then go ----------------------------------
  if (window.Plotly) { init(); return; }
  var existing = document.getElementById("plotly-cdn-loader");
  if (existing) { existing.addEventListener("load", init); return; }
  var s = document.createElement("script");
  s.id = "plotly-cdn-loader";
  s.src = "https://cdn.plot.ly/plotly-2.35.2.min.js";
  s.onload = init;
  document.head.appendChild(s);
})();
</script>
"""
