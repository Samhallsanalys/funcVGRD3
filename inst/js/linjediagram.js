/* vgr_D3 — linjediagram (func_d3_linjediagram).
 *
 * All x-data kommer som tal (datum = ms sedan 1970, kategorier = position 0..n-1) så att en
 * linjär skala räcker för alla typer. Hela SVG:en byggs om vid varje tillståndsändring (läge,
 * dolda grupper, zoom) — det är enkelt och snabbt nog för några hundra linjer. Hover (hårkors,
 * markering) ritas i ett eget lager ovanpå utan ombyggnad.
 */
(function () {
  "use strict";
  var vgrD3 = window.vgrD3;
  var clamp = vgrD3.clamp;
  function pt(v) { return v * 96 / 72; }
  var LINES = pt(11 * 1.2);   // ggplot2-enheten "lines" (11 pt text × radavstånd 1,2)

  vgrD3.renderers.linjediagram = function (el, spec) {
    var W = spec.width, H = spec.height, font = spec.font;
    var T = spec.text, X = spec.x, L = spec.legend, LB = spec.labels, TT = spec.tooltip;
    var IA = spec.interactive, LN = spec.line, F = spec.facets;
    var dur = spec.animation.duration || 0;
    var esc = vgrD3.esc;
    var fileName = vgrD3.slug((T.title && T.title.join(" ")) || "linjediagram");

    var root = d3.select(el).style("max-width", W + "px");
    var controls = vgrD3.controls(el, font);
    var svg = root.append("svg").attr("class", "vgr-d3-svg")
      .attr("width", W).attr("height", H).attr("viewBox", "0 0 " + W + " " + H)
      .attr("role", "img").attr("aria-label", (T.title && T.title.join(" ")) || "Linjediagram")
      .attr("font-family", font)
      .style("width", "100%").style("height", "auto").style("display", "block");
    var tip = vgrD3.tooltip(el, font);

    if (spec.empty) {
      svg.append("text").attr("x", W / 2).attr("y", H / 2).attr("text-anchor", "middle")
        .style("font-size", (18 * (spec.text_scale || 1)) + "px").text("Inga data att visa");
      return;
    }

    var groups = spec.groups;
    var groupByKey = {};
    groups.forEach(function (g) { groupByKey[g.key] = g; });
    var hlSet = {};
    spec.highlight.groups.forEach(function (g) { hlSet[g] = true; });
    var hasFacets = !(F.levels.length === 1 && F.levels[0] === "__alla");

    var state = { mode: "varde", hidden: {}, xDomain: null, hoverGroup: null, first: true };

    // -------------------------------------------------------------------------
    //  Index/förändring: basvärde per serie
    // -------------------------------------------------------------------------
    spec.series.forEach(function (s) {
      s.base = null;
      if (IA.index_base != null) {
        // Exakt x-värde om det finns, annars närmaste x i serien (så linjerna inte försvinner)
        var i = s.x.indexOf(IA.index_base);
        if (i < 0) {
          var best = Infinity;
          s.x.forEach(function (x, k) {
            if (s.y[k] != null && s.y[k] !== 0 && Math.abs(x - IA.index_base) < best) { best = Math.abs(x - IA.index_base); i = k; }
          });
        }
        if (i >= 0 && s.y[i] != null && s.y[i] !== 0) s.base = s.y[i];
      } else {
        for (var k = 0; k < s.y.length; k++) if (s.y[k] != null && s.y[k] !== 0) { s.base = s.y[k]; break; }
      }
    });
    function tr(v, s) {
      if (v == null) return null;
      if (state.mode === "varde") return v;
      if (s.base == null) return null;
      return state.mode === "index" ? v / s.base * 100 : (v / s.base - 1) * 100;
    }
    // Index och förändring (%) har 1 decimal om inte tooltip_decimals är angivet
    function modeDecimals() { return state.mode === "varde" ? TT.decimals : (TT.set_decimals != null ? TT.set_decimals : 1); }
    function modeUnit() { return state.mode === "varde" ? TT.unit : (state.mode === "forandring" ? " %" : ""); }
    function fmtV(v, dec) { return vgrD3.fmt(v, dec == null ? modeDecimals() : dec, " "); }

    function isVisible(s) { return !state.hidden[s.group]; }
    // dataDomain: vilka punkter som visas (x_axis_min/max, inzoomning). xDomain: skalans domän = dataDomain plus
    // eventuellt etikettutrymme (labelDomain) — utrymmet får inte släppa in punkter utanför gränserna
    function dataDomain() { return state.xDomain || X.domain; }
    function xDomain() { return state.xDomain || state.labelDomain || X.domain; }
    function inX(x) { var d = dataDomain(); return x >= d[0] && x <= d[1]; }
    function seriesIn(facet) { return spec.series.filter(function (s) { return s.facet === facet; }); }
    function seriesOf(facet, group) {
      for (var i = 0; i < spec.series.length; i++) {
        var s = spec.series[i];
        if (s.facet === facet && s.group === group) return s;
      }
      return null;
    }

    function xText(x) {
      if (X.type === "discrete") return X.levels[x];
      for (var i = 0; i < spec.series.length; i++) {
        var k = spec.series[i].x.indexOf(x);
        if (k >= 0) return spec.series[i].x_text[k];
      }
      return X.type === "date" ? vgrD3.timeLocale.utcFormat("%Y-%m-%d")(new Date(x)) : vgrD3.fmt(x, 0, null);
    }

    // -------------------------------------------------------------------------
    //  Skalor och axlar
    // -------------------------------------------------------------------------
    function yDomainFor(facets) {
      var lo = Infinity, hi = -Infinity;
      spec.series.forEach(function (s) {
        if (facets.indexOf(s.facet) < 0 || !isVisible(s)) return;
        for (var i = 0; i < s.x.length; i++) {
          if (!inX(s.x[i])) continue;
          [s.y[i], s.lo ? s.lo[i] : null, s.hi ? s.hi[i] : null].forEach(function (raw) {
            var v = tr(raw, s);
            if (v != null && isFinite(v)) { lo = Math.min(lo, v); hi = Math.max(hi, v); }
          });
        }
      });
      refY().forEach(function (v) { lo = Math.min(lo, v); hi = Math.max(hi, v); });
      if (!isFinite(lo)) { lo = 0; hi = 1; }
      if (state.mode === "varde") {
        if (spec.y.min != null) lo = spec.y.min;
        if (spec.y.max != null) hi = spec.y.max;
      }
      if (lo === hi) { lo -= Math.abs(lo) * 0.1 || 1; hi += Math.abs(hi) * 0.1 || 1; }
      var span = hi - lo;
      return [lo - span * 0.05, hi + span * 0.15];
    }
    function refY() {
      if (state.mode === "index") return [100];
      if (state.mode === "forandring") return [0];
      return spec.ref.y || [];
    }

    function yTicks(ys) {
      var ticks = ys.ticks(5);
      var step = ticks.length > 1 ? ticks[1] - ticks[0] : 1;
      var dec = d3.precisionFixed(step);
      return ticks.map(function (t) { return { v: t, label: vgrD3.fmt(t, dec, " ") }; });
    }

    function xTicks() {
      var d = xDomain();
      if (!state.xDomain) {
        return X.ticks.map(function (t, i) { return { v: t, label: X.tick_labels[i] }; })
          .filter(function (t) { return X.clip_off || (t.v >= d[0] && t.v <= d[1]); });
      }
      if (X.type === "date") {
        var ut = d3.scaleUtc().domain([new Date(d[0]), new Date(d[1])]).ticks(Math.max(3, X.n_breaks));
        // Inzoomat blir stegen kortare än formatet visar (t.ex. "%Y" på månader -> "2020, 2020, 2020"):
        // byt då till ett format med månad respektive dag
        var steg = ut.length > 1 ? (ut[1] - ut[0]) / 86400000 : 365;
        var f = X.date_format;
        if (steg < 360 && !/%[mbBd]/.test(f)) f = "%b %Y";
        if (steg < 28 && !/%d/.test(f)) f = "%e %b %Y";
        var fmt = vgrD3.timeLocale.utcFormat(f);
        return ut.map(function (t) { return { v: +t, label: fmt(t) }; });
      }
      var tk = d3.scaleLinear().domain(d).ticks(X.n_breaks);
      // "whole": bara heltal (annars 2018, 2018, 2019 ... när halvår avrundas)
      if (X.number_format === "whole") {
        tk = tk.filter(function (t) { return Number.isInteger(t); });
        if (tk.length < 2) tk = d3.range(Math.ceil(d[0]), Math.floor(d[1]) + 1);
      }
      var dec = tk.length > 1 ? d3.precisionFixed(tk[1] - tk[0]) : 0;
      return tk.map(function (t) {
        return { v: t, label: X.number_format === "whole" ? String(Math.round(t)) : vgrD3.fmt(t, dec, " ") };
      });
    }

    function dashFor(g) {
      if (!LN.dash) return null;
      return LN.dash.map(function (v) { return v * g.line_size; }).join(" ");
    }

    // -------------------------------------------------------------------------
    //  Legend
    // -------------------------------------------------------------------------
    var swatchW = pt(17.28);
    function drawLegend(area) {
      if (!spec.has_groups || L.position === "none") return;
      var rowH = Math.max(L.text_size * 1.3, pt(17.28));
      var vertical = L.position === "left" || L.position === "right";
      var items = groups.map(function (g) {
        return { g: g, w: swatchW + 6 + vgrD3.measureText(svg, g.label, L.text_size, font) };
      });
      var titleW = L.title ? vgrD3.measureText(svg, L.title, L.title_size, font) : 0;
      // Lodrät legend: titel ovanför. Vågrät legend: titel till vänster på första raden (som ggplot2)
      var titleH = L.title && vertical ? L.title_size * 1.6 : 0;
      var lw, lh;
      if (vertical) {
        items.forEach(function (it, i) { it.x = 0; it.y = titleH + i * rowH; });
        lw = Math.max(titleW, d3.max(items, function (it) { return it.w; })) + 8;
        lh = titleH + items.length * rowH;
      } else {
        var startX = L.title ? titleW + 10 : 0;
        var avail = area.x1 - area.x0, x = startX, y = 0, maxX = 0;
        items.forEach(function (it) {
          if (x > startX && x + it.w > avail) { x = startX; y += rowH; }
          it.x = x; it.y = y; x += it.w + 16; maxX = Math.max(maxX, x - 16);
        });
        lw = maxX;
        lh = y + rowH;
      }
      var lx, ly, gap = pt(11);
      if (L.position === "right") { lx = area.x1 - lw; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x1 -= lw + gap; }
      else if (L.position === "left") { lx = area.x0; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x0 += lw + gap; }
      else if (L.position === "top") { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y0; area.y0 += lh + gap; }
      else { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y1 - lh; area.y1 -= lh + gap; }

      var lg = svg.append("g").attr("transform", "translate(" + lx + "," + ly + ")");
      if (L.title) {
        lg.append("text").attr("y", vertical ? L.title_size : rowH / 2).attr("dy", vertical ? null : "0.35em")
          .style("font-size", L.title_size + "px").style("font-family", font).text(L.title);
      }
      var item = lg.selectAll("g.vgr-leg").data(items).join("g").attr("class", "vgr-leg")
        .attr("transform", function (it) { return "translate(" + it.x + "," + it.y + ")"; })
        .attr("opacity", function (it) { return state.hidden[it.g.key] ? 0.35 : 1; })
        .style("cursor", IA.legend ? "pointer" : null);
      item.append("rect").attr("width", it => it.w).attr("height", rowH).attr("fill", "#ffffff").attr("fill-opacity", 0);
      item.append("line")
        .attr("x1", 0).attr("x2", swatchW).attr("y1", rowH / 2).attr("y2", rowH / 2)
        .attr("stroke", function (it) { return it.g.color; })
        .attr("stroke-opacity", function (it) { return it.g.opacity; })
        .attr("stroke-width", function (it) { return Math.min(it.g.line_size, rowH * 0.4); })
        .attr("stroke-dasharray", function (it) { return dashFor(it.g); });
      item.append("text")
        .attr("x", swatchW + 6).attr("y", rowH / 2).attr("dy", "0.35em")
        .style("font-size", L.text_size + "px").style("font-family", font)
        .text(function (it) { return it.g.label; });
      if (IA.legend) {
        item.append("title").text("Klicka: dölj/visa · Dubbelklicka: visa bara denna");
        item
          .on("mouseenter", function (event, it) { setHoverGroup(it.g.key); })
          .on("mouseleave", function () { setHoverGroup(null); })
          .on("click", function (event, it) {
            if (state.hidden[it.g.key]) delete state.hidden[it.g.key]; else state.hidden[it.g.key] = true;
            draw();
          })
          .on("dblclick", function (event, it) {
            state.hidden = {};
            groups.forEach(function (g) { if (g.key !== it.g.key) state.hidden[g.key] = true; });
            draw();
          });
      }
    }

    function setHoverGroup(key) {
      state.hoverGroup = key;
      svg.selectAll(".vgr-line, .vgr-band").attr("opacity", function () {
        var g = this.getAttribute("data-group");
        var base = +this.getAttribute("data-opacity");
        return key == null || g === key ? base : base * 0.15;
      });
    }

    // -------------------------------------------------------------------------
    //  Etiketter vid linjeslut (legend_direct och group_highlight)
    // -------------------------------------------------------------------------
    function endLabelSpecs() {
      var out = [];
      if (L.direct && spec.has_groups) out.push({ filter: function () { return true; }, size: L.text_size, box: false, color: "inherit" });
      if (spec.highlight.label) {
        out.push({ filter: function (g) { return hlSet[g]; }, size: spec.highlight.label_size,
                   box: spec.highlight.label_box, color: spec.highlight.label_color });
      }
      return out;
    }
    function endLabelReserve() {
      var w = 0;
      endLabelSpecs().forEach(function (o) {
        groups.forEach(function (g) {
          if (o.filter(g.key) && !state.hidden[g.key]) w = Math.max(w, vgrD3.measureText(svg, g.label, o.size, font) + 14);
        });
      });
      return w;
    }

    // -------------------------------------------------------------------------
    //  Rita allt
    // -------------------------------------------------------------------------
    var panels = [];   // för hover: {facet, rect, xs, ys, hoverG}

    function draw() {
      svg.selectAll("*").remove();
      tip.hide();
      panels = [];
      state.labelDomain = null;   // räknas om i layoutPanels när panelbredden är känd
      svg.append("rect").attr("width", W).attr("height", H).attr("fill", "#ffffff");

      var m = { t: pt(hasFacets && F.wrapped ? 20 : 15), r: pt(10), b: pt(10), l: pt(10) };
      var box = { x0: m.l, y0: m.t, x1: W - m.r, y1: H - m.b };
      var y = box.y0;
      var th = vgrD3.textBlock(svg, T.title, { x: box.x0, y: y, size: T.title_size, font: font, anchor: "start", lineHeight: 1.1 });
      y += th + (th ? pt(5) : 0);
      var sh = vgrD3.textBlock(svg, T.subtitle, { x: box.x0, y: y, size: T.subtitle_size, font: font, anchor: "start", lineHeight: 1.2 });
      y += sh + (sh ? pt(5) : 0);
      if (th || sh) y += pt(5);

      // Bildtexten linjeras mot panelens högerkant (som ggplot2) — ritas när panelerna är placerade
      var bottom = box.y1;
      var ch = T.caption.length * T.caption_size * 1.2;
      if (ch) bottom -= ch + pt(5.5);
      var area = { x0: box.x0, y0: y, x1: box.x1, y1: bottom };
      drawLegend(area);

      // Axeltitlar
      var ytitle = state.mode === "index"
        ? "Index (" + (IA.index_base != null ? xText(IA.index_base) : "första värdet") + " = 100)"
        : state.mode === "forandring" ? IA.index_labels.forandring : T.y_axis_title;
      if (T.x_axis_title) {
        svg.append("text").attr("x", 0).attr("y", 0).attr("text-anchor", "middle")
          .attr("transform", "translate(" + ((area.x0 + area.x1) / 2) + "," + (area.y1 - T.axis_title_size * 0.25) + ")")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(T.x_axis_title);
        area.y1 -= T.axis_title_size * 1.5;
      }
      if (ytitle) {
        svg.append("text").attr("text-anchor", "middle")
          .attr("transform", "translate(" + (area.x0 + T.axis_title_size * 0.9) + "," + ((area.y0 + area.y1) / 2) + ") rotate(-90)")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(ytitle);
        area.x0 += T.axis_title_size * 1.6;
      }
      area.x1 -= endLabelReserve();

      var panelRight = layoutPanels(area);
      if (ch) vgrD3.textBlock(svg, T.caption, { x: panelRight, y: box.y1 - ch, size: T.caption_size, font: font, anchor: "end" });
      refreshControls();
      state.first = false;
    }

    function layoutPanels(area) {
      var n = F.levels.length;
      var ncol = Math.min(F.ncol, n), nrow = Math.ceil(n / ncol);
      var sx = hasFacets ? (F.wrapped ? 0.3 : 0.6) * LINES : 0;
      var sy = hasFacets ? (F.wrapped ? 0.2 : 0.8) * LINES : 0;
      var stripLines = d3.max(F.levels, function (f) { return String(f).split("\n").length; });
      var stripH = hasFacets ? stripLines * T.facet_title_size * 0.9 + pt(F.wrapped ? 14 : 16) : 0;

      // Samma y-skala för alla paneler (facet_same_y_scale) eller en per panel
      var shared = F.same_y ? yDomainFor(F.levels) : null;
      var domains = F.levels.map(function (f) { return shared || yDomainFor([f]); });

      var xt = xTicks();
      var xLabW = d3.max(xt, function (t) { return vgrD3.measureText(svg, t.label, T.x_axis_text_size, font); }) || 0;
      var rad = X.angle * Math.PI / 180;
      var xAxisH = (X.angle > 0 ? xLabW * Math.sin(rad) + T.x_axis_text_size * Math.cos(rad) : T.x_axis_text_size) + pt(5);

      var cellW = (area.x1 - area.x0 - (ncol - 1) * sx) / ncol;
      var cellH = (area.y1 - area.y0 - (nrow - 1) * sy) / nrow;

      // Plats i y-led för etiketter utan repel (panelens höjd är känd här): delad skala breddas efter alla
      // paneler, annars var och en efter sina egna etiketter — före axelbredden, som beror på brytpunkterna
      var panelH = cellH - stripH - xAxisH;
      if (shared) {
        var sharedY = labelYDomain(shared, F.levels, panelH);
        domains = domains.map(function () { return sharedY; });
      } else {
        domains = domains.map(function (dom, i) { return labelYDomain(dom, [F.levels[i]], panelH); });
      }

      // Axelbredd: bredaste y-etiketten i någon panel, så att panelerna linjerar
      var yAxisW = 0;
      domains.forEach(function (dom) {
        var ys = d3.scaleLinear().domain(dom);
        yTicks(ys).forEach(function (t) {
          yAxisW = Math.max(yAxisW, vgrD3.measureText(svg, t.label, T.y_axis_text_size, font));
        });
      });
      yAxisW += pt(4);
      // Delad y-axel (facet_same_y_scale): axeltexten bara i första kolumnen och panelerna delar på den
      // frigjorda bredden (som ggplot med scales = "fixed")
      var leftShared = hasFacets && ncol > 1 && !!F.same_y;
      var panelW = (area.x1 - area.x0 - yAxisW - (ncol - 1) * sx) / ncol;
      // Plats för etiketter utan repel vid x-axelns ändar (räknas om vid varje ritning: läge, dolda serier;
      // nollställd i draw())
      state.labelDomain = labelDomain(leftShared ? panelW : cellW - yAxisW);

      F.levels.forEach(function (f, i) {
        var col = i % ncol, row = Math.floor(i / ncol);
        var cx = area.x0 + col * (cellW + sx), cy = area.y0 + row * (cellH + sy);
        var rect = { x0: cx + yAxisW, y0: cy + stripH, x1: cx + cellW, y1: cy + cellH - xAxisH };
        if (leftShared) {
          rect.x0 = area.x0 + yAxisW + col * (panelW + sx);
          rect.x1 = rect.x0 + panelW;
        }
        if (hasFacets) {
          vgrD3.textBlock(svg, String(f).split("\n"), {
            x: (rect.x0 + rect.x1) / 2, y: cy + pt(F.wrapped ? 6 : 4),
            size: T.facet_title_size, font: font, lineHeight: 0.9
          });
        }
        drawPanel(f, i, rect, domains[i], xt, !leftShared || col === 0);
      });
      return area.x0 + ncol * cellW + (ncol - 1) * sx;
    }

    function drawPanel(facet, idx, rect, yDom, xt, yLabels) {
      var xs = d3.scaleLinear().domain(xDomain()).range([rect.x0, rect.x1]);
      var ys = d3.scaleLinear().domain(yDom).range([rect.y1, rect.y0]);
      var g = svg.append("g").attr("class", "vgr-panel");
      var clipId = el.id + "-clip-" + idx;
      g.append("clipPath").attr("id", clipId).append("rect")
        .attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0).attr("height", rect.y1 - rect.y0);
      var clipped = g.append("g").attr("clip-path", "url(#" + clipId + ")");
      var yt = yTicks(ys);

      // Rutnät
      var grid = clipped.append("g").attr("pointer-events", "none");
      function gridLines(vals, isX, color) {
        grid.selectAll(null).data(vals).enter().append("line")
          .attr("x1", function (v) { return isX ? xs(v) : rect.x0; })
          .attr("x2", function (v) { return isX ? xs(v) : rect.x1; })
          .attr("y1", function (v) { return isX ? rect.y0 : ys(v); })
          .attr("y2", function (v) { return isX ? rect.y1 : ys(v); })
          .attr("stroke", color).attr("stroke-width", 1.42 * (color === "#f2f2f2" ? 0.5 : 1));
      }
      function mids(v) { var o = []; for (var i = 1; i < v.length; i++) o.push((v[i - 1] + v[i]) / 2); return o; }
      // true/false eller "x"/"y": linjer från x-axeln (lodräta) resp. y-axeln (vågräta)
      function gridOn(setting, isX) { return setting === true || (setting === "x" && isX) || (setting === "y" && !isX); }
      var GMIN = spec.grid.minor, GMAJ = spec.grid.major;
      if (gridOn(GMIN, false)) gridLines(mids(yt.map(function (t) { return t.v; })), false, "#f2f2f2");
      if (gridOn(GMIN, true) && X.type !== "discrete") gridLines(mids(xt.map(function (t) { return t.v; })), true, "#f2f2f2");
      if (gridOn(GMAJ, false)) gridLines(yt.map(function (t) { return t.v; }), false, "#e5e5e5");
      if (gridOn(GMAJ, true)) gridLines(xt.map(function (t) { return t.v; }), true, "#e5e5e5");

      // Markerade perioder
      spec.shades.forEach(function (s) {
        var x0 = xs(Math.min(s.from, s.to)), x1 = xs(Math.max(s.from, s.to));
        clipped.append("rect").attr("x", x0).attr("y", rect.y0).attr("width", Math.max(0, x1 - x0))
          .attr("height", rect.y1 - rect.y0).attr("fill", spec.shade_color).attr("pointer-events", "none");
        if (s.label) {
          clipped.append("text").attr("x", (x0 + x1) / 2).attr("y", rect.y0 + 14).attr("text-anchor", "middle")
            .attr("fill", "#546e7a").style("font-size", (13 * (spec.text_scale || 1)) + "px").style("font-family", font).text(s.label);
        }
      });

      // Panelram (facetter)
      if (hasFacets) {
        g.append("rect").attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0)
          .attr("height", rect.y1 - rect.y0).attr("fill", "none").attr("stroke", "#b3b3b3").attr("stroke-width", 0.85);
      }

      // Referenslinjer
      // En etikett per linje; färre etiketter än linjer återanvänds i tur och ordning
      function refLabel(list, i) { return list && list.length ? list[i % list.length] : null; }
      refY().forEach(function (v, i) {
        var isUser = state.mode === "varde";
        clipped.append("line").attr("x1", rect.x0).attr("x2", rect.x1).attr("y1", ys(v)).attr("y2", ys(v))
          .attr("stroke", isUser ? "#000000" : "#9e9e9e").attr("stroke-width", 1.42)
          .attr("stroke-dasharray", isUser ? null : "5 4");
        var lab = isUser ? refLabel(spec.ref.y_label, i) : null;
        if (lab) {
          clipped.append("text").attr("x", rect.x1 - 4).attr("y", ys(v) - 5).attr("text-anchor", "end")
            .attr("fill", "#37474F").attr("stroke", "#ffffff").attr("stroke-width", 3).attr("paint-order", "stroke")
            .style("font-size", (13 * (spec.text_scale || 1)) + "px").style("font-family", font).text(lab);
        }
      });
      spec.ref.x.forEach(function (v, i) {
        clipped.append("line").attr("x1", xs(v)).attr("x2", xs(v)).attr("y1", rect.y0).attr("y2", rect.y1)
          .attr("stroke", "#000000").attr("stroke-width", 1.42).attr("stroke-dasharray", "5.7 5.7");
        var lab = refLabel(spec.ref.x_label, i);
        if (lab) {
          clipped.append("text").attr("x", xs(v) + 5).attr("y", rect.y0 + 14)
            .attr("fill", "#37474F").attr("stroke", "#ffffff").attr("stroke-width", 3).attr("paint-order", "stroke")
            .style("font-size", (13 * (spec.text_scale || 1)) + "px").style("font-family", font).text(lab);
        }
      });
      if (LN.forecast_from != null && inX(LN.forecast_from)) {
        var fx = xs(LN.forecast_from);
        clipped.append("line").attr("x1", fx).attr("x2", fx).attr("y1", rect.y0).attr("y2", rect.y1)
          .attr("stroke", "#9e9e9e").attr("stroke-width", 1).attr("stroke-dasharray", "2 3");
        if (LN.forecast_label) {
          clipped.append("text").attr("x", fx + 5).attr("y", rect.y0 + 14).attr("fill", "#607d8b")
            .style("font-size", (13 * (spec.text_scale || 1)) + "px").style("font-style", "italic").style("font-family", font).text(LN.forecast_label);
        }
      }

      // Band och linjer (bleka först, markerade sist)
      var list = seriesIn(facet).filter(isVisible);
      list.sort(function (a, b) {
        var ga = groupByKey[a.group], gb = groupByKey[b.group];
        return (gb.faded - ga.faded) || (ga.line_size - gb.line_size);
      });
      var linesG = clipped.append("g");
      list.forEach(function (s) {
        var gr = groupByKey[s.group];
        var pts = s.x.map(function (x, i) {
          return { x: x, y: tr(s.y[i], s), lo: s.lo ? tr(s.lo[i], s) : null, hi: s.hi ? tr(s.hi[i], s) : null };
        });
        if (LN.band) {
          var bandGen = d3.area().defined(function (d) { return d.lo != null && d.hi != null; })
            .x(function (d) { return xs(d.x); }).y0(function (d) { return ys(d.lo); }).y1(function (d) { return ys(d.hi); });
          linesG.append("path").attr("class", "vgr-band").attr("data-group", s.group)
            .attr("data-opacity", LN.band_alpha * gr.opacity).attr("opacity", LN.band_alpha * gr.opacity)
            .attr("d", bandGen(pts)).attr("fill", gr.color).attr("pointer-events", "none");
        }
        var line = d3.line().defined(function (d) { return d.y != null; })
          .x(function (d) { return xs(d.x); }).y(function (d) { return ys(d.y); });
        var parts = [];
        if (LN.forecast_from != null) {
          parts.push({ pts: pts.filter(function (d) { return d.x <= LN.forecast_from; }), dash: dashFor(gr) });
          parts.push({ pts: pts.filter(function (d) { return d.x >= LN.forecast_from; }),
                       dash: [3, 2].map(function (v) { return v * gr.line_size; }).join(" ") });
        } else {
          parts.push({ pts: pts, dash: dashFor(gr) });
        }
        parts.forEach(function (part) {
          if (part.pts.length < 1) return;
          var path = linesG.append("path").attr("class", "vgr-line").attr("data-group", s.group)
            .attr("data-opacity", gr.opacity).attr("opacity", gr.opacity)
            .attr("d", line(part.pts)).attr("fill", "none")
            .attr("stroke", gr.color).attr("stroke-width", gr.line_size)
            .attr("stroke-linejoin", "round").attr("stroke-linecap", "butt")
            .attr("stroke-dasharray", part.dash).attr("pointer-events", "none");
          if (state.first && spec.animation.draw && dur > 0) {
            if (part.dash) {
              path.attr("opacity", 0).transition().duration(dur).attr("opacity", gr.opacity);
            } else {
              var len = path.node().getTotalLength();
              path.attr("stroke-dasharray", len + " " + len).attr("stroke-dashoffset", len)
                .transition().duration(dur).ease(d3.easeCubicOut).attr("stroke-dashoffset", 0)
                .on("end", function () { d3.select(this).attr("stroke-dasharray", null).attr("stroke-dashoffset", null); });
            }
          }
        });
      });

      // Axlar
      var axisColor = hasFacets ? "#b3b3b3" : "#000000", axisW = hasFacets ? 0.85 : 1.42;
      var AXL = spec.axis_line || { x: true, y: true };   // x_axis_line / y_axis_line (standard: båda, som tidigare)
      if (AXL.x) g.append("line").attr("x1", rect.x0).attr("x2", rect.x1).attr("y1", rect.y1).attr("y2", rect.y1)
        .attr("stroke", axisColor).attr("stroke-width", axisW);
      if (AXL.y) g.append("line").attr("x1", rect.x0).attr("x2", rect.x0).attr("y1", rect.y0).attr("y2", rect.y1)
        .attr("stroke", axisColor).attr("stroke-width", axisW);
      g.selectAll(null).data(yLabels ? yt : []).enter().append("text")
        .attr("x", rect.x0 - pt(3)).attr("y", function (t) { return ys(t.v); }).attr("dy", "0.35em")
        .attr("text-anchor", "end").attr("fill", "#4d4d4d")
        .style("font-size", T.y_axis_text_size + "px").style("font-family", font)
        .text(function (t) { return t.label; });
      var xl = g.selectAll(null).data(xt.filter(function (t) { return t.v >= xDomain()[0] && t.v <= xDomain()[1]; }))
        .enter().append("text")
        .attr("fill", "#4d4d4d").style("font-size", T.x_axis_text_size + "px").style("font-family", font)
        .text(function (t) { return t.label; });
      if (X.angle > 0) {
        xl.attr("text-anchor", "end").attr("transform", function (t) {
          return "translate(" + xs(t.v) + "," + (rect.y1 + pt(3)) + ") rotate(" + (-X.angle) + ")";
        }).attr("dy", "0.71em");
      } else {
        xl.attr("text-anchor", "middle").attr("x", function (t) { return xs(t.v); })
          .attr("y", rect.y1 + pt(3)).attr("dy", "0.85em");
      }

      // Dataetiketter
      var labelLayer = X.clip_off ? g.append("g") : clipped.append("g");
      if (LB.show) drawDataLabels(labelLayer, facet, rect, xs, ys);
      drawEndLabels(g, facet, rect, xs, ys);

      // Fritt placerad bildtext
      if (spec.free_caption && inX(spec.free_caption.x)) {
        var fc = spec.free_caption;
        g.append("text").attr("x", xs(fc.x)).attr("y", ys(fc.y))
          .attr("text-anchor", fc.hjust >= 0.75 ? "end" : fc.hjust <= 0.25 ? "start" : "middle")
          .attr("dy", fc.vjust >= 0.75 ? "0.8em" : fc.vjust <= 0.25 ? "0" : "0.35em")
          .attr("fill", "#666666").style("font-size", T.caption_size + "px").style("font-family", font)
          .text(fc.text);
      }

      // Interaktion
      var hoverG = g.append("g").attr("pointer-events", "none");
      var overlay = g.append("g").attr("class", "vgr-overlay");
      if (IA.zoom) {
        var brush = d3.brushX().extent([[rect.x0, rect.y0], [rect.x1, rect.y1]])
          .on("end", function (event) {
            if (!event.selection) return;
            var a = xs.invert(event.selection[0]), b = xs.invert(event.selection[1]);
            if (Math.abs(event.selection[1] - event.selection[0]) < 8) { overlay.call(brush.move, null); return; }
            // Inte utanför det som visas: skalan kan vara breddad för etiketter (labelDomain), och en markering ut i den
            // luften får inte släppa in punkter förbi x_axis_min/max
            var dd = dataDomain();
            a = Math.max(a, dd[0]); b = Math.min(b, dd[1]);
            if (!(b > a)) { overlay.call(brush.move, null); return; }
            state.xDomain = [a, b];
            draw();
          });
        overlay.call(brush);
        overlay.select(".selection").attr("fill", "#37474F").attr("fill-opacity", 0.12).attr("stroke", "none");
      } else {
        overlay.append("rect").attr("x", rect.x0).attr("y", rect.y0)
          .attr("width", rect.x1 - rect.x0).attr("height", rect.y1 - rect.y0)
          .attr("fill", "#ffffff").attr("fill-opacity", 0);
      }
      var panel = { facet: facet, rect: rect, xs: xs, ys: ys, hoverG: hoverG };
      panels.push(panel);
      overlay
        .on("mousemove.vgr", function (event) { hover(panel, event); })
        .on("mouseleave.vgr", function () { hoverG.selectAll("*").remove(); tip.hide(); setHoverGroup(null); })
        .on("dblclick.vgr", function () { if (state.xDomain) { state.xDomain = null; draw(); } });
    }

    // Datatikettens värde och text (null = ritas inte) — delas av ritningen och av etikettutrymmet nedan
    function labelValue(p) {
      if (state.hidden[p.group] || !inX(p.x)) return null;
      var s = seriesOf(p.facet, p.group);
      return s ? tr(s.y[s.x.indexOf(p.x)], s) : null;
    }
    function labelText(p, v) {
      var text = fmtV(v, state.mode === "varde" ? LB.decimals : 1);
      if (LB.format === "x_y" || LB.format === "both") text = p.x_text + ": " + text;
      if (LB.show_group) text = groupByKey[p.group].label + LB.separator + text;
      return text;
    }

    // Etiketter utan repel ligger mitt på punkten, så x-axeln behöver plats för halva etiketten utanför
    // första/sista punkten (som func_graf_linjediagram sedan 2026-10-09; här med uppmätt textbredd och
    // verklig panelbredd). Utökar bara domänen, aldrig förbi att data får hälften av panelen. Inte med
    // repel (håller etiketterna inne), x_axis_clip_off eller inzoomning
    function labelDomain(panelW) {
      var base = X.domain;
      if (!LB.show || LB.repel || X.clip_off || state.xDomain || !(panelW > 0)) return null;
      var nudgeX = X.type === "date" ? LB.nudge_x * 86400000 : LB.nudge_x;
      var halfW = 0, lo = Infinity, hi = -Infinity;
      LB.points.forEach(function (p) {
        var v = labelValue(p);
        if (v == null) return;
        var lines = labelText(p, v).split("\n");
        var weight = groupByKey[p.group].bold ? "bold" : null;   // framhävda grupper ritas i fetstil (drawDataLabels)
        var w = d3.max(lines, function (t) { return vgrD3.measureText(svg, t, LB.size, font, weight); }) +
                (LB.box && !LB.glow ? LB.size * 0.5 : 0) + (LB.glow ? LB.glow_width : 0);   // ruta / glödens kontur
        halfW = Math.max(halfW, w / 2 + pt(2));
        lo = Math.min(lo, p.x + nudgeX); hi = Math.max(hi, p.x + nudgeX);
      });
      if (!isFinite(lo) || halfW === 0) return null;
      return vgrD3.padDomain(base, lo, hi, halfW, halfW, panelW);   // data behåller minst halva panelen
    }

    // Samma sak i y-led: etiketten ligger mitt på punkten, så den högsta/lägsta etiketten behöver halva sin höjd
    // ovanför/under (vid stor text stack den lägsta ut under panelen). Utökar bara domänen, inte förbi en angiven
    // y_axis_min/max och aldrig förbi att data får hälften av panelen
    function labelYDomain(dom, facets, panelH) {
      if (!LB.show || LB.repel || !(panelH > 0)) return dom;
      var nudgeY = state.mode === "varde" ? LB.nudge_y : 0;
      var halfH = 0, lo = Infinity, hi = -Infinity;
      LB.points.forEach(function (p) {
        if (facets.indexOf(p.facet) < 0) return;
        var v = labelValue(p);
        if (v == null || !isFinite(v)) return;
        var nLines = labelText(p, v).split("\n").length;
        halfH = Math.max(halfH, (nLines * LB.size * 1.2 + (LB.box && !LB.glow ? LB.size * 0.5 : 0) +
                                 (LB.glow ? LB.glow_width : 0)) / 2 + pt(2));
        lo = Math.min(lo, v + nudgeY); hi = Math.max(hi, v + nudgeY);
      });
      if (!isFinite(lo)) return dom;
      // En angiven y_axis_min/max (värdeläget) låser den sidan
      return vgrD3.padDomain(dom, lo, hi, halfH, halfH, panelH,
                             { lo: state.mode === "varde" && spec.y.min != null, hi: state.mode === "varde" && spec.y.max != null });
    }

    function drawDataLabels(layer, facet, rect, xs, ys) {
      var nudgeX = X.type === "date" ? LB.nudge_x * 86400000 : LB.nudge_x;
      var items = [];
      LB.points.forEach(function (p) {
        if (p.facet !== facet) return;
        var v = labelValue(p);
        if (v == null) return;
        var text = labelText(p, v);
        var gr = groupByKey[p.group];
        var ax = xs(p.x + nudgeX), ay = ys(v + (state.mode === "varde" ? LB.nudge_y : 0));
        items.push({ text: text, color: LB.color === "auto" ? gr.base_color : LB.color,
                     bold: !!gr.bold, ax: ax, ay: ay, x: ax, y: ay, ox: xs(p.x), oy: ys(v) });
      });
      if (!items.length) return;
      var lines = layer.append("g");
      var nodes = vgrD3.labelNodes(layer, items, {
        size: LB.size, font: font, box: LB.box && !LB.glow, box_color: LB.box_color, box_alpha: LB.box_alpha,
        glow: LB.glow, glow_color: LB.glow_color, glow_width: LB.glow_width
      });
      if (LB.repel) {
        var dir = LB.direction !== "auto" ? LB.direction
          : (LB.nudge_x !== 0 && LB.nudge_y === 0) ? "x"
          : (LB.nudge_y !== 0 && LB.nudge_x === 0) ? "y" : "both";
        var bounds = X.clip_off ? { x0: rect.x0, y0: rect.y0, x1: W, y1: rect.y1 } : rect;
        var pointR = Math.max(pt(4), d3.max(groups, function (g) { return g.line_size; }) * 1.5);
        vgrD3.relaxLabels(items, bounds, {
          sep: pt(3), direction: dir,
          points: items.map(function (d) { return { x: d.ox, y: d.oy, r: pointR }; })
        });
        vgrD3.leaderLines(lines, items, { color: "#7f7f7f", width: 0.85 });
      } else {
        // Utan repel: när axelutrymmet inte räcker (smala facetter — taket på halva panelen) flyttas en etikett
        // som skulle sticka ut in i panelen i stället för att klippas. Sidled inte med x_axis_clip_off
        var gw = LB.glow ? LB.glow_width / 2 : 0;
        items.forEach(function (d) {
          var hw = d.w / 2 + gw + 1, hh = d.h / 2 + gw + 1;
          if (!X.clip_off && hw * 2 < rect.x1 - rect.x0) d.x = clamp(d.x, rect.x0 + hw, rect.x1 - hw);
          if (hh * 2 < rect.y1 - rect.y0) d.y = clamp(d.y, rect.y0 + hh, rect.y1 - hh);
        });
      }
      nodes.place();
      if (state.first && dur > 0 && spec.animation.draw) {
        layer.attr("opacity", 0).transition().delay(dur * 0.7).duration(300).attr("opacity", 1);
      }
    }

    function drawEndLabels(layer, facet, rect, xs, ys) {
      endLabelSpecs().forEach(function (o) {
        var items = [];
        seriesIn(facet).forEach(function (s) {
          if (!isVisible(s) || !o.filter(s.group)) return;
          for (var i = s.x.length - 1; i >= 0; i--) {
            var v = tr(s.y[i], s);
            if (v != null && inX(s.x[i])) {
              var gr = groupByKey[s.group];
              items.push({ text: gr.label, color: o.color === "inherit" ? gr.base_color : o.color,
                           px: xs(s.x[i]), py: ys(v) });
              break;
            }
          }
        });
        if (!items.length) return;
        var nodes = vgrD3.labelNodes(layer, items, { size: o.size, font: font, box: o.box, box_color: "#ffffff", box_alpha: 0.9 });
        // Sprid ut vertikalt så att namnen inte krockar
        items.sort(function (a, b) { return a.py - b.py; });
        var yCur = rect.y0;
        items.forEach(function (d) { d.y = Math.max(d.py, yCur + d.h / 2); yCur = d.y + d.h / 2 + 1; });
        var yb = rect.y1;
        for (var k = items.length - 1; k >= 0; k--) {
          if (items[k].y + items[k].h / 2 > yb) items[k].y = yb - items[k].h / 2;
          yb = items[k].y - items[k].h / 2 - 1;
        }
        items.forEach(function (d) { d.x = d.px + 6 + d.w / 2; });
        nodes.place();
      });
    }

    // -------------------------------------------------------------------------
    //  Hover: hårkors med alla linjer, eller närmaste linje
    // -------------------------------------------------------------------------
    function hover(panel, event) {
      var pos = d3.pointer(event, svg.node());
      var mx = pos[0], my = pos[1];
      var hg = panel.hoverG;
      hg.selectAll("*").remove();
      if (!TT.show) return;
      var list = seriesIn(panel.facet).filter(isVisible);
      if (!list.length) return;
      var mode = TT.mode === "auto" ? (list.length > 12 ? "nearest" : "all") : TT.mode;
      var dec = modeDecimals(), unit = modeUnit();

      if (mode === "all") {
        var xsAll = [];
        list.forEach(function (s) { s.x.forEach(function (x) { if (inX(x)) xsAll.push(x); }); });
        if (!xsAll.length) return;
        var xv = panel.xs.invert(mx);
        var xc = xsAll.reduce(function (best, x) { return Math.abs(x - xv) < Math.abs(best - xv) ? x : best; }, xsAll[0]);
        var px = panel.xs(xc);
        hg.append("line").attr("x1", px).attr("x2", px).attr("y1", panel.rect.y0).attr("y2", panel.rect.y1)
          .attr("stroke", "#90a4ae").attr("stroke-width", 1);
        var rows = [];
        list.forEach(function (s) {
          var i = s.x.indexOf(xc);
          if (i < 0) return;
          var v = tr(s.y[i], s);
          if (v == null) return;
          var gr = groupByKey[s.group];
          rows.push({ s: s, i: i, v: v, gr: gr });
          hg.append("circle").attr("cx", px).attr("cy", panel.ys(v)).attr("r", Math.max(3.5, gr.line_size))
            .attr("fill", gr.color).attr("stroke", "#ffffff").attr("stroke-width", 1.5);
        });
        rows.sort(function (a, b) { return b.v - a.v; });
        var html = "<div class='vgr-tt-title'>" + esc(xText(xc)) + "</div>";
        if (hasFacets) html += "<div class='vgr-tt-sub'>" + esc(panel.facet) + "</div>";
        if (LN.forecast_from != null && xc > LN.forecast_from && LN.forecast_label) html += "<div class='vgr-tt-sub'>" + esc(LN.forecast_label) + "</div>";
        rows.slice(0, 20).forEach(function (r) { html += rowHtml(r, dec, unit, rows.length > 1); });
        if (rows.length > 20) html += "<div class='vgr-tt-sub'>… och " + (rows.length - 20) + " till</div>";
        tip.show(html, event);
      } else {
        var best = null, bestD = Infinity;
        list.forEach(function (s) {
          s.x.forEach(function (x, i) {
            if (!inX(x)) return;
            var v = tr(s.y[i], s);
            if (v == null) return;
            var d = Math.pow(panel.xs(x) - mx, 2) + Math.pow(panel.ys(v) - my, 2);
            if (d < bestD) { bestD = d; best = { s: s, i: i, v: v }; }
          });
        });
        if (!best) return;
        var gr = groupByKey[best.s.group];
        setHoverGroup(best.s.group);
        hg.append("circle").attr("cx", panel.xs(best.s.x[best.i])).attr("cy", panel.ys(best.v))
          .attr("r", Math.max(4, gr.line_size)).attr("fill", gr.color).attr("stroke", "#ffffff").attr("stroke-width", 1.5);
        var html2 = "<div class='vgr-tt-title'>" + esc(xText(best.s.x[best.i])) + "</div>";
        if (hasFacets) html2 += "<div class='vgr-tt-sub'>" + esc(panel.facet) + "</div>";
        html2 += rowHtml({ s: best.s, i: best.i, v: best.v, gr: gr }, dec, unit);
        tip.show(html2, event);
      }
    }

    function rowHtml(r, dec, unit, indent) {
      var txt = fmtV(r.v, dec) + unit;
      if (r.s.lo && r.s.hi) {
        var lo = tr(r.s.lo[r.i], r.s), hi = tr(r.s.hi[r.i], r.s);
        if (lo != null && hi != null) txt += " (" + fmtV(lo, dec) + "–" + fmtV(hi, dec) + ")";
      }
      return vgrD3.tooltipRow(spec.has_groups ? r.gr.label : (T.y_axis_title || "Värde"), txt, r.gr.base_color) +
        extraRows(r, indent);
    }

    // tooltip_columns: punktens extra kolumner (formaterade i R) under dess rad, indragna när flera linjer visas
    function extraRows(r, indent) {
      var out = "";
      (TT.columns || []).forEach(function (label, j) {
        var val = r.s.tt && r.s.tt[j] ? r.s.tt[j][r.i] : null;
        if (val == null) return;
        var row = vgrD3.tooltipRow(label, val);
        if (indent) row = row.replace("<div class='vgr-tt-row'>", "<div class='vgr-tt-row' style='padding-left:16px'>");
        out += row;
      });
      return out;
    }

    // -------------------------------------------------------------------------
    //  Kontroller
    // -------------------------------------------------------------------------
    if (IA.index_toggle) {
      vgrD3.buttonGroup(controls, [
        { key: "varde", label: IA.index_labels.varde },
        { key: "index", label: IA.index_labels.index },
        { key: "forandring", label: IA.index_labels.forandring }
      ], state.mode, function (k) { state.mode = k; draw(); });
    }
    var dynamic = controls.append("span").style("display", "inline-flex").style("gap", "8px");
    controls.append("span").attr("class", "vgr-spacer");
    if (IA.export) vgrD3.exportButtons(controls, svg.node(), fileName);
    if (IA.zoom) {
      controls.append("span").style("color", "#78909c").style("font-size", "12px")
        .text("Dra i diagrammet för att zooma");
    }

    function refreshControls() {
      dynamic.selectAll("*").remove();
      if (state.xDomain) {
        vgrD3.button(dynamic, "Återställ zoom", function () { state.xDomain = null; draw(); });
      }
      if (Object.keys(state.hidden).length) {
        vgrD3.button(dynamic, "Visa alla", function () { state.hidden = {}; draw(); });
      }
    }
    if (controls.selectAll("button,input").empty() && !IA.zoom) controls.remove();

    draw();
  };
})();
