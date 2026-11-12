/* vgr_D3 — histogram (func_d3_histogram).
 *
 * Intervallen (gränserna) och antalet per filtervärde, panel, grupp och intervall kommer från R (samma beräkning
 * som func_graf_histogram). Här räknas staplarnas höjd fram efter läge: antal, andel eller täthet — andelen av
 * panelen när grupperna staplas, av gruppen när de står bredvid varandra eller över varandra. Hela SVG:en byggs
 * om vid varje ändring; vid byte av filtervärde växer staplarna från sina tidigare höjder.
 */
(function () {
  "use strict";
  var vgrD3 = window.vgrD3;
  function pt(v) { return v * 96 / 72; }
  var LINES = pt(11 * 1.2);
  var SEP = "\u0001";

  vgrD3.renderers.histogram = function (el, spec) {
    var W = spec.width, H = spec.height, font = spec.font;
    var T = spec.text, L = spec.legend, LB = spec.labels, TT = spec.tooltip;
    var IA = spec.interactive, F = spec.facets, B = spec.bar;
    var dur = spec.animation.duration || 0;
    var esc = vgrD3.esc;
    var fileName = vgrD3.slug((T.title && T.title.join(" ")) || "histogram");

    var root = d3.select(el).style("max-width", W + "px");
    var controls = vgrD3.controls(el, font);
    var svg = root.append("svg").attr("class", "vgr-d3-svg")
      .attr("width", W).attr("height", H).attr("viewBox", "0 0 " + W + " " + H)
      .attr("role", "img").attr("aria-label", (T.title && T.title.join(" ")) || "Histogram")
      .attr("font-family", font)
      .style("width", "100%").style("height", "auto").style("display", "block");
    var tip = vgrD3.tooltip(el, font);

    if (spec.empty) {
      svg.append("text").attr("x", W / 2).attr("y", H / 2).attr("text-anchor", "middle")
        .style("font-size", (18 * (spec.text_scale || 1)) + "px").text("Inga data att visa");
      return;
    }

    var edges = spec.bins.edges, bw = spec.bins.width, nBins = edges.length - 1;
    var groups = spec.has_groups ? spec.groups : [{ key: "__alla", label: "", color: B.color }];
    var groupByKey = {};
    groups.forEach(function (g) { groupByKey[g.key] = g; });
    var hasFacets = !(F.levels.length === 1 && F.levels[0] === "__alla");
    var countMap = {};
    spec.counts.forEach(function (c) { countMap[c.k + SEP + c.f + SEP + c.g + SEP + c.b] = c.n; });

    var state = { filter: spec.filter.default, hidden: {}, type: B.type, ymode: B.y, first: true };

    function visibleGroups() { return groups.filter(function (g) { return !state.hidden[g.key]; }); }
    function alphaNow() { return B.alpha != null ? B.alpha : (spec.has_groups && state.type === "overlay" ? 0.5 : 1); }

    // Staplar för ett filtervärde och en panel: {key, g, b, n, y, y0, y1, x0, x1, den}. Sparas per ritning
    // (barsCache töms i draw()), eftersom y-axeln, x-axeln och panelen frågar efter samma staplar
    var barsCache = {};
    function barsFor(k, f) {
      var ck = k + SEP + f;
      if (!barsCache[ck]) barsCache[ck] = computeBars(k, f);
      return barsCache[ck];
    }
    function computeBars(k, f) {
      var vis = visibleGroups(), out = [];
      var stacked = !spec.has_groups || state.type === "stacked";
      // Nämnarna räknas över alla grupper, även dolda: att dölja en grupp i förklaringen ändrar inte de andras
      // andelar (staplat = andel av panelen, som func_graf_histogram)
      var panelTotal = 0, groupTotal = {};
      groups.forEach(function (g) {
        groupTotal[g.key] = 0;
        for (var b = 0; b < nBins; b++) {
          var n = countMap[k + SEP + f + SEP + g.key + SEP + b] || 0;
          groupTotal[g.key] += n; panelTotal += n;
        }
      });
      var cum = [];
      for (var b = 0; b < nBins; b++) {
        cum[b] = 0;
        vis.forEach(function (g, j) {
          var n = countMap[k + SEP + f + SEP + g.key + SEP + b] || 0;
          var den = stacked ? panelTotal : groupTotal[g.key];
          var y = state.ymode === "count" ? n : den > 0 ? (state.ymode === "percent" ? n / den : n / (den * bw)) : 0;
          var x0 = edges[b], x1 = edges[b + 1];
          if (spec.has_groups && state.type === "grouped") { x0 = edges[b] + bw * j / vis.length; x1 = edges[b] + bw * (j + 1) / vis.length; }
          var y0 = stacked ? cum[b] : 0;
          if (stacked) cum[b] += y;
          out.push({ key: f + SEP + g.key + SEP + b, g: g.key, b: b, n: n, y: y, y0: y0, y1: y0 + y, x0: x0, x1: x1, den: den });
        });
      }
      return out;
    }

    function fmtX(v) {
      var dec = Math.max(0, d3.precisionFixed(bw));
      return spec.x.number_format === "whole" ? String(Math.round(v)) : vgrD3.fmt(v, dec, " ");
    }
    function fmtY(v, dec) {
      if (state.ymode === "count") return vgrD3.fmt(v, 0, " ");
      if (state.ymode === "percent") return vgrD3.fmt(100 * v, dec, " ") + " %";
      return vgrD3.fmt(v, dec, " ");
    }
    // x-axelns markeringar på intervallgränserna (var, varannan ... så att det blir högst cirka 10), som i R
    var step = Math.ceil(edges.length / 10);
    var xTickVals = edges.filter(function (e, i) { return i % step === 0; });

    function xDomainFor(f) {
      var lo = edges[0], hi = edges[nBins];
      if (F.scales === "free" || F.scales === "free_x") {
        var used = barsFor(state.filter, f).filter(function (d) { return d.n > 0; });
        if (used.length) { lo = d3.min(used, function (d) { return edges[d.b]; }); hi = d3.max(used, function (d) { return edges[d.b + 1]; }); }
      }
      var pad = (hi - lo) * 0.02;
      return [spec.x.min != null ? spec.x.min : lo - pad, spec.x.max != null ? spec.x.max : hi + pad];
    }
    function yMaxFor(facets) {
      // y_axis_max gäller i det läge (antal/andel/täthet) som den angavs för
      if (spec.y.max != null && state.ymode === B.y) return spec.y.max;
      var keys = spec.filter.same_y ? spec.filter.values : [state.filter];
      var mx = 0;
      keys.forEach(function (k) {
        facets.forEach(function (f) { barsFor(k, f).forEach(function (d) { mx = Math.max(mx, d.y1); }); });
      });
      return (mx || 1) * (LB.show ? 1.12 : 1.05);
    }

    // -------------------------------------------------------------------------
    //  Förklaring (klickbar)
    // -------------------------------------------------------------------------
    function drawLegend(area) {
      if (!spec.has_groups || L.position === "none") return;
      var rowH = Math.max(L.text_size * 1.3, pt(17.28));
      var sw = pt(12);
      var vertical = L.position === "left" || L.position === "right";
      var items = groups.map(function (g) { return { g: g, w: sw + 6 + vgrD3.measureText(svg, g.label, L.text_size, font) }; });
      var titleW = L.title ? vgrD3.measureText(svg, L.title, L.title_size, font) : 0;
      var titleH = L.title && vertical ? L.title_size * 1.6 : 0;
      var lw, lh;
      if (vertical) {
        items.forEach(function (it, i) { it.x = 0; it.y = titleH + i * rowH; });
        lw = Math.max(titleW, d3.max(items, function (it) { return it.w; })) + 8;
        lh = titleH + items.length * rowH;
      } else {
        var startX = L.title ? titleW + 10 : 0, avail = area.x1 - area.x0, x = startX, y = 0, maxX = 0;
        items.forEach(function (it) {
          if (x > startX && x + it.w > avail) { x = startX; y += rowH; }
          it.x = x; it.y = y; x += it.w + 16; maxX = Math.max(maxX, x - 16);
        });
        lw = maxX; lh = y + rowH;
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
      item.append("rect").attr("width", function (it) { return it.w; }).attr("height", rowH).attr("fill", "#ffffff").attr("fill-opacity", 0);
      item.append("rect").attr("y", (rowH - sw) / 2).attr("width", sw).attr("height", sw)
        .attr("fill", function (it) { return it.g.color; }).attr("fill-opacity", alphaNow());
      item.append("text").attr("x", sw + 6).attr("y", rowH / 2).attr("dy", "0.35em")
        .style("font-size", L.text_size + "px").style("font-family", font).text(function (it) { return it.g.label; });
      if (IA.legend) {
        item.append("title").text("Klicka: dölj/visa · Dubbelklicka: visa bara denna");
        item.on("click", function (event, it) {
          if (state.hidden[it.g.key]) delete state.hidden[it.g.key]; else state.hidden[it.g.key] = true;
          draw();
        }).on("dblclick", function (event, it) {
          state.hidden = {};
          groups.forEach(function (g) { if (g.key !== it.g.key) state.hidden[g.key] = true; });
          draw();
        });
      }
    }

    // -------------------------------------------------------------------------
    //  Rita allt
    // -------------------------------------------------------------------------
    // Staplarnas lägen just nu (nyckel -> {y, h}), lästa ur SVG:en — mitt i en övergång är det där de faktiskt
    // står, så att nästa byte fortsätter därifrån i stället för att hoppa
    function liveBars() {
      var pos = {};
      svg.selectAll("rect.vgr-bar").each(function () {
        pos[this.getAttribute("data-key")] = { y: +this.getAttribute("y"), h: +this.getAttribute("height") };
      });
      return pos;
    }

    // animateFrom: lägen att växa från (filter- eller lägesbyte), utelämnad = ingen animation
    function draw(animateFrom) {
      svg.selectAll("*").interrupt();
      svg.selectAll("*").remove();
      tip.hide();
      barsCache = {};
      svg.append("rect").attr("width", W).attr("height", H).attr("fill", "#ffffff");

      var m = { t: pt(hasFacets && F.wrapped ? 20 : 15), r: pt(10), b: pt(10), l: pt(10) };
      var box = { x0: m.l, y0: m.t, x1: W - m.r, y1: H - m.b };
      var y = box.y0;
      var th = vgrD3.textBlock(svg, T.title, { x: box.x0, y: y, size: T.title_size, font: font, anchor: "start", lineHeight: 1.1 });
      y += th + (th ? pt(5) : 0);
      var sh = vgrD3.textBlock(svg, T.subtitle, { x: box.x0, y: y, size: T.subtitle_size, font: font, anchor: "start", lineHeight: 1.2 });
      y += sh + (sh ? pt(5) : 0);
      if (th || sh) y += pt(5);
      var bottom = box.y1;
      var ch = T.caption.length * T.caption_size * 1.2;
      if (ch) bottom -= ch + pt(5.5);
      var area = { x0: box.x0, y0: y, x1: box.x1, y1: bottom };
      drawLegend(area);
      if (T.x_axis_title) {
        svg.append("text").attr("text-anchor", "middle")
          .attr("transform", "translate(" + ((area.x0 + area.x1) / 2) + "," + (area.y1 - T.axis_title_size * 0.25) + ")")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(T.x_axis_title);
        area.y1 -= T.axis_title_size * 1.5;
      }
      // y-axelns titel följer läget när det har växlats (annars den angivna titeln)
      var ytitle = state.ymode === B.y ? T.y_axis_title : (state.ymode === "count" ? "Antal" : state.ymode === "percent" ? "Andel" : "Täthet");
      if (ytitle) {
        svg.append("text").attr("text-anchor", "middle")
          .attr("transform", "translate(" + (area.x0 + T.axis_title_size * 0.9) + "," + ((area.y0 + area.y1) / 2) + ") rotate(-90)")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(ytitle);
        area.x0 += T.axis_title_size * 1.6;
      }
      var panelRight = layoutPanels(area, animateFrom);
      if (ch) vgrD3.textBlock(svg, T.caption, { x: panelRight, y: box.y1 - ch, size: T.caption_size, font: font, anchor: "end" });
      refreshControls();
      state.first = false;
    }

    function layoutPanels(area, animateFrom) {
      var n = F.levels.length;
      var ncol = Math.min(F.ncol, n), nrow = Math.ceil(n / ncol);
      var sx = hasFacets ? (F.wrapped ? 0.3 : 0.6) * LINES : 0;
      var sy = hasFacets ? (F.wrapped ? 0.2 : 0.8) * LINES : 0;
      var stripLines = d3.max(F.levels, function (f) { return String(f).split("\n").length; });
      var stripH = hasFacets ? stripLines * T.facet_title_size * 0.9 + pt(F.wrapped ? 14 : 16) : 0;
      var xAxisH = T.x_axis_text_size + pt(5);
      var freeY = F.scales === "free" || F.scales === "free_y";
      var sharedMax = freeY ? null : yMaxFor(F.levels);
      var yMax = F.levels.map(function (f) { return sharedMax != null ? sharedMax : yMaxFor([f]); });
      var yAxisW = 0;
      yMax.forEach(function (mx) {
        yTicks(d3.scaleLinear().domain([0, mx])).forEach(function (t) {
          yAxisW = Math.max(yAxisW, vgrD3.measureText(svg, t.label, T.y_axis_text_size, font));
        });
      });
      yAxisW += pt(4);
      var leftShared = hasFacets && ncol > 1 && !freeY;
      var cellW = (area.x1 - area.x0 - (ncol - 1) * sx) / ncol;
      var cellH = (area.y1 - area.y0 - (nrow - 1) * sy) / nrow;
      var panelW = (area.x1 - area.x0 - yAxisW - (ncol - 1) * sx) / ncol;
      F.levels.forEach(function (f, i) {
        var col = i % ncol, row = Math.floor(i / ncol);
        var cx = area.x0 + col * (cellW + sx), cy = area.y0 + row * (cellH + sy);
        var rect = { x0: cx + yAxisW, y0: cy + stripH, x1: cx + cellW, y1: cy + cellH - xAxisH };
        if (leftShared) { rect.x0 = area.x0 + yAxisW + col * (panelW + sx); rect.x1 = rect.x0 + panelW; }
        if (hasFacets) {
          vgrD3.textBlock(svg, String(f).split("\n"), {
            x: (rect.x0 + rect.x1) / 2, y: cy + pt(F.wrapped ? 6 : 4), size: T.facet_title_size, font: font, lineHeight: 0.9
          });
        }
        drawPanel(f, i, rect, yMax[i], !leftShared || col === 0, animateFrom);
      });
      return area.x0 + ncol * cellW + (ncol - 1) * sx;
    }

    function yTicks(ys) {
      var t = ys.ticks(5);
      var dec = t.length > 1 ? d3.precisionFixed(state.ymode === "percent" ? 100 * (t[1] - t[0]) : t[1] - t[0]) : 0;
      return t.map(function (v) { return { v: v, label: fmtY(v, dec) }; });
    }

    function drawPanel(facet, idx, rect, yMax, yLabels, animateFrom) {
      var xs = d3.scaleLinear().domain(xDomainFor(facet)).range([rect.x0, rect.x1]);
      var ys = d3.scaleLinear().domain([0, yMax]).range([rect.y1, rect.y0]);
      var yt = yTicks(ys);
      var xt = xTickVals.filter(function (v) { return v >= xs.domain()[0] - 1e-9 && v <= xs.domain()[1] + 1e-9; });
      var g = svg.append("g").attr("class", "vgr-panel");
      var clipId = el.id + "-clip-" + idx;
      g.append("clipPath").attr("id", clipId).append("rect")
        .attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0).attr("height", rect.y1 - rect.y0);
      var clipped = g.append("g").attr("clip-path", "url(#" + clipId + ")");

      var grid = clipped.append("g").attr("pointer-events", "none");
      function gridLines(vals, isX, color) {
        grid.selectAll(null).data(vals).enter().append("line")
          .attr("x1", function (v) { return isX ? xs(v) : rect.x0; }).attr("x2", function (v) { return isX ? xs(v) : rect.x1; })
          .attr("y1", function (v) { return isX ? rect.y0 : ys(v); }).attr("y2", function (v) { return isX ? rect.y1 : ys(v); })
          .attr("stroke", color).attr("stroke-width", 1.42 * (color === "#f2f2f2" ? 0.5 : 1));
      }
      function mids(v) { var o = []; for (var i = 1; i < v.length; i++) o.push((v[i - 1] + v[i]) / 2); return o; }
      function gridOn(setting, isX) { return setting === true || (setting === "x" && isX) || (setting === "y" && !isX); }
      var yv = yt.map(function (t) { return t.v; });
      if (gridOn(spec.grid.minor, false)) gridLines(mids(yv), false, "#f2f2f2");
      if (gridOn(spec.grid.minor, true)) gridLines(mids(xt), true, "#f2f2f2");
      if (gridOn(spec.grid.major, false)) gridLines(yv, false, "#e5e5e5");
      if (gridOn(spec.grid.major, true)) gridLines(xt, true, "#e5e5e5");
      if (hasFacets) {
        g.append("rect").attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0)
          .attr("height", rect.y1 - rect.y0).attr("fill", "none").attr("stroke", "#b3b3b3").attr("stroke-width", 0.85);
      }

      // Staplar; vid filter- eller lägesbyte från där de står, första gången från 0
      var bars = barsFor(state.filter, facet).filter(function (d) { return d.y > 0; });   // tomma intervall ritas inte
      var animate = dur > 0 && ((state.first && spec.animation.grow) || !!animateFrom);
      var rects = clipped.append("g").selectAll("rect").data(bars).enter().append("rect")
        .attr("class", "vgr-bar").attr("data-key", function (d) { return d.key; })
        .attr("x", function (d) { return xs(d.x0); })
        .attr("width", function (d) { return Math.max(0, xs(d.x1) - xs(d.x0)); })
        .attr("fill", function (d) { return groupByKey[d.g].color; }).attr("fill-opacity", alphaNow())
        .attr("stroke", B.line_color || "none").attr("stroke-width", B.line_color ? B.line_width : 0);
      if (animate) {
        rects.attr("y", function (d) { var o = animateFrom && animateFrom[d.key]; return o ? o.y : ys(d.y0); })
          .attr("height", function (d) { var o = animateFrom && animateFrom[d.key]; return o ? o.h : 0; })
          .transition().duration(dur).ease(d3.easeCubicOut)
          .attr("y", function (d) { return ys(d.y1); }).attr("height", function (d) { return Math.max(0, ys(d.y0) - ys(d.y1)); });
      } else {
        rects.attr("y", function (d) { return ys(d.y1); }).attr("height", function (d) { return Math.max(0, ys(d.y0) - ys(d.y1)); });
      }
      rects.on("mousemove", function (event, d) { showTip(event, d, facet); d3.select(this).attr("fill-opacity", Math.min(1, alphaNow() + 0.25)); })
        .on("mouseleave", function () { tip.hide(); d3.select(this).attr("fill-opacity", alphaNow()); });

      // Referenslinje
      // En eller flera linjer; en text per linje, färre texter än linjer återanvänds i tur och ordning
      var rxv = spec.ref.x ? spec.ref.x[state.filter] : null;
      var rxLabels = spec.ref.x_label == null ? [] : [].concat(spec.ref.x_label);
      (rxv == null ? [] : [].concat(rxv)).forEach(function (rx, i) {
        clipped.append("line").attr("x1", xs(rx)).attr("x2", xs(rx)).attr("y1", rect.y0).attr("y2", rect.y1)
          .attr("stroke", "#000000").attr("stroke-width", 1.42).attr("stroke-dasharray", "5.7 5.7").attr("pointer-events", "none");
        var lab = rxLabels.length ? rxLabels[i % rxLabels.length] : null;
        if (lab) {
          clipped.append("text").attr("x", xs(rx) + 5).attr("y", rect.y0 + 14)
            .attr("fill", "#37474F").attr("stroke", "#ffffff").attr("stroke-width", 3).attr("paint-order", "stroke")
            .style("font-size", (13 * (spec.text_scale || 1)) + "px").style("font-family", font).text(lab);
        }
      });

      // Etiketter: summan per intervall när grupperna staplas, annars en per stapel; tomma intervall ingen
      if (LB.show) {
        var stacked = !spec.has_groups || state.type === "stacked";
        var labs = [];
        // (bars har redan bara staplar med värde, så tomma intervall får ingen etikett)
        if (stacked) {
          // Stapelns topp = summan för intervallet (antal, eller summan av gruppernas andelar)
          var tot = {};
          bars.forEach(function (d) {
            var t = tot[d.b] || (tot[d.b] = { y: 0, x0: edges[d.b], x1: edges[d.b + 1] });
            t.y = Math.max(t.y, d.y1);
          });
          Object.keys(tot).forEach(function (k) { labs.push({ y: tot[k].y, value: tot[k].y, x0: tot[k].x0, x1: tot[k].x1 }); });
        } else {
          bars.forEach(function (d) { labs.push({ y: d.y1, value: d.y, x0: d.x0, x1: d.x1 }); });
        }
        var labG = clipped.append("g").attr("pointer-events", "none");
        labG.selectAll(null).data(labs).enter().append("text")
          .attr("x", function (t) { return (xs(t.x0) + xs(t.x1)) / 2; }).attr("y", function (t) { return ys(t.y) - pt(3); })
          .attr("text-anchor", "middle").attr("fill", LB.color)
          .style("font-size", LB.size + "px").style("font-family", font)
          .text(function (t) { return fmtY(t.value, LB.decimals); });
        if (animate) labG.attr("opacity", 0).transition().delay(dur * 0.8).duration(300).attr("opacity", 1);
      }

      // Axlar
      var axisColor = hasFacets ? "#b3b3b3" : "#000000", axisW = hasFacets ? 0.85 : 1.42;
      if (spec.axis_line.x) g.append("line").attr("x1", rect.x0).attr("x2", rect.x1).attr("y1", rect.y1).attr("y2", rect.y1)
        .attr("stroke", axisColor).attr("stroke-width", axisW);
      if (spec.axis_line.y) g.append("line").attr("x1", rect.x0).attr("x2", rect.x0).attr("y1", rect.y0).attr("y2", rect.y1)
        .attr("stroke", axisColor).attr("stroke-width", axisW);
      g.selectAll(null).data(yLabels ? yt : []).enter().append("text")
        .attr("x", rect.x0 - pt(3)).attr("y", function (t) { return ys(t.v); }).attr("dy", "0.35em")
        .attr("text-anchor", "end").attr("fill", "#4d4d4d")
        .style("font-size", T.y_axis_text_size + "px").style("font-family", font).text(function (t) { return t.label; });
      g.selectAll(null).data(xt).enter().append("text")
        .attr("x", function (v) { return xs(v); }).attr("y", rect.y1 + pt(3)).attr("dy", "0.85em")
        .attr("text-anchor", "middle").attr("fill", "#4d4d4d")
        .style("font-size", T.x_axis_text_size + "px").style("font-family", font).text(function (v) { return fmtX(v); });
    }

    function showTip(event, d, facet) {
      if (!TT.show) return;
      var last = d.b === nBins - 1;
      // Intervallet: nedre gränsen ingår, den övre inte (utom i det sista)
      var html = "<div class='vgr-tt-title'>" + esc(fmtX(edges[d.b]) + "–" + fmtX(edges[d.b + 1]) + (TT.unit || "")) + "</div>";
      var sub = [];
      if (TT.x_title) sub.push(TT.x_title + (last ? " (inklusive " + fmtX(edges[d.b + 1]) + ")" : ""));
      if (hasFacets) sub.push(facet);
      if (spec.filter.enabled) sub.push(state.filter);
      if (sub.length) html += "<div class='vgr-tt-sub'>" + esc(sub.join(" · ")) + "</div>";
      if (spec.has_groups) html += vgrD3.tooltipRow(L.title || "Grupp", groupByKey[d.g].label, groupByKey[d.g].color);
      html += vgrD3.tooltipRow("Antal", vgrD3.fmt(d.n, 0, " "));
      var shareOf = !spec.has_groups || state.type === "stacked" ? (hasFacets ? "av panelen" : "av alla") : "av gruppen";
      html += vgrD3.tooltipRow("Andel " + shareOf, d.den > 0 ? vgrD3.fmt(100 * d.n / d.den, TT.decimals, " ") + " %" : "–");
      tip.show(html, event);
    }

    // -------------------------------------------------------------------------
    //  Kontroller
    // -------------------------------------------------------------------------
    var timer = null, playBtn = null, filterSel = null;
    function stopPlay() { if (timer) { timer.stop(); timer = null; } if (playBtn) playBtn.text("▶"); }
    function setFilter(k) { var from = liveBars(); state.filter = k; if (filterSel) filterSel.property("value", k); draw(from); }
    if (spec.filter.enabled) {
      if (spec.filter.label) controls.append("span").style("color", "#546e7a").text(spec.filter.label + ":");
      filterSel = controls.append("select").attr("aria-label", spec.filter.label || "Filter")
        .style("font", "inherit").style("padding", "3px 6px").style("border", "1px solid #b0bec5").style("border-radius", "4px")
        .on("change", function () { stopPlay(); setFilter(this.value); });
      filterSel.selectAll("option").data(spec.filter.values).join("option")
        .attr("value", function (d) { return d; }).text(function (d) { return d; });
      filterSel.property("value", state.filter);
      playBtn = vgrD3.button(controls, "▶", function () {
        if (timer) { stopPlay(); return; }
        var vals = spec.filter.values;
        if (vals.indexOf(state.filter) >= vals.length - 1) setFilter(vals[0]);
        playBtn.text("❚❚");
        timer = d3.interval(function () {
          var i = vals.indexOf(state.filter);
          if (i >= vals.length - 1) { stopPlay(); return; }
          setFilter(vals[i + 1]);
        }, spec.animation.play_interval);
      }, "Spela upp");
    }
    if (IA.y_toggle) {
      var yOpts = [{ key: "count", label: "Antal" }, { key: "percent", label: "Andel" }];
      if (B.y === "density") yOpts.push({ key: "density", label: "Täthet" });
      vgrD3.buttonGroup(controls, yOpts, state.ymode, function (k) { var from = liveBars(); state.ymode = k; draw(from); });
    }
    if (IA.bar_type_toggle) {
      vgrD3.buttonGroup(controls, [{ key: "stacked", label: "Staplade" }, { key: "grouped", label: "Bredvid" },
                                   { key: "overlay", label: "Överlagrade" }],
                        state.type, function (k) { state.type = k; draw(); });
    }
    var dynamic = controls.append("span").style("display", "inline-flex").style("gap", "8px");
    controls.append("span").attr("class", "vgr-spacer");
    if (IA.export) vgrD3.exportButtons(controls, svg.node(), fileName);
    function refreshControls() {
      dynamic.selectAll("*").remove();
      if (Object.keys(state.hidden).length) vgrD3.button(dynamic, "Visa alla", function () { state.hidden = {}; draw(); });
    }
    if (controls.selectAll("button,select").empty()) controls.style("display", "none");

    draw();
  };
})();
