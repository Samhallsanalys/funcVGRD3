/* vgr_D3 — spridningsdiagram (func_d3_spridningsdiagram).
 *
 * En punkt per rad. Trendlinjer, referenslinjer, axeldomäner och punkternas radier räknas fram i R (samma
 * beräkning som func_graf_spridningsdiagram), så här ritas bara det som spec:en säger. Hela SVG:en byggs om vid
 * varje tillståndsändring (filtervärde, dolda grupper, zoom, sökning); vid byte av filtervärde startar punkterna
 * där de stod förut och glider till sina nya lägen (kopplade med sin nyckel).
 */
(function () {
  "use strict";
  var vgrD3 = window.vgrD3;
  function pt(v) { return v * 96 / 72; }
  var LINES = pt(11 * 1.2);   // ggplot2-enheten "lines" (11 pt text × radavstånd 1,2)

  vgrD3.renderers.spridningsdiagram = function (el, spec) {
    var W = spec.width, H = spec.height, font = spec.font;
    var T = spec.text, L = spec.legend, LB = spec.labels, TT = spec.tooltip;
    var IA = spec.interactive, F = spec.facets, PT = spec.point;
    var dur = spec.animation.duration || 0;
    var esc = vgrD3.esc;
    var fileName = vgrD3.slug((T.title && T.title.join(" ")) || "spridningsdiagram");

    var root = d3.select(el).style("max-width", W + "px");
    var controls = vgrD3.controls(el, font);
    var svg = root.append("svg").attr("class", "vgr-d3-svg")
      .attr("width", W).attr("height", H).attr("viewBox", "0 0 " + W + " " + H)
      .attr("role", "img").attr("aria-label", (T.title && T.title.join(" ")) || "Spridningsdiagram")
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
    var hasFacets = !(F.levels.length === 1 && F.levels[0] === "__alla");
    var freeX = F.scales === "free" || F.scales === "free_x";
    var freeY = F.scales === "free" || F.scales === "free_y";

    // zoom: per panel {x: [..], y: [..]}
    var state = { filter: spec.filter.default, hidden: {}, zoom: {}, hoverGroup: null, search: null, first: true };

    function colorOf(p) { return spec.has_groups ? groupByKey[p.g].color : PT.color; }
    function shown(p) { return p.k === state.filter && !state.hidden[p.g]; }
    function pointsIn(f) { return spec.points.filter(function (p) { return p.f === f && shown(p); }); }

    // Referenslinjernas lägen för det valda filtervärdet (ett tal eller flera) och text nummer i
    function refsOf(byFilter) {
      var v = byFilter ? byFilter[state.filter] : null;
      return v == null ? [] : [].concat(v);
    }
    function refLabel(labels, i) {
      if (labels == null) return null;
      var list = [].concat(labels);
      return list.length ? list[i % list.length] : null;
    }

    function fmtX(v, dec) { return spec.x.number_format === "whole" ? String(Math.round(v)) : vgrD3.fmt(v, dec, " "); }
    function domainsFor(f) {
      var z = state.zoom[f];
      if (z) return z;
      var fd = F.domains[f] || { x: spec.x.domain, y: spec.y.domain };
      return { x: freeX ? fd.x : spec.x.domain, y: freeY ? fd.y : spec.y.domain };
    }
    function ticksOf(scale, isX) {
      var t = scale.ticks(5);
      if (isX && spec.x.number_format === "whole") t = t.filter(function (v) { return Number.isInteger(v); });
      var dec = t.length > 1 ? d3.precisionFixed(t[1] - t[0]) : 0;
      return t.map(function (v) { return { v: v, label: isX ? fmtX(v, dec) : vgrD3.fmt(v, dec, " ") }; });
    }

    // -------------------------------------------------------------------------
    //  Förklaring: färg per grupp (klickbar) och punktstorlek
    // -------------------------------------------------------------------------
    function drawLegend(area) {
      var showColor = spec.has_groups, showSize = !!spec.size;
      if (L.position === "none" || (!showColor && !showSize)) return;
      var vertical = L.position === "left" || L.position === "right";
      var rowH = Math.max(L.text_size * 1.3, pt(17.28));
      var blocks = [];
      if (showColor) {
        blocks.push({ title: L.title, items: groups.map(function (g) {
          return { kind: "color", g: g, r: Math.min(rowH * 0.3, 6), label: g.label };
        }) });
      }
      if (showSize) {
        blocks.push({ title: spec.size.title, items: spec.size.breaks.map(function (b) {
          return { kind: "size", r: b.r, label: b.label };
        }) });
      }
      // Mät: symbolbredd = största radien i blocket, rader minst så höga som symbolen
      blocks.forEach(function (b) {
        b.symW = 2 * d3.max(b.items, function (it) { return it.r; }) + 4;
        b.items.forEach(function (it) {
          it.h = Math.max(rowH, 2 * it.r + 4);
          it.w = b.symW + 6 + vgrD3.measureText(svg, it.label, L.text_size, font);
        });
        b.titleW = b.title ? vgrD3.measureText(svg, b.title, L.title_size, font) : 0;
      });
      var gap = pt(11), lw = 0, lh = 0;
      if (vertical) {
        blocks.forEach(function (b, bi) {
          b.y = lh + (bi > 0 ? pt(8) : 0);
          var y = b.y + (b.title ? L.title_size * 1.6 : 0);
          b.items.forEach(function (it) { it.x = 0; it.y = y; y += it.h; });
          lh = y; lw = Math.max(lw, b.titleW, d3.max(b.items, function (it) { return it.w; }));
        });
        lw += 8;
      } else {
        var avail = area.x1 - area.x0, x = 0, y = 0, lineH = 0;
        blocks.forEach(function (b) {
          if (b.title) {
            if (x > 0 && x + b.titleW > avail) { x = 0; y += lineH; lineH = 0; }
            b.tx = x; b.ty = y; x += b.titleW + 10;
          }
          b.items.forEach(function (it) {
            if (x > 0 && x + it.w > avail) { x = 0; y += lineH; lineH = 0; }
            it.x = x; it.y = y; x += it.w + 16; lineH = Math.max(lineH, it.h); lw = Math.max(lw, x - 16);
          });
          x += 10;
        });
        lh = y + lineH;
      }
      var lx, ly;
      if (L.position === "right") { lx = area.x1 - lw; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x1 -= lw + gap; }
      else if (L.position === "left") { lx = area.x0; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x0 += lw + gap; }
      else if (L.position === "top") { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y0; area.y0 += lh + gap; }
      else { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y1 - lh; area.y1 -= lh + gap; }

      var lg = svg.append("g").attr("transform", "translate(" + lx + "," + ly + ")");
      blocks.forEach(function (b) {
        if (b.title) {
          var tx = vertical ? 0 : b.tx, ty = vertical ? b.y + L.title_size : b.ty + rowH / 2;
          lg.append("text").attr("x", tx).attr("y", ty).attr("dy", vertical ? null : "0.35em")
            .style("font-size", L.title_size + "px").style("font-family", font).text(b.title);
        }
        var item = lg.selectAll(null).data(b.items).enter().append("g")
          .attr("class", function (it) { return it.kind === "color" ? "vgr-leg" : null; })
          .attr("transform", function (it) { return "translate(" + it.x + "," + it.y + ")"; })
          .attr("opacity", function (it) { return it.kind === "color" && state.hidden[it.g.key] ? 0.35 : 1; })
          .style("cursor", function (it) { return it.kind === "color" && IA.legend ? "pointer" : null; });
        item.append("rect").attr("width", function (it) { return it.w; }).attr("height", function (it) { return it.h; })
          .attr("fill", "#ffffff").attr("fill-opacity", 0);
        item.append("circle").attr("cx", b.symW / 2).attr("cy", function (it) { return it.h / 2; })
          .attr("r", function (it) { return it.r; })
          .attr("fill", function (it) { return it.kind === "color" ? it.g.color : "#9e9e9e"; })
          .attr("fill-opacity", PT.alpha)
          .attr("stroke", PT.border || "none").attr("stroke-width", PT.border ? 0.8 : 0);
        item.append("text").attr("x", b.symW + 6).attr("y", function (it) { return it.h / 2; }).attr("dy", "0.35em")
          .style("font-size", L.text_size + "px").style("font-family", font)
          .text(function (it) { return it.label; });
        if (IA.legend) {
          var colorItems = item.filter(function (it) { return it.kind === "color"; });
          colorItems.append("title").text("Klicka: dölj/visa · Dubbelklicka: visa bara denna");
          colorItems
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
      });
    }

    function setHoverGroup(key) {
      if (key === state.hoverGroup) return;   // anropas vid varje musrörelse: skriv bara om när gruppen byts
      state.hoverGroup = key;
      svg.selectAll(".vgr-pt, .vgr-trend").attr("opacity", function () {
        var g = this.getAttribute("data-group");
        var base = +this.getAttribute("data-opacity");
        return key == null || g === key ? base : base * 0.15;
      });
    }

    // -------------------------------------------------------------------------
    //  Rita allt
    // -------------------------------------------------------------------------
    var panels = [];

    // Punkternas lägen just nu (nyckel -> {x, y, r}), lästa ur SVG:en — mitt i en övergång är det där de faktiskt
    // står, så att ett nytt filterbyte fortsätter därifrån i stället för att hoppa
    function livePositions() {
      var pos = {};
      svg.selectAll("circle.vgr-pt").each(function () {
        pos[this.getAttribute("data-key")] = { x: +this.getAttribute("cx"), y: +this.getAttribute("cy"), r: +this.getAttribute("r") };
      });
      return pos;
    }

    // animateFrom: lägen att glida från (filterbyte), {} = alla punkter växer fram, utelämnad = ingen animation
    function draw(animateFrom) {
      svg.selectAll("*").interrupt();
      svg.selectAll("*").remove();
      tip.hide();
      panels = [];
      state.hoverGroup = null;
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
      if (T.y_axis_title) {
        svg.append("text").attr("text-anchor", "middle")
          .attr("transform", "translate(" + (area.x0 + T.axis_title_size * 0.9) + "," + ((area.y0 + area.y1) / 2) + ") rotate(-90)")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(T.y_axis_title);
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

      // Axelbredd: bredaste y-etiketten i någon panel, så att panelerna linjerar
      var yAxisW = 0;
      F.levels.forEach(function (f) {
        var ys = d3.scaleLinear().domain(domainsFor(f).y);
        ticksOf(ys, false).forEach(function (t) {
          yAxisW = Math.max(yAxisW, vgrD3.measureText(svg, t.label, T.y_axis_text_size, font));
        });
      });
      yAxisW += pt(4);
      // Gemensam y-skala: axeltexten bara i första kolumnen och panelerna delar på den frigjorda bredden (som ggplot)
      var leftShared = hasFacets && ncol > 1 && !freeY && Object.keys(state.zoom).length === 0;
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
        drawPanel(f, i, rect, !leftShared || col === 0, animateFrom);
      });
      return area.x0 + ncol * cellW + (ncol - 1) * sx;
    }

    function drawPanel(facet, idx, rect, yLabels, animateFrom) {
      var dom = domainsFor(facet);
      var xs = d3.scaleLinear().domain(dom.x).range([rect.x0, rect.x1]);
      var ys = d3.scaleLinear().domain(dom.y).range([rect.y1, rect.y0]);
      var xt = ticksOf(xs, true), yt = ticksOf(ys, false);
      var g = svg.append("g").attr("class", "vgr-panel");
      var clipId = el.id + "-clip-" + idx;
      g.append("clipPath").attr("id", clipId).append("rect")
        .attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0).attr("height", rect.y1 - rect.y0);
      var clipped = g.append("g").attr("clip-path", "url(#" + clipId + ")");

      // Rutnät: true/false eller "x"/"y" (linjer från x-axeln = lodräta, från y-axeln = vågräta)
      var grid = clipped.append("g").attr("pointer-events", "none");
      function gridLines(vals, isX, color) {
        grid.selectAll(null).data(vals).enter().append("line")
          .attr("x1", function (v) { return isX ? xs(v) : rect.x0; }).attr("x2", function (v) { return isX ? xs(v) : rect.x1; })
          .attr("y1", function (v) { return isX ? rect.y0 : ys(v); }).attr("y2", function (v) { return isX ? rect.y1 : ys(v); })
          .attr("stroke", color).attr("stroke-width", 1.42 * (color === "#f2f2f2" ? 0.5 : 1));
      }
      function mids(v) { var o = []; for (var i = 1; i < v.length; i++) o.push((v[i - 1] + v[i]) / 2); return o; }
      function gridOn(setting, isX) { return setting === true || (setting === "x" && isX) || (setting === "y" && !isX); }
      var vals = function (t) { return t.map(function (d) { return d.v; }); };
      if (gridOn(spec.grid.minor, false)) gridLines(mids(vals(yt)), false, "#f2f2f2");
      if (gridOn(spec.grid.minor, true)) gridLines(mids(vals(xt)), true, "#f2f2f2");
      if (gridOn(spec.grid.major, false)) gridLines(vals(yt), false, "#e5e5e5");
      if (gridOn(spec.grid.major, true)) gridLines(vals(xt), true, "#e5e5e5");

      if (hasFacets) {
        g.append("rect").attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0)
          .attr("height", rect.y1 - rect.y0).attr("fill", "none").attr("stroke", "#b3b3b3").attr("stroke-width", 0.85);
      }

      // Trendband (under punkterna)
      var trends = spec.trends.filter(function (t) {
        return t.k === state.filter && t.f === facet && !(t.g != null && state.hidden[t.g]);
      });
      var TR = spec.trend;
      function trendColor(t) { return TR.per_group && t.g != null ? groupByKey[t.g].color : TR.color; }
      if (TR.se) {
        trends.forEach(function (t) {
          var area = d3.area().x(function (d, i) { return xs(t.x[i]); })
            .y0(function (d, i) { return ys(t.lo[i]); }).y1(function (d, i) { return ys(t.hi[i]); });
          clipped.append("path").attr("class", "vgr-trend").attr("data-group", t.g).attr("data-opacity", 0.15)
            .attr("d", area(t.x)).attr("fill", trendColor(t)).attr("opacity", 0.15).attr("pointer-events", "none");
        });
      }

      // Referenslinjer (som func_graf_spridningsdiagram: x streckad, y heldragen), en eller flera, med valfri text:
      // en text per linje, färre texter än linjer återanvänds i tur och ordning (som func_d3_linjediagram)
      var refText = { fill: "#37474F", size: (13 * (spec.text_scale || 1)) + "px" };
      refsOf(spec.ref.y).forEach(function (ry, i) {
        clipped.append("line").attr("x1", rect.x0).attr("x2", rect.x1).attr("y1", ys(ry)).attr("y2", ys(ry))
          .attr("stroke", "#000000").attr("stroke-width", 1.42);
        var lab = refLabel(spec.ref.y_label, i);
        if (lab) {
          clipped.append("text").attr("x", rect.x1 - 4).attr("y", ys(ry) - 5).attr("text-anchor", "end")
            .attr("fill", refText.fill).attr("stroke", "#ffffff").attr("stroke-width", 3).attr("paint-order", "stroke")
            .style("font-size", refText.size).style("font-family", font).text(lab);
        }
      });
      refsOf(spec.ref.x).forEach(function (rx, i) {
        clipped.append("line").attr("x1", xs(rx)).attr("x2", xs(rx)).attr("y1", rect.y0).attr("y2", rect.y1)
          .attr("stroke", "#000000").attr("stroke-width", 1.42).attr("stroke-dasharray", "5.7 5.7");
        var lab = refLabel(spec.ref.x_label, i);
        if (lab) {
          clipped.append("text").attr("x", xs(rx) + 5).attr("y", rect.y0 + 14)
            .attr("fill", refText.fill).attr("stroke", "#ffffff").attr("stroke-width", 3).attr("paint-order", "stroke")
            .style("font-size", refText.size).style("font-family", font).text(lab);
        }
      });

      // Punkter: de som visas i panelen; startar där de stod förut (filterbyte) eller växer fram (första ritningen)
      var pts = pointsIn(facet);
      var animate = dur > 0 && ((state.first && spec.animation.grow) || !!animateFrom);
      var ptG = clipped.append("g");
      var circles = ptG.selectAll("circle").data(pts).enter().append("circle")
        .attr("class", "vgr-pt").attr("data-group", function (p) { return p.g; }).attr("data-key", function (p) { return p.key; })
        .attr("data-opacity", PT.alpha).attr("opacity", PT.alpha)
        .attr("fill", colorOf)
        .attr("stroke", PT.border || "none").attr("stroke-width", PT.border ? 0.8 : 0)
        .attr("pointer-events", "none");
      if (animate) {
        circles
          .attr("cx", function (p) { var o = animateFrom && animateFrom[p.key]; return o ? o.x : xs(p.x); })
          .attr("cy", function (p) { var o = animateFrom && animateFrom[p.key]; return o ? o.y : ys(p.y); })
          .attr("r", function (p) { var o = animateFrom && animateFrom[p.key]; return o ? o.r : 0; })
          .transition().duration(dur).ease(d3.easeCubicInOut)
          .attr("cx", function (p) { return xs(p.x); }).attr("cy", function (p) { return ys(p.y); })
          .attr("r", function (p) { return p.r; });
      } else {
        circles.attr("cx", function (p) { return xs(p.x); }).attr("cy", function (p) { return ys(p.y); })
          .attr("r", function (p) { return p.r; });
      }

      // Trendlinjer ovanpå punkterna
      trends.forEach(function (t) {
        var line = d3.line().x(function (d, i) { return xs(t.x[i]); }).y(function (d, i) { return ys(t.y[i]); });
        clipped.append("path").attr("class", "vgr-trend").attr("data-group", t.g).attr("data-opacity", 1)
          .attr("d", line(t.x)).attr("fill", "none").attr("stroke", trendColor(t)).attr("stroke-width", TR.width)
          .attr("pointer-events", "none");
      });

      // Sökträff: ring runt punkten
      if (state.search != null) {
        pts.filter(function (p) { return p.lab === state.search; }).forEach(function (p) {
          clipped.append("circle").attr("cx", xs(p.x)).attr("cy", ys(p.y)).attr("r", p.r + 5)
            .attr("fill", "none").attr("stroke", "#263238").attr("stroke-width", 2).attr("pointer-events", "none");
        });
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
        .attr("x", function (t) { return xs(t.v); }).attr("y", rect.y1 + pt(3)).attr("dy", "0.85em")
        .attr("text-anchor", "middle").attr("fill", "#4d4d4d")
        .style("font-size", T.x_axis_text_size + "px").style("font-family", font).text(function (t) { return t.label; });

      // Etiketter
      drawLabels(clipped, pts, rect, xs, ys, animate);

      // Interaktion: dra en ruta = zoom, dubbelklick = tillbaka, hover = närmaste punkt
      var hoverG = g.append("g").attr("pointer-events", "none");
      var overlay = g.append("g").attr("class", "vgr-overlay");
      if (IA.zoom) {
        var brush = d3.brush().extent([[rect.x0, rect.y0], [rect.x1, rect.y1]])
          .on("end", function (event) {
            var s = event.selection;
            if (!s) return;
            if (Math.abs(s[1][0] - s[0][0]) < 8 || Math.abs(s[1][1] - s[0][1]) < 8) { overlay.call(brush.move, null); return; }
            state.zoom[facet] = { x: [xs.invert(s[0][0]), xs.invert(s[1][0])], y: [ys.invert(s[1][1]), ys.invert(s[0][1])] };
            draw();
          });
        overlay.call(brush);
        overlay.select(".selection").attr("fill", "#37474F").attr("fill-opacity", 0.12).attr("stroke", "none");
      } else {
        overlay.append("rect").attr("x", rect.x0).attr("y", rect.y0)
          .attr("width", rect.x1 - rect.x0).attr("height", rect.y1 - rect.y0).attr("fill", "#ffffff").attr("fill-opacity", 0);
      }
      var panel = { rect: rect, xs: xs, ys: ys, hoverG: hoverG, pts: pts };
      panels.push(panel);
      overlay
        .on("mousemove.vgr", function (event) { hover(panel, event); })
        .on("mouseleave.vgr", function () { hoverG.selectAll("*").remove(); tip.hide(); setHoverGroup(null); })
        .on("dblclick.vgr", function () { if (state.zoom[facet]) { delete state.zoom[facet]; draw(); } });
    }

    function drawLabels(layer, pts, rect, xs, ys, animate) {
      var items = [];
      pts.forEach(function (p) {
        if (!p.show_lab || p.lab == null) return;
        var px = xs(p.x), py = ys(p.y);
        if (px < rect.x0 || px > rect.x1 || py < rect.y0 || py > rect.y1) return;   // utanför inzoomningen
        var ay = LB.repel ? py : py - p.r - LB.size * 0.7;   // utan repel: strax ovanför punkten (som vjust -0.8)
        items.push({ text: p.lab, color: LB.color === "auto" ? colorOf(p) : LB.color,
                     ax: px, ay: ay, x: px, y: ay, ox: px, oy: py, r: p.r });
      });
      if (!items.length) return;
      var labG = layer.append("g");
      var lines = labG.append("g");
      var nodes = vgrD3.labelNodes(labG, items, { size: LB.size, font: font, box: LB.box, box_color: LB.box_color,
                                                  box_alpha: LB.box_alpha });
      if (LB.repel) {
        // Etiketterna skjuts bort från varandra och från alla punkter i panelen (som ggrepel)
        vgrD3.relaxLabels(items, rect, {
          sep: pt(3), direction: "both", maxOverlaps: LB.max_overlaps,
          points: pts.map(function (p) { return { x: xs(p.x), y: ys(p.y), r: p.r + pt(2) }; })
        });
        vgrD3.leaderLines(lines, items, { color: "#7f7f7f", width: 0.85 });
      }
      nodes.place();
      if (animate) labG.attr("opacity", 0).transition().delay(dur * 0.8).duration(300).attr("opacity", 1);
    }

    // -------------------------------------------------------------------------
    //  Tooltip: närmaste punkt inom 20 px
    // -------------------------------------------------------------------------
    function tipHtml(p) {
      var title = spec.has_label_col && p.lab != null ? p.lab : (spec.has_groups ? groupByKey[p.g].label : "");
      var html = title ? "<div class='vgr-tt-title'>" + esc(title) + "</div>" : "";
      var sub = [];
      if (hasFacets) sub.push(p.f);
      if (spec.filter.enabled) sub.push(state.filter);
      if (sub.length) html += "<div class='vgr-tt-sub'>" + esc(sub.join(" · ")) + "</div>";
      if (spec.has_groups && spec.has_label_col) html += vgrD3.tooltipRow(L.title || "Grupp", groupByKey[p.g].label, colorOf(p));
      html += vgrD3.tooltipRow(TT.x_title, fmtX(p.x, TT.x_decimals));
      html += vgrD3.tooltipRow(TT.y_title, vgrD3.fmt(p.y, TT.y_decimals, " ") + TT.unit);
      if (TT.size_title != null && p.s != null) html += vgrD3.tooltipRow(TT.size_title, vgrD3.fmt(p.s, TT.size_decimals, " "));
      (TT.columns || []).forEach(function (label, j) {
        var val = p.tt ? p.tt[j] : null;
        if (val != null) html += vgrD3.tooltipRow(label, val);
      });
      return html;
    }

    function hover(panel, event) {
      var pos = d3.pointer(event, svg.node());
      var hg = panel.hoverG;
      hg.selectAll("*").remove();
      if (!TT.show) return;
      var best = null, bestD = Infinity, R = panel.rect;
      panel.pts.forEach(function (p) {
        var px = panel.xs(p.x), py = panel.ys(p.y);
        if (px < R.x0 || px > R.x1 || py < R.y0 || py > R.y1) return;   // utanför inzoomningen: syns inte
        var d = Math.pow(px - pos[0], 2) + Math.pow(py - pos[1], 2);
        if (d < bestD) { bestD = d; best = p; }
      });
      if (!best || Math.sqrt(bestD) > Math.max(20, best.r + 6)) { tip.hide(); setHoverGroup(null); return; }
      if (spec.has_groups) setHoverGroup(best.g);
      hg.append("circle").attr("cx", panel.xs(best.x)).attr("cy", panel.ys(best.y)).attr("r", best.r + 3)
        .attr("fill", "none").attr("stroke", "#263238").attr("stroke-width", 1.5);
      tip.show(tipHtml(best), event);
    }

    // -------------------------------------------------------------------------
    //  Kontroller: filter med uppspelning, sökruta, återställ, export
    // -------------------------------------------------------------------------
    var timer = null, playBtn = null, filterSel = null;
    function stopPlay() { if (timer) { timer.stop(); timer = null; } if (playBtn) playBtn.text("▶"); }
    // Med etikettkolumn glider varje punkt från där den står till sitt nya läge. Utan den finns inget som
    // identifierar en punkt mellan filtervärdena, så punkterna växer fram på nytt i stället
    function setFilter(k) {
      var from = spec.has_label_col ? livePositions() : {};
      state.filter = k;
      if (filterSel) filterSel.property("value", k);
      draw(from);
    }
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
    if (IA.search) {
      var names = Array.from(new Set(spec.points.map(function (p) { return p.lab; }).filter(function (v) { return v != null; }))).sort();
      var listId = el.id + "-sok";
      var input = controls.append("input").attr("type", "search").attr("placeholder", "Sök punkt …")
        .attr("aria-label", "Sök punkt").attr("list", listId)   // utseendet från kärnans CSS för sökrutor
        .on("input", function () {
          var v = this.value.trim().toLowerCase();
          var hit = names.find(function (n) { return n.toLowerCase() === v; });
          if (hit === undefined) hit = null;
          if (hit === state.search) return;   // ritas bara om när träffen ändras, inte vid varje tangent
          state.search = hit;
          // Finns punkten inte i det valda filtervärdet (t.ex. året): byt till det senaste värde där den finns
          if (hit != null && spec.filter.enabled &&
              !spec.points.some(function (p) { return p.lab === hit && p.k === state.filter; })) {
            var finns = spec.filter.values.filter(function (k) {
              return spec.points.some(function (p) { return p.lab === hit && p.k === k; });
            });
            if (finns.length) { stopPlay(); state.filter = finns[finns.length - 1]; filterSel.property("value", state.filter); }
          }
          draw();
          if (hit != null) showSearchTip(hit);
        });
      controls.append("datalist").attr("id", listId).selectAll("option").data(names).join("option")
        .attr("value", function (d) { return d; });
    }
    // Tooltipen vid sökträffen (i widgetens koordinater: SVG:en kan vara nedskalad)
    function showSearchTip(name) {
      if (!TT.show) return;
      for (var i = 0; i < panels.length; i++) {
        var pnl = panels[i];
        var p = pnl.pts.find(function (q) { return q.lab === name; });
        if (!p) continue;
        var node = svg.node(), k = node.clientWidth / W;
        tip.showAt(tipHtml(p), node.offsetLeft + pnl.xs(p.x) * k, node.offsetTop + pnl.ys(p.y) * k);
        return;
      }
    }
    var dynamic = controls.append("span").style("display", "inline-flex").style("gap", "8px");
    controls.append("span").attr("class", "vgr-spacer");
    if (IA.export) vgrD3.exportButtons(controls, svg.node(), fileName);
    if (IA.zoom) {
      controls.append("span").style("color", "#78909c").style("font-size", "12px").text("Dra i diagrammet för att zooma");
    }
    function refreshControls() {
      dynamic.selectAll("*").remove();
      if (Object.keys(state.zoom).length) vgrD3.button(dynamic, "Återställ zoom", function () { state.zoom = {}; draw(); });
      if (Object.keys(state.hidden).length) vgrD3.button(dynamic, "Visa alla", function () { state.hidden = {}; draw(); });
    }
    if (controls.selectAll("button,select,input").empty() && !IA.zoom) controls.remove();

    draw();
  };
})();
